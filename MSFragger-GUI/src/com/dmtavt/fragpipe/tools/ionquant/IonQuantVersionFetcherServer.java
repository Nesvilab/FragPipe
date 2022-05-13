/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */
package com.dmtavt.fragpipe.tools.ionquant;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.VersionFetcher;
import com.dmtavt.fragpipe.messages.MessagePhiDlProgress;
import com.dmtavt.fragpipe.tools.philosopher.PhiDownloadProgress;
import com.github.chhh.utils.Holder;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.SwingUtilities;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSink;
import okio.ForwardingSource;
import okio.Okio;
import okio.Source;
import org.jetbrains.annotations.NotNull;

/**
 *
 * @author Dmitry Avtonomov
 */
public class IonQuantVersionFetcherServer implements VersionFetcher {

    private final Pattern re = Pattern.compile("([\\d.]+)");
    private static final String serverUrl = "http://msfragger-upgrader.nesvilab.org/ionquant/";
    private String latestVerResponse = null;
    private String lastVersionStr = null;
    private static final Object lock = new Object();

    private final String name;
    private final String email;
    private final String institution;
    private final boolean receiveEmail;


    public IonQuantVersionFetcherServer() {
        this(null, null, null, false);
    }

    public IonQuantVersionFetcherServer(String name, String email, String institution, boolean receiveEmail) {
        this.name = name;
        this.email = email;
        this.institution = institution;
        this.receiveEmail = receiveEmail;
    }

    @Override
    public String fetchVersion() throws IOException {
        synchronized (lock) {
            String verSvcResponse = fetchVersionResponse();
            Matcher m = re.matcher(verSvcResponse);
            if (m.find()) {
                latestVerResponse = verSvcResponse;
                return m.group(1);
            }

            throw new IllegalStateException("Version string retrieved from the remote service was not recoginsed: '" + verSvcResponse + "'");
        }
    }

    @Override
    public String getDownloadUrl() {
        return serverUrl;
    }

    @Override
    public String getToolName() {
        return "IonQuant";
    }

    @Override
    public boolean canAutoUpdate() {
        return false;
    }

    private String fetchVersionResponse() throws IOException {
        synchronized (lock) {
            String response = org.apache.commons.io.IOUtils.toString(new URL(serverUrl + "/latest_version.php"), StandardCharsets.UTF_8);
            if (StringUtils.isNullOrWhitespace(response)) {
                throw new IllegalStateException("Update server returned empty string for the latest available version.");
            }
            return response.trim();
        }
    }

    @Override
    public Path autoUpdate(Path toolsPath) throws IOException {
        if (toolsPath == null || !Files.exists(toolsPath)) {
            throw new IllegalArgumentException("The path to file to be updated must be non-null, must exist and not point to a directory.");
        }

        if (StringUtils.isNullOrWhitespace(lastVersionStr)) {
            lastVersionStr = fetchVersion();
        }

        Path jarPath = toolsPath.resolve("IonQuant-" + lastVersionStr + ".jar");

        RequestBody requestBody;
        if (receiveEmail) {
            requestBody = new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart("transfer", "academic")
                .addFormDataPart("name", name)
                .addFormDataPart("email", email)
                .addFormDataPart("organization", institution)
                .addFormDataPart("receive_email", "1")
                .addFormDataPart("download", latestVerResponse + "$jar")
                .build();
        } else {
            requestBody = new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart("transfer", "academic")
                .addFormDataPart("name", name)
                .addFormDataPart("email", email)
                .addFormDataPart("organization", institution)
                .addFormDataPart("download", latestVerResponse + "$jar")
                .build();
        }

        OkHttpClient client2 = new OkHttpClient();
        Request request = new Request.Builder().url(serverUrl + "/upgrade_download.php").post(requestBody).build();

        try {
            Response response = client2.newCall(request).execute();
            if (!response.isSuccessful()) {
                throw new IllegalStateException("Request unsuccessful");
            }

            ResponseBody body = response.body();
            if (body == null) {
                throw new IllegalStateException("Null response body during download");
            }

            long contentLength = body.contentLength();

            if (contentLength <= 0) {
                throw new Exception("Could not download MSFragger from the server.");
            }

            final Holder<PhiDownloadProgress> dlProgress = new Holder<>();
            SwingUtilities.invokeLater(() -> {
                dlProgress.obj = new PhiDownloadProgress();
                Bus.registerQuietly(dlProgress.obj);
            });

            try (BufferedSink sink = Okio.buffer(Okio.sink(jarPath))) {
                final AtomicLong received = new AtomicLong(0);
                Source fwd = new ForwardingSource(body.source()) {
                    @Override
                    public long read(@NotNull Buffer sink, long byteCount) throws IOException {
                        long read = super.read(sink, byteCount);
                        long totalRead = received.addAndGet(read);
                        Bus.post(new MessagePhiDlProgress(totalRead, contentLength));
                        return read;
                    }
                };

                long read = 0;
                while (read >= 0) {
                    read = fwd.read(sink.getBuffer(), 8192);
                    if (dlProgress.obj != null && dlProgress.obj.isCancel) {
                        return null;
                    }
                }
            } finally {
                if (dlProgress.obj != null) {
                    Bus.unregister(dlProgress.obj);
                    dlProgress.obj.close();
                }
            }
            body.close();
            response.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        List<Path> possibleBins = PathUtils.findFilesQuietly(toolsPath, path -> path.getFileName().toString().trim().toLowerCase().matches("^ionquant-.+\\.jar$")).sorted(Comparator.reverseOrder()).collect(Collectors.toList());

        if (possibleBins.size() == 0) {
            throw new IllegalStateException("Could not find the downloaded IonQuant.jar.");
        }
        return possibleBins.get(0).normalize();
    }
    
}
