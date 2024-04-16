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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tools.philosopher;

import static com.github.chhh.utils.OsUtils.isWindows;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessageDownloadProgress;
import com.dmtavt.fragpipe.messages.MessagePhilosopherNewBin;
import com.dmtavt.fragpipe.tools.DownloadProgress;
import com.github.chhh.utils.Holder;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import com.github.chhh.utils.ZipUtils;
import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.SwingUtilities;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSink;
import okio.ForwardingSource;
import okio.Okio;
import okio.Source;
import org.jetbrains.annotations.NotNull;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Philosopher {

  private static final Logger log = LoggerFactory.getLogger(Philosopher.class);
  private static final String DOWNLOAD_GITHUB_PAGE_URL = "https://github.com/Nesvilab/philosopher/releases/latest";
  private static final String GITHUB_RELEASE_LATEST_API_URL = Fragpipe.propsFix().getProperty(PhilosopherProps.PROP_DOWNLOAD_URL, "https://api.github.com/repos/Nesvilab/philosopher/releases/latest");
  private static final Pattern reVer = Pattern.compile(".*version[^=]*?=\\s*v?\\.?([^\\s,;]+).*", Pattern.CASE_INSENSITIVE);
  private static final Pattern reVer2 = Pattern.compile("philosopher[-_v]*(\\d+\\.\\d+\\.\\d+[^._]*).*", Pattern.CASE_INSENSITIVE);

  public static void downloadPhilosopherManually() {
    try {
      Desktop.getDesktop()
          .browse(URI.create(DOWNLOAD_GITHUB_PAGE_URL));
    } catch (IOException ex) {
      log.error("Error opening browser to download philosopher", ex);
      SwingUtils.showErrorDialogWithStacktrace(ex, null);
    }
  }

  public static Version validate(String path) throws ValidationException, UnexpectedException {

    String binPath = PathUtils.testBinaryPath(path);
    if (binPath == null) {
      throw new ValidationException("Does not appear to be an executable file: \"" + path + "\"");
    }
    if (binPath.contains(" ")) {
      throw new ValidationException("There are spaces in the path: \"" + path + "\"");
    }
    if (binPath.toLowerCase().endsWith(".exe") && !isWindows()) {
      throw new ValidationException("The Philosopher binary file is a windows version");
    }
    if (!binPath.toLowerCase().endsWith(".exe") && isWindows()) {
      throw new ValidationException("The Philosopher binary file is not a windows version");
    }

    StringBuilder sbVer = new StringBuilder();

    // Try to get the version by file or folder name, which is faster
    Path pp = Paths.get(binPath);
    Matcher matcher = reVer2.matcher(pp.getFileName().toString());
    if (matcher.find()) {
      sbVer.append(matcher.group(1));
    } else {
      matcher = reVer2.matcher(pp.getName(Math.max(0, pp.getNameCount() - 2)).toString());
      if (matcher.find()) {
        sbVer.append(matcher.group(1));
      } else {
        // get the version reported by the current executable
        ProcessBuilder pb = new ProcessBuilder(binPath, "version");
        pb.redirectErrorStream(true);
        ProcessUtils.consumeLines(pb, line -> {
          Matcher mVer = reVer.matcher(line);
          if (mVer.matches()) {
            log.debug("Detected philosopher version: {}", mVer.group(1));
            sbVer.append(mVer.group(1));
          }
          return sbVer.length() == 0;
        });
      }
    }

    if (sbVer.length() == 0) {
      throw new ValidationException("Version string not found for Philosopher");
    }

    final String fragpipeVerMajor = com.dmtavt.fragpipe.Version.version().split("[-_]+")[0];
    final Properties p = Fragpipe.propsFix();
    log.debug("Validating philosopher version compatibility with FragPipe major version: {}",
        fragpipeVerMajor);
    String minPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name
            .startsWith(PhilosopherProps.PROP_LOWEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);
    String maxPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name
            .startsWith(PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);
    log.debug("Figured fragpipe {} requires philosopher versions in range [{}, {}]",
        fragpipeVerMajor, minPhiVer, maxPhiVer);

    final String version = sbVer.toString();
    log.debug("Phi validation, proceeding with Version: {}", version);

    if (StringUtils.isNotBlank(version)) {
      StringBuilder sb = new StringBuilder();
      if (minPhiVer != null && VersionComparator.cmp(version, minPhiVer) < 0) {
        sb.append("This Philosopher version is no longer supported by FragPipe.\n");
        sb.append("Minimum required version: ").append(minPhiVer).append("\n");
      }
      if (maxPhiVer != null && VersionComparator.cmp(version, maxPhiVer) > 0) {
        sb.append("Latest known compatible version: ").append(maxPhiVer).append("\n");
      }
      if (sb.length() > 0) {
        sb.append("<a href=\"").append(DOWNLOAD_GITHUB_PAGE_URL).append("\">Click here</a> to download a newer one.");
        throw new ValidationException(SwingUtils.makeHtml(sb.toString()));
      }
    }

    return new Version(version, false, DOWNLOAD_GITHUB_PAGE_URL);
  }

  public static UpdateInfo checkUpdates(String path)
      throws ValidationException, UnexpectedException {
    String binPath = PathUtils.testBinaryPath(path);
    if (binPath == null) {
      throw new ValidationException("Does not appear to be an executable file");
    }
    try {
      ProcessBuilder pb = new ProcessBuilder(binPath, "version");
      pb.redirectErrorStream(true);

      final Pattern reNewVer = Pattern
          .compile(".*new\\s+version.*available.*", Pattern.CASE_INSENSITIVE);
      final AtomicBoolean isUpdateFound = new AtomicBoolean(false);
      ProcessUtils.consumeLines(pb, line -> {
        Matcher mVer = reNewVer.matcher(line);
        if (mVer.matches()) {
          log.debug("Detected philosopher has an update");
          isUpdateFound.set(true);
          return false;
        }
        return true;
      });

      return new UpdateInfo(isUpdateFound.get(), DOWNLOAD_GITHUB_PAGE_URL);
    } catch (Exception e) {
      throw new UnexpectedException(e);
    }
  }

  public static void downloadPhilosopherAutomatically() throws IOException {
    final Pattern re;
    if (isWindows()) {
      re = Pattern.compile(
          "(/Nesvilab/philosopher/releases/download/v[\\d.]+/philosopher_v[\\d.]+_windows_amd64.zip)");
    } else if (OsUtils.isUnix()) {
      re = Pattern.compile(
          "(/Nesvilab/philosopher/releases/download/v[\\d.]+/philosopher_v[\\d.]+_linux_amd64.zip)");
    } else {
      SwingUtils.showInfoDialog(null, "Automatic downloads for Mac not supported", "Error");
      return;
    }

    OkHttpClient client = new OkHttpClient();
    String html;
    Request request = new Request.Builder().url(GITHUB_RELEASE_LATEST_API_URL).build();
    try (Response response = client.newCall(request).execute()) {
      ResponseBody body = response.body();
      if (body == null) {
        throw new IllegalStateException("Response body was null");
      }
      html = body.string();
    }

    Matcher m = re.matcher(html);
    if (!m.find()) {
      throw new IllegalStateException("Did not find supported download links on the page");
    }
    String link = m.group(1);
    log.debug("Found download link: {}", link);
    if (!link.toLowerCase().startsWith("https://github.com")) {
      link = StringUtils.prependOnce(link, "/");
      link = StringUtils.prependOnce(link, "https://github.com");
      log.debug("Assuming full download link is: {}", link);
    }

    Path dirCache = FragpipeLocations.get().getDirCache();
    String fn = Seq.of(link.split("/")).findLast().orElseThrow(
        () -> new IllegalStateException("Could not get the last part of download URL"));
    Path dlLocation = dirCache.resolve(fn);
    log.debug("Downloading to file: {}", dlLocation);

    boolean isDlSuccess = false;
    try (Response response = client.newCall(new Request.Builder().url(link).build()).execute()) {
      if (!response.isSuccessful()) {
        throw new IllegalStateException("Request unsuccessful");
      }
      try (ResponseBody body = response.body()) {
        if (body == null) {
          throw new IllegalStateException("Null response body during download");
        }
        final long totalDlSize = body.contentLength();
        log.debug("Got response, code: {}", response.code());

        final Holder<DownloadProgress> dlProgress = new Holder<>();
        SwingUtilities.invokeLater(() -> {
          dlProgress.obj = new DownloadProgress();
          Bus.registerQuietly(dlProgress.obj);
        });
        try (BufferedSink sink = Okio.buffer(Okio.sink(dlLocation))) {
          final AtomicLong received = new AtomicLong(0);
          Source fwd = new ForwardingSource(body.source()) {
            @Override
            public long read(@NotNull Buffer sink, long byteCount) throws IOException {
              long read = super.read(sink, byteCount);
              long totalRead = received.addAndGet(read);
              Bus.post(new MessageDownloadProgress(totalRead, totalDlSize));
              //log.debug("read {}", FileUtils.fileSize(totalRead));
              return read;
            }
          };
          log.debug("Start writing to file: {}", dlLocation);

          long read = 0;
          while (read >= 0) {
            read = fwd.read(sink.getBuffer(), 8192);
            if (dlProgress.obj != null && dlProgress.obj.isCancel) {
              log.debug("Download cancelled");
              return;
            }
          }
          //sink.writeAll(fwd);
          isDlSuccess = true;

        } finally {
          log.debug("Download process finished");
          if (dlProgress.obj != null) {
            Bus.unregister(dlProgress.obj);
            dlProgress.obj.close();
          }
        }
      }
    }

    // unzip after downloading
    if (isDlSuccess) {
      log.debug("Download complete: {}", dlLocation);
      Path unzipTo = PathUtils
          .createDirs(FragpipeLocations.get().getDirTools().resolve(fn.substring(0, fn.lastIndexOf("."))));
      ZipUtils.unzip(dlLocation, unzipTo);
      final String bin = isWindows() ? "philosopher.exe" : "philosopher";
      List<Path> possibleBins = PathUtils.findFilesQuietly(unzipTo, p -> Files.isRegularFile(p) && p.getFileName().toString().equalsIgnoreCase(bin)).sorted(Comparator.comparing(Path::toString, String.CASE_INSENSITIVE_ORDER)).collect(Collectors.toList());

      if (possibleBins.size() == 0) {
        throw new IllegalStateException("Could not find Philosopher binary after unpacking zip.");
      } else {
        Bus.post(new MessagePhilosopherNewBin(possibleBins.get(possibleBins.size() - 1).toRealPath().toString()));
      }
    }
  }

  public static class Version {

    public final String version;
    public final boolean isNewVersionFound;
    public final String downloadUrl;

    public Version(String version, boolean isNewVersionFound,
        String downloadUrl) {
      this.version = version;
      this.isNewVersionFound = isNewVersionFound;
      this.downloadUrl = downloadUrl;
    }
  }

  public static class UpdateInfo {

    public final boolean isUpdateAvailable;
    public final String downloadUrl;

    public UpdateInfo(boolean isUpdateAvailable, String downloadUrl) {
      this.isUpdateAvailable = isUpdateAvailable;
      this.downloadUrl = downloadUrl;
    }
  }
}
