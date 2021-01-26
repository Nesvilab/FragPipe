/* 
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.dmtavt.fragpipe.tools.fragger;

import com.dmtavt.fragpipe.api.VersionFetcher;
import com.github.chhh.utils.StringUtils;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.http.Header;
import org.apache.http.HeaderElement;
import org.apache.http.HttpEntity;
import org.apache.http.NameValuePair;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.FormBodyPart;
import org.apache.http.entity.mime.FormBodyPartBuilder;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerVersionFetcherServer implements VersionFetcher {

    private final Pattern re = Pattern.compile("([\\d.]+)");
    private String latestVerResponse = null;
    private static final Object lock = new Object();
    
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
        return MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_UPDATESERVER_WEBSITE_URL, "");
    }

    @Override
    public String getToolName() {
        return MsfraggerProps.PROGRAM_NAME;
    }

    @Override
    public boolean canAutoUpdate() {
        return false;
    }

    private String fetchVersionResponse() throws IOException {
        synchronized (lock) {
            String serviceUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_UPDATESERVER_VERSION_URL);
            if (serviceUrl == null) {
                throw new IllegalStateException("Property " + MsfraggerProps.PROP_UPDATESERVER_VERSION_URL + " not found");
            }
            String response = org.apache.commons.io.IOUtils.toString(new URL(serviceUrl), StandardCharsets.UTF_8);
            if (StringUtils.isNullOrWhitespace(response)) {
                throw new IllegalStateException("Update server returned empty string for the latest available version.");
            }
            return response.trim();
        }
    }
    @Override
    public Path autoUpdate(Path p) throws IOException {
        if (p == null || !Files.exists(p) || Files.isDirectory(p))
            throw new IllegalArgumentException("The path to file to be updated must be non-null, must exist and not point to a directory.");
        
        String lastVersionStr = fetchVersion();
        
        String updateSvcUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_UPDATESERVER_UPDATE_URL);
        if (updateSvcUrl == null) {
            throw new IllegalStateException("Obtained properties file didn't contain a URL for the updater service.");
        }
        
        if (StringUtils.isNullOrWhitespace(latestVerResponse)) {
            latestVerResponse = fetchVersionResponse();
        }

        CloseableHttpClient client = HttpClients.createDefault();
        HttpPost post = new HttpPost(updateSvcUrl);

        final FormBodyPart formPartDownload = FormBodyPartBuilder.create()
                .setName("download")
                .setBody(new StringBody(latestVerResponse + "$jar", ContentType.TEXT_PLAIN))
                .build();
        final FormBodyPart formPartTradeinFile = FormBodyPartBuilder.create()
                .setName("jarkey")
                .setBody(new FileBody(p.toFile()))
                .build();

        HttpEntity req = MultipartEntityBuilder.create()
                .addPart(formPartDownload)
                .addPart(formPartTradeinFile)
                .build();
        post.setEntity(req);

        try (final CloseableHttpResponse response = client.execute(post)) {
            final HttpEntity entity = response.getEntity();
            final long contentLength = entity.getContentLength();
            final Header[] headers = response.getAllHeaders();
            String file = null;
            outerLoop:
            for (Header h : headers) {
                for (HeaderElement he : h.getElements()) {
                    if ("attachment".equalsIgnoreCase(he.getName())) {
                        NameValuePair nvp = he.getParameterByName("filename");
                        if (nvp == null)
                            continue;
                        file = nvp.getValue();
                        break outerLoop;
                    }
                }
            }
            
            boolean gotName = !StringUtils.isNullOrWhitespace(file);
            int dot = file != null ? file.lastIndexOf('.') : -1;
                
            final String nameBase = gotName && dot > 0 ? file.substring(0, dot) : "MSFragger-" + lastVersionStr;
            final String nameExt = gotName && dot > 0 ? file.substring(dot) : ".jar";
            Path pathOut = p.resolveSibling(nameBase + nameExt);
            if (Files.exists(pathOut)) {
                // find another suitable name
                pathOut = Files.createTempFile(p.getParent(), nameBase + "_dup_", nameExt);
            }
            final byte[] upgradedFragger = EntityUtils.toByteArray(entity);
            
            try (BufferedOutputStream bos = new BufferedOutputStream(Files.newOutputStream(pathOut))) {
                bos.write(upgradedFragger);
            }
            EntityUtils.consumeQuietly(entity);
            
            return pathOut;
        }
    }
    
}
