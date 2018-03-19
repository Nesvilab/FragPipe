/*
 * Copyright 2018 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.params.fragger;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.StatusLine;
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
import umich.msfragger.gui.api.VersionFetcher;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.StringUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerVersionFetcherServer implements VersionFetcher {
    
    private final Pattern re = Pattern.compile("([\\d.]+)");
    String latestVerResponse = null;
    String latestVerParsed = null;
    String downloadUrl = null;
    /** Properties are persisted here after fetchVersion() call, in case we need to auto-update. */
    Properties props = null;
    
    @Override
    public String fetchVersion() throws MalformedURLException, IOException {
        
        
         props = loadProps();
        // TODO: this is here for testing only, while nothing has been pushed to github yet
        props = PropertiesUtils.loadPropertiesLocal(MsfraggerProps.class, MsfraggerProps.PROPERTIES_FILE_NAME);
        
        String verSvcResponse = fetchVersionResponse();
        
        Matcher m = re.matcher(verSvcResponse);
        if (m.find()) {
            downloadUrl = props.getProperty(MsfraggerProps.PROP_UPDATESERVER_WEBSITE_URL, MsfraggerProps.DOWNLOAD_URL);
            latestVerResponse = verSvcResponse;
            latestVerParsed = m.group(1);
            return m.group(1);
        }
        
        throw new IllegalStateException("Version string retrieved from the remote service was not recoginsed: '" + verSvcResponse + "'");
    }
    
    private Properties loadProps() {
        Properties p = PropertiesUtils.loadPropertiesRemoteOrLocal(Collections.singletonList(MsfraggerProps.PROPERTIES_URI), 
                MsfraggerProps.class, MsfraggerProps.PROPERTIES_FILE_NAME);
        if (p == null)
            throw new IllegalStateException(String.format("Could not laod %s "
                    + "neither from GitHub nor from local jar", MsfraggerProps.PROPERTIES_FILE_NAME));
        return p;
    }

    @Override
    public String getDownloadUrl() {
        return downloadUrl;
    }

    @Override
    public String getToolName() {
        return MsfraggerProps.PROGRAM_NAME;
    }

    @Override
    public boolean canAutoUpdate() {
        return true;
    }

    private String fetchVersionResponse() throws MalformedURLException, IOException {
        String serviceUrl = props.getProperty(MsfraggerProps.PROP_UPDATESERVER_VERSION_URL);
        if (StringUtils.isNullOrWhitespace(serviceUrl))
            throw new IllegalStateException("Could not get versionServiceUrl");
        String response = org.apache.commons.io.IOUtils.toString(new URL(serviceUrl), Charset.forName("UTF-8"));
        if (StringUtils.isNullOrWhitespace(response))
            throw new IllegalStateException("Update server returned empty string for the latest available version.");
        return response.trim();
    }
    
    @Override
    public Path autoUpdate(Path p) throws MalformedURLException, IOException {
        if (p == null || !Files.exists(p) || Files.isDirectory(p))
            throw new IllegalArgumentException("The path to file to be updated must be non-null, must exist and not point to a directory.");
        
        String lastVersionStr = fetchVersion();
        
        String updateSvcUrl = props.getProperty(MsfraggerProps.PROP_UPDATESERVER_UPDATE_URL);
        if (updateSvcUrl == null)
            throw new IllegalStateException("Obtained properties file didn't contain a URL for the updater service.");
        
        if (StringUtils.isNullOrWhitespace(latestVerResponse))
            latestVerResponse = fetchVersionResponse();
        
        
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
            final Header contentType = entity.getContentType();
            final Header contentEncoding = entity.getContentEncoding();
            final Header[] allHeaders = response.getAllHeaders();
            final StatusLine statusLine = response.getStatusLine();
            
            final Path pathOut = p.resolveSibling("MSFragger-" + lastVersionStr + ".jar");
            final byte[] upgradedFragger = EntityUtils.toByteArray(entity);
            
            try (BufferedOutputStream bos = new BufferedOutputStream(Files.newOutputStream(pathOut))) {
                bos.write(upgradedFragger);
            }
            EntityUtils.consumeQuietly(entity);
            
            return pathOut;
        }
    }
    
}
