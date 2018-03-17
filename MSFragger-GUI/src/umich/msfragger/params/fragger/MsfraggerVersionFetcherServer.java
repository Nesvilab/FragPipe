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

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import umich.msfragger.gui.api.VersionFetcher;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.StringUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerVersionFetcherServer implements VersionFetcher {
    
    private final Pattern re = Pattern.compile("([\\d.]+)");
    String latestVer = null;
    String downloadUrl = null;
    /** Properties are persisted here after fetchVersion() call, in case we need to auto-update. */
    Properties props = null;
    
    @Override
    public String fetchVersion() throws MalformedURLException, IOException {
        
        
         props = loadProps();
        // TODO: this is here for testing only, while nothing has been pushed to github yet
        //Properties props = PropertiesUtils.loadPropertiesLocal(MsfraggerProps.class, MsfraggerProps.PROPERTIES_FILE_NAME);
        
        String verSvcUrl = props.getProperty(MsfraggerProps.PROP_UPDATESERVER_VERSION_URL);
        if (StringUtils.isNullOrWhitespace(verSvcUrl))
            throw new IllegalStateException("Could not get versionServiceUrl");
        
        URL url = new URL(verSvcUrl);
        String verSvcResponse = org.apache.commons.io.IOUtils.toString(url, Charset.forName("UTF-8"));
        if (StringUtils.isNullOrWhitespace(verSvcResponse))
            throw new IllegalStateException("Update server returned empty string for the latest available version.");
        
        Matcher m = re.matcher(verSvcResponse);
        if (m.find()) {
            downloadUrl = props.getProperty(MsfraggerProps.PROP_UPDATESERVER_WEBSITE_URL, MsfraggerProps.DOWNLOAD_URL);
            latestVer = verSvcResponse;
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

    @Override
    public void autoUpdate(Path p) throws MalformedURLException {
        if (p == null || !Files.exists(p) || Files.isDirectory(p))
            throw new IllegalArgumentException("The path to file to be updated must be non-null, must exist and not point to a directory.");
        if (props == null)
            props = loadProps();
        String updateSvcUrl = props.getProperty(MsfraggerProps.PROP_UPDATESERVER_UPDATE_URL);
        if (updateSvcUrl == null)
            throw new IllegalStateException("Obtained properties file didn't contain a URL for the updater service.");
        
        URL url = new URL(updateSvcUrl);
        
        // This requires Apacge HttpClient, HC Fluent, HttpMime
        CloseableHttpClient client = HttpClients.createDefault();
        HttpPost post = new HttpPost("http://msfragger.arsci.com/upgrader/upgrade_download.php");
        //Path path = Paths.get("C:\\data\\andy\\fragger-programs\\MSFragger-20171106\\fragger.params");
        Path path = Paths.get("C:\\data\\andy\\fragger-programs\\MSFragger-20171106\\MSFragger-20171106.jar");
//        FileBody file = new FileBody(path.toFile());
//        StringBody comment = new StringBody("Original MSFragger jar from user", ContentType.TEXT_PLAIN);

        final FormBodyPart formPartDownload = FormBodyPartBuilder.create()
                .setName("download")
                .setBody(new StringBody("Release 20180316$jar", ContentType.TEXT_PLAIN))
                .build();
        final FormBodyPart formPartTradeinFile = FormBodyPartBuilder.create()
                .setName("jarkey")
                .setBody(new FileBody(path.toFile()))
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

            //final InputStream content = entity.getContent();

            //final String s = EntityUtils.toString(entity, Charset.forName("UTF-8"));
            final Path pathOut = path.resolveSibling("_FRAGGER_UPGRADE.jar");
            final byte[] upgradedFragger = EntityUtils.toByteArray(entity);
            try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(pathOut.toFile()))) {
                bos.write(upgradedFragger);
            }
            EntityUtils.consume(entity);
        }

        int a = 1;
        
        
    }
    
}
