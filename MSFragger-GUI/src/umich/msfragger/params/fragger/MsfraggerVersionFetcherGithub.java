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

import java.nio.file.Path;
import java.util.Properties;
import umich.msfragger.gui.api.VersionFetcher;
import umich.msfragger.util.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerVersionFetcherGithub implements VersionFetcher {
    String downloadUrl = "";
    
    @Override
    public String fetchVersion() {
        Properties props = PropertiesUtils.loadPropertiesRemote(MsfraggerProps.PROPERTIES_URI);
        final String latestKnownVer = props.getProperty(MsfraggerProps.PROP_LATEST_VERSION);
        if (latestKnownVer == null) {
            throw new IllegalStateException(String.format("Property '%s' was not found in '%s' from github", 
                    MsfraggerProps.PROP_LATEST_VERSION, MsfraggerProps.PROPERTIES_FILE_NAME));
        }
        downloadUrl = props.getProperty(MsfraggerProps.PROP_DOWNLOAD_URL);
        if (downloadUrl == null)
            throw new IllegalStateException(String.format("Property '%s' was not found in '%s' from github", 
                    MsfraggerProps.PROP_DOWNLOAD_URL, MsfraggerProps.PROPERTIES_FILE_NAME));
        
        return latestKnownVer;
    }

    @Override
    public String getToolName() {
        return MsfraggerProps.PROGRAM_NAME;
    }

    @Override
    public String getDownloadUrl() {
        return downloadUrl;
    }

    @Override
    public boolean canAutoUpdate() {
        return false;
    }

    @Override
    public void autoUpdate(Path p) {
        throw new UnsupportedOperationException("Not supported.");
    }
    
}
