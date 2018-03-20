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
    public Path autoUpdate(Path p) {
        throw new UnsupportedOperationException("Not supported.");
    }
    
}
