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
package org.nesvilab.fragpipe.tools.fragger;

import java.nio.file.Path;
import java.util.Properties;
import org.nesvilab.fragpipe.api.VersionFetcher;
import org.nesvilab.utils.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerVersionFetcherLocal implements VersionFetcher {

    private Properties getProperties() {
        return PropertiesUtils.loadPropertiesLocal(MsfraggerProps.class, MsfraggerProps.PROPERTIES_FILE_NAME);
    }

    @Override
    public String fetchVersion() {
        final Properties props = getProperties();
        final String latestKnownVer = props.getProperty(MsfraggerProps.PROP_LATEST_VERSION);
        if (latestKnownVer == null) {
            throw new IllegalStateException(String.format("Property '%s' was not found in '%s' from local jar", 
                    MsfraggerProps.PROP_LATEST_VERSION, MsfraggerProps.PROPERTIES_FILE_NAME));
        }
        return latestKnownVer;
    }

    @Override
    public String getDownloadUrl() {
        Properties props = getProperties();
        String url = props.getProperty(MsfraggerProps.PROP_UPDATESERVER_WEBSITE_URL);
        if (url == null) {
            throw new IllegalStateException("Download URL should not be null in the local version fetcher");
        }
        return url;
    }
    
    @Override
    public String getToolName() {
        return MsfraggerProps.PROGRAM_NAME;
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
