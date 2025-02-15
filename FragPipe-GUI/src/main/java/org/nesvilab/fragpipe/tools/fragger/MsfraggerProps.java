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

import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.utils.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerProps {
    private static final Logger log = LoggerFactory.getLogger(MsfraggerProps.class);
    public static final String PROGRAM_NAME = "MSFragger";

//    private static class Holder {
//        private static final Properties properties = PropertiesUtils.initProperties(PROPERTIES_URLS, PROPERTIES_FILE_NAME, MsfraggerProps.class);
//        public static Properties getProperties() {
//            return properties;
//        }
//    }
    
    // This URL wil be checked to compare versions of MSFragger.
    // This way the user will get notifications about new versions of
    // MSFragger when we only update the online repository.
    public static final List<String> PROPERTIES_URLS = Arrays.asList(
        "https://raw.githubusercontent.com/Nesvilab/FragPipe/master/FragPipe-GUI/src/main/java/org/nesvilab/fragpipe/tools/fragger/msfragger.properties"
    );
    public static final String PROPERTIES_FILE_NAME = "Bundle.properties";
    public static final String PROP_LATEST_VERSION = "msfragger.version.latest-known";
    public static final String PROP_UPDATESERVER_WEBSITE_URL = "msfragger.update-server.website.url";
    public static final String PROP_UPDATESERVER_VERSION_URL = "msfragger.update-server.version-service.url";
    public static final String PROP_UPDATESERVER_UPDATE_URL = "msfragger.update-server.update-service.url";

    public static Properties getProperties() {
        return getRemotePropertiesWithLocalDefaults();
    }


    private static class HolderRemote {
        private static final Properties propsRemote = PropertiesUtils.initProperties(PROPERTIES_URLS, 30);
        public static Properties getRemoteProperties() {
            return propsRemote;
        }
    }

    private static class HolderLocal {
        private static final Properties propsLocal = PropertiesUtils.initProperties(PROPERTIES_FILE_NAME, MsfraggerProps.class);

        public static Properties getLocalProperties() {
            return propsLocal;
        }
    }

    public static Properties getLocalProperties() {
        return HolderLocal.getLocalProperties();
    }

    public static Properties getRemoteProperties() {
        return HolderRemote.getRemoteProperties();
    }

    public static Properties getRemotePropertiesWithLocalDefaults() {
        final Properties p = new Properties(getLocalProperties());
        // merge with remote properties
        Properties remote = getRemoteProperties();
        if (remote!= null) {
            for (String name : remote.stringPropertyNames()) {
                p.setProperty(name, remote.getProperty(name));
            }
        }
        return p;
    }
}
