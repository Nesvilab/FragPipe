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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.MsfraggerGuiFrame;
import umich.msfragger.util.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerProps {
    private static final Logger log = LoggerFactory.getLogger(MsfraggerProps.class);
    public static final String PROGRAM_NAME = "MSFragger";
    public static final String DBSPLIT_SCRIPT_NAME = "msfragger_pep_split.py";

//    private static class Holder {
//        private static final Properties properties = PropertiesUtils.initProperties(PROPERTIES_URLS, PROPERTIES_FILE_NAME, MsfraggerProps.class);
//        public static Properties getProperties() {
//            return properties;
//        }
//    }
    
    // This URL wil be checked to compare versions of MSFragger.
    // This way the user will get notifications about new versions of
    // MSFragger when we only update the online repository.
    /** Github master branch location of "umich/msfragger/params/fragger/msfragger.properties". */
    public static final List<String> PROPERTIES_URLS = Arrays.asList(
        "https://raw.githubusercontent.com/Nesvilab/FragPipe/master/MSFragger-GUI/src/umich/msfragger/params/fragger/msfragger.properties",
        "https://raw.githubusercontent.com/chhh/FragPipe/updates/MSFragger-GUI/src/umich/msfragger/params/fragger/msfragger.properties",
        "https://raw.githubusercontent.com/chhh/FragPipe/master/MSFragger-GUI/src/umich/msfragger/params/fragger/msfragger.properties"
    );
    //public static final String PROPERTIES_URL = "https://raw.githubusercontent.com/Nesvilab/FragPipe/master/MSFragger-GUI/src/umich/msfragger/params/fragger/msfragger.properties";
    public static final String PROPERTIES_FILE_NAME = "msfragger.properties";
        
    public static final String PROP_LATEST_VERSION = "msfragger.version.latest-known";
    public static final String PROP_MIN_VERSION_SLICING = "msfragger.version.min-for-slicing";
    public static final String PROP_MIN_VERSION_MSADJUSTER = "msfragger.version.min-for-msadjuster";
    public static final String PROP_MIN_VERSION_FRAGGER_MASS_CALIBRATE = "msfragger.version.min-for-calibrate";
    public static final String PROP_DOWNLOAD_URL = "msfragger.download.url";
    public static final String PROP_FRAGGER_SITE_URL = "msfragger.site.url";
    public static final String PROP_DBSPLIT_INSTRUCTIONS_URL = "msfragger.dbsplit.instructions.url";
    public static final String PROP_SPECLIBGEN_INSTRUCTIONS_URL = "msfragger.speclibgen.instructions.url";

    public static final String PROP_UPDATESERVER_WEBSITE_URL = "msfragger.update-server.website.url";
    public static final String PROP_UPDATESERVER_VERSION_URL = "msfragger.update-server.version-service.url";
    public static final String PROP_UPDATESERVER_UPDATE_URL = "msfragger.update-server.update-service.url";
    
    public static FraggerRunResult testJar(String jarPath) {
        String verStr = null;
        boolean isVersionPrintedAtAll = false;
        try {
            Pattern regex = Pattern.compile("MSFragger version (MSFragger-([\\d\\.]{4,}))", Pattern.CASE_INSENSITIVE);
            ProcessBuilder pb = new ProcessBuilder("java", "-jar", jarPath);
            pb.redirectErrorStream(true);
            Process pr = pb.start();
            try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
                String line;
                while ((line = in.readLine()) != null) {
                    Matcher m = regex.matcher(line);
                    if (m.matches()) {
                        isVersionPrintedAtAll = true;
                        verStr = m.group(2);
                    }
                }
                pr.waitFor();
            }
        } catch (IOException | InterruptedException e) {
            throw new IllegalStateException("Error while creating a java process for MSFragger test.");
        }
        return new FraggerRunResult(isVersionPrintedAtAll, verStr);
    }

    public static class FraggerRunResult {
        final public boolean isVersionPrintedAtAll;
        final public String version;

        public FraggerRunResult(boolean isVersionPrintedAtAll, String version) {
            this.isVersionPrintedAtAll = isVersionPrintedAtAll;
            this.version = version;
        }
    }

    public static Properties getProperties() {
        return getRemotePropertiesWithLocalDefaults();
    }


    private static class HolderRemote {
        private static final Properties propsRemote = PropertiesUtils.initProperties(PROPERTIES_URLS);
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
