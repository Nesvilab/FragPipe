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
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import umich.msfragger.util.PropertiesUtils;

/**
 *
 * @author Dmitry Avtonomov
 */
public class MsfraggerProps {
    public static final String PROGRAM_NAME = "MSFragger";
    
    /** Umich Tech Ttransfer Office MSFragger download URL. */
    public static final String DOWNLOAD_URL = "http://inventions.umich.edu/technologies/7143_msfrager-ultrafast-and-comprehensive-identification-of-peptides-from-tandem-mass-spectra";
    /** Umich Tech Ttransfer Office MSFragger download URI. */
    public static final URI DOWNLOAD_URI = URI.create(DOWNLOAD_URL);
    
    // This URL wil be checked to compare versions of MSFragger.
    // This way the user will get notifications about new versions of
    // MSFragger when we only update the online repository.
    /** Github master branch location of "umich/msfragger/params/fragger/msfragger.properties". */
    public static final String PROPERTIES_URL = "https://raw.githubusercontent.com/chhh/FragPipe/master/MSFragger-GUI/src/umich/msfragger/params/fragger/msfragger.properties";
    /** Github master branch location of "umich/msfragger/params/fragger/msfragger.properties". */
    public static final URI PROPERTIES_URI = URI.create(PROPERTIES_URL);
    public static final String PROPERTIES_FILE_NAME = "msfragger.properties";
        
    public static final String PROP_LATEST_VERSION = "msfragger.version.latest-known";
    public static final String PROP_DOWNLOAD_URL = "msfragger.download.url";
    
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
    
    public static Properties loadProperties() {
        return PropertiesUtils.loadPropertiesLocal(MsfraggerProps.class, PROPERTIES_FILE_NAME);
    }
}
