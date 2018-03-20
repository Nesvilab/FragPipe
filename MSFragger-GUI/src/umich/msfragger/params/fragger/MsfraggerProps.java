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

import java.net.URI;

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
    public static final String PROPERTIES_URL = "https://raw.githubusercontent.com/chhh/MSFragger-GUI/master/MSFragger-GUI/src/umich/msfragger/params/fragger/msfragger.properties";
    /** Github master branch location of "umich/msfragger/params/fragger/msfragger.properties". */
    public static final URI PROPERTIES_URI = URI.create(PROPERTIES_URL);
    public static final String PROPERTIES_FILE_NAME = "msfragger.properties";
        
    public static final String PROP_LATEST_VERSION = "msfragger.version.latest-known";
    public static final String PROP_DOWNLOAD_URL = "msfragger.download.url";
    
    public static final String PROP_UPDATESERVER_WEBSITE_URL = "msfragger.update-server.website.url";
    public static final String PROP_UPDATESERVER_VERSION_URL = "msfragger.update-server.version-service.url";
    public static final String PROP_UPDATESERVER_UPDATE_URL = "msfragger.update-server.update-service.url";
    
}
