/* 
 * Copyright 2016 Dmitry Avtonomov.
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
package umich.msfragger.params.philosopher;

import java.net.URI;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PhilosopherProps {
    public static final String DOWNLOAD_LINK = "https://github.com/prvst/philosopher/releases/latest";
    
    public static final String CMD_COMET = "comet";
    public static final String CMD_PEPTIDE_PROPHET = "peptideprophet";
    public static final String CMD_PROTEIN_PROPHET = "proteinprophet";
    public static final String CMD_DATABASE = "database";
    public static final String CMD_FILTER = "filter";
    public static final String CMD_REPORT = "report";
    
    public static final String PROTEIN_PROPHET_OUTPUT_FILE = "interact.prot.xml";
    
    public static final String PROPERTY_FILE_NAME = "philosopher.properties";
    
    public static final String PROP_LATEST_COMPATIBLE_VERSION = "philosopher.version.latest-compatible";
    public static final String PROP_DOWNLOAD_URL = "philosopher.download.url";
    
    public static final String PROPERTIES_URL = "https://raw.githubusercontent.com/chhh/MSFragger-GUI/master/MSFragger-GUI/src/umich/msfragger/params/philosopher/philosopher.properties";
    public static final URI PROPERTIES_URI = URI.create(PROPERTIES_URL);
}
