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
package umich.msfragger.params;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Properties;
import javax.swing.JFileChooser;
import javax.swing.text.JTextComponent;
import umich.msfragger.Version;
import umich.msfragger.util.PathUtils;

public class ThisAppProps extends Properties {
    //private static final Logger log = LoggerFactory.getLogger(ThisAppProps.class);
    public static final String PROP_DB_FILE_IN = "path.db.file.in";
    public static final String PROP_FRAGGER_PARAMS_FILE_IN = "path.fragger.params.file.in";
    public static final String PROP_BINARIES_IN = "path.params.bins.in";
    public static final String PROP_LCMS_FILES_IN = "path.lcms.files.in";
    public static final String PROP_FILE_OUT = "path.file.out";

    public static final String TEMP_DIR = System.getProperty("java.io.tmpdir");
    public static final String TEMP_FILE_NAME = "msfragger.cache";
    
    public static final String PROP_TEXTFIELD_PATH_MSCONVERT = "path.textfield.msconvert";
    public static final String PROP_BIN_PATH_MSFRAGGER = "path.textfield.msfragger";
    public static final String PROP_BIN_PATH_PHILOSOPHER = "path.textfield.peptide-prophet";
    public static final String PROP_TEXTFIELD_PATH_PROTEIN_PROPHET = "path.textfield.protein-prophet";
    public static final String PROP_TEXTFIELD_REPORT_ANNOTATE = "report.annotate";
    public static final String PROP_TEXTFIELD_REPORT_FILTER = "report.filter";
    public static final String PROP_TEXTFIELD_SEQUENCE_DB = "sequence.db";
    public static final String PROP_TEXTFIELD_DECOY_TAG = "decoy.tag";
    public static final String PROP_CHECKBOX_REPORT_PROTEIN_LEVEL_FDR = "report.proteinlevelfdr";
    
    public static final String PROP_TEXT_CMD_PEPTIDE_PROPHET = "peptideprophet.cmd.line.opts";
    public static final String PROP_TEXT_CMD_PROTEIN_PROPHET = "proteinprophet.cmd.line.opts";

    public static final String PROP_MGF_WARNING = "warn.mgf";
    
    public ThisAppProps() {
        this.setProperty(Version.PROP_VER, Version.VERSION);
    }
    
    public static void clearCache() {
        ThisAppProps thisAppProps = new ThisAppProps();
        thisAppProps.save();
    }
    
    
    /**
     * Tries to load previously saved properties.
     * @return null if the file didn't exist or could not be loaded.
     */
    public static ThisAppProps loadFromTemp()  {
        Path path = Paths.get(TEMP_DIR, TEMP_FILE_NAME);
        if (!Files.exists(path)) {
            return null;
        }
        try {
            ThisAppProps props = new ThisAppProps();
            props.load(new FileInputStream(path.toFile()));
            return props;

        } catch (IOException ex) {
            //log.warn("Could not load properties from temporary directory: {}", ex.getMessage());
        }

        return null;
    }

    
    /**
     * Attempts to set file chooser's directory to the one saved in the property.
     * @param fileChooser
     * @param propName 
     */
    public static void load(String propName, JFileChooser fileChooser) {
        ThisAppProps thisAppProps = ThisAppProps.loadFromTemp();
        if (thisAppProps == null) {
            return;
        }
        String inputPath = thisAppProps.getProperty(propName);
        if (inputPath != null) {
            File file = Paths.get(inputPath).toFile();
            fileChooser.setCurrentDirectory(file);
        }
    }

    public static void save(String propName, JTextComponent txt) {
        String text = txt.getText().trim();
        if (!text.isEmpty()) {
            ThisAppProps.save(propName, text);
        }
    }

    public static void save(String propName, File file) {
        ThisAppProps thisAppProps = ThisAppProps.loadFromTemp();
        if (thisAppProps == null) {
            thisAppProps = new ThisAppProps();
        }
        thisAppProps.setProperty(propName, file.getAbsolutePath());
        thisAppProps.save();
    }

    public static boolean load(String propName, JTextComponent txt) {
        String cached = ThisAppProps.load(propName);
        if (cached == null) {
            return false;
        }
        txt.setText(cached);
        return true;
    }

    /**
     * Attempts to search for properties in cache, returns the first non-null found.
     * @param props  List of properties to search for.
     * @param locateJar  If no property was found, will try to locate the current jar
     *                   and return its location.
     * @return
     */
    public static String tryFindPath(List<String> props, boolean locateJar) {
        String path = null;
        for (String prop : props) {
            path = ThisAppProps.load(prop);
            if (path != null) {
                break;
            }
        }
        if (path == null && locateJar) {
            URI thisJarUri = PathUtils.getCurrentJarPath();
            if (thisJarUri != null) {
                path = Paths.get(thisJarUri).toString();
            }
        }
        return path;
    }

    public void save() {
        Path path = Paths.get(TEMP_DIR, TEMP_FILE_NAME);
        try (FileOutputStream fos = new FileOutputStream(path.toFile())) {
            store(fos, "MSFragger GUI runtime properties");
        } catch (IOException ex) {
            //log.warn("Could not load properties from temporary directory: {}", ex.getMessage());
        }
    }
    
    public static void save(String propName, String propVal) {
        if (propName == null)
            throw new IllegalArgumentException("Property name must be non-null");
        ThisAppProps thisAppProps = ThisAppProps.loadFromTemp();
        if (thisAppProps == null)
            thisAppProps = new ThisAppProps();
        if (propVal == null)
            thisAppProps.remove(propName);
        else
            thisAppProps.setProperty(propName, propVal);
        thisAppProps.save();
    }
    
    public static String load(String propName) {
        if (propName == null)
            throw new IllegalArgumentException("Property name must be non-null");
        ThisAppProps thisAppProps = ThisAppProps.loadFromTemp();
        if (thisAppProps == null)
            return null;
        return thisAppProps.getProperty(propName);
    }
    
    public static String load(String propName, String defaultVal) {
        if (propName == null)
            throw new IllegalArgumentException("Property name must be non-null");
        ThisAppProps thisAppProps = ThisAppProps.loadFromTemp();
        if (thisAppProps == null)
            return defaultVal;
        return thisAppProps.getProperty(propName, defaultVal);
    }
}
