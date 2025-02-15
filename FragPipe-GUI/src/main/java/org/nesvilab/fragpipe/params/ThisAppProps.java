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
package org.nesvilab.fragpipe.params;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.messages.NoteConfigDiaTracer;
import org.nesvilab.fragpipe.messages.NoteConfigIonQuant;
import org.nesvilab.fragpipe.messages.NoteConfigMsfragger;
import org.nesvilab.utils.JarUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.ResourceBundle;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.text.JTextComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.api.SearchTypeProp;
import org.nesvilab.utils.BundleUtils;
import org.nesvilab.utils.CacheUtils;
import org.nesvilab.utils.PropertiesUtils;

public class ThisAppProps extends Properties {

  public static final String PROP_LAB_SITE_URL = "lab.site.url";
  public static final String PROP_MANUSCRIPT_URL = "manuscript.url";
  public static final String PROP_MANUSCRIPT_DOI = "manuscript.doi";
  public static final String PROP_FRAGPIPE_SITE_URL = "msfragger.gui.site.url";
  public static final String PROP_SETUP_TUTORIAL_URL = "fragpipe.setup-tutorial.url";
  public static final String PROP_PYTHON_DOWNLOAD_URL = "python.url.download";
  public static final String LAST_RECURSIVE_FOLDER_ADDED = "path.recursive-add-folder.last";
  public static final String CONFIG_SAVE_LOCATION = "path.store.configs";

  private static final Logger log = LoggerFactory.getLogger(ThisAppProps.class);
  //private static final Logger log = LoggerFactory.getLogger(ThisAppProps.class);
  public static final String PROP_DB_FILE_IN = "path.db.file.in";
  public static final String PROP_FRAGGER_PARAMS_FILE_IN = "path.fragger.params.file.in";
  public static final String PROP_BINARIES_IN = "path.params.bins.in";
  public static final String PROP_LCMS_FILES_IN = "path.lcms.files.in";
  public static final String PROP_FILE_OUT = "path.file.out";

  public static final String APP_TEMP_DIR = "fragpipe";
  public static final String TEMP_FILE_EXT = ".cache";
  public static final String TEMP_FILE_NAME = "msfragger" + TEMP_FILE_EXT;
  public static final String LOG_FILE_NAME = "log-fragpipe-run-at";
  public static final String LOG_FILE_EXT = ".log";

  public static final String PROP_BIN_PATH_MSFRAGGER = "path.textfield.msfragger";
  public static final String PROP_BIN_PATH_PHILOSOPHER = "path.textfield.peptide-prophet";
  public static final String PROP_BIN_PATH_PYTHON = "path.bin.python";
  public static final String PROP_TEXTFIELD_PATH_PROTEIN_PROPHET = "path.textfield.protein-prophet";
  public static final String PROP_TEXTFIELD_REPORT_ANNOTATE = "report.annotate";
  public static final String PROP_TEXTFIELD_REPORT_FILTER = "report.filter";
  public static final String PROP_TEXTFIELD_REPORT_ABACUS = "report.abacus";
  public static final String PROP_TEXTFIELD_LABELFREE = "report.labelfree";
  public static final String PROP_TEXTFIELD_SEQUENCE_DB = "sequence.db";
  public static final String PROP_TEXTFIELD_DECOY_TAG = "decoy.tag";
  public static final String PROP_CHECKBOX_REPORT_PROTEIN_LEVEL_FDR = "report.proteinlevelfdr";
  public static final String PROP_CHECKBOX_PROCESS_GROUPS_SEPARATELY = "process.groups.separately";
  public static final String PROP_CHECKBOX_COMBINE_PEPXML = "peptideprophet.combine.pepxml";
  public static final String PROP_CHECKBOX_REPORT_ABACUS = "report.run.abacus";
  public static final String PROP_CHECKBOX_WRITE_MZID = "report.output.format";
  public static final String PROP_CHECKBOX_REPORT_FILTER_NO_PROTXML = "report.filter.no-protxml";
  public static final String PROP_CHECKBOX_REPORT_PRINT_DECOYS = "report.print-decoys";
  public static final String PROP_DB_SAVE_PATH = "db.save.path";

  public static final String PROP_TEXT_CMD_PEPTIDE_PROPHET = "peptideprophet.cmd.line.opts";
  public static final String PROP_TEXT_CMD_PROTEIN_PROPHET = "proteinprophet.cmd.line.opts";

  public static final String PROP_CRYSTALC_USE = "crystalc.use";
  public static final String PROP_SPECLIBGEN_RUN = "speclibgen.run";

  public static final String PROP_MGF_WARNING = "warn.mgf";

  @Deprecated
  public static final String JAR_FILE_AS_RESOURCE_EXT = ".jazz";
  public static final Path UNPACK_TEMP_SUBDIR = Paths.get("fragpipe");
  public static final String DEFAULT_LCMS_EXP_NAME = "";

  public static final String PATH_BUNDLE = "org/nesvilab/fragpipe/Bundle";
  public static final List<String> PROPERTIES_URLS = Arrays.asList(
      "https://raw.githubusercontent.com/Nesvilab/FragPipe/updates/FragPipe-GUI/src/" + PATH_BUNDLE
          + ".properties"
  );

  private static class HolderRemote {

    private static final Properties propsRemote = PropertiesUtils.initProperties(PROPERTIES_URLS, 30);

    public static Properties getRemoteProperties() {
      return propsRemote;
    }
  }

  private static class HolderLocal {

    private static final Properties propsLocal = PropertiesUtils
        .initProperties("Bundle.properties", Fragpipe.class);

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
    Properties remote = ThisAppProps.getRemoteProperties();
    if (remote != null) {
      for (String name : remote.stringPropertyNames()) {
        p.setProperty(name, remote.getProperty(name));
      }
    }
    return p;
  }

  /**
   * Default definition of properties added during re-implementation while old FragPipe code is
   * still being used.
   */
  public static Properties def() {
    return getRemotePropertiesWithLocalDefaults();
  }

  public static ResourceBundle getLocalBundle() {
    return BundleUtils.getBundle(PATH_BUNDLE);
  }

  public ThisAppProps() {
    this.setProperty(Version.PROP_VER, Version.version());
  }

  /**
   * Tries to load previously saved properties.
   *
   * @return null if the file didn't exist or could not be loaded.
   */
  public static ThisAppProps load() {
    try {
      final Path path = CacheUtils.locateTempFile(TEMP_FILE_NAME);
      final ThisAppProps props = new ThisAppProps();
      props.load(new FileInputStream(path.toFile()));
      return props;
    } catch (FileNotFoundException ex) {
      return null;
    } catch (IOException ex) {
      log.debug("Could not load properties from temporary directory: {}", ex.getMessage());
    }
    return null;
  }

  public static ThisAppProps loadOrDefault() {
    ThisAppProps p = load();
    return p == null ? new ThisAppProps() : p;
  }

  /**
   * Attempts to set file chooser's directory to the one saved in the property.
   */
  public static void load(String propName, JFileChooser fileChooser) {
    try {
      ThisAppProps thisAppProps = ThisAppProps.load();
      if (thisAppProps == null) {
        return;
      }
      String inputPath = thisAppProps.getProperty(propName);
      FileChooserUtils.setPath(fileChooser, inputPath);
    } catch (Exception ignored) {
    }
  }

  public static void save(String propName, JTextComponent txt) {
    String text = txt.getText().trim();
    if (!text.isEmpty()) {
      save(propName, text);
    }
  }

  public static void save(String propName, File file) {
    save(propName, file.getAbsolutePath());
  }

  public static void save(JCheckBox checkBox,  String propName) {
    save(propName, Boolean.toString(checkBox.isSelected()));
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
   *
   * @param props     List of properties to search for.
   * @param locateJar If no property was found, will try to locate the current jar and return its
   *                  location.
   * @return Null in case path could not be found.
   */
  public static String tryFindPath(List<String> props, boolean locateJar) {
    for (String prop : props) {
      String path = ThisAppProps.load(prop);
      if (path != null) {
        return path;
      }
    }
    if (locateJar) {
      URI thisJarUri = JarUtils.getCurrentJarUri();
      if (thisJarUri != null) {
        return Paths.get(thisJarUri).toString();
      }
    }
    return null;
  }

  public static boolean load(JTextComponent text, String propName) {
    String val = load(propName);
    if (val != null) {
      text.setText(val);
      return true;
    }
    return false;
  }

  public static boolean load(JCheckBox box, String propName) {
    String val = load(propName);
    if (val != null) {
      Boolean bool = Boolean.valueOf(val);
      box.setSelected(bool);
      return true;
    }
    return false;
  }

  public static void save(JTextComponent text, String propName) {
    save(propName, text.getText().trim());
  }

  public static void loadFromBundle(JTextComponent text, String propName, String type) {
    final String prop = propName + "." + type;
    loadFromBundle(text, prop);
  }

  public static void loadFromBundle(JTextComponent text, String propName, SearchTypeProp type) {
    final String prop = propName + "." + type.name();
    loadFromBundle(text, prop);
  }

  public static void loadFromBundle(JTextComponent text, String propName) {
    String val = getLocalProperties().getProperty(propName);
    text.setText(val);
    save(propName, val);
  }

  public static void loadFromBundle(JCheckBox checkBox, String propName, SearchTypeProp type) {
    final String prop = propName + "." + type.name();
    loadFromBundle(checkBox, prop);
  }

  public static void loadFromBundle(JCheckBox checkBox, String propName) {
    String val = getLocalProperties().getProperty(propName);
    checkBox.setSelected(Boolean.parseBoolean(val));
    save(propName, val);
  }

  public static String[] cacheComments() {
    NoteConfigMsfragger noteConfigMsfragger = Fragpipe.getStickyStrict(NoteConfigMsfragger.class);
    String[] ss = {
        Version.PROGRAM_TITLE + " (" + Version.version() + ") runtime properties",
        "MSFragger version " + (noteConfigMsfragger == null ? "N/A" : noteConfigMsfragger.version),
        "IonQuant version " + NoteConfigIonQuant.version,
        "diaTracer version " + NoteConfigDiaTracer.version
    };
    return ss;
  }

  private static Path getCacheFilePath() {
    return CacheUtils.getTempFile(TEMP_FILE_NAME);
  }

  public void save() {
    try (OutputStream os = Files.newOutputStream(getCacheFilePath())) {
      PropertiesUtils.storeSorted(this, os, cacheComments(), true);
      os.flush();
    } catch (IOException ex) {
      //log.warn("Could not load properties from temporary directory: {}", ex.getMessage());
    }
  }

  public static void save(String propName, String propVal) {
    Objects.requireNonNull(propName, "propName");
    log.debug("ThisAppProps saving property: {} = {} to {}", propName, propVal, getCacheFilePath().toString());
    ThisAppProps p = ThisAppProps.loadOrDefault();
    if (propVal == null) {
      p.remove(propName);
    } else {
      p.setProperty(propName, propVal);
    }
    p.save();
  }

  public static String load(String propName) {
    if (propName == null) {
      throw new IllegalArgumentException("Property name must be non-null");
    }
    ThisAppProps thisAppProps = ThisAppProps.load();
    if (thisAppProps == null) {
      return null;
    }
    return thisAppProps.getProperty(propName);
  }

  public static String load(String propName, String defaultVal) {
    if (propName == null) {
      throw new IllegalArgumentException("Property name must be non-null");
    }
    ThisAppProps thisAppProps = ThisAppProps.load();
    if (thisAppProps == null) {
      return defaultVal;
    }
    return thisAppProps.getProperty(propName, defaultVal);
  }
}
