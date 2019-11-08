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
package umich.msfragger;

import static umich.msfragger.params.ThisAppProps.PATH_BUNDLE;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.TreeMap;
import java.util.TreeSet;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.MsfraggerGuiFrame;
import umich.msfragger.messages.MessageTipNotification;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.VersionComparator;

/**
 * @author Dmitry Avtonomov
 */
public class Version {
  private static final Logger log = LoggerFactory.getLogger(Version.class);
  public static final String PROGRAM_TITLE = "FragPipe";
  public static final String PROP_VER = "msfragger.gui.version";
  public static final String PROP_LAST_RELEASE_VER = "fragpipe.last.release.version";
  public static final String PROP_DOWNLOAD_URL = "msfragger.gui.download-url";
  public static final String PROP_ISSUE_TRACKER_URL = "msfragger.gui.issue-tracker";
  public static final String PROP_DOWNLOAD_MESSAGE = "msfragger.gui.download-message";
  public static final String PROP_IMPORTANT_UPDATES = "msfragger.gui.important-updates";
  public static final String PROP_CRITICAL_UPDATES = "msfragger.gui.critical-updates";



  private static final TreeMap<String, List<String>> CHANGELOG = new TreeMap<>(
      new VersionComparator());

  static {
    CHANGELOG.put("11.0", Arrays.asList(
        "Parallel execution engine. Used only for PeptideProphet now, utilizing all CPU cores.",
        "DIA-Umpire requires MSConvert from ProteoWizard on Linux."
    ));

    CHANGELOG.put("10.0", Arrays.asList(
        "Add PTMShepherd with UI.",
        "Thermo RAW files and Bruker TimsTOF .d directories are supported. They do require "
            + "new MSFragger with 'ext' directory for libraries and additional binaries.",
        "Fix for changes to modification tables not always being propagated to config files "
            + "if the user didn't leave the editing field.",
        "Saving all the FragPipe options before run + buttons to Save/Load configurations.",
        "Fix iProphet command threads and how it's invoked when multi-experiment is enabled."
        ));

    CHANGELOG.put("9.4", Arrays.asList("Fixes to MSFragger Split program for very large databases."));

    CHANGELOG.put("9.3", Arrays.asList(
        "Calibrate masses option in MSFragger",
        "Custom ion series option in MSFragger",
        "Turning off usage of protxml file in Filter command when Protein Prophet is not run",
        "Query user if protxml files exist while Protein Prophet is not run",
        "'Print Decoys' option/checkbox for Report command",
        "Update CrystalC-1.0.5",
        "Checkbox for generating report in mzID format",
        "UI for downloading protein databases via philosopher"));

    CHANGELOG.put("9.1", Arrays.asList(
        "Fix Abacus command bug with unrecognized options from Filter command being carried over."));

    CHANGELOG.put("9.0", Arrays.asList(
        "Downstream tab groups all downstream processing tools in one place.",
        "Simplified Config tab with more links and hint.",
        "Much improved saving and restoring of edited fields.",
        "Support MSFragger 20190222"));

    CHANGELOG.put("8.8", Arrays.asList(
        "iProphet for peptide level reports.",
        "Moved all downstream tools (Peptide/Protein Prophet, Crystal-C) to a single tab.",
        "Simplified reports tab.",
        "Ask twice about loading defaults automatically.",
        "Removed decoy tag specification from command line fields. It's now always added implicitly."));

    CHANGELOG.put("8.7", Arrays.asList(
        "Support for new Philosopher 20181119, Report Abacus --protein flag."));

    CHANGELOG.put("8.6", Arrays.asList(
        "Updated nonspecific search default options.",
        "Colorized console output.",
        "Multiple UI fixes and updates."));

    CHANGELOG.put("8.5", Arrays.asList(
        "Updated spectral library generation. LCMS files are copied and deleted from the right locations.",
        "Multi-experiment protein level report.",
        "Multi-experiment quantitation",
        "Allow separate processing of input files one-by-one."));

    CHANGELOG.put("8.4", Arrays.asList(
        "Combined protein report for multiple file groups."));

    CHANGELOG.put("8.3", Arrays.asList(
        "LCMS files can be processed in separate groups.",
        "Python detection on Windows via registry.",
        "Python binary location can be specified manually."));

    CHANGELOG.put("8.1", Arrays.asList(
        "DIA-Umpire SE added as an optional component. Mark the checkbox on Config panel to enable."));

    CHANGELOG.put("8.0", Arrays.asList(
        "Added MSAdjuster, Crystal-C. Both packaged with the release, no extra downloads."));

    CHANGELOG.put("7.2", Arrays.asList(
        "Added database slicing via a python script (Requires "
            + "Python 3, NumPy, Pandas)."));

    CHANGELOG.put("7.1", Arrays.asList(
        "Added label free quantitation."));

    CHANGELOG.put("7.0", Arrays.asList(
        "MFFragger-GUI is now calledFragPipe.",
        "Clear out Fragger modification tables when loading new parameter files to avoid ghost entries.",
        "Update the github urls for checking new versions."));

    CHANGELOG.put("6.0.1", Arrays.asList(
        "Allow loading of empty-valued parameters from fragger *.properties files.",
        "Comments in fragger.properties won't overwrite non-commented properties anymore."));

    CHANGELOG.put("6.0", Arrays.asList(
        "Automatic updates for MSFragger",
        "mass_offsets parameter for MSFragger",
        "Lower default number of fragments required in Closed search to 4",
        "Improved tooltips in MSFragger tab",
        "Initial defaults are loaded for Closed search now instead of Open"));

    CHANGELOG.put("5.4", Arrays.asList(
        "Restore last location of MSfragger params file save/load operation.",
        "Show errors from loading msfragger.params files"));

    CHANGELOG.put("5.3", Arrays.asList(
        "Button for auto-detection of decoy prefixes",
        "When sequence DB changes, display the number of proteins.",
        "Auto-detect buggy cached --clevel option and change it to -2."));

    CHANGELOG.put("5.2", Arrays.asList(
        "Revert PeptideProphet --clevel option default to '-2' for Open Searching."));

    CHANGELOG.put("5.1", Arrays.asList(
        "Bug fixes for cross-tool decoy tag updates."));

    CHANGELOG.put("5.0", Arrays.asList(
        "Separate tab for sequence database.",
        "Display info about known compatibility of newer versions of Philosopher."));

    CHANGELOG.put("4.9", Arrays.asList(
        "Stop execution of the pipeline if one of the processes returns non-zero exit code.",
        "Colorize console output a bit, red color for errors.",
        "Button that redirects to the issue tracker online for bug reporting."));

    CHANGELOG.put("4.8", Arrays.asList(
        "Introduce notifications about update contents",
        "User-message can now be shown without a newer version available",
        "Added export button and context menu item to the console to simplify bug reporting by users."));

    CHANGELOG.put("4.7", Arrays.asList(
        "Support new packaging of MSFragger jar with onejar."));

    CHANGELOG.put("4.6",  Arrays.asList(
        "Fix mixed up order of philosopher calls."
    ));

    CHANGELOG.put("4.5",  Arrays.asList(
        "Show parsed versions of tools in the UI.",
        "Print detected versions of tools to console before each run."
    ));

    CHANGELOG.put("4.4",  Arrays.asList(
        "Only run Philosopher workspace --clean/--init once per analysis."
    ));

    CHANGELOG.put("4.3",  Arrays.asList(
        "Locale dependency fix for MSfragger parameters panel.",
        "Philosopher checks version comparing to GitHub at startup."
    ));

    CHANGELOG.put("4.2",  Arrays.asList(
        "Fix the issue that Fragger panel constructor could cause IOException and prevent the whole app from loading."
    ));

    CHANGELOG.put("4.1",  Arrays.asList(
        "Variable mod site definition warning text if cached from older versions.",
        "Java 9 warning for Fragger.",
        "Non-symmetric precursor mass tolerance and detection of \"[*\" for var mods.",
        "Cache fragger params after a dry-run or a real run."
    ));

    CHANGELOG.put("4.0",  Arrays.asList(
        "Added version check for MSFragger-GUI itself, comparing to GitHub.",
        "Two way sync of decoy/tag prefix used by Peptide Prophet and Philosopher Report.",
        "Fix how msfragger jar is auto-found.",
        "Added version to msfragger properties.",
        "MSFragger version check."
    ));
  }

  public static Map<String, List<String>> getChangelog() {
    return Collections.unmodifiableMap(CHANGELOG);
  }

  /**
   *
   */
  public static List<String> updatesSinceCurrentVersion(String versionList) {
    if (StringUtils.isNullOrWhitespace(versionList)) {
      return Collections.emptyList();
    }

    VersionComparator vc = new VersionComparator();
    String[] split = versionList.trim().split("\\s*,\\s*");
    List<String> res = new ArrayList<>();
    for (String updateVersion : split) {
      if (vc.compare(version(), updateVersion) < 0) {
        res.add(updateVersion);
      }
    }
    return res;
  }

  public static String version() {
    Properties p = loadPropertiesWithIdeDebugHack();
    if (!p.containsKey(Version.PROP_VER)) {
      throw new IllegalStateException(String.format("Key '%s' not found in bundle '%s'",
          Version.PROP_VER, PATH_BUNDLE));
    }
    return p.getProperty(Version.PROP_VER);
  }

  private static String chop(String original, String toChop) {
    return original.endsWith(toChop) ?
        original.substring(0, original.length() - toChop.length()) :
        original;
  }

  /**
   * To print changelog using just the jar file use:<br/>
   * <code>java -cp ".\build\libs\FragPipe.jar" umich.msfragger.Version true 2</code>
   *
   * @param args The 1st param is a boolean whether to print GitHub release info preamble or not.
   * Use true, to indicate "yes", any other string for "no". The 2nd parameter is an integer how
   * many versions back worth of changelog to print.
   */
  public static void main(String[] args) {
    int maxVersionsToPrint = 0;
    boolean printGihubPreamble = true;
    if (args != null && args.length > 0) {
      printGihubPreamble = Boolean.parseBoolean(args[0]);

      if (args.length > 1) {
        try {
          long num = Long.parseLong(args[1]);
          maxVersionsToPrint = num > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) num;
        } catch (Exception e) {
          System.err.println("Unrecognized 2nd parameter. "
              + "Should be no params or an integer for how many versions"
              + " back to print changelog for.");
        }
      }
    }

    Properties props = loadPropertiesWithIdeDebugHack();

    if (!props.containsKey(PROP_DOWNLOAD_URL)) {
      throw new IllegalStateException(String.format("Didn't find '%s' in "
          + "FragPipe Bundle file", PROP_DOWNLOAD_URL));
    }
    String url = props.getProperty(PROP_DOWNLOAD_URL);
    String latest = "latest";
    url = chop(url, "/");
    url = chop(url, latest);
    url = chop(url, "/");

    final String version = version();
    if (printGihubPreamble) {
      final String exeFn = String.format("%s.exe", PROGRAM_TITLE);
      final String exeUrl = String.format("%s/download/%s/%s", url, version, exeFn);
      final String jarFn = String.format("%s-%s.jar", PROGRAM_TITLE, version);
      final String jarUrl = String.format("%s/download/%s/%s", url, version, jarFn);
      final String zipFn = String.format("%s-%s.zip", PROGRAM_TITLE, version);
      final String zipUrl = String.format("%s/download/%s/%s", url, version, zipFn);
      String githubReleaseMessage = "FragPipe v" + version + "\n\n"
          + "## Windows users\n" +
          "- Download [" + exeFn + "](" + exeUrl + ") and just run that. Windows 10 might "
          + "show a UAC prompt, saying that this is not a trusted program, it's up to you whether "
          + "to run it or not.\n"
          + "  - If you don't have a compatible Java version, you will be redirected to a website "
          + "where you can download one.\n"
          + "- Or download [" + zipFn + "](" + zipUrl + ").\n"
          + "  - Zip contains `FragPipe.bat` file, which can be started by either clicking or "
          + "running it from the command line.\n"
          + "- Or download the [" + jarFn + "](" + jarUrl + ").\n"
          + "  - You can start the `jar` file with `java -jar " + jarFn + "` or `start javaw -jar "
          + jarFn + "` if you prefer to not have the console window open.\n"
          + "If Java is configured to auto-run `.jar` files, double clicking might also work.\n\n\n"
          + "## Linux/MacOS users\n"
          + "- Either download [" + zipFn + "](" + zipUrl + "), which contains *FragPipe* shell "
          + "script to start the program\n"
          + "- Or download the [" + jarFn + "](" + jarUrl + ") and run "
          + "with `java -jar " + jarFn + "`.";
      System.out.println(githubReleaseMessage);
      System.out.println();
      System.out.println();

    }

    StringBuilder sb = new StringBuilder();
    sb.append("### ").append("Changelog:\n");
    int cnt = 0;
    for (Map.Entry<String, List<String>> e : CHANGELOG.descendingMap().entrySet()) {
      if (maxVersionsToPrint > 0 && ++cnt > maxVersionsToPrint) {
        break;
      }
      String ver = e.getKey();
      sb.append("\nv").append(ver).append(":\n");
      for (String change : e.getValue()) {
        sb.append(" - ").append(change).append("\n");
      }
    }
    System.out.println(sb.toString());
  }

  private static Properties loadPropertiesWithIdeDebugHack() {
    Properties props = new Properties();
    try {
      ResourceBundle bundle = ThisAppProps.getLocalBundle();
      bundle.keySet().forEach(k -> props.setProperty(k, bundle.getString(k)));
    } catch (Exception e) {
      log.debug("Could not load local Bundle for fragpipe, retrying with properties", e);
      try {
        Properties p = new Properties();
        Path path = Paths
            .get(".").toAbsolutePath().resolve("src").resolve(PATH_BUNDLE + ".properties");
        p.load(Files.newBufferedReader(path));
        p.stringPropertyNames().forEach(k -> props.setProperty(k, p.getProperty(k)));
      } catch (IOException ex) {
        log.error("Could not load local Bundle for fragpipe at all", ex);
      }
    }
    return props;
  }

  public static Properties loadPropertiesFromBundle() {
    try (InputStream is = MsfraggerGuiFrame.class.getResourceAsStream("Bundle.properties")) {
      if (is == null) {
        throw new IllegalStateException("Could not read Bundle.properties from the classpath");
      }
      Properties props = new Properties();
      props.load(is);
      return props;
    } catch (IOException e) {
      throw new IllegalStateException("Error reading Bundle.properties from the classpath");
    }
  }

  public static String loadPropFromBundle(String propName) {
    Properties props = loadPropertiesFromBundle();
    String value = props.getProperty(propName);
    if (value == null) {
      throw new IllegalStateException("Property " + propName
          + " was not found in Bundle.properties");
    }
    return value;
  }

  private static List<String> createGuiUpdateMessages(TreeSet<String> newerVersionStrings,
      Properties propsRemote) {
    List<String> messages = new ArrayList<>();
    for (String newerVersion : newerVersionStrings) {
      String verMsg = propsRemote
          .getProperty(Version.PROP_DOWNLOAD_MESSAGE + "." + newerVersion, "");
      if (StringUtils.isNullOrWhitespace(verMsg)) {
        continue;
      }
      messages.add(verMsg);
    }
    return messages;
  }

  public static void checkUpdates() {
    Properties props = ThisAppProps.getRemoteProperties();
    if (props == null) {
      log.info("Didn't get update info from any of the sources");
      return;
    }

    String lastVerKey = PROP_LAST_RELEASE_VER;
    if (!props.stringPropertyNames().contains(lastVerKey)) {
      List<String> urls = Arrays.asList(
          "https://raw.githubusercontent.com/Nesvilab/FragPipe/last-release/MSFragger-GUI/src/" + PATH_BUNDLE + ".properties");
      props = PropertiesUtils.initProperties(urls);
      if (props == null) {
        log.debug("Didn't get dev update info");
        return;
      }
      if (!props.stringPropertyNames().contains(Version.PROP_VER)) {
        log.info("Release branch didn't contain version info in: {}", Version.PROP_VER);
        return;
      }
      lastVerKey = Version.PROP_VER;
    }

//      final URI tmp = URI.create("https://raw.githubusercontent.com/chhh/FragPipe/master/MSFragger-GUI/src/" + PATH_BUNDLE + ".properties");
//      final URI tmp = URI.create("https://raw.githubusercontent.com/chhh/FragPipe/fragger-advanced/MSFragger-GUI/src/" + PATH_BUNDLE + ".properties");
//      String githubProps = IOUtils.toString(tmp, StandardCharsets.UTF_8);
//      Properties props = new Properties();
//      props.load(new StringReader(githubProps));

    // this is used to test functionality without pushing changes to github
//                        props.put("msfragger.gui.version", "5.7");
//                        props.put("msfragger.gui.important-updates", "3.1,3.5,4.9,5.2");
//                        props.put("msfragger.gui.critical-updates", "2.0,3.0,4.6,5.0, 4.7");
//                        props.put("msfragger.gui.download-message", "Happy new year!");
//                        props.put("msfragger.gui.download-message.4.7", "Crit 4.7");
//                        props.put("msfragger.gui.download-message.2.0", "Crit 2.0");
//                        props.put("msfragger.gui.download-message.5.0", "Crit 4.7");
//                        props.put("msfragger.gui.download-message.5.0", "Crit 5.0");
//                        props.put("msfragger.gui.download-message.3.1", "Important 3.1");
//                        props.put("msfragger.gui.download-message.4.9", "Important 4.9");

    final StringBuilder sb = new StringBuilder();
    final VersionComparator vc = new VersionComparator();

    // add new versions notification
    final String githubVersion = props.getProperty(lastVerKey);
    final String localVersion = Version.version();
    if (githubVersion != null && vc.compare(localVersion, githubVersion) < 0) {
      if (sb.length() > 0) {
        sb.append("<br><br>");
      }
      final String defaultDlUrl = loadPropFromBundle(Version.PROP_DOWNLOAD_URL);
      final String dlUrl = props.getProperty(Version.PROP_DOWNLOAD_URL, defaultDlUrl);
      sb.append(String.format(Locale.ROOT,
          "Your %s version is [%s]<br>\n"
              + "There is a newer version of %s available [%s]).<br/>\n"
              + "Please <a href=\"%s\">click here</a> to download a newer one.<br/>",
          Version.PROGRAM_TITLE, localVersion, Version.PROGRAM_TITLE, githubVersion, dlUrl));

      // check for critical or important updates since the current version
      List<String> updatesImportant = Version.updatesSinceCurrentVersion(
          props.getProperty(Version.PROP_IMPORTANT_UPDATES, ""));
      List<String> updatesCritical = Version.updatesSinceCurrentVersion(
          props.getProperty(Version.PROP_CRITICAL_UPDATES, ""));

      if (!updatesCritical.isEmpty()) {
        TreeSet<String> newerVersions = new TreeSet<>(updatesCritical);
        List<String> messages = createGuiUpdateMessages(newerVersions, props);
        if (!messages.isEmpty()) {
          sb.append("<br/><br/><b>Critical updates:</b><br><ul>");
          for (String message : messages) {
            sb.append("<li>").append(message).append("</li>");
          }
          sb.append("</ul>");
        } else {
          sb.append("<br/><b>There have been critical updates.</b><br>");
        }
      }

      if (!updatesImportant.isEmpty()) {
        TreeSet<String> newerVersions = new TreeSet<>(updatesImportant);
        List<String> messages = createGuiUpdateMessages(newerVersions, props);
        if (!messages.isEmpty()) {
          sb.append("<br/>Important updates:<br><ul>");
          for (String message : messages) {
            sb.append("<li>").append(message).append("</li>");
          }
          sb.append("</ul>");
        } else {
          sb.append("<br/><br/>There have been important updates.<br>");
        }
      }
    }

    final String downloadMessage = props.getProperty(Version.PROP_DOWNLOAD_MESSAGE, "");
    if (!StringUtils.isNullOrWhitespace(downloadMessage)) {
      if (sb.length() > 0) {
        sb.append("<br><br><b>");
      }
      sb.append(downloadMessage).append("</b>");
    }

    if (sb.length() > 0) {
      EventBus.getDefault().post(new MessageTipNotification(Version.PROP_VER, sb.toString()));
    }
  }

}
