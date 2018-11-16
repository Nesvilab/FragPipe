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

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.TreeMap;
import javax.swing.JOptionPane;
import umich.msfragger.util.BundleUtils;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.VersionComparator;

/**
 * @author Dmitry Avtonomov
 */
public class Version {
    public static final String PROGRAM_TITLE = "FragPipe";
    public static final String PROP_VER = "msfragger.gui.version";
    public static final String PROP_DOWNLOAD_URL = "msfragger.gui.download-url";
    public static final String PROP_ISSUE_TRACKER_URL = "msfragger.gui.issue-tracker";
    public static final String PROP_DOWNLOAD_MESSAGE = "msfragger.gui.download-message";
    public static final String PROP_IMPORTANT_UPDATES = "msfragger.gui.important-updates";
    public static final String PROP_CRITICAL_UPDATES = "msfragger.gui.critical-updates";
    
    public static final String PATH_BUNDLE = "umich/msfragger/gui/Bundle";
    public static final String PROPERTIES_REMOTE_URL = 
            "https://raw.githubusercontent.com/chhh/FragPipe/master/MSFragger-GUI/src/" 
            + PATH_BUNDLE + ".properties";
    public static final URI PROPERTIES_REMOTE_URI = URI.create(PROPERTIES_REMOTE_URL);
    
    private static final TreeMap<String, List<String>> CHANGELOG = new TreeMap<>(new VersionComparator());
    
    static {
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
        
        {
            String ver = "4.6";
            List<String> changes = new ArrayList();
            changes.add(String.format(Locale.ROOT, "Fix mixed up order of philosopher calls."));
            CHANGELOG.put(ver, changes);
        }
        
        {
            String ver = "4.5";
            List<String> changes = new ArrayList();
            changes.add(String.format(Locale.ROOT, "Show parsed versions of tools in the UI."));
            changes.add(String.format(Locale.ROOT, "Print detected versions of tools to console before each run."));
            CHANGELOG.put(ver, changes);
        }
        
        {
            String ver = "4.4";
            List<String> changes = new ArrayList();
            changes.add(String.format(Locale.ROOT, "Only run Philosopher workspace --clean/--init once per analysis."));
            CHANGELOG.put(ver, changes);
        }
        
        {
            String ver = "4.3";
            List<String> changes = new ArrayList();
            changes.add(String.format(Locale.ROOT, "Locale dependency fix for MSfragger parameters panel."));
            changes.add(String.format(Locale.ROOT, "Philosopher checks version comparing to GitHub at startup."));
            CHANGELOG.put(ver, changes);
        }
        
        {
            String ver = "4.2";
            List<String> changes = new ArrayList();
            changes.add(String.format(Locale.ROOT, "Fix the issue that Fragger panel constructor could cause IOException and prevent the whole app from loading."));
            CHANGELOG.put(ver, changes);
        }
        
        {
            String ver = "4.1";
            List<String> changes = new ArrayList();
            changes.add(String.format(Locale.ROOT, "Variable mod site definition warning text if cached from older versions."));
            changes.add(String.format(Locale.ROOT, "Java 9 warning for Fragger."));
            changes.add(String.format(Locale.ROOT, "Non-symmetric precursor mass tolerance and detection of \"[*\" for var mods."));
            changes.add(String.format(Locale.ROOT, "Cache fragger params after a dry-run or a real run."));
            CHANGELOG.put(ver, changes);
        }
        
        {
            String ver = "4.0";
            List<String> changes = new ArrayList();
            changes.add(String.format(Locale.ROOT, "Added version check for MSFragger-GUI itself, comparing to GitHub."));
            changes.add(String.format(Locale.ROOT, "Two way sync of decoy/tag prefix used by Peptide Prophet and Philosopher Report."));
            changes.add(String.format(Locale.ROOT, "Fix how msfragger jar is auto-found."));
            changes.add(String.format(Locale.ROOT, "Added version to msfragger properties."));
            changes.add(String.format(Locale.ROOT, "MSFragger version check."));
            CHANGELOG.put(ver, changes);
        }
    }
    
    public static java.util.ResourceBundle bundle() {
        return java.util.ResourceBundle.getBundle(PATH_BUNDLE);
    }
    
    public static Map<String, List<String>> getChangelog() {
        return Collections.unmodifiableMap(CHANGELOG);
    }
    
    /**
     * 
     * @param versionList
     * @return 
     */
    public static List<String> updatesSinceCurrentVersion(String versionList) {
        if (StringUtils.isNullOrWhitespace(versionList)) return Collections.EMPTY_LIST;
        
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
      ResourceBundle bundle;
      try {
        bundle = ResourceBundle.getBundle(Version.PATH_BUNDLE);
      } catch (Exception e) {
        throw new IllegalStateException("Could not fetch the resource bundle at: " + Version.PATH_BUNDLE ,e);
      }
      if (!bundle.containsKey(Version.PROP_VER)) {
        throw new IllegalStateException(String.format("Key '%s' not found in bundle '%s'",
            Version.PROP_VER, Version.PATH_BUNDLE));
      }
      return bundle.getString(Version.PROP_VER);
    }
    
    private static String chop(String original, String toChop) {
        return original.endsWith(toChop) ?
                original.substring(0, original.length() - toChop.length()) :
                original;
    }
    
    /**
     * To print changelog using just the jar file use:<br/>
     * <code>`java -cp ".\dist\FragPipe.jar" umich.msfragger.Version true 2`</code>
     * 
     * @param args The 1st param is a boolean whether to print GitHub release
     * info preamble or not. Use true, to indicate "yes", any other string for 
     * "no".
     * The 2nd parameter is an integer how many versions back worth of 
     * changelog to print.
     */
    public static void main(String[] args) {
        int maxVersionsToPrint = 0;
        boolean printGihubPreamble = true;
        if (args != null && args.length > 0) {
            printGihubPreamble = Boolean.parseBoolean(args[0]);
            
            if (args.length > 1) {
                try {
                    long num = Long.parseLong(args[1]);
                    maxVersionsToPrint = num > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int)num; 
                } catch (Exception e) {
                    System.err.println("Unrecognized 2nd parameter. "
                            + "Should be no params or an integer for how many versions"
                            + " back to print changelog for.");
                }
            }
        }
        
        ResourceBundle fragpipeBundle = Version.bundle();
        if (!fragpipeBundle.containsKey(PROP_DOWNLOAD_URL)) 
            throw new IllegalStateException(String.format("Didn't find '%s' in "
                    + "FragPipe Bundle file", PROP_DOWNLOAD_URL));
        String url = fragpipeBundle.getString(PROP_DOWNLOAD_URL);
        String latest = "latest";
        url = chop(url, "/");
        url = chop(url, latest);
        url = chop(url, "/");



      final String version = version();
      if (printGihubPreamble) {
            String githubReleaseMessage = String.format(
                    "### Windows users\n" +
                            "- You may download the [*.zip* file]"
                            + "(%s/download/v%s/%s_v%s.zip).\n" +
                            "  - You can start the `jar` file with `start javaw -jar %s.jar` or "
                            + "`java -jar %s.jar` or using the provided `.bat` script in the zip archive. "
                            + "If Java is configured to auto-run `.jar` files, double clicking might also work.\n" +
                            "- You may download the [*.exe* file]"
                            + "(%s/download/v%s/%s.exe) and "
                            + "just run that. Windows 10 might show a UAC prompt, saying that this is not a trusted "
                            + "program, it's up to you whether to run it or not.\n" +
                            "  - If you don't have a compatible Java version, you will be redirected to a website where you "
                            + "can download one.\n" +
                            "\n" +
                            "\n" +
                            "### Linux/MacOS users\n" +
                            "Download the [*.zip* file]"
                            + "(%s/download/v%s/%s_v%s.zip) "
                            + "and either run the included launcher shell script or just with "
                            + "`java -jar %s.jar`.", 
                            url, version, PROGRAM_TITLE, version,
                            PROGRAM_TITLE,
                            PROGRAM_TITLE,
                            url, version, PROGRAM_TITLE,
                            url, version, PROGRAM_TITLE, version,
                            PROGRAM_TITLE);
            System.out.println(githubReleaseMessage);
            System.out.println("");
            System.out.println("");
            
        }
        
        
        StringBuilder sb = new StringBuilder();
        sb.append("## ").append(PROGRAM_TITLE).append(" v").append(version).append(" changelog:\n");
        int cnt = 0;
        for (Map.Entry<String, List<String>> e : CHANGELOG.descendingMap().entrySet()) {
            if (maxVersionsToPrint > 0 && ++cnt > maxVersionsToPrint)
                break;
            String ver = e.getKey();
            sb.append("\nv").append(ver).append(":\n");
            for (String change : e.getValue()) {
                sb.append(" - ").append(change).append("\n");
            }
        }
        System.out.println(sb.toString());
    }
}
