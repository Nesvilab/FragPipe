/*
 * Copyright 2017 Dmitry Avtonomov.
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
package umich.msfragger;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import javax.swing.JOptionPane;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.VersionComparator;

/**
 * @author Dmitry Avtonomov
 */
public class Version {
    public static final String PROGRAM_TITLE = "MSFragger-GUI";
    public static final String PROP_VER = "msfragger.gui.version";
    public static final String VERSION = "4.8";
    public static final String PROP_DOWNLOAD_URL = "msfragger.gui.download-url";
    public static final String PROP_DOWNLOAD_MESSAGE = "msfragger.gui.download-message";
    public static final String PROP_IMPORTANT_UPDATES = "msfragger.gui.important-updates";
    public static final String PROP_CRITICAL_UPDATES = "msfragger.gui.critical-updates";
    
    public static final String PROPERTIES_URL = "https://raw.githubusercontent.com/chhh/MSFragger-GUI/master/MSFragger-GUI/src/umich/msfragger/gui/Bundle.properties";
    public static final URI PROPERTIES_URI = URI.create(PROPERTIES_URL);
    
    private static final TreeMap<String, List<String>> CHANGELOG = new TreeMap<>();
    
    static {
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
    
    public static String getVersion() {
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N
        String val = bundle.getString(PROP_VER);
        if (!VERSION.equals(val)) {
            JOptionPane.showMessageDialog(null, String.format(Locale.ROOT, 
                    "Version in the bundle (%s) doesn't match hardcoded value (%s).\n"
                            + "Have you modified MSFragger-GUI.jar/umich/msfragger/gui/Bundle.properties?", val, VERSION), 
                    "Version mismatch", JOptionPane.WARNING_MESSAGE);
        }
        return VERSION;
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
            if (vc.compare(Version.VERSION, updateVersion) < 0) {
                res.add(updateVersion);
            }
        }
        return res;
    }
    
    /**
     * To print changelog using just the jar file use:<br/>
     * <code>`java -cp ".\dist\MSFragger-GUI.jar" umich.msfragger.Version true 2`</code>
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
        
        if (printGihubPreamble) {
            String githubReleaseMessage = String.format(
                    "### Windows users\n" +
                            "- You may download the [*.zip* file]"
                            + "(https://github.com/chhh/MSFragger-GUI/releases/download/v%s/MSFragger-GUI_v%s.zip).\n" +
                            "  - You can start the `jar` file with `start javaw -jar MSFragger-GUI.jar` or "
                            + "`java -jar MSFragger-GUI.jar` or using the provided `.bat` script in the zip archive. If Java is configured to auto-run `.jar` files, double clicking might also work.\n" +
                            "- You may download the [*.exe* file]"
                            + "(https://github.com/chhh/MSFragger-GUI/releases/download/v%s/MSFragger-GUI.exe) and "
                            + "just run that. Windows 10 might show a UAC prompt, saying that this is not a trusted "
                            + "program, it's up to you whether to run it or not.\n" +
                            "  - If you don't have a compatible Java version, you will be redirected to a website where you "
                            + "can download one.\n" +
                            "\n" +
                            "\n" +
                            "### Linux/MacOS users\n" +
                            "Download the [*.zip* file]"
                            + "(https://github.com/chhh/MSFragger-GUI/releases/download/v%s/MSFragger-GUI_v%s.zip) "
                            + "and either run the included launcher shell script or just with "
                            + "`java -jar MSFragger-GUI.jar`.", VERSION, VERSION, VERSION, VERSION, VERSION);
            System.out.println(githubReleaseMessage);
            System.out.println("");
            System.out.println("");
            
        }
        
        
        StringBuilder sb = new StringBuilder();
        sb.append("## ").append(PROGRAM_TITLE).append(" v").append(VERSION).append(" changelog:\n");
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
