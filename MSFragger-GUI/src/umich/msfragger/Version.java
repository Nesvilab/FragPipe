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
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import javax.swing.JOptionPane;

/**
 * @author Dmitry Avtonomov
 */
public class Version {
    public static final String PROGRAM_TITLE = "MSFragger-GUI";
    public static final String PROP_VER = "msfragger.gui.version";
    public static final String VERSION = "4.4";
    public static final String PROP_DOWNLOAD_URL = "msfragger.gui.download-url";
    
    public static final String PROPERTIES_URL = "https://raw.githubusercontent.com/chhh/MSFragger-GUI/master/MSFragger-GUI/src/umich/msfragger/gui/Bundle.properties";
    public static final URI PROPERTIES_URI = URI.create(PROPERTIES_URL);
    
    private static final TreeMap<String, List<String>> CHANGELOG = new TreeMap<>();
    
    static {
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
     * @param args the first parameter is how many versions back worth of
     *             changelog to print.
     */
    public static void main(String[] args) {
        int maxVersionsToPrint = 0;
        if (args != null && args.length > 0) {
            try {
                maxVersionsToPrint = Integer.parseInt(args[0]);
            } catch (Exception e) {
                System.err.println("Unrecognized first parameter. "
                        + "Should be no params or an integer for how many versions"
                        + " back to print changelog for.");
            }
        }
        StringBuilder sb = new StringBuilder();
        sb.append(PROGRAM_TITLE).append(" v").append(VERSION).append(" changelog:\n");
        int cnt = 0;
        for (Map.Entry<String, List<String>> e : CHANGELOG.descendingMap().entrySet()) {
            if (maxVersionsToPrint > 0 && ++cnt > maxVersionsToPrint)
                break;
            String ver = e.getKey();
            sb.append("v").append(ver).append(":\n");
            for (String change : e.getValue()) {
                sb.append(" - ").append(change).append("\n");
            }
        }
        System.out.println(sb.toString());
    }
}
