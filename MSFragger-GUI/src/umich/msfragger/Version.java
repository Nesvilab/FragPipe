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

import javax.swing.JOptionPane;

/**
 *
 * @author Dmitry Avtonomov
 */
public class Version {
    public static final String PROP_VER = "msfragger.gui.version";
    public static final String VERSION = "2.4";
    
    public static String getVersion() {
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N
        String val = bundle.getString(PROP_VER);
        if (!VERSION.equals(val)) {
            JOptionPane.showMessageDialog(null, String.format(
                    "Version in the bundle (%s) doesn't match hardcoded value (%s).\n"
                            + "Have you modified MSFragger-GUI.jar/umich/msfragger/gui/Bundle.properties?", val, VERSION), 
                    "Version mismatch", JOptionPane.WARNING_MESSAGE);
        }
        return VERSION;
    }
    
    
}
