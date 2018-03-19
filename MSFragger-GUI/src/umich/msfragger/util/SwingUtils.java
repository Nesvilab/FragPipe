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
package umich.msfragger.util;

import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import umich.msfragger.gui.MsfraggerGuiFrame;

/**
 *
 * @author Dmitry Avtonomov
 */
public class SwingUtils {

    public static Component findParentComponentForDialog(Component origin) {
        if (origin == null) {
            return null;
        }
        Container parent = origin.getParent();
        if (parent instanceof JFrame) {
            return parent;
        }
        return findParentComponentForDialog(parent);
    }

    private SwingUtils() {}
    
    public static void setFileChooserPath(JFileChooser fileChooser, String path) {
        if (path == null) {
            fileChooser.setCurrentDirectory(null);
            return;
        }
        Path p = Paths.get(path);
        if (Files.exists(p)) {
            fileChooser.setCurrentDirectory(p.toFile());
        } else {
            fileChooser.setCurrentDirectory(null);
        }
    }
    
    
    public static void enableComponents(Container container, boolean enable) {
        Component[] components = container.getComponents();
        for (Component component : components) {
            component.setEnabled(enable);
//            if (component instanceof JScrollPane) {
//                JScrollPane jsp = (JScrollPane)component;
//                enableComponents(jsp.getViewport(), enable);
//            }
            if (component instanceof Container) {
                enableComponents((Container)component, enable);
            }
        }
    }
    
    /**
     * Installs a listener to receive notification when the text of any
     * {@code JTextComponent} is changed. Internally, it installs a
     * {@link DocumentListener} on the text component's {@link Document}, and a
     * {@link PropertyChangeListener} on the text component to detect if the
     * {@code Document} itself is replaced.
     *
     * @param text any text component, such as a {@link JTextField} or
     * {@link JTextArea}
     * @param changeListener a listener to receieve {@link ChangeEvent}s when
     * the text is changed; the source object for the events will be the text
     * component
     * @throws NullPointerException if either parameter is null
     * 
     * Taken from http://stackoverflow.com/questions/3953208/value-change-listener-to-jtextfield
     * 
     * @author Boann
     */
    public static void addChangeListener(final JTextComponent text, final ChangeListener changeListener) {
        if (text == null || changeListener == null)
            throw new IllegalArgumentException("Both the text component and the change listener need to be non-null");
                
        
        final DocumentListener dl = new DocumentListener() {
            private int lastChange = 0, lastNotifiedChange = 0;

            @Override
            public void insertUpdate(DocumentEvent e) {
                changedUpdate(e);
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                changedUpdate(e);
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                lastChange++;
                
                Runnable runnable = new Runnable() {
                    @Override
                    public void run() {
                        if (lastNotifiedChange != lastChange) {
                            lastNotifiedChange = lastChange;
                            changeListener.stateChanged(new ChangeEvent(text));
                        }
                    }
                };
                
                SwingUtilities.invokeLater(runnable);
            }
        };
   
        PropertyChangeListener pcl = new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent e) {
                Document d1 = (Document) e.getOldValue();
                Document d2 = (Document) e.getNewValue();
                if (d1 != null) {
                    d1.removeDocumentListener(dl);
                }
                if (d2 != null) {
                    d2.addDocumentListener(dl);
                }
                dl.changedUpdate(null);
            }
        };
        text.addPropertyChangeListener("document", pcl);
        
        Document d = text.getDocument();
        if (d != null) {
            d.addDocumentListener(dl);
        }
    }
    
    /**
     * Creates a non-editable JEditorPane that has the same styling as default
     * JLabels and with hyperlinks clickable. They will be opened in the system
     * default browser.
     * @param text Your text to be displayed in HTML context. Don't add the 
     * opening and closing HTML tags. To include links use the regular A tags.
     * @param addDefaultHyperlinkHandler If true, will add a handler for all hyperlinks
     * to be opened in the default system browser.
     * @return 
     */
    public static JEditorPane createClickableHtml(String text, boolean addDefaultHyperlinkHandler)  {
        // for copying style
        JLabel label = new JLabel();
        Font font = label.getFont();

        // create some css from the label's font
        StringBuilder style = new StringBuilder("font-family:" + font.getFamily() + ";");
        style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
        style.append("font-size:").append(font.getSize()).append("pt;");

        JEditorPane ep = new JEditorPane("text/html", "<html><body style=\"" + style + "\">"
            + text
            + "</body></html>");
        ep.setEditable(false);
        
        // handle link events
        if (addDefaultHyperlinkHandler) {
            ep.addHyperlinkListener(new HyperlinkListener()
            {
                @Override
                public void hyperlinkUpdate(HyperlinkEvent e) {
                    if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
                        try {
                            openBrowserOrThrow(e.getURL().toURI());
                        } catch (URISyntaxException ex) {
                            throw new IllegalStateException("Incorrect url/uri", ex);
                        }
                        
                    }
                }
            });
        }
        
        return ep;
    }
    
    public static void openBrowserOrThrow(URI uri) {
        try {
            Desktop.getDesktop().browse(uri);
        } catch (IOException ex) {
            throw new IllegalStateException("Could not open link in default system browser", ex);
        }
    }
    
    /**
     * Creates a non-editable JEditorPane that has the same styling as default
     * JLabels. Hyperlink clicks are not handled, user {@link JEditorPane#addHyperlinkListener(javax.swing.event.HyperlinkListener) }
     * @param text Your text to be displayed in HTML context. Don't add the 
     * opening and closing HTML tags. To include links use the regular A tags.
     * @return 
     */
    public static JEditorPane createClickableHtml(String text)  {
        return createClickableHtml(text, true);
    }
}
