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
package org.nesvilab.utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dialog;
import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.ItemSelectable;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jsoup.Jsoup;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.utils.swing.ContentChangedFocusAdapter;
import org.nesvilab.utils.swing.GhostedTextComponent;
import org.nesvilab.utils.swing.HtmlStyledJEditorPane;
import org.nesvilab.utils.swing.JPanelWithEnablement;
import org.nesvilab.utils.swing.StringRepresentable;
import org.nesvilab.utils.swing.UiUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author dmitriya
 */
public class SwingUtils {

  private static final Logger log = LoggerFactory.getLogger(SwingUtils.class);
  private static volatile String[] fontNames = null;
  private static volatile Font[] fonts = null;
  private static final Object fontLock = new Object();
  private static final Pattern re = Pattern.compile("^\\s*<\\s*html\\s*>\\s*");
  private static final Pattern reNewline = Pattern.compile("(?<!<br/>)(\n)");

  private SwingUtils() {
  }

  public static JLabel htmlLabel(String text) {
    String s = SwingUtils.makeHtml(text);
    return new JLabel(s);
  }

  /**
   * Drills down component hierarchy renaming every component that has non-empty {@link
   * JComponent#getName()} with a prefix and suffix.
   */
  public static void renameDeep(Component origin, boolean includeOrigin, String prefix,
      String suffix) {
    final boolean doPrefix = StringUtils.isNotBlank(prefix);
    final boolean doSuffix = StringUtils.isNotBlank(suffix);
    if (!doPrefix && !doSuffix) {
      return;
    }
    SwingUtils.traverse(origin, includeOrigin, component -> {
      String name = component.getName();
      if (StringUtils.isNotBlank(name)) {
        if (doPrefix) {
          name = StringUtils.prependOnce(name, prefix);
        }
        if (doSuffix) {
          name = StringUtils.appendOnce(name, suffix);
        }
        component.setName(name);
      }
    });
  }

  /**
   * Wraps a component in JScrollPane and sets scroll-bar speed to a reasonable value.
   */
  public static JScrollPane wrapInScroll(Component comp) {
    JScrollPane s = new JScrollPane(comp);
    s.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    s.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
    s.getVerticalScrollBar().setUnitIncrement(16);
    return s;
  }

  public static DialogAndThread runThreadWithProgressBar(String title, Component parent,
      Runnable runnable) {
    JFrame frame = SwingUtils.findParentFrame(parent);
    final JDialog dialog = new JDialog(frame, title, true);
    JProgressBar bar = new JProgressBar(0, 100);
    bar.setIndeterminate(true);
    Dimension d = new Dimension(300, 75);
    bar.setMinimumSize(d);
    bar.setSize(d);
    dialog.add(bar, BorderLayout.CENTER);
    dialog.setSize(d);
    dialog.setLocationRelativeTo(parent);

    Thread thread = new Thread(() -> {
      try {
        runnable.run();
      } catch (Exception ex) {
        throw new IllegalStateException("Something happened while running behind a progress bar",
            ex);
      } finally {
        while (!dialog.isVisible())
          try {
            Thread.sleep(1000);
          } catch (InterruptedException ignored) {
          }
        dialog.dispose();
      }
    });
    return new DialogAndThread(dialog, thread);
  }

  public static void setLaf() {
    /* Set the Nimbus look and feel */
    //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
    /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
     */
    try {
      if (OsUtils.isWindows()) {
        // native look on windows
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } else {
        // nimbus otherwise
        for (UIManager.LookAndFeelInfo info : UIManager
            .getInstalledLookAndFeels()) {
          if ("Nimbus".equals(info.getName())) {
            UIManager.setLookAndFeel(info.getClassName());
            break;
          }
        }
      }
    } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e1) {
      java.util.logging.Logger.getLogger(Fragpipe.class.getName())
          .log(java.util.logging.Level.SEVERE, null, e1);
      log.error("Error setting LAF", e1);
      try {
        for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
          if ("Nimbus".equals(info.getName())) {
            UIManager.setLookAndFeel(info.getClassName());
            break;
          }
        }
      } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e2) {
        java.util.logging.Logger.getLogger(Fragpipe.class.getName())
            .log(java.util.logging.Level.SEVERE, null, e2);
        log.error("Error setting LAF", e2);
      }
    }
    //</editor-fold>
  }

  public static class DialogAndThread {

    public final JDialog dialog;
    public final Thread thread;

    public DialogAndThread(JDialog dialog, Thread thread) {
      this.dialog = dialog;
      this.thread = thread;
    }
  }

  public static JTable tableFromData(List<String> headers, List<? extends List<?>> rows) {
    boolean badData = rows.stream().anyMatch(row -> row.size() != headers.size());
    if (badData) {
      throw new IllegalArgumentException("Some data rows were not the same size as table header");
    }
    String[] columns = headers.toArray(new String[0]);
    String[][] data = new String[rows.size()][headers.size()];
    int index = -1;
    for (List<?> row: rows) {
      index += 1;
      for (int i = 0; i < headers.size(); i++) {
        Object o = row.get(i);
        data[index][i] = o != null ? o.toString() : "";
      }
    }

    DefaultTableModel model = new DefaultTableModel(data, columns);
    JTable table = new JTable(model);
    table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);

    return table;
  }

  public static JTable tableFromTwoSiblingFiles(Map<Path, Path> paths) {
    String[] columns = {"From", "To", "At"};
    String[][] data = new String[paths.size()][3];
    int index = -1;
    for (Entry<Path, Path> kv : paths.entrySet()) {
      data[++index][0] = kv.getKey().getFileName().toString();
      data[index][1] = kv.getValue().getFileName().toString();
      if (!kv.getValue().toAbsolutePath().getParent().equals(kv.getKey().toAbsolutePath().getParent())) {
        throw new IllegalArgumentException("Files must be siblings");
      }
      data[index][2] = kv.getKey().toAbsolutePath().getParent().toString();
    }

    DefaultTableModel model = new DefaultTableModel(data, columns);
    JTable table = new JTable(model);
    table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);

    return table;
  }

  public static String getStrVal(Component c) {

    String val;
    if (c instanceof StringRepresentable) {
      val = ((StringRepresentable) c).asString();
    } else if (c instanceof JFormattedTextField) {
      val = ((JFormattedTextField) c).getText();
    } else if (c instanceof JTextField) {
      val = ((JTextField) c).getText();
    } else if (c instanceof JSpinner) {
      val = ((JSpinner) c).getValue().toString();
    } else if (c instanceof JCheckBox) {
      val = Boolean.valueOf(((JCheckBox) c).isSelected()).toString();
    } else if (c instanceof JComboBox) {
      val = ((JComboBox<?>) c).getModel().getSelectedItem().toString();
    } else {
      throw new UnsupportedOperationException(
          "getStrVal() not implemented for type: " + c.getClass().getCanonicalName());
    }

    return val.trim();
  }

  public static void setStrVal(Component c, String val) {
    if (c instanceof StringRepresentable) {
      ((StringRepresentable) c).fromString(val);
    } else if (c instanceof JFormattedTextField) {
      ((JFormattedTextField) c).setText(val);
    } else if (c instanceof JTextField) {
      ((JTextField) c).setText(val);
    } else if (c instanceof JCheckBox) {
      ((JCheckBox) c).setSelected(Boolean.parseBoolean(val));
    } else if (c instanceof JComboBox) {
      ((JComboBox<?>) c).getModel().setSelectedItem(val);
    } else if (c instanceof JSpinner) {
      ((JSpinner) c).setValue(Double.parseDouble(val));
    } else {
      throw new UnsupportedOperationException(
          "setStrVal() not implemented for type: " + c.getClass().getCanonicalName());
    }
  }

  private static DocumentListener createDocListenerNotifyingOnce(final JTextComponent textComp,
      final ChangeListener changeListener) {
    return new DocumentListener() {
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
        SwingUtilities.invokeLater(() -> {
          if (lastNotifiedChange != lastChange) {
            lastNotifiedChange = lastChange;
            changeListener.stateChanged(new ChangeEvent(textComp));
          }
        });
      }
    };
  }

  /**
   * Installs a listener to receive notification when the text of any {@code JTextComponent} is
   * changed. Internally, it installs a {@link DocumentListener} on the text component's {@link
   * Document}, and a {@link PropertyChangeListener} on the text component to detect if the {@code
   * Document} itself is replaced.
   *
   * @param textComp       any text component, such as a {@link JTextField} or {@link JTextArea}
   * @param changeListener a listener to receieve {@link ChangeEvent}s when the text is changed; the
   *                       source object for the events will be the text component
   * @throws NullPointerException if either parameter is null
   *                              <p>
   *                              Taken from http://stackoverflow.com/questions/3953208/value-change-listener-to-jtextfield
   * @author Boann
   */
  public static void addChangeListener(final JTextComponent textComp,
      final ChangeListener changeListener) {
    Objects.requireNonNull(textComp, "text component");
    Objects.requireNonNull(changeListener, "change listener");

    final DocumentListener dl = createDocListenerNotifyingOnce(textComp, changeListener);

    PropertyChangeListener pcl = e -> {
      Document d1 = (Document) e.getOldValue();
      Document d2 = (Document) e.getNewValue();
      if (d1 != null) {
        d1.removeDocumentListener(dl);
      }
      if (d2 != null) {
        d2.addDocumentListener(dl);
      }
      dl.changedUpdate(null);
    };
    textComp.addPropertyChangeListener("document", pcl);

    Document d = textComp.getDocument();
    if (d != null) {
      d.addDocumentListener(dl);
    }
  }

  /**
   * @param itemSelectable E.g. a {@link JCheckBox} will do.
   * @param triggerOnInit  If true, will call the appropriate action based on the initial state of
   *                       the checkbox, before adding the listener.
   */
  public static void addSelectedStateChangeListener(ItemSelectable itemSelectable,
      boolean triggerOnInit,
      Runnable onSelected, Runnable onDeselected) {
    final boolean initState = itemSelectable.getSelectedObjects() != null;
    if (triggerOnInit) {
      if (initState) {
        onSelected.run();
      } else {
        onDeselected.run();
      }
    }
    itemSelectable.addItemListener(e -> {
      switch (e.getStateChange()) {
        case ItemEvent.SELECTED:
          onSelected.run();
          break;
        case ItemEvent.DESELECTED:
          onDeselected.run();
          break;
        default:
          throw new IllegalStateException("Unknown state change event: " + e.getStateChange());
      }
    });
  }

  public static void addItemSelectedListener(ItemSelectable selectable, boolean triggerOnInit, Consumer<ItemEvent> handler) {
    if (triggerOnInit && selectable.getSelectedObjects() != null) {
      handler.accept(new ItemEvent(selectable, ItemEvent.ITEM_STATE_CHANGED, selectable.getSelectedObjects()[0], ItemEvent.SELECTED));
    }
    selectable.addItemListener(e -> {
      if (ItemEvent.SELECTED == e.getStateChange()) {
        handler.accept(e);
      }
    });
  }

  public static void setEnablementUpdater(JPanelWithEnablement p, final Component toToggle,
      ItemSelectable check) {
    addSelectedStateChangeListener(check, true,
        () -> p.updateEnabledStatus(toToggle, true),
        () -> p.updateEnabledStatus(toToggle, false));
  }

  // Disable a component when the provided toggle component is checked. Same as setEnablementUpdater,
  // but disabling when the toggle is activated instead of enabling.
  public static void setDisablementUpdater(JPanelWithEnablement p, final Component toToggle,
                                          ItemSelectable check) {
    addSelectedStateChangeListener(check, true,
            () -> p.updateEnabledStatus(toToggle, false),
            () -> p.updateEnabledStatus(toToggle, true));
  }

  public static void enableComponents(Container container, boolean enabled) {
    enableComponents(container, enabled, false);
  }

  public static void enableComponents(Container container, boolean enabled,
      boolean applyToContainer) {
    enableComponents(container, enabled, applyToContainer, Collections.emptyList());
  }

  public static void enableComponents(Container container, boolean enabled,
      boolean applyToContainer, List<Component> exclusions) {
    if (applyToContainer) {
      container.setEnabled(enabled);
    }
    Component[] components = container.getComponents();
    for (Component component : components) {
      if (exclusions.contains(component)) {
        continue; // skipping excluded components
      }
      component.setEnabled(enabled);
//            if (component instanceof JScrollPane) {
//                JScrollPane jsp = (JScrollPane)component;
//                enableComponents(jsp.getViewport(), enable);
//            }
      if (component instanceof Container) {
        enableComponents((Container) component, enabled, applyToContainer);
      }
    }
  }

  /**
   * Creates a non-editable JEditorPane that has the same styling as default JLabels. Hyperlink
   * clicks are opened using the default browser.
   *
   * @param text Your text to be displayed in HTML context. Don't add the opening and closing HTML
   *             tags. To include links use the regular A tags.
   */
  public static HtmlStyledJEditorPane createClickableHtml(String text) {
    return createClickableHtml(text, true, true, null);
  }

  public static final int NONE = 0, TOP = 1, VCENTER = 2, BOTTOM = 4, LEFT = 8, HCENTER = 16, RIGHT = 32;
  private static final int OFFSET = 100; // Required for hack (see below).
  /**
   * Scroll to specified location.  e.g. <tt>scroll(component, BOTTOM);</tt>.
   *
   * @param c JComponent to scroll.
   * @param part Location to scroll to.  Should be a bit-wise OR of one or more of the values:
   * {@link SwingUtils#NONE}, {@link SwingUtils#TOP}, {@link SwingUtils#VCENTER},
   * {@link SwingUtils#BOTTOM}, {@link SwingUtils#LEFT}, {@link SwingUtils#HCENTER}, {@link SwingUtils#RIGHT}.
   */
  public static void scrollTo(JComponent c, int part) {
    scrollTo(c, part & (LEFT|HCENTER|RIGHT), part & (TOP|VCENTER|BOTTOM));
  }

  /**
   * Scroll to specified location.  e.g. <tt>scroll(component, LEFT, BOTTOM);</tt>.
   *
   * @param c JComponent to scroll.
   * @param horizontal Horizontal location.  Should take the value: LEFT, HCENTER or RIGHT.
   * @param vertical Vertical location.  Should take the value: TOP, VCENTER or BOTTOM.
   */
  public static void scrollTo(JComponent c, int horizontal, int vertical) {
    Rectangle visible = c.getVisibleRect();
    Rectangle bounds = c.getBounds();

    switch (vertical) {
      case TOP:     visible.y = 0; break;
      case VCENTER: visible.y = (bounds.height - visible.height) / 2; break;
      case BOTTOM:  visible.y = bounds.height - visible.height + OFFSET; break;
    }

    switch (horizontal) {
      case LEFT:    visible.x = 0; break;
      case HCENTER: visible.x = (bounds.width - visible.width) / 2; break;
      case RIGHT:   visible.x = bounds.width - visible.width + OFFSET; break;
    }

    // When scrolling to bottom or right of viewport, add an OFFSET value.
    // This is because without this certain components (e.g. JTable) would
    // not scroll right to the bottom (presumably the bounds calculation
    // doesn't take the table header into account.  It doesn't matter if
    // OFFSET is a huge value (e.g. 10000) - the scrollRectToVisible method
    // still works correctly.

    c.scrollRectToVisible(visible);
  }

  /**
   * Creates a non-editable JEditorPane that has the same styling as default JLabels. Hyperlink
   * clicks are opened using the default browser.
   *
   * @param applyMakeHtml Apply {@link #makeHtml(String)} function before creating Editor Pane.
   * @param text          Your text to be displayed in HTML context. Don't add the opening and
   *                      closing HTML tags. To include links use the regular A tags.
   */
  public static HtmlStyledJEditorPane createClickableHtml(boolean applyMakeHtml, String text) {
    return createClickableHtml(applyMakeHtml ? makeHtml(text) : text, true, true, null);
  }

  public static JScrollPane createClickableHtmlInScroll(boolean applyMakeHtml, String text) {
    return createClickableHtmlInScroll(applyMakeHtml, text, null);
  }

  public static JScrollPane createClickableHtmlInScroll(boolean applyMakeHtml, String text,
      Dimension preferredEditorPaneSize) {
    JEditorPane ep = createClickableHtml(applyMakeHtml ? makeHtml(text) : text, true,
        true, null);
    ep.setPreferredSize(preferredEditorPaneSize);
    JScrollPane s = new JScrollPane();
    s.setViewportView(ep);
    return s;
  }

  /**
   * If given text is HTML, will return the body as text, otherwise will just return the text. This
   * is used for Editor Panes which have text/html content and are styled using css, which in the
   * end gets in the way of saving the contents of said Editor Pane.
   */
  public static String tryExtractHtmlBody(String t) {
    String body;
    if (t.contains("<html")) {
      org.jsoup.nodes.Document d = Jsoup.parse(t);
      body = d.body() != null ? d.body().html() : d.html();
    } else {
      body = t;
    }
    return body;
  }

  public static String createCssStyle() {
    return createCssStyle(null);
  }

  public static String createCssStyle(Font font) {
    // for copying style
    if (font == null) {
      font = new JLabel().getFont();
    }

    // create some css from the label's font
    StringBuilder style = new StringBuilder("font-family:" + font.getFamily() + ";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");
    return style.toString();
  }

  public static String wrapInStyledHtml(String text) {
    return wrapInStyledHtml(text, null);
  }

  public static String wrapInStyledHtml(String text, Font font) {
    String body = SwingUtils.tryExtractHtmlBody(text);
    StringBuilder sb = new StringBuilder();
    sb.append("<html><body style=\"").append(createCssStyle(font)).append("\">")
        .append(body)
        .append("</body></html>");
    return sb.toString();
  }

//  /**
//   * Creates a non-editable JEditorPane that has the same styling as default JLabels and with
//   * hyperlinks clickable. They will be opened in the system default browser.
// * @param text Your text to be displayed in HTML context. Don't add the opening and closing HTML
//   * tags. To include links use the regular A tags.
//   * @param handleHyperlinks Add a handler for hyperlinks to be opened in the
// * @param useJlabelBackground Use default background of JLabels.
//   */
//  public static JEditorPane createClickableHtml(String text, boolean handleHyperlinks,
//      boolean useJlabelBackground) {
//    return createClickableHtml(text, handleHyperlinks, useJlabelBackground, null);
//  }

  /**
   * Creates a non-editable JEditorPane that has the same styling as default JLabels and with
   * hyperlinks clickable. They will be opened in the system default browser.
   *
   * @param text    Your text to be displayed in HTML context. Don't add the opening and closing
   *                HTML tags. To include links use the regular A tags.
   * @param bgColor if {@code useJlabelBackground} is false, force this color. Can be null
   */
  public static HtmlStyledJEditorPane createClickableHtml(String text,
      Color bgColor) {
    return createClickableHtml(text, true, false, bgColor);
  }

  /**
   * Creates a non-editable JEditorPane that has the same styling as default JLabels and with
   * hyperlinks clickable. They will be opened in the system default browser.
   *
   * @param text                Your text to be displayed in HTML context. Don't add the opening and
   *                            closing HTML tags. To include links use the regular A tags.
   * @param handleHyperlinks    Add a handler for hyperlinks to be opened in the
   * @param useJlabelBackground Use default background of JLabels.
   * @param bgColor             if {@code useJlabelBackground} is false, force this color. Can be
   *                            null
   */
  public static HtmlStyledJEditorPane createClickableHtml(String text, boolean handleHyperlinks,
      boolean useJlabelBackground, Color bgColor) {
    return createClickableHtml(text, handleHyperlinks, useJlabelBackground, bgColor,
        false);
  }

  /**
   * Creates a non-editable JEditorPane that has the same styling as default JLabels and with
   * hyperlinks clickable. They will be opened in the system default browser.
   *
   * @param text                Your text to be displayed in HTML context. Don't add the opening and
   *                            closing HTML tags. To include links use the regular A tags.
   * @param handleHyperlinks    Add a handler for hyperlinks to be opened in the
   * @param useJlabelBackground Use default background of JLabels.
   * @param bgColor             if {@code useJlabelBackground} is false, force this color. Can be
   *                            null
   * @param editable            If the editor pane should be editable.
   */
  public static HtmlStyledJEditorPane createClickableHtml(String text, boolean handleHyperlinks,
      boolean useJlabelBackground, Color bgColor, boolean editable) {

    String html1 = wrapInStyledHtml(text);
    HtmlStyledJEditorPane ep = new HtmlStyledJEditorPane(handleHyperlinks);
    ep.setText(html1);
    ep.setEditable(editable);

    if (useJlabelBackground) {
      ep.setBackground(new JLabel().getBackground());
    } else if (bgColor != null) {
      ep.setBackground(bgColor);
    }

    return ep;
  }

  /**
   * Make the parent JDialog of a component resizable using the HierarchyListener. Taken from:
   * https://stackoverflow.com/a/7989417/88814
   */
  public static void makeDialogResizable(Component c) {
    c.addHierarchyListener(e -> {
      Window window = SwingUtilities.getWindowAncestor(c);
      if (window instanceof Dialog) {
        Dialog dialog = (Dialog) window;
        if (!dialog.isResizable()) {
          dialog.setResizable(true);
        }
      }
    });
  }

  /**
   * Tries to open the default browser.
   *
   * @throws IllegalStateException if the operation fails.
   */
  public static void openBrowserOrThrow(String url) {
    try {
      Desktop.getDesktop().browse(new URI(url));
    } catch (IOException | URISyntaxException ex) {
      throw new IllegalStateException("Could not open link in default system browser", ex);
    }
  }


  /**
   * Tries to open the default browser.
   *
   * @throws IllegalStateException if the operation fails.
   */
  public static void openBrowserOrThrow(URI uri) {
    try {
      Desktop.getDesktop().browse(uri);
    } catch (IOException ex) {
      throw new IllegalStateException("Could not open link in default system browser", ex);
    }
  }

  /**
   * Tries to open the default browser. Does nothing if the operation fails.
   *
   * @param doLog Log the error with slf4j or not.
   */
  public static void openBrowserOrLog(URI uri, boolean doLog) {
    try {
      Desktop.getDesktop().browse(uri);
    } catch (IOException e) {
      if (doLog) {
        log.error("Could not open link in default system browser", e);
      }
    }
  }

  public static boolean isEnabledAndChecked(JCheckBox checkbox) {
    return checkbox.isEnabled() && checkbox.isSelected();
  }

  public static boolean isEnabledAndChecked(JToggleButton toggle) {
    return toggle.isEnabled() && toggle.isSelected();
  }

  public static void traverse(Component origin, boolean includeOrigin,
      Consumer<Component> callback) {
    synchronized (origin.getTreeLock()) {
      ArrayDeque<Component> lifo = new ArrayDeque<>();
      if (includeOrigin) {
        lifo.addLast(origin);
      } else if (origin instanceof Container) {
        for (Component child : ((Container) origin).getComponents()) {
          lifo.addLast(child);
        }
      }
      while (!lifo.isEmpty()) {
        Component comp = lifo.removeLast();
        callback.accept(comp);
        if (comp instanceof Container) {
          for (Component child : ((Container) comp).getComponents()) {
            lifo.addLast(child);
          }
        }
      }
    }
  }

  /**
   * Drills down a {@link Container}, mapping all components that 1) have their name set, 2) are
   * {@link StringRepresentable} and returns the mapping.<br/> Useful for persisting values from
   * Swing windows.
   *
   * @param compNameFilter Can be null, will accept all Component names then.
   */
  public static Map<String, String> valuesGet(Container origin, Predicate<String> compNameFilter) {
    compNameFilter = compNameFilter == null ? s -> true : compNameFilter;
    Map<String, Component> comps = SwingUtils.mapComponentsByName(origin, true);
    Map<String, String> map = new HashMap<>(comps.size());
    for (Entry<String, Component> e : comps.entrySet()) {
      final String name = e.getKey();
      if (name == null || name.isEmpty()) {
        continue;
      }
      if (!compNameFilter.test(name)) {
        log.debug("Skipping serializing component, name filtered out: {}", name);
        continue;
      }

      final Component comp = e.getValue();
      String value = valueGet(comp);

      if (value == null) {
        log.debug(String
            .format("SwingUtils.valuesToMap() found component of type [%s] by name [%s] which "
                    + "does not implement [%s] and is not [%s, %s]",
                comp.getClass().getSimpleName(), comp.getName(),
                StringRepresentable.class.getSimpleName(), JCheckBox
                    .class.getSimpleName(), JTextComponent.class.getSimpleName()));
        continue;
      }

//      if (comp instanceof GhostedTextComponent && value
//          .equals(((GhostedTextComponent) comp).getGhostText())) {
//        log.debug("Skipping serializing ghost text component to map: '{}' has ghost value: '{}'",
//            name, value);
//      }

      map.put(name, value);

    }
    return map;
  }

  public static String valueGet(Component comp) {
    String value = null;
    if (comp instanceof StringRepresentable) {
      value = ((StringRepresentable) comp).asString();
    } else if (comp instanceof GhostedTextComponent) {
      value = ((GhostedTextComponent)comp).getNonGhostText();
    } else if (comp instanceof JEditorPane) {
      JEditorPane ep = (JEditorPane)comp;
      if ("text/html".equalsIgnoreCase(ep.getContentType())) {
        value = Jsoup.parse(ep.getText()).body().html();
      } else {
        value = ep.getText();
      }
    } else if (comp instanceof JToggleButton) {
      value = Boolean.toString(((JToggleButton)comp).isSelected());
    } else if (comp instanceof JTextComponent) {
      value = ((JTextComponent) comp).getText();
    }
    return value;
  }

  public static void valueSet(Component comp, String s) {
    if (comp instanceof StringRepresentable) {
      ((StringRepresentable) comp).fromString(s);
    } else if (comp instanceof HtmlStyledJEditorPane) {
      HtmlStyledJEditorPane ep = (HtmlStyledJEditorPane) comp;
      ep.setText(s); // HtmlStyledJEditorPane automatically does html wrapping/unwrapping
    } else if (comp instanceof JEditorPane) {
      JEditorPane ep = (JEditorPane) comp;
      ep.setContentType("text/html");
      ep.setText(SwingUtils.wrapInStyledHtml(s));
    } else if (comp instanceof JToggleButton) {
      ((JToggleButton) comp).setSelected(Boolean.parseBoolean(s));
    } else if (comp instanceof JTextComponent) {
      ((JTextComponent) comp).setText(s);
    } else {
      throw new IllegalArgumentException(
          "Component not StringRepresentable, JCheckBox or JTextComponent. Can't set.");
    }
  }

  /**
   * Sets values for components in a {@link Container}. Components must 1) have their name set, 2)
   * be either {@link StringRepresentable} or 3) {@link JCheckBox}, {@link JTextComponent}.
   */
  public static void valuesSet(Container origin, Map<String, String> map) {
    Map<String, Component> comps = SwingUtils.mapComponentsByName(origin, true);
    for (Entry<String, String> kv : map.entrySet()) {
      final String name = kv.getKey();
      Component comp = comps.get(name);
      if (comp == null) {
        continue;
      }
      valueSet(comp, kv.getValue());
    }
  }

  /**
   * @param comp Must implement {@link StringRepresentable}
   */
  public static void addOnFocusLostAndContentChanged(Component comp,
      BiConsumer<String, String> onContentChanged) {
    if (!(comp instanceof StringRepresentable)) {
      throw new IllegalArgumentException("component not StringRepresentable");
    }
    comp.addFocusListener(
        new ContentChangedFocusAdapter((StringRepresentable) comp, onContentChanged));
  }

  /**
   * @return Null if none of the fonts are available, otherwise the first available font found.
   */
  public static String checkFontAvailable(String... fontName) {
    final String[] fontsLocal = getAvailableFontNames();
    Font[] availableFonts = getAvailableFonts();
    for (String fontSearched : fontName) {
      for (String fontSystem : fontsLocal) {
        if (fontSystem.equals(fontSearched)) {
          return fontSystem;
        }
      }
    }
    return null;
  }

  public static String[] getAvailableFontNames() {
    String[] fontsLocal = fontNames;
    if (fontsLocal == null) {
      synchronized (fontLock) {
        fontsLocal = fontNames;
        if (fontsLocal == null) {
          GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
          fontNames = fontsLocal = ge.getAvailableFontFamilyNames();

        }
      }
    }
    return fontsLocal;
  }

  public static Font[] getAvailableFonts() {
    Font[] fontsLocal = fonts;
    if (fontsLocal == null) {
      synchronized (fontLock) {
        fontsLocal = fonts;
        if (fontsLocal == null) {
          GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
          fonts = fontsLocal = ge.getAllFonts();
        }
      }
    }
    return fontsLocal;
  }

  /**
   * Traverses from origin down the hierarchy putting all components with names set to a map.
   */
  public static Map<String, Component> mapComponentsByName(Container origin,
      boolean includeOrigin) {
    if (origin == null) {
      return Collections.emptyMap();
    }
    Map<String, Component> map = new HashMap<>();
    traverse(origin, includeOrigin, c -> {
      String name = c.getName();
      if (!StringUtils.isNullOrWhitespace(name)) {
        map.put(name, c);
      }
    });
    return map;
  }

  /**
   * Show a message dialog wrapped into a scroll pane.
   *
   * @param parent    The parent for the dialog, null is ok.
   * @param component The component to be used as the message.
   */
  public static void showDialog(Component parent, final Component component) {
    makeDialogResizable(component);
    JOptionPane.showMessageDialog(parent, wrapInScrollForDialog(component));
  }

  /**
   * Show a message dialog wrapped into a scroll pane.
   *
   * @param parent    The parent for the dialog, null is ok.
   * @param component The component to be used as the message.
   */
  public static void showDialog(Component parent, final Component component, String title, int msgType) {
    makeDialogResizable(component);
    JOptionPane.showMessageDialog(parent, wrapInScrollForDialog(component), title, msgType);
  }

  /**
   * Show a message dialog wrapped into a scroll pane.
   *
   * @param parent    The parent for the dialog, null is ok.
   * @param component The component to be used as the message.
   */
  public static int showConfirmDialog(Component parent, final Component component) {
    makeDialogResizable(component);
    return JOptionPane.showConfirmDialog(parent, wrapInScrollForDialog(component));
  }

  public static int showConfirmDialog2(Component parent, final Component component, final String title, final int optionType) {
    makeDialogResizable(component);
    return JOptionPane.showConfirmDialog(parent, wrapInScrollForDialog(component), title, optionType);
  }

  public static JButton createButtonOpenInFileManager(Component parent, String text, Supplier<Path> pathProvider) {
    JButton b = UiUtils.createButton(text, e -> {
      try {
        Path path = pathProvider.get();
        if (path == null) {
          log.warn("Path provider returned null path");
          return;
        }
        Desktop.getDesktop().open(path.toFile());
      } catch (IOException ex) {
        SwingUtils
            .showErrorDialog(parent, "Could not open path in system file browser.", "Error");
      }
    });
    return b;
  }

  /** Use to display a short confirmation message, like "Sure you want to delete stuff?". */
  public static boolean showConfirmDialogShort(Component parent, String text) {
    int confirmation = JOptionPane.showConfirmDialog(parent, new JLabel(SwingUtils.makeHtml(text)));
    return JOptionPane.OK_OPTION == confirmation;
  }

  public static int showConfirmDialog(Component parent, final Component component, String title) {
    makeDialogResizable(component);
    return JOptionPane.showConfirmDialog(parent, wrapInScrollForDialog(component), title, JOptionPane.OK_CANCEL_OPTION);
  }

  public static int showChoiceDialog(Component parent, String title, Object message, String[] options, int startingOption) {
    return JOptionPane.showOptionDialog(parent, message, title, JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[startingOption]);
  }

  /**
   * Wraps the given component in a scroll pane and attaches a hierarchy listener that makes the
   * parent dialog resizeable if the component is attached to a {@link Dialog}. This is mainly for
   * use with {@link JOptionPane#showMessageDialog(Component, Object)} and the likes.
   */
  public static JScrollPane wrapInScrollForDialog(Component component) {
    // wrap a scrollpane around the component
    final JScrollPane scrollPane = new JScrollPane(component);
    // make the dialog resizable
    component.addHierarchyListener(e -> {
      Window window = SwingUtilities.getWindowAncestor(component);
      if (window != null) {
        Fragpipe.decorateFrame(window);
        window.setAlwaysOnTop(true);
        if (window instanceof java.awt.Dialog) {
          Dialog dialog = (Dialog) window;
          if (!dialog.isResizable()) {
            dialog.setResizable(true);
          }
          if (!dialog.isModal()) {
            dialog.setModal(true);
            dialog.setModalityType(ModalityType.APPLICATION_MODAL);
          }
        }
      }
    });
    scrollPane.setBorder(new EmptyBorder(10, 10, 10, 10));
    return scrollPane;
  }

  /**
   * Sets the uncaught exception handler for the thread this method is invoked in to a handler that
   * shows a Swing GUI message dialog with error stacktrace.
   */
  public static void setUncaughtExceptionHandlerMessageDialog(Component parent) {
    Thread.setDefaultUncaughtExceptionHandler((t, e) -> {
      final String notes = ExceptionUtils.getStackTrace(e);

      JPanel panel = new JPanel();
      panel.setLayout(new BorderLayout());
      panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
      panel.add(new JLabel("Something unexpected happened"), BorderLayout.PAGE_START);
      JTextArea notesArea = new JTextArea(40, 80);
      notesArea.setText(notes);
      JScrollPane notesScroller = new JScrollPane();
      notesScroller.setBorder(BorderFactory.createTitledBorder("Details: "));
      notesScroller.setViewportView(notesArea);
      panel.add(notesScroller, BorderLayout.CENTER);

      //JOptionPane.showMessageDialog(frame, "Some error details:\n\n" + notes, "Error", JOptionPane.ERROR_MESSAGE);
      //JOptionPane.showMessageDialog(frame, panel, "Error", JOptionPane.ERROR_MESSAGE);
      makeDialogResizable(panel);
      showDialog(parent, panel);
    });
  }

  /**
   * @param parent Can be null.
   */
  public static void showErrorDialogWithStacktrace(Throwable e, Component parent) {
    showErrorDialogWithStacktrace(e, parent, true);
  }

  public static String makeHtml(String html) {
    if (html == null) {
      return "";
    } else {
      Matcher m = reNewline.matcher(html);
      String s = m.replaceAll("<br/>");
      return re.matcher(s).find() ? s : "<html>" + s;
    }
  }

  public static String stripHtml(String html) {
    if (html == null) {
      return "";
    } else {
      return html.replaceAll("[\n\r]", " ");
    }
  }

  public static void showInfoDialog(Component parent, String htmlMessage, String title) {
    showDialog(parent, htmlMessage, title, JOptionPane.INFORMATION_MESSAGE);
  }

  public static void showWarningDialog(Component parent, String htmlMessage, String title) {
    showDialog(parent, htmlMessage, title, JOptionPane.WARNING_MESSAGE);
  }

  public static void showErrorDialog(Component parent, String htmlMessage, String title) {
    showDialog(parent, htmlMessage, title, JOptionPane.ERROR_MESSAGE);
  }

  public static void showDialog(Component parent, String htmlMessage, String title, int messageType) {
    if (Fragpipe.headless) {
      if (messageType == JOptionPane.INFORMATION_MESSAGE) {
        log.info(stripHtml(htmlMessage));
      } else {
        log.error(stripHtml(htmlMessage));
      }
    } else {
      JOptionPane.showMessageDialog(parent, new JLabel(makeHtml(htmlMessage)), title, messageType);
    }
  }

  /**
   * @param parent Can be null.
   */
  public static void showErrorDialogWithStacktrace(Throwable e, Component parent, JComponent content, boolean doShowStacktrace) {
    if (Fragpipe.headless) {
      if (doShowStacktrace) {
        log.error(ExceptionUtils.getStackTrace(e));
      } else {
        log.error(e.getMessage());
      }
    } else {
      JPanel panel = new JPanel();
      panel.setLayout(new BorderLayout());
      panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
      if (content == null) {
        content = new JLabel("Something unexpected happened (" + e.getClass().getSimpleName() + ")");
      }
      panel.add(content, BorderLayout.PAGE_START);
      JTextArea notesArea = new JTextArea(40, 80);
      if (doShowStacktrace) {
        notesArea.setText(ExceptionUtils.getStackTrace(e));
      } else {
        notesArea.setText(e.getMessage());
      }
      JScrollPane notesScroller = new JScrollPane();
      notesScroller.setBorder(BorderFactory.createTitledBorder("Details: "));
      notesScroller.setViewportView(notesArea);
      panel.add(notesScroller, BorderLayout.CENTER);

      //JOptionPane.showMessageDialog(frame, "Some error details:\n\n" + notes, "Error", JOptionPane.ERROR_MESSAGE);
      //JOptionPane.showMessageDialog(frame, panel, "Error", JOptionPane.ERROR_MESSAGE);
      makeDialogResizable(panel);
      showDialog(parent, panel);
    }
  }


  /**
   * @param parent Can be null.
   */
  public static void showErrorDialogWithStacktrace(Throwable e, Component parent, boolean doShowStacktrace) {
    showErrorDialogWithStacktrace(e, parent, null, doShowStacktrace);
  }

  /**
   * @param path If the passed path already exists, just returns it.
   * @return null if no existing Path could be found on the filesystem all the way up to root.
   */
  public static Path findExistingUpstreamPath(Path path) {
    if (path == null || Files.exists(path)) {
      return path;
    } else {
      return findExistingUpstreamPath(path.toAbsolutePath().getParent());
    }
  }

  public static JFrame findParentFrame(Component origin) {
    Component parentFrameForDialog = findParentFrameForDialog(origin);
    if (parentFrameForDialog instanceof JFrame) {
      return (JFrame) parentFrameForDialog;
    }
    return null;
  }

  /**
   * Bubbles up the component hierarchy searching for first instance of a {@link JFrame}.
   */
  public static Component findParentFrameForDialog(Component origin) {
    if (origin == null) {
      return null;
    }
    if (origin instanceof JFrame) {
      return origin;
    }

    Container parent = origin.getParent();
    while (parent != null && !(parent instanceof JFrame)) {
      parent = parent.getParent();
    }
    return parent;
  }

  /**
   * Tries to set the LAF to native for the platform. Does nothing if the LAF is not available.
   *
   * @return true if setting the LAF succeeded.
   */
  public static boolean setPlatformLookAndFeel() {
    try {
      String laf = UIManager.getSystemLookAndFeelClassName();
      UIManager.setLookAndFeel(laf);
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * Tries to set the LAF to native for the platform or Nimbus if failed.
   *
   * @return true if setting the LAF succeeded.
   */
  public static boolean setPlatformLafOrNimbus() {
    if (setPlatformLookAndFeel()) {
      return true;
    }
    try {
      for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
        if ("Nimbus".equals(info.getName())) {
          UIManager.setLookAndFeel(info.getClassName());
          return true;
        }
      }
    } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException ignored) {
    }
    return false;
  }

  /**
   * Centers a JFrame on screen.
   *
   * @param frame the frame to be centered
   */
  public static void centerFrame(JFrame frame) {
    Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
    frame.setLocation(dim.width / 2 - frame.getSize().width / 2,
        dim.height / 2 - frame.getSize().height / 2);
  }

  /**
   * Sets the icons for a frame. These are also used to display icons in the taskbar.
   *
   * @param frame                The frame to set the icons for.
   * @param iconPaths            The simplest way is to provide just the file names.
   * @param classToFindResources A class relative to which the icons will be searched. This is a
   *                             kludge to make things more fool-proof.
   */
  public static void setFrameIcons(JFrame frame, java.util.List<String> iconPaths,
      Class<?> classToFindResources) {
    java.util.List<Image> icons = new ArrayList<>();
    for (String iconPath : iconPaths) {
      java.net.URL imgURL = classToFindResources.getResource(iconPath);
      ImageIcon image = new ImageIcon(imgURL);
      icons.add(image.getImage());
    }
    frame.setIconImages(icons);
  }

  /**
   * Nicely closes the frame, respecting its {@code setOnCloseOperation()} settings.
   *
   * @param frame the frame to be closed
   */
  public static void closeFrameNicely(JFrame frame) {
    frame.dispatchEvent(new WindowEvent(frame, WindowEvent.WINDOW_CLOSING));
  }

  public static boolean isGraphicalEnvironmentAvailable() {
    boolean headless = true;

    String nm = System.getProperty("java.awt.headless");

    if (nm == null) {
      /* No need to ask for DISPLAY when run in a browser */
      if (System.getProperty("javaplugin.version") != null) {
        headless = Boolean.FALSE;
      } else {
        String osName = System.getProperty("os.name");
        headless = ("Linux".equals(osName) ||
            "SunOS".equals(osName)) && (System.getenv("DISPLAY") == null);
      }
    } else if (nm.equals("true")) {
      headless = Boolean.TRUE;
    } else {
      headless = Boolean.FALSE;
    }

    return !headless;
  }

  public static void userShowDialog(Component frame, final Component component) {
    // wrap a scrollpane around the component
    JScrollPane scrollPane = new JScrollPane(component);
    // make the dialog resizable
    component.addHierarchyListener(e -> {
      Window window = SwingUtilities.getWindowAncestor(component);
      if (window instanceof Dialog) {
        Dialog dialog = (Dialog) window;
        if (!dialog.isResizable()) {
          dialog.setResizable(true);
        }
      }
    });
    // display them in a message dialog
    JOptionPane.showMessageDialog(frame, scrollPane);
  }

  public static void userShowError(Component frame, String stacktrace) {
    JPanel panel = new JPanel();
    panel.setLayout(new BorderLayout());
    panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    panel.add(new JLabel("Something unexpected happened"), BorderLayout.PAGE_START);
    JTextArea notesArea = new JTextArea(40, 80);
    notesArea.setText(stacktrace);
    JScrollPane notesScroller = new JScrollPane();
    notesScroller.setBorder(BorderFactory.createTitledBorder("Details: "));
    notesScroller.setViewportView(notesArea);
    panel.add(notesScroller, BorderLayout.CENTER);
    //JOptionPane.showMessageDialog(frame, "Some error details:\n\n" + notes, "Error", JOptionPane.ERROR_MESSAGE);
    //JOptionPane.showMessageDialog(frame, panel, "Error", JOptionPane.ERROR_MESSAGE);
    makeDialogResizable(panel);
    userShowDialog(frame, panel);
  }
}
