package com.github.chhh.utils.swing;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import sun.tools.jconsole.JConsole;

public class MigUtils {
  private static final MigUtils INSTANCE = new MigUtils();

  private MigUtils() {
  }

  public static MigUtils get() {
    return MigUtils.INSTANCE;
  }

  public CC ccGx() {
    return new CC().growX();
  }

  public CC ccL() {
    return new CC().alignX("left");
  }

  public CC ccR() {
    return new CC().alignX("right");
  }

  public JPanel panel() {
    return panel(false, null);
  }

  public JPanel panel(boolean zeroInsets, String borderTitle) {
    LC lc = new LC().fillX();
    if (zeroInsets)
      lc = lc.insetsAll("0px");
    JPanel p = new JPanel(new MigLayout(lc));
    if (borderTitle != null)
      p.setBorder(new TitledBorder(borderTitle));
    return p;
  }

  public CC add(JComponent host, JComponent child) {
    CC cc = ccL();
    host.add(child, cc);
    return cc;
  }

  public FormEntry fe(String name, JComponent comp) {
    return fe(name, FormEntry.LABEL_NOT_SHOWN, comp, null);
  }

  public FormEntry fe(String name, JComponent comp, String tooltip) {
    return fe(name, FormEntry.LABEL_NOT_SHOWN, comp, tooltip);
  }

  public FormEntry fe(String name, String label, JComponent comp, String tooltip) {
    return new FormEntry(name, label, comp, tooltip);
  }
}
