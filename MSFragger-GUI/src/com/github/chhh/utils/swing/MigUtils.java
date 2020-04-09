package com.github.chhh.utils.swing;

import com.github.chhh.utils.swing.FormEntry.Builder;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

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

  public LC layout(JComponent comp) {
    LC lc = new LC();
    comp.setLayout(new MigLayout(lc));
    return lc;
  }

  public LC layout(JComponent comp, LC lc) {
    comp.setLayout(new MigLayout(lc));
    return lc;
  }

  public JComponent border(JComponent comp, String borderText) {
    comp.setBorder(new TitledBorder(borderText));
    return comp;
  }

  public JComponent border(JComponent comp, Border border) {
    comp.setBorder(border);
    return comp;
  }

  public JComponent borderEmpty(JComponent comp) {
    comp.setBorder(new EmptyBorder(0, 0, 0, 0));
    return comp;
  }

  public JComponent borderEmpty(JComponent comp, int margin) {
    comp.setBorder(new EmptyBorder(margin, margin, margin, margin));
    return comp;
  }

  public JPanel newPanel(String borderTitle, boolean setLayout) {
    JPanel p = new JPanel();
    if (borderTitle != null)
      p.setBorder(new TitledBorder(borderTitle));
    if (setLayout)
      p.setLayout(new MigLayout(new LC().fillX()));
    return p;
  }

  public JPanel newPanel(LC layoutConstraints) {
    return newPanel(null, layoutConstraints);
  }

  public JPanel newPanel(String borderTitle, LC layoutConstraints) {
    JPanel p = new JPanel();
    if (borderTitle != null)
      p.setBorder(new TitledBorder(borderTitle));
    if (layoutConstraints != null)
      p.setLayout(new MigLayout(layoutConstraints));
    return p;
  }

  public LC lcFillX() {
    return new LC().fillX();
  }

  public LC lcFillXNoInsetsTopBottom() {
    return lcFillX().insets("0px", null, "0px", null);
  }

  public CC add(JComponent host, JComponent child) {
    CC cc = ccL();
    host.add(child, cc);
    return cc;
  }

  public CC add(JComponent host, JComponent child, CC cc) {
    host.add(child, cc);
    return cc;
  }

  public CC add(JComponent host, JComponent child, boolean isAlignLeft) {
    CC cc = isAlignLeft ? ccL() : ccR();
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

  public FormEntry.Builder feb(String name, JComponent component) {
    return new Builder(component, name);
  }

  public FormEntry.Builder feb(JComponent component) {
    return new Builder(component);
  }
}
