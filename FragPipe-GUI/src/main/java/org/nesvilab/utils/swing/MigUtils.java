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

package org.nesvilab.utils.swing;

import org.nesvilab.utils.swing.FormEntry.Builder;

import java.awt.*;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
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

  public JComponent border(JComponent comp, int thickness) {
    comp.setBorder(new LineBorder(Color.LIGHT_GRAY, 1));
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

  public LC lcNoInsetsTopBottom() {
    return new LC().insets("0px", null, "0px", null);
  }

  public CC add(JComponent host, Component child) {
    CC cc = ccL();
    host.add(child, cc);
    return cc;
  }

  public CC add(JComponent host, Component child, CC cc) {
    host.add(child, cc);
    return cc;
  }

  public CC add(JComponent host, Component child, boolean isAlignLeft) {
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
