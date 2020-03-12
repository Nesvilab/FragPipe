package com.github.chhh.utils.swing;

import javax.swing.Icon;
import javax.swing.JCheckBox;

public class UiCheck extends JCheckBox implements StringRepresentable {

  public UiCheck() {
  }

  public UiCheck(String text, Icon icon) {
    super(text, icon);
  }

  public UiCheck(String text, Icon icon, boolean selected) {
    super(text, icon, selected);
  }

  @Override
  public String asString() {
    return Boolean.toString(isSelected());
  }

  @Override
  public void fromString(String s) {
    try {
      setSelected(Boolean.parseBoolean(s));
    } catch (Exception e1) {
      try {
        int i = Integer.parseInt(s);
        setSelected(i != 0);
      } catch (Exception e2) {
        throw new IllegalArgumentException("String does not represent a boolean value.");
      }
    }
  }

  public static UiCheck of(String label, boolean initState) {
    return new UiCheck(label, null, initState);
  }
}
