package com.github.chhh.utils.swing;

import javax.swing.Icon;
import javax.swing.JRadioButton;

public class UiRadio extends JRadioButton implements StringRepresentable {

  public UiRadio(Icon icon, boolean selected) {
    super(icon, selected);
  }

  public UiRadio(String text, Icon icon, boolean selected) {
    super(text, icon, selected);
  }


  @Override
  public String asString() {
    return Boolean.toString(isSelected());
  }

  @Override
  public void fromString(String s) {
    try {
      setSelected(Boolean.valueOf(s));
    } catch (Exception e1) {
      try {
        int i = Integer.parseInt(s);
        setSelected(i != 0);
      } catch (Exception e2) {
        throw new IllegalArgumentException("String does not represent a boolean value.");
      }
    }
  }
}
