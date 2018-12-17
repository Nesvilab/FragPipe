package com.github.chhh.utils.swing;

import javax.swing.JComboBox;

public class UiCombo extends JComboBox<String> implements StringRepresentable {

  @Override
  public String asString() {
    final int selectedIndex = getSelectedIndex();
    final int index = selectedIndex < 0 ? 0 : selectedIndex;
    return getModel().getElementAt(index);
  }

  @Override
  public void fromString(String s) {
    setSelectedItem(s);
  }
}
