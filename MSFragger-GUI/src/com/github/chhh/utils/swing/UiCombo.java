package com.github.chhh.utils.swing;

import java.util.NoSuchElementException;
import javax.swing.JComboBox;

public class UiCombo extends JComboBox<String> implements StringRepresentable {

  @Override
  public String asString() {
    final int selectedIndex = getSelectedIndex();
    final int index = Math.max(selectedIndex, 0);
    return getModel().getElementAt(index);
  }

  @Override
  public void fromString(String s) {
    if (!isInModel(s)) {
      throw new NoSuchElementException("String '" + s + "' is not in the UiCombo current range. Component name: [" + getName() + "]");
    }
    setSelectedItem(s);
  }

  private boolean isInModel(String s) {
    if (s == null)
      return false;
    final int count = getItemCount();
    for (int i = 0; i < count; i++) {
      if (s.equals(getItemAt(i)))
        return true;
    }
    return false;
  }
}
