package com.github.chhh.utils.swing;

import com.github.chhh.utils.SwingUtils;
import java.util.NoSuchElementException;
import javax.swing.JComboBox;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UiCombo extends JComboBox<String> implements StringRepresentable {
  private static final Logger log = LoggerFactory.getLogger(UiCombo.class);

  @Override
  public String asString() {
    final int selectedIndex = getSelectedIndex();
    final int index = Math.max(selectedIndex, 0);
    return getModel().getElementAt(index);
  }

  @Override
  public void fromString(String s) {
    if (!isInModel(s)) {
      String message = "String '" + s + "' is not in the UiCombo current range.\n"
          + "Component name: [" + getName() + "]\n"
          + "Selection will be set to the first value in list: " + getModel().getElementAt(0);
      log.error(message);
      SwingUtils.showErrorDialog(null, message, "Dropdown menu options changed");
      //throw new NoSuchElementException(message);
      setSelectedIndex(0);
    } else {
      setSelectedItem(s);
    }

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
