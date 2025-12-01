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
      String message = "String '" + s + "' is not in the UiCombo current range. "
          + "Component name: [" + getName() + "]. "
          + "Selection will be set to the first value in list: " + getModel().getElementAt(0);
      log.debug(message);
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
