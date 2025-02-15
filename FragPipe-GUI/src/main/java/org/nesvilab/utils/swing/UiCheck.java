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
