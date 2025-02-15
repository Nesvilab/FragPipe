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

import java.text.Format;
import javax.swing.JFormattedTextField;

public class UiFormattedText extends JFormattedTextField implements StringRepresentable {

  public UiFormattedText() {
  }

  public UiFormattedText(Format format) {
    super(format);
  }

  public UiFormattedText(AbstractFormatter formatter) {
    super(formatter);
  }

  public UiFormattedText(AbstractFormatterFactory factory) {
    super(factory);
  }

  public UiFormattedText(AbstractFormatterFactory factory, Object currentValue) {
    super(factory, currentValue);
  }

  @Override
  public String asString() {
    return getText().trim();
  }

  @Override
  public void fromString(String s) {
    setText(s);
  }
}
