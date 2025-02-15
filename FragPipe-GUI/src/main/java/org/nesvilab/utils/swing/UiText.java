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

import javax.swing.JTextField;
import javax.swing.text.Document;
import java.util.Objects;

public class UiText extends JTextField implements StringRepresentable, GhostedTextComponent {
  private String ghostText = null;

  public UiText() {
    super();
  }

  public UiText(String text) {
    super(text);
  }

  public UiText(String text, String ghostText) {
    super(text);
    this.ghostText = ghostText;
  }

  public UiText(int columns) {
    super(columns);
  }

  public UiText(String text, int columns) {
    super(text, columns);
  }

  public UiText(Document doc, String text, int columns) {
    super(doc, text, columns);
  }

  @Override
  public String asString() {
    return getNonGhostText().trim();
  }

  @Override
  public void fromString(String s) {
    setText(s);
  }

  @Override
  public void setGhostText(String text) {
    this.ghostText = text;
  }

  @Override
  public String getGhostText() {
    return this.ghostText;
  }

  @Override
  public String getNonGhostText() {
    final String t = getText().trim();
    return Objects.equals(ghostText, t) ? "" : t;
  }
}
