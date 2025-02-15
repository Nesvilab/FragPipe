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

import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.util.Objects;
import java.util.function.BiConsumer;

public class ContentChangedFocusAdapter extends FocusAdapter {
  private final StringRepresentable component;
  private final BiConsumer<String, String> onContentChanged;
  private String before = null;
  private String after = null;

  public ContentChangedFocusAdapter(StringRepresentable component, BiConsumer<String, String> onContentChanged) {
    this.component = component;
    this.onContentChanged = onContentChanged;
  }

  @Override
  public void focusGained(FocusEvent e) {
    before = component.asString();
    after = before;
    super.focusGained(e);
  }

  @Override
  public void focusLost(FocusEvent e) {
    after = component.asString();
    if (!Objects.equals(after, before)) {
      onContentChanged.accept(before, after);
    }
    super.focusLost(e);
  }
}
