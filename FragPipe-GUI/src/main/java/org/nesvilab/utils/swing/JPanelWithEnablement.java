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

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JPanelWithEnablement extends JPanel {
  private static final Logger log = LoggerFactory.getLogger(JPanelWithEnablement.class);

  protected Map<Component, Boolean> enablementMapping = new HashMap<>();

  public void updateEnabledStatus(Component top, boolean enabled) {
    if (top == null)
      return;
    log.debug("Enabled={} Update called for {}", enabled, StringUtils.isNullOrWhitespace(top.getName()) ? top : top.getName());

    enablementMapping.put(top, enabled);
    SwingUtilities.invokeLater(() -> {
      ArrayDeque<Component> stack = new ArrayDeque<>();
      stack.push(top);
      while (!stack.isEmpty()) {
        Component c = stack.pop();
        Container parent = c.getParent();
        boolean isParentEnabled = parent != null && parent.isEnabled();
        boolean enabledStatus = enabled && isParentEnabled && enablementMapping.getOrDefault(c, true);
        c.setEnabled(enabledStatus);

        if (c instanceof Container) {
          for (Component child : ((Container) c).getComponents()) {
            stack.push(child);
          }
        }
      }
    });
  }

  public Map<String, String> toMap() {
    return SwingUtils.valuesGet(this, (name) -> !name.contains("Spinner.formattedTextField"));
  }


}
