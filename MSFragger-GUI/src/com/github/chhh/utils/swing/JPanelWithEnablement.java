package com.github.chhh.utils.swing;

import com.github.chhh.utils.StringUtils;
import java.awt.Component;
import java.awt.Container;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.chhh.utils.SwingUtils;

public class JPanelWithEnablement extends JPanel {
  private static final Logger log = LoggerFactory.getLogger(JPanelWithEnablement.class);

  protected Map<Component, Boolean> enablementMapping = new HashMap<>();

  protected void updateEnabledStatus(Component top, boolean enabled) {
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
    return SwingUtils.valuesToMap(this, (name) -> !name.startsWith("Spinner.formattedTextField"));
  }
}
