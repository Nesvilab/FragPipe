package umich.msfragger.util.swing;

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import umich.msfragger.util.SwingUtils;

public class JPanelWithEnablement extends JPanel {

  protected Map<Component, Boolean> enablementMapping = new HashMap<>();

  protected void updateEnabledStatus(Component top, boolean enabled) {
    if (top == null || top.isEnabled() == enabled)
      return;
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
