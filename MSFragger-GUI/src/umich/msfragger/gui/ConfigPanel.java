package umich.msfragger.gui;

import javax.swing.JPanel;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

public class ConfigPanel extends JPanel {
  public static final String TAB_NAME = "Config";

  public ConfigPanel() {
    super();
    initMore();
  }

  private void initMore() {
    LC lc = new LC();
    this.setLayout(new MigLayout(lc));

    
  }
}
