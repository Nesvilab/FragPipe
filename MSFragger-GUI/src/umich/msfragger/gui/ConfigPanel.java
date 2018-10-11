package umich.msfragger.gui;

import javax.swing.JPanel;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.NotImplementedException;

public class ConfigPanel extends JPanel {
  public static final String TAB_NAME = "Conf";

  public ConfigPanel() {
    super();
    initMore();
  }

  private void initMore() {
    LC lc = new LC();
    this.setLayout(new MigLayout(lc));
    CC ccGrowX = new CC().growX();

    throw new NotImplementedException("TODO: replace the old Config panel with a hand-built one with MigLayout"); // TODO: Not implemented
  }
}
