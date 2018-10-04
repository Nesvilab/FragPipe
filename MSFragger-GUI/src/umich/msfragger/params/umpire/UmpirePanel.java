package umich.msfragger.params.umpire;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.NotImplementedException;

public class UmpirePanel extends JPanel {

  private ImageIcon icon;

  public UmpirePanel() {
    initMore();
  }

  private void initMore() {
    this.setLayout( new MigLayout( ) );
    JLabel label = new JLabel("This is some test text");
    this.add( label, "push, align center"); // give the label MigLayout constraints
    icon = new ImageIcon(
        getClass().getResource("/umich/msfragger/gui/icons/dia-umpire-16x16.png"));
  }

  public UmpireParams collectUmpireParams() {
    throw new NotImplementedException("TODO: gather params object from the form"); // TODO: Not implemented
  }

  public ImageIcon getIcon() {
    return icon;
  }
}
