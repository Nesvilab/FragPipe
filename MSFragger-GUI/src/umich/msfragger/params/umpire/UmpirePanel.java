package umich.msfragger.params.umpire;

import java.awt.Dimension;
import java.text.DecimalFormat;
import java.text.ParseException;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JFormattedTextField;
import javax.swing.JFormattedTextField.AbstractFormatter;
import javax.swing.JFormattedTextField.AbstractFormatterFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.NotImplementedException;

public class UmpirePanel extends JPanel {

  private ImageIcon icon;

  public UmpirePanel() {
    initMore();
  }

  private void initMore() {
    icon = new ImageIcon(
        getClass().getResource("/umich/msfragger/gui/icons/dia-umpire-16x16.png"));

    //    this.setLayout( new MigLayout( ) );
    //    JLabel label = new JLabel("This is some test text");
    //    this.add( label, "push, align center"); // give the label MigLayout constraints
    //    new JLabel()

    //this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    LC lc = new LC().flowY();
    this.setLayout(new MigLayout(lc));

    // top panel
    JPanel pTop = new JPanel(new MigLayout());
    pTop.setBorder(new TitledBorder("General options"));
//    pTop.setMinimumSize(new Dimension(100, 10));
//    pTop.setPreferredSize(new Dimension(150, 50));
//    pTop.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));

    JCheckBox checkRunUmpireSe = new JCheckBox("Run DIA-Umpire SE (Signal Extraction)");

//    pTop.add(checkRunUmpireSe, "span, left, gapbottom 15");
    pTop.add(checkRunUmpireSe, new CC().gapBottom("30px"));


    // fragment grouping options
    JPanel pFrag = new JPanel(new MigLayout());
    pFrag.setBorder(new TitledBorder("Fragment grouping options"));

    DefaultFormatterFactory fmtFactoryDecimalAsInt = new DefaultFormatterFactory(
        new NumberFormatter(new DecimalFormat("#0")));
    DefaultFormatterFactory fmtFactoryDecimal = new javax.swing.text.DefaultFormatterFactory(
        new javax.swing.text.NumberFormatter());
    Dimension dimFmt = new Dimension(50, 24);

    JLabel lblRpMax = new JLabel("RPmax");
    JLabel lblRfMax = new JLabel("RFMax");
    JFormattedTextField fmtRpMax = new JFormattedTextField(fmtFactoryDecimalAsInt);
    JFormattedTextField fmtRfMax = new JFormattedTextField(fmtFactoryDecimalAsInt);
    JCheckBox checkBoostComplimentaryIons = new JCheckBox("Boost complimentary ions");

//    pFrag.add(lblRpMax, "align label");
    pFrag.add(lblRpMax, new CC().alignX("label"));
    pFrag.add(fmtRpMax);
    pFrag.add(lblRfMax);
//    pFrag.add(fmtRfMax, "wrap");
    pFrag.add(fmtRfMax, new CC().wrap());
//    pFrag.add(checkBoostComplimentaryIons, "align label");
    pFrag.add(checkBoostComplimentaryIons, new CC().alignX("label"));

    this.add(pTop);
    this.add(pFrag);
//    this.add(Box.createVerticalGlue());
  }

  public UmpireParams collectUmpireParams() {
    throw new NotImplementedException("TODO: gather params object from the form"); // TODO: Not implemented
  }

  public ImageIcon getIcon() {
    return icon;
  }

  private void buildUmpireSeParamsForm(JPanel p) {

  }
}
