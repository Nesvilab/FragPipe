package umich.msfragger.params.umpire;

import java.awt.Dimension;
import java.text.DecimalFormat;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
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


    LC lc = new LC().flowY().fillX();
    this.setLayout(new MigLayout(lc));

    // Panel - top
    JPanel pTop = new JPanel(new MigLayout());
    //pTop.setBorder(new TitledBorder("General options"));

    JCheckBox checkRunUmpireSe = new JCheckBox("Run DIA-Umpire SE (Signal Extraction)");
    pTop.add(checkRunUmpireSe);


    // Panel - fragment grouping options
    JPanel pFrag = new JPanel(new MigLayout());
    pFrag.setBorder(new TitledBorder("Fragment grouping options"));

    DefaultFormatterFactory fmtFactoryDecimalAsInt = new DefaultFormatterFactory(
        new NumberFormatter(new DecimalFormat("#0")));
    DefaultFormatterFactory fmtFactoryDecimal = new javax.swing.text.DefaultFormatterFactory(
        new javax.swing.text.NumberFormatter());

    JLabel lblRpMax = new JLabel("RPmax");
    JLabel lblRfMax = new JLabel("RFMax");
    JLabel lblCorrThresh = new JLabel("Corr Threshold");
    JLabel lblDeltaApex = new JLabel("Delta Apex");
    JLabel lblRtOverlap = new JLabel("RT Overlap");

    JFormattedTextField fmtRpMax = new JFormattedTextField(fmtFactoryDecimalAsInt);
    JFormattedTextField fmtRfMax = new JFormattedTextField(fmtFactoryDecimal);
    JFormattedTextField fmtCorrThresh = new JFormattedTextField(fmtFactoryDecimal);
    JFormattedTextField fmtDeltaApex = new JFormattedTextField(fmtFactoryDecimal);
    JFormattedTextField fmtRtOverlap = new JFormattedTextField(fmtFactoryDecimal);
    JCheckBox checkBoostComplimentaryIons = new JCheckBox("Boost complimentary ions");
    JCheckBox checkAdjustFragIntensity = new JCheckBox("Adjust fragment intensity");

    CC ccFmt = new CC().width("30:50:70px");
    CC ccFmtWrap = new CC().width("30:50:70px").wrap();
    CC ccLbl = new CC().alignX("right").gapBefore("5px");
    pFrag.add(lblRpMax, ccLbl);
    pFrag.add(fmtRpMax, ccFmt);
    pFrag.add(lblRfMax, ccLbl);
    pFrag.add(fmtRfMax, ccFmt);
    pFrag.add(lblCorrThresh, ccLbl);
    pFrag.add(fmtCorrThresh, ccFmt);
    pFrag.add(lblDeltaApex, ccLbl);
    pFrag.add(fmtDeltaApex, ccFmt);
    pFrag.add(lblRtOverlap, ccLbl);
    pFrag.add(fmtRtOverlap, ccFmtWrap);
    pFrag.add(checkBoostComplimentaryIons, "span, split 2");
    pFrag.add(checkAdjustFragIntensity);



    this.add(pTop, new CC().growX());
    this.add(pFrag, new CC().growX());
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
