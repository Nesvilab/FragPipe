package umich.msfragger.params.umpire;

import java.text.DecimalFormat;
import java.text.Normalizer.Form;
import java.util.ArrayList;
import java.util.List;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;
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

    DefaultFormatterFactory decimalAsInt = new DefaultFormatterFactory(
        new NumberFormatter(new DecimalFormat("#0")));
    DefaultFormatterFactory decimal = new javax.swing.text.DefaultFormatterFactory(
        new javax.swing.text.NumberFormatter());



    JLabel lblRtOverlap = new JLabel("RT Overlap");

    FormEntry feRpMax = new FormEntry(UmpireParams.PROP_RPmax, "RP max", new JFormattedTextField(decimalAsInt));
    FormEntry feRfMax = new FormEntry(UmpireParams.PROP_RFmax, "RF max", new JFormattedTextField(decimal));
    FormEntry feCorrThresh = new FormEntry(UmpireParams.PROP_CorrThreshold, "Corr Threshold", new JFormattedTextField(decimal));
    FormEntry feDeltaApex = new FormEntry(UmpireParams.PROP_DeltaApex, "Delta Apex", new JFormattedTextField(decimal));
    FormEntry feRtOverlap = new FormEntry(UmpireParams.PROP_RTOverlap, "RT Overlap", new JFormattedTextField(decimal));
    //FormEntry fe = new FormEntry(UmpireParams.PROP_, "", );
    JCheckBox checkBoostComplimentaryIons = new JCheckBox("Boost complimentary ions");
    JCheckBox checkAdjustFragIntensity = new JCheckBox("Adjust fragment intensity");

    CC ccFmt = new CC().width("30:50:70px");
    CC ccFmtWrap = new CC().width("30:50:70px").wrap();
    CC ccLbl = new CC().alignX("right").gapBefore("5px");
    pFrag.add(feRpMax.label(), ccLbl);
    pFrag.add(feRpMax.comp, ccFmt);
    pFrag.add(feRfMax.label(), ccLbl);
    pFrag.add(feRfMax.comp, ccFmt);
    pFrag.add(feCorrThresh.label(), ccLbl);
    pFrag.add(feCorrThresh.comp, ccFmt);
    pFrag.add(feDeltaApex.label(), ccLbl);
    pFrag.add(feDeltaApex.comp, ccFmt);
    pFrag.add(feRtOverlap.label(), ccLbl);
    pFrag.add(feRtOverlap.comp, ccFmtWrap);
    pFrag.add(checkBoostComplimentaryIons, "span, split 2");
    pFrag.add(checkAdjustFragIntensity);


    // Panel - fragment grouping options
    List<FormEntry> entries = new ArrayList<>();
    //entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));
    entries.add(new FormEntry(UmpireParams.PROP_MS1PPM, "MS1 PPM", new JFormattedTextField(decimalAsInt)));
    entries.add(new FormEntry(UmpireParams.PROP_MS2PPM, "MS2 PPM", new JFormattedTextField(decimalAsInt)));
    entries.add(new FormEntry(UmpireParams.PROP_MaxCurveRTRange, "Max Curve RT Range", new JFormattedTextField(decimal)));

    entries.add(new FormEntry(UmpireParams.PROP_MinMSIntensity, "Min MS1 Intensity", new JFormattedTextField(decimal)));
    entries.add(new FormEntry(UmpireParams.PROP_MinMSMSIntensity, "Min MS2 Intensity", new JFormattedTextField(decimal)));
    entries.add(new FormEntry(UmpireParams.PROP_MinFrag, "Min Fragments", new JFormattedTextField(decimalAsInt)));

    entries.add(new FormEntry(UmpireParams.PROP_SN, "MS1 S/N", new JFormattedTextField(decimalAsInt)));
    entries.add(new FormEntry(UmpireParams.PROP_MS2SN, "MS2 S/N", new JFormattedTextField(decimalAsInt)));
    entries.add(new FormEntry(UmpireParams.PROP_EstimateBG, "Estimate Background", new JCheckBox()));

    entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));
    entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));
    entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));
    //entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));


    this.add(pTop, new CC().growX());
    this.add(pFrag, new CC().growX());
  }

  private static class FormEntry {
    JComponent comp;
    String propName;
    String label;

    public FormEntry(String propName, String label, JComponent comp) {
      this.propName = propName;
      this.label = label;
      this.comp = comp;
    }

    JLabel label() {
      return new JLabel(label);
    }
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
