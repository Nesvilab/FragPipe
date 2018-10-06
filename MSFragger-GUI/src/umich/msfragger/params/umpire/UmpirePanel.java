package umich.msfragger.params.umpire;

import java.awt.Component;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.NotImplementedException;
import umich.msfragger.util.SwingUtils;

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
    pFrag.add(feRfMax.comp, ccFmtWrap);
    pFrag.add(feCorrThresh.label(), ccLbl);
    pFrag.add(feCorrThresh.comp, ccFmt);
    pFrag.add(feDeltaApex.label(), ccLbl);
    pFrag.add(feDeltaApex.comp, ccFmt);
    pFrag.add(feRtOverlap.label(), ccLbl);
    pFrag.add(feRtOverlap.comp, ccFmtWrap);
    pFrag.add(checkBoostComplimentaryIons, "span, split 2");
    pFrag.add(checkAdjustFragIntensity);


    // Panel - fragment grouping options
    JPanel pSe = new JPanel(new MigLayout());
    pSe.setBorder(new TitledBorder("Signal Extraction Parameters"));
    List<FormEntry> feSe = new ArrayList<>();
    //entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));
    feSe.add(new FormEntry(UmpireParams.PROP_MS1PPM, "MS1 PPM", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_MS2PPM, "MS2 PPM", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_MaxCurveRTRange, "Max Curve RT Range", new JFormattedTextField(decimal)));

    feSe.add(new FormEntry(UmpireParams.PROP_MinMSIntensity, "Min MS1 Intensity", new JFormattedTextField(decimal)));
    feSe.add(new FormEntry(UmpireParams.PROP_MinMSMSIntensity, "Min MS2 Intensity", new JFormattedTextField(decimal)));
    feSe.add(new FormEntry(UmpireParams.PROP_MinFrag, "Min Fragments", new JFormattedTextField(decimalAsInt)));

    feSe.add(new FormEntry(UmpireParams.PROP_SN, "MS1 S/N", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_MS2SN, "MS2 S/N", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_EstimateBG, "Estimate Background", new JCheckBox()));

    feSe.add(new FormEntry(UmpireParams.PROP_MinNoPeakCluster, "Min N Peaks/Cluster", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_MaxNoPeakCluster, "Max N Peaks/Cluster", new JFormattedTextField(decimalAsInt)));
    feSe.add(new FormEntry(UmpireParams.PROP_NoMissedScan, "Max Missed Scans", new JFormattedTextField(decimalAsInt)));
    //entries.add(new FormEntry(UmpireParams.PROP_, "", new JFormattedTextField()));

    for (int i = 0; i < feSe.size(); i++) {
      CC ccComp = (i+1) % 3 != 0
          ? new CC().width("30:50:70px")
          : new CC().width("30:50:70px").wrap();
      CC ccLabel = new CC().alignX("right").gapBefore("5px");
      FormEntry fe = feSe.get(i);
      pSe.add(fe.label(), ccLabel);
      pSe.add(fe.comp, ccComp);
    }


    // Panel - SWATH window parameters
    JPanel pSwath = new JPanel(new MigLayout());
    pSwath.setBorder(new TitledBorder("SWATH window parameters"));

    List<FormEntry> feSwath = new ArrayList<>();
    JComboBox<String> comboWindowType = new JComboBox<>();
    comboWindowType.setModel(new DefaultComboBoxModel<>(new String[]{"SWATH"}));
    FormEntry feWinType = new FormEntry(UmpireParams.PROP_WindowType, "Window type", comboWindowType);
    FormEntry feWinSize = new FormEntry(UmpireParams.PROP_WindowSize, "Window size", new JFormattedTextField(decimal));

    pSwath.add(feWinType.label(), ccLbl);
    pSwath.add(feWinType.comp, new CC().width("70:80:120px"));
    pSwath.add(feWinSize.label(), ccLbl);
    pSwath.add(feWinSize.comp, ccFmt);


    CC ccGrowX = new CC().growX();
    this.add(pTop, ccGrowX);
    this.add(pFrag, ccGrowX);
    this.add(pSe, ccGrowX);
    this.add(pSwath, ccGrowX);
  }

  private static class FormEntry {
    JComponent comp;
    String propName;
    String label;

    public FormEntry(String propName, String label, JComponent comp) {
      this.propName = propName;
      this.label = label;
      this.comp = comp;
      comp.setName(propName);
    }

    JLabel label() {
      return new JLabel(label);
    }
  }

  /** Use {@link SwingUtils#getStrVal} to get string values from most common Java Swing GUI elements. */
  public UmpireParams collectUmpireParams() {
    Map<String, Component> map = SwingUtils.mapComponentsByName(this, true);
    // The map contains all named params with their corresponding UI elements.
    throw new NotImplementedException("TODO: gather params object from the form"); // TODO: Not implemented

    //SwingUtils.getStrVal(..) TODO: use this function
  }

  public ImageIcon getIcon() {
    return icon;
  }
}
