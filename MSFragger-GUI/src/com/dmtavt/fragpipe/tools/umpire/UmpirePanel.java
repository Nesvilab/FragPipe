package com.dmtavt.fragpipe.tools.umpire;

import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_AdjustFragIntensity;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_BoostComplementaryIon;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_CorrThreshold;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_DeltaApex;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_EstimateBG;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_ExportPrecursorPeak;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_IsoPattern;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_MS1PPM;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_MS2PPM;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_MS2SN;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_MassDefectFilter;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_MassDefectOffset;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_MinMSIntensity;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_MinMSMSIntensity;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_NoMissedScan;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_Q1;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_Q2;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_Q3;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_RFmax;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_RPmax;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_RTOverlap;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_SN;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_Thread;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_WindowSize;
import static com.dmtavt.fragpipe.tools.umpire.UmpireParams.PROP_WindowType;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageIsUmpireRun;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

public class UmpirePanel extends JPanel {
  public JCheckBox checkRunUmpireSe;
  private JPanel keyPanel;
  private JPanel pSe;
  private JPanel textPanel;
  private ImageIcon icon;
  private UiCombo uiComboLoadDefaultsNames;
  private String customParamsPath = null;

  private final List<String> paramNames = Arrays.asList(
      PROP_RPmax,
      PROP_RFmax,
      PROP_CorrThreshold,
      PROP_DeltaApex,
      PROP_RTOverlap,
      PROP_AdjustFragIntensity,
      PROP_BoostComplementaryIon,
      PROP_MS1PPM,
      PROP_MS2PPM,
      PROP_MinMSIntensity,
      PROP_MinMSMSIntensity,
      PROP_NoMissedScan,
      PROP_EstimateBG,
      PROP_IsoPattern,
      PROP_MassDefectOffset,
      PROP_MassDefectFilter,
      PROP_ExportPrecursorPeak,
      PROP_WindowType,
      PROP_WindowSize,
      PROP_SN,
      PROP_MS2SN);

  public UmpirePanel() {
    initMore();
    Bus.postSticky(this);
  }

  public boolean isRunUmpire() {
    return SwingUtils.isEnabledAndChecked(checkRunUmpireSe);
  }

  private void initMore() {
    icon = new ImageIcon(
        getClass().getResource("/com/dmtavt/fragpipe/icons/dia-umpire-16x16.png"));

    this.setLayout(new MigLayout(new LC().flowY().fillX()));

    LC lc = new LC();//.debug();

    // Panel - top
    JPanel pTop = new JPanel(new MigLayout(lc));

    checkRunUmpireSe = new UiCheck("Run DIA-Umpire SE (Signal Extraction)", null, false);
    checkRunUmpireSe.setName("run-diaumpire");
    pTop.add(checkRunUmpireSe, new CC().spanX().wrap());

    List<String> loadOptions = new ArrayList<>(2);
    loadOptions.add("Default DIA-Umpire parameter file");
    loadOptions.add("Custom DIA-Umpire parameter file from disk");
    uiComboLoadDefaultsNames = UiUtils.createUiCombo(loadOptions);
    JButton btnLoad = new JButton("Load");
    btnLoad.addActionListener(this::actionBtnConfigLoad);

    pTop.add(btnLoad);
    pTop.add(new JLabel(":"));
    pTop.add(uiComboLoadDefaultsNames);

    DefaultFormatterFactory decimalAsInt = new DefaultFormatterFactory(
        new NumberFormatter(new DecimalFormat("#0")));
    DefaultFormatterFactory decimal = new javax.swing.text.DefaultFormatterFactory(
        new javax.swing.text.NumberFormatter());

    FormEntry feRpMax = new FormEntry(PROP_RPmax, "RP max", new JFormattedTextField(decimal));
    FormEntry feRfMax = new FormEntry(PROP_RFmax, "RF max", new JFormattedTextField(decimal));
    FormEntry feCorrThresh = new FormEntry(PROP_CorrThreshold, "Corr Threshold", new JFormattedTextField(decimal));
    FormEntry feDeltaApex = new FormEntry(PROP_DeltaApex, "Delta Apex", new JFormattedTextField(decimal));
    FormEntry feRtOverlap = new FormEntry(PROP_RTOverlap, "RT Overlap", new JFormattedTextField(decimal));
    FormEntry feCheckBoostComplimentaryIons = new FormEntry(PROP_BoostComplementaryIon, "Boost complimentary ions", new JCheckBox());
    FormEntry feCheckAdjustFragIntensitys = new FormEntry(PROP_AdjustFragIntensity, "Adjust fragment intensity", new JCheckBox());

    FormEntry feMs1Ppm = new FormEntry(UmpireParams.PROP_MS1PPM, "MS1 PPM", new JFormattedTextField(decimalAsInt));
    FormEntry feMs2Ppm = new FormEntry(UmpireParams.PROP_MS2PPM, "MS2 PPM", new JFormattedTextField(decimalAsInt));
    FormEntry feNoMissedScans = new FormEntry(UmpireParams.PROP_NoMissedScan, "Max Missed Scans", new JFormattedTextField(decimalAsInt));
    FormEntry feEstimateBG = new FormEntry(UmpireParams.PROP_EstimateBG, "Remove Background", new JCheckBox());
    FormEntry feIsoPattern = new FormEntry(PROP_IsoPattern, "Isotope Pattern", new JFormattedTextField(decimal));
    FormEntry feMassDefectFilter = new FormEntry(PROP_MassDefectFilter, "Mass Defect Filter", new JCheckBox());
    FormEntry feMassDefectOffset = new FormEntry(PROP_MassDefectOffset, "Mass Defect Offset", new JFormattedTextField(decimal));
    FormEntry feExportPrecursorPeak = new FormEntry(PROP_ExportPrecursorPeak, "Export Precursor Peak", new JCheckBox());
    FormEntry feQ1 = new FormEntry(PROP_Q1, "Q1", new JCheckBox());
    FormEntry feQ2 = new FormEntry(PROP_Q2, "Q2", new JCheckBox());
    FormEntry feQ3 = new FormEntry(PROP_Q3, "Q3", new JCheckBox());
    FormEntry feSN = new FormEntry(PROP_SN, "MS1 SN", new JFormattedTextField(decimal));
    FormEntry feMS2SN = new FormEntry(PROP_MS2SN, "MS2 SN", new JFormattedTextField(decimal));

    CC ccComp = new CC().width("30:50:70px");
    CC ccFmtWrap = new CC().width("30:50:70px").wrap();
    CC ccLbl = new CC().alignX("right").gapBefore("5px");

    // Key options
    keyPanel = new JPanel(new MigLayout(lc));
    keyPanel.setBorder(new TitledBorder("Main options"));
    keyPanel.add(feMs1Ppm.label(), ccLbl);
    keyPanel.add(feMs1Ppm.comp, ccComp);
    keyPanel.add(feMs2Ppm.label(), ccLbl);
    keyPanel.add(feMs2Ppm.comp, ccFmtWrap);

    keyPanel.add(feNoMissedScans.label(), ccLbl);
    keyPanel.add(feNoMissedScans.comp, ccFmtWrap);

    keyPanel.add(feEstimateBG.label(), ccLbl);
    keyPanel.add(feEstimateBG.comp, ccFmtWrap);

    keyPanel.add(feMassDefectFilter.label(), ccLbl);
    keyPanel.add(feMassDefectFilter.comp, ccFmtWrap);

    // Panel - Signal Extraction Parameters
    pSe = new JPanel(new MigLayout(lc));
    pSe.setBorder(new TitledBorder("Advanced options"));
    pSe.add(feRpMax.label(), ccLbl);
    pSe.add(feRpMax.comp, ccComp);
    pSe.add(feRfMax.label(), ccLbl);
    pSe.add(feRfMax.comp, ccComp);
    pSe.add(feRtOverlap.label(), ccLbl);
    pSe.add(feRtOverlap.comp, ccFmtWrap);

    pSe.add(feCorrThresh.label(), ccLbl);
    pSe.add(feCorrThresh.comp, ccComp);
    pSe.add(feDeltaApex.label(), ccLbl);
    pSe.add(feDeltaApex.comp, ccComp);
    pSe.add(feQ1.label(), ccLbl);
    pSe.add(feQ1.comp, ccFmtWrap);

    pSe.add(feMassDefectOffset.label(), ccLbl);
    pSe.add(feMassDefectOffset.comp, ccComp);
    pSe.add(feIsoPattern.label(), ccLbl);
    pSe.add(feIsoPattern.comp, ccComp);
    pSe.add(feQ2.label(), ccLbl);
    pSe.add(feQ2.comp, ccFmtWrap);

    pSe.add(feSN.label(), ccLbl);
    pSe.add(feSN.comp, ccComp);
    pSe.add(feMS2SN.label(), ccLbl);
    pSe.add(feMS2SN.comp, ccComp);
    pSe.add(feQ3.label(), ccLbl);
    pSe.add(feQ3.comp, ccFmtWrap);

    pSe.add(feCheckBoostComplimentaryIons.label(), ccLbl);
    pSe.add(feCheckBoostComplimentaryIons.comp, ccComp);
    pSe.add(feCheckAdjustFragIntensitys.label(), ccLbl);
    pSe.add(feCheckAdjustFragIntensitys.comp, ccComp);
    pSe.add(feExportPrecursorPeak.label(), ccLbl);
    pSe.add(feExportPrecursorPeak.comp, ccFmtWrap);

    textPanel = new JPanel(new MigLayout(lc));
    textPanel.setBorder(new TitledBorder("Notes"));
    JEditorPane epInfo = SwingUtils.createClickableHtml("<b>Sensitivity vs. Runtime:</b><br>"
        + "<table>\n"
        + "<tbody>\n"
        + "  <tr>\n"
        + "    <td class=\"tg-zv4m\">Highest sensitivity</td>\n"
        + "    <td class=\"tg-zv4m\">Max Missed Scans: 2; Remove Background: OFF</td>\n"
        + "  </tr>\n"
        + "  <tr>\n"
        + "    <td class=\"tg-zv4m\">Default</td>\n"
        + "    <td class=\"tg-zv4m\">Max Missed Scans: 1; Remove Background: OFF</td>\n"
        + "  </tr>\n"
        + "  <tr>\n"
        + "    <td class=\"tg-zv4m\">Fastest runtime</td>\n"
        + "    <td class=\"tg-zv4m\">Max Missed Scans: 1; Remove Background: ON</td>\n"
        + "  </tr>\n"
        + "</tbody>\n"
        + "</table>\n"
        + "<br>"
        + "<b>PTM searches:</b><br>"
        + "Change Mass Defect Filter to OFF<br><br>"
        + "<b>Sciex 5600/6600 data:</b><br>"
        + "Max Missed Scans: 1; Remove Background: ON;  MS1 SN: 2; MS2 SN: 2<br><br>");
    epInfo.setPreferredSize(new Dimension(500, 100));
    textPanel.add(epInfo);

    CC ccGrowX = new CC().growX();
    this.add(pTop, ccGrowX);
    this.add(keyPanel, ccGrowX);
    this.add(pSe, ccGrowX);
    this.add(textPanel, ccGrowX);

    enablePanels(checkRunUmpireSe.isSelected());
    checkRunUmpireSe.addChangeListener(e -> {
      final boolean isRun = checkRunUmpireSe.isSelected();
      MessageIsUmpireRun m = Bus.getStickyEvent(MessageIsUmpireRun.class);
      if (m != null && m.isEnabled == isRun) {
        return; // no change since we last observed it
      }
      enablePanels(isRun);
      Bus.postSticky(new MessageIsUmpireRun(isRun));
    });

    reloadUmpireParams();
  }

  private void actionBtnConfigLoad(ActionEvent actionEvent) {
    String option = (String) uiComboLoadDefaultsNames.getSelectedItem();

    if (option == null || option.contentEquals("Default DIA-Umpire parameter file")) {
      try {
        UmpireParams params = new UmpireParams();
        params.loadDefault();
        fillFrom(params);
      } catch (Exception ex) {
        JOptionPane.showMessageDialog(this, "<html>Could not load the default parameter file: <br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
      }
    } else {
      FileNameExtensionFilter filter = new FileNameExtensionFilter("Properties/Params", "params");
      JFileChooser fc = FileChooserUtils.create("Select saved file", "Load", false, FcMode.FILES_ONLY, true, filter);
      fc.setFileFilter(filter);
      FileChooserUtils.setPath(fc, Stream.of(ThisAppProps.load(ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN)));
      Component parent = SwingUtils.findParentFrameForDialog(this);
      int saveResult = fc.showOpenDialog(parent);
      if (JFileChooser.APPROVE_OPTION == saveResult) {
        File f = fc.getSelectedFile();
        Path p = f.toPath();
        customParamsPath = p.toAbsolutePath().toString();
        ThisAppProps.save(ThisAppProps.PROP_FRAGGER_PARAMS_FILE_IN, p.toString());
        if (Files.exists(p)) {
          UmpireParams params = new UmpireParams();
          try (InputStream is = Files.newInputStream(p, StandardOpenOption.READ)) {
            params.load(is);
            UmpirePanel.this.fillFrom(params);
          } catch (Exception ex) {
            JOptionPane.showMessageDialog(parent, "<html>Could not load the saved file: <br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
          }
        } else {
          JOptionPane.showMessageDialog(parent, "<html>This is strange,<br/> but the file you chose to load doesn't exist anymore.", "Strange", JOptionPane.ERROR_MESSAGE);
        }
      }
    }
  }

  private void enablePanels(boolean enabled) {
    List<Container> comps = Arrays.asList(keyPanel, pSe);
    for (Container c : comps) {
      SwingUtils.enableComponents(c, enabled);
    }
  }

  public void reloadUmpireParams() {
    new Thread(() -> {
      UmpireParams params = new UmpireParams();
      try {
        // load original defaults
        params.loadDefault();
        // load user specified defaults
        if (!StringUtils.isNullOrWhitespace(customParamsPath)) {
          Path path = Paths.get(customParamsPath);
          try (InputStream is = Files.newInputStream(path)) {
            params.load(is);
          }
        }
        // load cached
        params.loadCache();

        fillFrom(params);
      } catch (IOException ignore) {}
    }).run();
  }

  /** Use {@link SwingUtils#getStrVal} to get string values from most common Java Swing GUI elements. */
  public UmpireParams collect() {

    UmpireParams params = new UmpireParams();

    // load defaults either from user specified config file
    try {
      if (!StringUtils.isNullOrWhitespace(customParamsPath)) {
        try (InputStream is = Files.newInputStream(Paths.get(customParamsPath))) {
          params.load(is);
        }
      } else {
        // OR load defaults either from the defaults in the jar
        params.loadDefault();
      }
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }

    // The map contains all named params with their corresponding UI elements.
    Map<String, Component> map = SwingUtils.mapComponentsByName(this, true);

    for (String paramName : paramNames) {
      Component component = map.get(paramName);
      if (component != null) {
        String strVal = SwingUtils.getStrVal(component);
        params.getProps().setProperty(paramName, strVal);
      }
    }

    TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
    params.getProps().setProperty(PROP_Thread, String.valueOf(tabWorkflow.getThreads()));

    return params;
  }

  public void fillFrom(UmpireParams params) {
    Map<String, Component> map = SwingUtils.mapComponentsByName(this, true);
    for (String name : params.getProps().stringPropertyNames()) {
      Component component = map.get(name);
      if (component != null) {
        String val = params.getProps().getProperty(name);
        SwingUtils.setStrVal(component, val);
      }
    }

  }
}
