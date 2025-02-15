/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.tools.umpire;

import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_AdjustFragIntensity;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_BoostComplementaryIon;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_CorrThreshold;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_DeltaApex;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_EstimateBG;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_ExportPrecursorPeak;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_IsoPattern;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_MS1PPM;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_MS2PPM;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_MS2SN;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_MassDefectFilter;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_MassDefectOffset;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_MinMSIntensity;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_MinMSMSIntensity;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_NoMissedScan;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_Q1;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_Q2;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_Q3;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_RFmax;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_RPmax;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_RTOverlap;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_SN;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_Thread;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_WindowSize;
import static org.nesvilab.fragpipe.tools.umpire.UmpireParams.PROP_WindowType;
import static org.nesvilab.utils.SwingUtils.isEnabledAndChecked;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.messages.MessageIsUmpireRun;
import org.nesvilab.fragpipe.messages.NoteConfigCrystalC;
import org.nesvilab.fragpipe.messages.NoteConfigPeptideProphet;
import org.nesvilab.fragpipe.messages.NoteConfigPtmProphet;
import org.nesvilab.fragpipe.messages.NoteConfigPtmShepherd;
import org.nesvilab.fragpipe.messages.NoteConfigTmtI;
import org.nesvilab.fragpipe.messages.NoteConfigUmpire;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.fragpipe.tabs.TabWorkflow;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.Component;
import java.awt.Container;
import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
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
import java.util.Objects;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
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
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

public class UmpirePanel extends JPanelBase {

  public static final String PREFIX = "diaumpire.";

  public JCheckBox checkRunUmpireSe;
  private JPanel keyPanel;
  private JPanel pSe;
  private ImageIcon icon;
  private UiCombo uiComboLoadDefaultsNames;
  private String customParamsPath = null;
  private FormEntry feQ1;
  private FormEntry feQ2;
  private FormEntry feQ3;

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
      PROP_MS2SN,
      PROP_Q1,
      PROP_Q2,
      PROP_Q3);

  public UmpirePanel() {
    Bus.postSticky(this);
  }

  @Override
  public boolean isRun() {
    return isEnabledAndChecked(checkRunUmpireSe);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigUmpire m) {
    updateEnabledStatus(this, m.isValid());
  }

  protected void init() {
    icon = new ImageIcon(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/dia-umpire-16x16.png")));

    this.setLayout(new MigLayout(new LC().flowY().fillX()));
    this.setBorder(new TitledBorder("DIA Spectrum Deconvolution"));

    LC lc = new LC();//.debug();

    // Panel - top
    JPanel pTop = mu.newPanel(mu.lcFillXNoInsetsTopBottom());

    checkRunUmpireSe = new UiCheck("Run DIA-Umpire SE (Signal Extraction)", null, false);
    checkRunUmpireSe.setName("diaumpire.run-diaumpire");
    pTop.add(checkRunUmpireSe, new CC().spanX().wrap());

    checkRunUmpireSe.addItemListener(e -> {
      if (isRun()) {
        Bus.post(new NoteConfigCrystalC(true));
        Bus.post(new NoteConfigPeptideProphet(true));
        Bus.post(new NoteConfigPtmProphet(true));
        Bus.post(new NoteConfigPtmShepherd(true));
      } else {
        TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
        if (tabWorkflow.hasDataType("DIA") || tabWorkflow.hasDataType("GPF-DIA") || tabWorkflow.hasDataType("DIA-Lib")) {
          Bus.post(new NoteConfigCrystalC(false));
          Bus.post(new NoteConfigPeptideProphet(false));
          Bus.post(new NoteConfigPtmProphet(false));
          Bus.post(new NoteConfigPtmShepherd(false));
          Bus.post(new NoteConfigTmtI(false));
        } else if (tabWorkflow.hasDataType("DDA+")) {
          Bus.post(new NoteConfigCrystalC(false));
          Bus.post(new NoteConfigPeptideProphet(false));
          Bus.post(new NoteConfigPtmProphet(true));
          Bus.post(new NoteConfigPtmShepherd(false));
          Bus.post(new NoteConfigTmtI(false));
        }
      }
    });

    List<String> loadOptions = new ArrayList<>(2);
    loadOptions.add("Default DIA-Umpire parameter file");
    loadOptions.add("Custom DIA-Umpire parameter file from disk");
    uiComboLoadDefaultsNames = UiUtils.createUiCombo(loadOptions);
    JButton btnLoad = new JButton("Load");
    btnLoad.addActionListener(this::actionBtnConfigLoad);

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/DIA-Umpire_logo.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(pTop, btnLoad).split(3);
    mu.add(pTop, new JLabel(":"));
    mu.add(pTop, uiComboLoadDefaultsNames);
    mu.add(pTop, imageLabel, mu.ccR()).gapRight("50").wrap();

    DefaultFormatterFactory decimalAsInt = new DefaultFormatterFactory(
        new NumberFormatter(new DecimalFormat("#0")));
    DefaultFormatterFactory decimal = new javax.swing.text.DefaultFormatterFactory(
        new javax.swing.text.NumberFormatter());

    FormEntry feRpMax = new FormEntry(PROP_RPmax, "RP max", new JFormattedTextField(decimal), PREFIX);
    FormEntry feRfMax = new FormEntry(PROP_RFmax, "RF max", new JFormattedTextField(decimal), PREFIX);
    FormEntry feCorrThresh = new FormEntry(PROP_CorrThreshold, "Corr Threshold", new JFormattedTextField(decimal), PREFIX);
    FormEntry feDeltaApex = new FormEntry(PROP_DeltaApex, "Delta Apex", new JFormattedTextField(decimal), PREFIX);
    FormEntry feRtOverlap = new FormEntry(PROP_RTOverlap, "RT Overlap", new JFormattedTextField(decimal), PREFIX);
    FormEntry feCheckBoostComplimentaryIons = new FormEntry(PROP_BoostComplementaryIon, "Boost complimentary ions", new JCheckBox(), PREFIX);
    FormEntry feCheckAdjustFragIntensitys = new FormEntry(PROP_AdjustFragIntensity, "Adjust fragment intensity", new JCheckBox(), PREFIX);

    FormEntry feMs1Ppm = new FormEntry(UmpireParams.PROP_MS1PPM, "MS1 PPM", new JFormattedTextField(decimalAsInt), PREFIX);
    FormEntry feMs2Ppm = new FormEntry(UmpireParams.PROP_MS2PPM, "MS2 PPM", new JFormattedTextField(decimalAsInt), PREFIX);
    FormEntry feNoMissedScans = new FormEntry(UmpireParams.PROP_NoMissedScan, "Max Missed Scans", new JFormattedTextField(decimalAsInt), PREFIX);
    FormEntry feEstimateBG = new FormEntry(UmpireParams.PROP_EstimateBG, "Remove Background", new JCheckBox(), PREFIX);
    FormEntry feIsoPattern = new FormEntry(PROP_IsoPattern, "Isotope Pattern", new JFormattedTextField(decimal), PREFIX);
    FormEntry feMassDefectFilter = new FormEntry(PROP_MassDefectFilter, "Mass Defect Filter", new JCheckBox(), PREFIX);
    FormEntry feMassDefectOffset = new FormEntry(PROP_MassDefectOffset, "Mass Defect Offset", new JFormattedTextField(decimal), PREFIX);
    FormEntry feExportPrecursorPeak = new FormEntry(PROP_ExportPrecursorPeak, "Export Precursor Peak", new JCheckBox(), PREFIX);
    feQ1 = new FormEntry(PROP_Q1, "Q1", new JCheckBox(), PREFIX);
    feQ2 = new FormEntry(PROP_Q2, "Q2", new JCheckBox(), PREFIX);
    feQ3 = new FormEntry(PROP_Q3, "Q3", new JCheckBox(), PREFIX);
    FormEntry feSN = new FormEntry(PROP_SN, "MS1 SN", new JFormattedTextField(decimal), PREFIX);
    FormEntry feMS2SN = new FormEntry(PROP_MS2SN, "MS2 SN", new JFormattedTextField(decimal), PREFIX);

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

    JPanel emptyPanel = mu.newPanel("", mu.lcFillXNoInsetsTopBottom());
    emptyPanel.setLayout(new BoxLayout(emptyPanel, BoxLayout.X_AXIS));
    emptyPanel.setBorder(null);

    mu.add(emptyPanel, keyPanel).spanX();
    mu.add(emptyPanel, pSe).spanX().wrap();

    CC ccGrowX = new CC().growX();
    this.add(pTop, ccGrowX);
    this.add(emptyPanel, ccGrowX);

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
      Component component = map.get(PREFIX + paramName);
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
      Component component = map.get(PREFIX + name);
      if (component != null) {
        String val = params.getProps().getProperty(name);
        SwingUtils.setStrVal(component, val);
      }
    }
  }

  public boolean generateQ1() {
    return isEnabledAndChecked((JCheckBox) feQ1.comp);
  }

  public boolean generateQ2() {
    return isEnabledAndChecked((JCheckBox) feQ2.comp);
  }

  public boolean generateQ3() {
    return isEnabledAndChecked((JCheckBox) feQ3.comp);
  }

  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRunUmpireSe;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return keyPanel;
  }

  @Override
  protected String getComponentNamePrefix() {
    return PREFIX;
  }
}
