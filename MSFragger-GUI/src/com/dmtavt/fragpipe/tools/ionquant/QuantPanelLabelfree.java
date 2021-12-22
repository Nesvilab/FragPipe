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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tools.ionquant;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.messages.MessageIsUmpireRun;
import com.dmtavt.fragpipe.messages.MessageLoadQuantDefaults;
import com.dmtavt.fragpipe.messages.NoteConfigIonQuant;
import com.dmtavt.fragpipe.tools.philosopher.ReportPanel;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.event.ItemEvent;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Map;
import java.util.Properties;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class QuantPanelLabelfree extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(QuantPanelLabelfree.class);
  private JPanel pTop;
  private JPanel pFreequant;
  private JPanel pIonquant;
  private JPanel pContent;
  private UiCheck checkRun;
  private UiRadio uiRadioUseFreequant;
  private UiRadio uiRadioUseIonquant;
  private ButtonGroup radioGroupQuant;

  @Override
  protected ItemSelectable getRunCheckbox() {
    return checkRun;
  }

  @Override
  protected Component getEnablementToggleComponent() {
    return pContent;
  }

  @Override
  protected String getComponentNamePrefix() {
    return null; // 2 different prefixes are used on this panel, so automatic renaming is disabled
  }

  @Override
  protected void initMore() {
    super.initMore();
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isIonquant() {
    return isRun() && SwingUtils.isEnabledAndChecked(uiRadioUseIonquant);
  }

  public boolean isFreequant() {
    return isRun() && SwingUtils.isEnabledAndChecked(uiRadioUseFreequant);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigIonQuant m) {
    updateEnabledStatus(this, m.isValid());
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageIsUmpireRun m) {
    updateEnabledStatus(this, !m.isEnabled);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLoadQuantDefaults m) {
    if (m.doAskUser) {
      int answer = SwingUtils.showConfirmDialog(this, new JLabel("<html>Load default quantitation options?"));
      if (JOptionPane.OK_OPTION != answer) {
        log.debug("User cancelled Loading Shepherd defaults");
        return;
      }
    }
    try {
      Properties props = PropertiesUtils.loadPropertiesLocal(QuantParams.class, QuantParams.DEFAULT_PROPERTIES_FN);
      Map<String, String> remapped = FragpipeCacheUtils.translateValuesToUi(PropertiesUtils.toMap(props));
      SwingUtils.valuesSet(this, remapped);
    } catch (Exception e) {
      log.error("Error loading quant defaults", e);
      SwingUtils.showErrorDialogWithStacktrace(e, this);
    }
  }

  public Map<String, String> toMap() {
    Map<String, String> map = SwingUtils
        .valuesGet(this, (name) -> !name.startsWith("Spinner.formattedTextField"));
    Map<String, String> remapped = FragpipeCacheUtils.translateValuesFromUi(map);
    return remapped;
  }

  public String getFreequantOptsAsText() {
    // report.labelfree.nonspecific=--ptw 0.4 --tol 10
    Map<String, String> m = toMap();
    StringBuilder sb = new StringBuilder();
    sb.append(" --ptw ").append(getOrThrow(m, "freequant.rt-tol"));
    sb.append(" --tol ").append(getOrThrow(m, "freequant.mz-tol"));
    return sb.toString();
  }

  private String getOrThrow(Map<String, String> m, String key) {
    String s = m.get(key);
    if (s == null)
      throw new IllegalStateException("Could not get key: " + key);
    return s;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("MS1 Quantification"));

    radioGroupQuant = new ButtonGroup();
    pTop = createPanelTop();
    pContent = createPanelContent(radioGroupQuant);

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  private JPanel createPanelTop() {
    JPanel p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(p);

    checkRun = new UiCheck("Run MS1 quant", null, false);
    checkRun.setName("quantitation.run-label-free-quant");

    checkRun.addItemListener(e -> {
      final ReportPanel reportPanel = Fragpipe.getStickyStrict(ReportPanel.class);
      if (isIonquant()) {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, false);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, false);
      } else {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, true);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, true);
      }
    });

    JButton btnLoadDefaults = new JButton("Load Quant defaults");
    btnLoadDefaults.addActionListener((e) -> Bus.post(new MessageLoadQuantDefaults(true)));

    mu.add(p, checkRun);
    mu.add(p, btnLoadDefaults).pushX().wrap();

    return p;
  }

  private JPanel createPanelContent(ButtonGroup buttonGroup) {
    JPanel p = mu.newPanel(null, mu.lcFillX());
    mu.borderEmpty(p);
    pFreequant = createPanelFreequant(buttonGroup);
    pIonquant = createPanelIonquant(buttonGroup);

    mu.add(p, pIonquant).spanX().wrap();
    mu.add(p, new JSeparator(SwingConstants.HORIZONTAL)).growX().spanX().wrap();
    mu.add(p, pFreequant).wrap();

    return p;
  }

  private JPanel createPanelFreequant(ButtonGroup buttonGroup) {
    JPanel p = mu.newPanel(null, true);
    uiRadioUseFreequant = new UiRadio("FreeQuant (alternative tool)", null, true);
    buttonGroup.add(uiRadioUseFreequant);
    FormEntry feRadioFreequant = new FormEntry("freequant.run-freequant", "Not shown",
        uiRadioUseFreequant);
    UiSpinnerDouble uiSpinnerRtTol = UiSpinnerDouble.builder(0.4, 0.05, 1000.0, 0.1)
        .setFormat(new DecimalFormat("0.0#")).setCols(5).create();
    FormEntry feRtTol = new FormEntry("freequant.rt-tol", "RT Window (minutes)",
        uiSpinnerRtTol);
    UiSpinnerDouble uiSpinnerMzTol = UiSpinnerDouble.builder(10.0, 0.1, 10000.0, 1)
        .setFormat(new DecimalFormat("0.#")).setCols(5).create();
    FormEntry feMzTol = new FormEntry("freequant.mz-tol", "m/z Window (ppm)",
        uiSpinnerMzTol);

    mu.add(p, feRadioFreequant.comp);
    mu.add(p, feRtTol.label(), mu.ccR());
    mu.add(p, feRtTol.comp);
    mu.add(p, feMzTol.label(), mu.ccR());
    mu.add(p, feMzTol.comp).spanX(2).wrap();

    return p;
  }

  private JPanel createPanelIonquant(ButtonGroup buttonGroup) {
    JPanel p = mu.newPanel(null, true);

    uiRadioUseIonquant = new UiRadio("IonQuant", null, false);
    buttonGroup.add(uiRadioUseIonquant);
    FormEntry feRadioIonquant = new FormEntry("ionquant.run-ionquant", "Not shown", uiRadioUseIonquant);

    uiRadioUseIonquant.addItemListener(e -> {
      final ReportPanel reportPanel = Fragpipe.getStickyStrict(ReportPanel.class);
      if (isIonquant()) {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, false);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, false);
      } else {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, true);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, true);
      }
    });

    UiCombo uiComboMinIsotopes = UiUtils.createUiCombo(Arrays.asList("1", "2", "3"));
    UiSpinnerInt uiSpinnerMinScans = UiUtils.spinnerInt(3, 0, 10000, 1).setCols(5).create();

    UiSpinnerDouble uiSpinnerMzTol = UiUtils.spinnerDouble(10.0, 1.0, 1000.0, 1.0)
        .setFormat("0.#").setCols(5).create();
    UiSpinnerDouble uiSpinnerRtTol = UiUtils.spinnerDouble(0.4, 0.01, 100.0, 0.1)
        .setFormat("0.0").setCols(5).create();
    UiSpinnerDouble uiSpinnerImTol = UiUtils.spinnerDouble(0.05, 0.001, 0.5, 0.01)
        .setFormat("0.00").setCols(5).create();


//    ionquant.mbrmincorr :: MBR min correlation <=> 0.5 (0 - 1, step: 0.1)
//    ionquant.mbrrttol:: MBR RT window (minutes) <=> 1.0  (0.01 - 100, step: 0.1)
//    ionquant.mbrimtol :: MBR IM window (1/k0) <=> 0.05 (0.001 - 0.5, step: 0.001)

//    ionquant.mbrtoprun :: MBR top runs <=> 3 (1 - a very large number, step: 1)

//    ionquant.ionfdr :: MBR ion FDR <=> 0.01 (0.001 - 1, step: 0.01)
//    ionquant.peptidefdr :: MBR peptide FDR <=> 1 (0.001 - 1, step: 0.01)
//    ionquant.proteinfdr :: MBR protein FDR <=> 1 (0.001 - 1, step: 0.01)
//
//    ionquant.label :: Labels <=> <string>

    UiSpinnerDouble uiSpinnerMbrMinCorr = UiUtils.spinnerDouble(0.0, 0, 1, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerDouble uiSpinnerMbrRtTol = UiUtils.spinnerDouble(1.0, 0.01, 100, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerDouble uiSpinnerMbrImTol = UiUtils.spinnerDouble(0.05, 0.001, 0.5, 0.001)
        .setCols(5).setFormat("#.###").create();


    UiSpinnerDouble uiSpinnerMinFreq = UiUtils.spinnerDouble(0.5, 0, 1, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerInt uiSpinnerTopIons = UiUtils.spinnerInt(3, 0, 10000, 1).setCols(5).create();
    UiSpinnerInt uiSpinnerMinIons = UiUtils.spinnerInt(1, 0, 10000, 1).setCols(5).create();
    UiSpinnerInt uiSpinnerMinExps = UiUtils.spinnerInt(1, 1, 10000, 1).setCols(5).create();

    UiSpinnerDouble uiSpinnerMbrIonFdr = UiUtils.spinnerDouble(0.01, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrPepFdr = UiUtils.spinnerDouble(1, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrProtFdr = UiUtils.spinnerDouble(1, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();

    UiSpinnerInt uiSpinnerMbrTopRuns = UiUtils.spinnerInt(100000, 0, Integer.MAX_VALUE, 1).setCols(5).create();
    UiText uiTextLight = UiUtils.uiTextBuilder().cols(15).create();
    UiText uiTextMedium = UiUtils.uiTextBuilder().cols(15).create();
    UiText uiTextHeavy = UiUtils.uiTextBuilder().cols(15).create();
    UiText uiTextExcludemods = UiUtils.uiTextBuilder().cols(45).create();

    UiCheck uiCheckMbr = UiUtils.createUiCheck("Match between runs (MBR)", true);
    uiCheckMbr.setName("ionquant.mbr");
    FormEntry feMaxLfq = mu.feb("ionquant.maxlfq", UiUtils.createUiCheck("MaxLFQ", true)).tooltip("Calculate MaxLFQ intensity.").create();
    FormEntry feRequant = mu.feb("ionquant.requantify", UiUtils.createUiCheck("Re-quantify", true)).tooltip("Re-quantify unidentified ions in labeling quantification").create();

    FormEntry feMzTol = mu.feb(uiSpinnerMzTol).name("ionquant.mztol").label("m/z tolerance (ppm)").create();
    FormEntry feRtTol = mu.feb(uiSpinnerRtTol).name("ionquant.rttol").label("RT tolerance (minutes)").create();
    FormEntry feImTol = mu.feb(uiSpinnerImTol).name("ionquant.imtol").label("IM tolerance (1/k0)").create();

    FormEntry feMinIons = mu.feb(uiSpinnerMinIons).name("ionquant.minions").label("Min ions").tooltip("Minimum ions required to quantify a protein").create();
    FormEntry feTopIons = mu.feb(uiSpinnerTopIons).name("ionquant.tp").label("Top N ions").tooltip("Number of ions to use in quantifying proteins").create();
    FormEntry feMinFreq = mu.feb(uiSpinnerMinFreq).name("ionquant.minfreq").label("Min freq").tooltip("Minimum proportion of experiments in which an ion must be found").create();
    FormEntry feMinExps = mu.feb(uiSpinnerMinExps).name("ionquant.minexps").label("Min exps").tooltip("Minimum number of experiments in which an ion must be found").create();

    FormEntry feMbrMinCorr = mu.feb(uiSpinnerMbrMinCorr).name("ionquant.mbrmincorr").label("MBR min correlation").tooltip("Minimum correlation between two runs").create();
    FormEntry feMbrRtTol = mu.feb(uiSpinnerMbrRtTol).name("ionquant.mbrrttol").label("MBR RT tolerance (minutes)").create();
    FormEntry feMbrImTol = mu.feb(uiSpinnerMbrImTol).name("ionquant.mbrimtol").label("MBR IM tolerance (1/k0)").create();

    FormEntry feMbrIonFdr = mu.feb(uiSpinnerMbrIonFdr).name("ionquant.ionfdr").label("MBR ion FDR").create();
    FormEntry feMbrPepFdr = mu.feb(uiSpinnerMbrPepFdr).name("ionquant.peptidefdr").label("MBR peptide FDR").create();
    FormEntry feMbrProtFdr = mu.feb(uiSpinnerMbrProtFdr).name("ionquant.proteinfdr").label("MBR protein FDR").create();

    FormEntry feMbrTopRuns = mu.feb(uiSpinnerMbrTopRuns).name("ionquant.mbrtoprun").label("MBR top runs").tooltip("Maximum number of donor runs used for a given acceptor run").create();
    FormEntry feLight = mu.feb(uiTextLight).name("ionquant.light").label("Light").tooltip("String description of mass deltas. A-Z for amino acids, n for N-terminus, and c for C-terminus. E.g. for SILAC: K0;R0").create();
    FormEntry feMedium = mu.feb(uiTextMedium).name("ionquant.medium").label("Medium").tooltip("String description of mass deltas. A-Z for amino acids, n for N-terminus, and c for C-terminus. E.g. for SILAC: K4.025107;R6.020129").create();
    FormEntry feHeavy = mu.feb(uiTextHeavy).name("ionquant.heavy").label("Heavy").tooltip("String description of mass deltas. A-Z for amino acids, n for N-terminus, and c for C-terminus. E.g. for SILAC: K8.014199;R10.008269").create();

    FormEntry feNormalize = mu.feb("ionquant.normalization", UiUtils.createUiCheck("Normalize", true)).tooltip("Normalize ion intensities among experiments").create();
    FormEntry feMinIsotopes = mu.feb(uiComboMinIsotopes).name("ionquant.minisotopes").label("Min isotopes").tooltip("Minimum number of isotopic peaks required for feature detection").create();
    FormEntry feMinScans = mu.feb(uiSpinnerMinScans).name("ionquant.minscans").label("Min scans").tooltip("Minimum scans required for feature detection").create();
    FormEntry feWriteIndex = mu.feb("ionquant.writeindex", UiUtils.createUiCheck("Keep index on disk", false)).tooltip("Keep built index on disk for further usage").create();

    UiSpinnerDouble uiSpinnerMinSiteProb = UiSpinnerDouble.builder(0.75, -1, 1.0, 0.01).setFormat(new DecimalFormat("#.##")).setCols(5).create();
    FormEntry feMinSiteProb = mu.feb(uiSpinnerMinSiteProb).name("ionquant.locprob").label("Min site probability").tooltip("Site localization confidence threshold").create();

    FormEntry feExcludemods = mu.feb(uiTextExcludemods).name("ionquant.excludemods").label("Excluded mods").tooltip("String specifying modifications to be excluded from protein quantification, e.g. M15.9949;STY79.96633").create();

    ((UiCheck) feMaxLfq.comp).addItemListener(e -> {
      if (e.getStateChange() == ItemEvent.DESELECTED) {
       uiSpinnerMinIons.setValue(1);
        updateEnabledStatus(uiSpinnerMinIons, false);
      } else {
        uiSpinnerMinIons.setValue(2);
        updateEnabledStatus(uiSpinnerMinIons, true);
      }
    });

    mu.add(p, feRadioIonquant.comp);
    mu.add(p, uiCheckMbr).push();
    mu.add(p, feMaxLfq.comp).push();
    mu.add(p, feMinIons.label(), mu.ccR());
    mu.add(p, feMinIons.comp).push();
    mu.add(p, feNormalize.comp).wrap();

    JPanel pDetection = mu.newPanel("Feature detection", mu.lcFillXNoInsetsTopBottom());

    mu.add(pDetection, feMzTol.label(), mu.ccR());
    mu.add(pDetection, feMzTol.comp);
    mu.add(pDetection, feRtTol.label(), mu.ccR());
    mu.add(pDetection, feRtTol.comp);
    mu.add(pDetection, feImTol.label(), mu.ccR());
    mu.add(pDetection, feImTol.comp).wrap();

    mu.add(p, pDetection).spanX().growX().wrap();

    JPanel pMbr = mu.newPanel("Match between runs (MBR)", mu.lcFillXNoInsetsTopBottom());

    mu.add(pMbr, feMbrRtTol.label(), mu.ccR());
    mu.add(pMbr, feMbrRtTol.comp);
    mu.add(pMbr, feMbrImTol.label(), mu.ccR());
    mu.add(pMbr, feMbrImTol.comp).spanX().wrap();

    mu.add(pMbr, feMbrMinCorr.label(), mu.ccR());
    mu.add(pMbr, feMbrMinCorr.comp);
    mu.add(pMbr, feMbrTopRuns.label(), mu.ccR());
    mu.add(pMbr, feMbrTopRuns.comp).spanX().wrap();
    mu.add(pMbr, feMbrIonFdr.label(), mu.ccR());
    mu.add(pMbr, feMbrIonFdr.comp);
    mu.add(pMbr, feMbrPepFdr.label(), mu.ccR());
    mu.add(pMbr, feMbrPepFdr.comp);
    mu.add(pMbr, feMbrProtFdr.label(), mu.ccR());
    mu.add(pMbr, feMbrProtFdr.comp).spanX().wrap();

    mu.add(p, pMbr).spanX().growX().wrap();

    JPanel pTopN = mu.newPanel("Top-N options", mu.lcFillXNoInsetsTopBottom());

    mu.add(pTopN, feTopIons.label(), mu.ccR());
    mu.add(pTopN, feTopIons.comp);
    mu.add(pTopN, feMinFreq.label(), mu.ccR());
    mu.add(pTopN, feMinFreq.comp);
    mu.add(pTopN, feMinExps.label(), mu.ccR());
    mu.add(pTopN, feMinExps.comp).spanX().wrap();

    mu.add(p, pTopN).spanX().growX().wrap();

    JPanel pLabel = mu.newPanel("Labeling-based quant", mu.lcFillXNoInsetsTopBottom());

    mu.add(pLabel, new JLabel("Labels:"), mu.ccR());
    mu.add(pLabel, feLight.label(), mu.ccL()).split().spanX();
    mu.add(pLabel, feLight.comp);
    mu.add(pLabel, feMedium.label());
    mu.add(pLabel, feMedium.comp);
    mu.add(pLabel, feHeavy.label());
    mu.add(pLabel, feHeavy.comp);
    mu.add(pLabel, feRequant.comp).wrap();

    mu.add(p, pLabel).spanX().growX().wrap();

    JPanel pa = mu.newPanel("Advanced options", mu.lcFillXNoInsetsTopBottom());

    mu.add(pa, feExcludemods.label(), mu.ccR());
    mu.add(pa, feExcludemods.comp).growX().spanX().wrap();

    mu.add(pa, feMinScans.label(), mu.ccR());
    mu.add(pa, feMinScans.comp);
    mu.add(pa, feMinIsotopes.label(), mu.ccR());
    mu.add(pa, feMinIsotopes.comp);
    mu.add(pa, feWriteIndex.comp);
    mu.add(pa, feMinSiteProb.label(), mu.ccR());
    mu.add(pa, feMinSiteProb.comp).spanX().wrap();

    mu.add(p, pa).spanX().growX().wrap();

    return p;
  }
}
