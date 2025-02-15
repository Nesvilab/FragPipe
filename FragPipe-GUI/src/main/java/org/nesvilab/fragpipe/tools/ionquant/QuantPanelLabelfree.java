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

package org.nesvilab.fragpipe.tools.ionquant;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.FragpipeCacheUtils;
import org.nesvilab.fragpipe.messages.MessageIsUmpireRun;
import org.nesvilab.fragpipe.messages.MessageLoadQuantDefaults;
import org.nesvilab.fragpipe.messages.NoteConfigIonQuant;
import org.nesvilab.fragpipe.tools.philosopher.ReportPanel;
import org.nesvilab.utils.PropertiesUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiRadio;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.event.ItemEvent;
import java.awt.image.BufferedImage;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
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
  private UiRadio uiRadioUseLfq;
  private UiRadio uiRadioUseLabeling;
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

  public boolean isChecked() {
    return checkRun.isSelected();
  }

  public boolean isCheckRunEnabled() {
    return checkRun.isEnabled();
  }

  public boolean isIonQuantChecked() {
    return uiRadioUseIonquant.isSelected();
  }

  public boolean isIonQuantEnabled() {
    return uiRadioUseIonquant.isEnabled();
  }

  public boolean isRunIonQuant() {
    return isRun() && SwingUtils.isEnabledAndChecked(uiRadioUseIonquant);
  }

  public boolean isRunFreeQuant() {
    return isRun() && SwingUtils.isEnabledAndChecked(uiRadioUseFreequant);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigIonQuant m) {
    updateEnabledStatus(pIonquant, m.isValid());
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
        log.debug("User cancelled Loading MS1 Quant defaults");
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
      if (isRunIonQuant()) {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, false);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, false);
      } else {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, true);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, true);
      }
    });

    JButton btnLoadDefaults = new JButton("Load Quant defaults");
    btnLoadDefaults.addActionListener((e) -> Bus.post(new MessageLoadQuantDefaults(true)));

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/ionquant.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(p, checkRun).split(2);
    mu.add(p, btnLoadDefaults);
    mu.add(p, imageLabel, mu.ccR()).gapRight("200").wrap();

    return p;
  }

  private JPanel createPanelContent(ButtonGroup buttonGroup) {
    JPanel p = mu.newPanel(null, mu.lcFillX());
    mu.borderEmpty(p);
    pFreequant = createPanelFreequant(buttonGroup);
    pIonquant = createPanelIonquant(buttonGroup);

    mu.add(p, pIonquant).wrap();
    mu.add(p, new JSeparator(SwingConstants.HORIZONTAL)).growX().spanX().wrap();
    mu.add(p, pFreequant).wrap();

    return p;
  }

  private JPanel createPanelFreequant(ButtonGroup buttonGroup) {
    JPanel p = mu.newPanel(null, true);
    uiRadioUseFreequant = new UiRadio("FreeQuant (deprecated)", null, true);
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
      if (isRunIonQuant()) {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, false);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, false);
      } else {
        updateEnabledStatus(reportPanel.uiCheckPepSummary, true);
        updateEnabledStatus(reportPanel.uiCheckProtSummary, true);
      }
    });

    UiCombo uiComboPeptideProteinUniqueness = UiUtils.createUiCombo(Arrays.asList("unique+razor", "unique only"));
    FormEntry fePeptideProteinUniqueness = mu.feb(uiComboPeptideProteinUniqueness).name("ionquant.uniqueness").label("Peptide-protein uniqueness").tooltip("Unique only: use peptides that are unique or confidently assigned to a single protein\n Unique+Razor: use 'unique plus razor' approach, with each shared peptide assigned as razor to one protein (as classified by Philosopher and defined in psm.tsv file)\n").create();

    UiCombo uiComboMinIsotopes = UiUtils.createUiCombo(Arrays.asList("1", "2", "3"));
    UiSpinnerInt uiSpinnerMinScans = UiUtils.spinnerInt(3, 1, 10000, 1).setCols(5).create();

    UiSpinnerDouble uiSpinnerMzTol = UiUtils.spinnerDouble(10.0, 1.0, 1000.0, 1.0)
        .setFormat("0.#").setCols(5).create();
    UiSpinnerDouble uiSpinnerRtTol = UiUtils.spinnerDouble(0.4, 0.01, 100.0, 0.1)
        .setFormat("0.0").setCols(5).create();
    UiSpinnerDouble uiSpinnerImTol = UiUtils.spinnerDouble(0.05, 0.001, 0.5, 0.01)
        .setFormat("0.00").setCols(5).create();

    UiSpinnerDouble uiSpinnerMbrMinCorr = UiUtils.spinnerDouble(0.0, 0, 1, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerDouble uiSpinnerMbrRtTol = UiUtils.spinnerDouble(1.0, 0.01, 100, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerDouble uiSpinnerMbrImTol = UiUtils.spinnerDouble(0.05, 0.001, 0.5, 0.001)
        .setCols(5).setFormat("#.###").create();


    UiSpinnerDouble uiSpinnerMinFreq = UiUtils.spinnerDouble(0, 0, 1, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerInt uiSpinnerTopIons = UiUtils.spinnerInt(0, 0, 10000, 1).setCols(5).create();
    UiSpinnerInt uiSpinnerMaxLfqMinIons = UiUtils.spinnerInt(1, 1, 10000, 1).setCols(5).create();

    UiSpinnerDouble uiSpinnerMbrIonFdr = UiUtils.spinnerDouble(0.01, 0.001, 1.1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrPepFdr = UiUtils.spinnerDouble(1, 0.001, 1.1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrProtFdr = UiUtils.spinnerDouble(1, 0.001, 1.1, 0.01)
        .setCols(5).setFormat("#.###").create();

    UiSpinnerInt uiSpinnerMbrTopRuns = UiUtils.spinnerInt(10, 1, Integer.MAX_VALUE, 1).setCols(5).create();
    UiText uiTextLight = UiUtils.uiTextBuilder().cols(40).create();
    UiText uiTextMedium = UiUtils.uiTextBuilder().cols(40).create();
    UiText uiTextHeavy = UiUtils.uiTextBuilder().cols(40).create();
    UiText uiTextExcludemods = UiUtils.uiTextBuilder().cols(90).create();

    FormEntry feMaxLfq = mu.feb("ionquant.maxlfq", UiUtils.createUiCheck("Add MaxLFQ", true)).tooltip("Calculate MaxLFQ intensity. Requires at least 3 experiment+bioreplicate combinations.").create();

    FormEntry feRequant = mu.feb("ionquant.requantify", UiUtils.createUiCheck("Re-quantify", true)).tooltip("Re-quantify unidentified ions in labeling quantification").create();

    FormEntry feMzTol = mu.feb(uiSpinnerMzTol).name("ionquant.mztol").label("m/z tolerance (ppm)").create();
    FormEntry feRtTol = mu.feb(uiSpinnerRtTol).name("ionquant.rttol").label("RT tolerance (minutes)").create();
    FormEntry feImTol = mu.feb(uiSpinnerImTol).name("ionquant.imtol").label("IM tolerance (1/k0)").create();

    FormEntry feMaxLfqMinIons = mu.feb(uiSpinnerMaxLfqMinIons).name("ionquant.minions").label("MaxLFQ min ions").tooltip("Minimum ions required to quantify a protein. Only used in MaxLFQ intensity.").create();
    FormEntry feTopIons = mu.feb(uiSpinnerTopIons).name("ionquant.tp").label("Top N ions").tooltip("Number of ions to use in quantifying proteins").create();
    FormEntry feMinFreq = mu.feb(uiSpinnerMinFreq).name("ionquant.minfreq").label("Min freq").tooltip("Minimum proportion of experiments in which an ion must be found").create();

    UiCheck uiCheckMbr = UiUtils.createUiCheck("Match between runs (MBR)", true);
    uiCheckMbr.setName("ionquant.mbr");

    FormEntry feMbrMinCorr = mu.feb(uiSpinnerMbrMinCorr).name("ionquant.mbrmincorr").label("MBR min correlation").tooltip("Minimum correlation between two runs").create();
    FormEntry feMbrRtTol = mu.feb(uiSpinnerMbrRtTol).name("ionquant.mbrrttol").label("MBR RT tolerance (minutes)").create();
    FormEntry feMbrImTol = mu.feb(uiSpinnerMbrImTol).name("ionquant.mbrimtol").label("MBR IM tolerance (1/k0)").create();

    FormEntry feMbrIonFdr = mu.feb(uiSpinnerMbrIonFdr).name("ionquant.ionfdr").label("MBR ion FDR").create();
    FormEntry feMbrPepFdr = mu.feb(uiSpinnerMbrPepFdr).name("ionquant.peptidefdr").label("MBR peptide FDR").create();
    FormEntry feMbrProtFdr = mu.feb(uiSpinnerMbrProtFdr).name("ionquant.proteinfdr").label("MBR protein FDR").create();

    FormEntry feMbrTopRuns = mu.feb(uiSpinnerMbrTopRuns).name("ionquant.mbrtoprun").label("MBR top runs").tooltip("Maximum number of donor runs used for a given acceptor run.").create();
    FormEntry feLight = mu.feb(uiTextLight).name("ionquant.light").label("Light    ").tooltip("String description of mass deltas. A-Z for amino acids, n for N-terminus, c for C-terminus, and * for any amino acids.\nE.g. (1) for SILAC: K0;R0 (2) for dimethyl labeling: Kn28.0313").create();
    FormEntry feMedium = mu.feb(uiTextMedium).name("ionquant.medium").label("Medium").tooltip("String description of mass deltas. A-Z for amino acids, n for N-terminus, c for C-terminus, and * for any amino acids.\nE.g. for SILAC: K4.025107;R6.020129").create();
    FormEntry feHeavy = mu.feb(uiTextHeavy).name("ionquant.heavy").label("Heavy  ").tooltip("String description of mass deltas. A-Z for amino acids, n for N-terminus, c for C-terminus, and * for any amino acids.\nE.g. (1) for SILAC: K8.014199;R10.008269 (2) for dimethyl labeling: Kn36.075670").create();

    FormEntry feNormalize = mu.feb("ionquant.normalization", UiUtils.createUiCheck("Normalize intensity across runs", true)).tooltip("Normalize ion intensities across all runs.").create();
    FormEntry feMinIsotopes = mu.feb(uiComboMinIsotopes).name("ionquant.minisotopes").label("Min isotopes").tooltip("Minimum number of isotopic peaks required for feature detection").create();
    FormEntry feMinScans = mu.feb(uiSpinnerMinScans).name("ionquant.minscans").label("Min scans").tooltip("Minimum scans required for feature detection").create();
    FormEntry feWriteIndex = mu.feb("ionquant.writeindex", UiUtils.createUiCheck("Keep index on disk", false)).tooltip("Keep built index on disk for further usage").create();
    UiSpinnerDouble uiSpinnerMinSiteProb = UiSpinnerDouble.builder(0.75, -1, 1.0, 0.01).setFormat(new DecimalFormat("#.##")).setCols(5).create();
    FormEntry feMinSiteProb = mu.feb(uiSpinnerMinSiteProb).name("ionquant.locprob").label("Min site localization probability").tooltip("Site localization confidence threshold").create();

    FormEntry feExcludemods = mu.feb(uiTextExcludemods).name("ionquant.excludemods").label("Excluded mods").tooltip("String specifying modifications to be excluded from protein quantification, e.g. M15.9949;STY79.96633").create();

    uiCheckMbr.addItemListener(e -> {
      if (e.getStateChange() == ItemEvent.DESELECTED) {
        updateEnabledStatus(uiSpinnerMbrRtTol, false);
        updateEnabledStatus(uiSpinnerMbrImTol, false);
        updateEnabledStatus(uiSpinnerMbrMinCorr, false);
        updateEnabledStatus(uiSpinnerMbrTopRuns, false);
        updateEnabledStatus(uiSpinnerMbrIonFdr, false);
        updateEnabledStatus(uiSpinnerMbrPepFdr, false);
        updateEnabledStatus(uiSpinnerMbrProtFdr, false);
      } else {
        updateEnabledStatus(uiSpinnerMbrRtTol, true);
        updateEnabledStatus(uiSpinnerMbrImTol, true);
        updateEnabledStatus(uiSpinnerMbrMinCorr, true);
        updateEnabledStatus(uiSpinnerMbrTopRuns, true);
        updateEnabledStatus(uiSpinnerMbrIonFdr, true);
        updateEnabledStatus(uiSpinnerMbrPepFdr, true);
        updateEnabledStatus(uiSpinnerMbrProtFdr, true);
      }
    });

    mu.add(p, feRadioIonquant.comp).wrap();


    // basic options panel
    JPanel pBasic = mu.newPanel("Basic options", mu.lcFillXNoInsetsTopBottom());
    ButtonGroup radioBasicGroup = new ButtonGroup();

    JPanel emptyPanel = mu.newPanel("", mu.lcFillXNoInsetsTopBottom());
    emptyPanel.setLayout(new BoxLayout(emptyPanel, BoxLayout.X_AXIS));
    emptyPanel.setBorder(null);

    // LFQ panel
    JPanel pLfq = mu.newPanel("", mu.lcFillXNoInsetsTopBottom());

    uiRadioUseLfq = new UiRadio("LFQ", null, false);
    radioBasicGroup.add(uiRadioUseLfq);
    FormEntry feRadioLfq = new FormEntry("ionquant.use-lfq", "Not shown", uiRadioUseLfq);

    uiRadioUseLfq.addItemListener(e -> {
      if (e.getStateChange() == ItemEvent.SELECTED) {
        updateEnabledStatus(feRequant.comp, false);
        updateEnabledStatus(uiTextLight, false);
        updateEnabledStatus(uiTextMedium, false);
        updateEnabledStatus(uiTextHeavy, false);
      } else {
        updateEnabledStatus(feRequant.comp, true);
        updateEnabledStatus(uiTextLight, true);
        updateEnabledStatus(uiTextMedium, true);
        updateEnabledStatus(uiTextHeavy, true);
      }
    });

    mu.add(pLfq, feRadioLfq.comp, mu.ccL()).wrap();
    mu.add(pLfq, feMaxLfq.comp, mu.ccL()).wrap();
    mu.add(pLfq, feMaxLfqMinIons.label(), mu.ccL()).split(2);
    mu.add(pLfq, feMaxLfqMinIons.comp, mu.ccL());

    mu.add(emptyPanel, pLfq).spanX();

    emptyPanel.add(Box.createHorizontalStrut(10));


    // Labeling panel
    JPanel pLabel = mu.newPanel("", mu.lcFillXNoInsetsTopBottom());

    uiRadioUseLabeling = new UiRadio("Labeling", null, false);
    radioBasicGroup.add(uiRadioUseLabeling);
    FormEntry feRadioLabeling = new FormEntry("ionquant.use-labeling", "Not shown", uiRadioUseLabeling);

    mu.add(pLabel, feRadioLabeling.comp, mu.ccL()).split(2).growX();
    mu.add(pLabel, feRequant.comp, mu.ccR()).wrap();
    mu.add(pLabel, feLight.label(), mu.ccL()).split(2);
    mu.add(pLabel, feLight.comp).growX().wrap();
    mu.add(pLabel, feMedium.label(), mu.ccL()).split(2);
    mu.add(pLabel, feMedium.comp).growX().wrap();
    mu.add(pLabel, feHeavy.label(), mu.ccL()).split(2);
    mu.add(pLabel, feHeavy.comp).growX().wrap();

    mu.add(emptyPanel, pLabel).spanX().wrap();

    mu.add(pBasic, emptyPanel).growX().wrap();
    mu.add(p, pBasic).wrap();


    // Common panel
    JPanel pCommon = mu.newPanel("Common", mu.lcFillXNoInsetsTopBottom());
    mu.add(pCommon, uiCheckMbr, mu.ccL());
    mu.add(pCommon, feMbrIonFdr.label(), mu.ccL()).split(2);
    mu.add(pCommon, feMbrIonFdr.comp, mu.ccL().gapRight("200")).wrap();
    mu.add(pCommon, feNormalize.comp, mu.ccL());
    mu.add(pCommon, fePeptideProteinUniqueness.label(), mu.ccL()).split(2);
    mu.add(pCommon, fePeptideProteinUniqueness.comp, mu.ccL().gapRight("200")).spanX().wrap();

    mu.add(pBasic, pCommon).spanX().growX().wrap();

    mu.add(p, pBasic).spanX().growX().wrap();


    // Advance options panel
    JPanel pAdvancedOptions = mu.newPanel("Advanced options", mu.lcFillXNoInsetsTopBottom());


    // feature detection panel
    JPanel pDetection = mu.newPanel("Feature detection and peak tracing", mu.lcFillXNoInsetsTopBottom());

    mu.add(pDetection, feMinScans.label(), mu.ccR()).split(2);
    mu.add(pDetection, feMinScans.comp);
    mu.add(pDetection, feMinIsotopes.label(), mu.ccR()).split(2);
    mu.add(pDetection, feMinIsotopes.comp).spanX().wrap();


    mu.add(pDetection, feMzTol.label(), mu.ccR()).split(2);
    mu.add(pDetection, feMzTol.comp);
    mu.add(pDetection, feRtTol.label(), mu.ccR()).split(2);
    mu.add(pDetection, feRtTol.comp);
    mu.add(pDetection, feImTol.label(), mu.ccR()).split(2);
    mu.add(pDetection, feImTol.comp, mu.ccR().gapRight("200")).spanX().wrap();

    mu.add(pAdvancedOptions, pDetection).spanX().growX().wrap();


    // MBR panel
    JPanel pMbr = mu.newPanel("Match between runs (MBR)", mu.lcFillXNoInsetsTopBottom());

    mu.add(pMbr, feMbrRtTol.label(), mu.ccR()).split(2);
    mu.add(pMbr, feMbrRtTol.comp);
    mu.add(pMbr, feMbrImTol.label(), mu.ccR()).split(2);
    mu.add(pMbr, feMbrImTol.comp);
    mu.add(pMbr, feMbrPepFdr.label(), mu.ccR()).split(2);
    mu.add(pMbr, feMbrPepFdr.comp, mu.ccR().gapRight("200")).spanX().wrap();

    mu.add(pMbr, feMbrMinCorr.label(), mu.ccR()).split(2);
    mu.add(pMbr, feMbrMinCorr.comp);
    mu.add(pMbr, feMbrTopRuns.label(), mu.ccR()).split(2);
    mu.add(pMbr, feMbrTopRuns.comp);
    mu.add(pMbr, feMbrProtFdr.label(), mu.ccR()).split(2);
    mu.add(pMbr, feMbrProtFdr.comp, mu.ccR().gapRight("200")).spanX().wrap();

    mu.add(pAdvancedOptions, pMbr).spanX().growX().wrap();


    // Intensity panel
    JPanel pIntensity = mu.newPanel("Intensity", mu.lcFillXNoInsetsTopBottom());

    mu.add(pIntensity, feTopIons.label(), mu.ccL()).split(4);
    mu.add(pIntensity, feTopIons.comp);
    mu.add(pIntensity, feMinFreq.label(), mu.ccL().gapLeft("50")).split(2);
    mu.add(pIntensity, feMinFreq.comp).spanX().wrap();

    mu.add(pAdvancedOptions, pIntensity).spanX().growX().wrap();


    // Other panel
    JPanel pOther = mu.newPanel("Other", mu.lcFillXNoInsetsTopBottom());

    mu.add(pOther, feExcludemods.label(), mu.ccR());
    mu.add(pOther, feExcludemods.comp).spanX().wrap();

    mu.add(pOther, feMinSiteProb.label(), mu.ccR());
    mu.add(pOther, feMinSiteProb.comp);
    mu.add(pOther, feWriteIndex.comp, mu.ccR().gapRight("400")).wrap();

    mu.add(pAdvancedOptions, pOther).spanX().growX().wrap();

    mu.add(p, pAdvancedOptions).spanX().growX().wrap();

    return p;
  }
}
