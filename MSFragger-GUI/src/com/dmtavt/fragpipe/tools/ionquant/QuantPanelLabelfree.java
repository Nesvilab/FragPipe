package com.dmtavt.fragpipe.tools.ionquant;

import com.dmtavt.fragpipe.Version;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.messages.MessageIsUmpireRun;
import com.dmtavt.fragpipe.messages.MessageLoadQuantDefaults;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiSpinnerInt.Builder;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import com.github.chhh.utils.swing.UiUtils.UiTextBuilder;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
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

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageIsUmpireRun m) {
    if (m.isEnabled) {
      checkRun.setSelected(false); // deselect when Umpire runs
    }
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
    // report.labelfree.nonspecific=--ptw 0.4 --tol 10 --isolated
    Map<String, String> m = toMap();
    StringBuilder sb = new StringBuilder();
    sb.append(" --ptw ").append(getOrThrow(m, "freequant.rt-tol"));
    sb.append(" --tol ").append(getOrThrow(m, "freequant.mz-tol"));
    sb.append(" --isolated ");
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
    this.setBorder(new TitledBorder("Label-Free Quantification"));

    radioGroupQuant = new ButtonGroup();
    pTop = createPanelTop();
    pContent = createPanelContent(radioGroupQuant);

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  private JPanel createPanelTop() {
    JPanel p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(p);

    checkRun = new UiCheck("Run Label-free quant", null, false);
    checkRun.setName("quantitation.run-label-free-quant");
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
    FormEntry feRtTol = new FormEntry("freequant.rt-tol", "RT Window (Minutes)",
        uiSpinnerRtTol);
    UiSpinnerDouble uiSpinnerMzTol = UiSpinnerDouble.builder(10.0, 0.1, 10000.0, 1)
        .setFormat(new DecimalFormat("0.#")).setCols(5).create();
    FormEntry feMzTol = new FormEntry("freequant.mz-tol", "M/z Window (ppm)",
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
    FormEntry feRadioIonquant = new FormEntry("ionquant.run-ionquant", "Not shown",
        uiRadioUseIonquant);

    UiCombo uiComboProtQuant = UiUtils.createUiCombo(Arrays.asList("Top-N", "MaxLFQ"));
    UiCombo uiComboMinIsotopes = UiUtils.createUiCombo(Arrays.asList("1", "2", "3"));

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
    UiSpinnerInt uiSpinnerTopIons = UiUtils.spinnerInt(3, 0, 10, 1).setCols(5).create();
    UiSpinnerInt uiSpinnerMinIons = UiUtils.spinnerInt(1, 0, 10, 1).setCols(5).create();
    UiSpinnerInt uiSpinnerMinExps = UiUtils.spinnerInt(1, 1, 10000, 1).setCols(5).create();

    UiSpinnerDouble uiSpinnerMbrIonFdr = UiUtils.spinnerDouble(0.01, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrPepFdr = UiUtils.spinnerDouble(1, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrProtFdr = UiUtils.spinnerDouble(1, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();

    UiSpinnerInt uiSpinnerMbrTopRuns = UiUtils.spinnerInt(3, 1, Integer.MAX_VALUE, 1).setCols(5).create();
    UiText uiTextLight = UiUtils.uiTextBuilder().cols(15).create();
    UiText uiTextMedium = UiUtils.uiTextBuilder().cols(15).create();
    UiText uiTextHeavy = UiUtils.uiTextBuilder().cols(15).create();
    UiText uiTextExcludemods = UiUtils.uiTextBuilder().cols(45).create();

    //FormEntry feDataType = mu.feb(uiComboTimsTOF).name("ionquant.ionmobility").label("Data type").create();
    UiCheck uiCheckMbr = UiUtils.createUiCheck("Match between runs (MBR)", false);
    uiCheckMbr.setName("ionquant.mbr");
    FormEntry feProtQuant = mu.feb(uiComboProtQuant).name("ionquant.proteinquant").label("Protein quant").tooltip("The algorithm used in calculating protein intensity.").create();
    UiCheck uiCheckRequant = UiUtils.createUiCheck("Re-quantify", false);
    uiCheckRequant.setName("ionquant.requantify");
    uiCheckRequant.createToolTip().setTipText("Re-quantifying unidentified ions in labelling quantification");

    FormEntry feMzTol = mu.feb(uiSpinnerMzTol).name("ionquant.mztol").label("M/Z Window (ppm)").create();
    FormEntry feRtTol = mu.feb(uiSpinnerRtTol).name("ionquant.rttol").label("RT Window (minutes)").create();
    FormEntry feImTol = mu.feb(uiSpinnerImTol).name("ionquant.imtol").label("IM Window (1/k0)").create();

    FormEntry feMinIons = mu.feb(uiSpinnerMinIons).name("ionquant.minions").label("Min ions").tooltip("Minimum ions required in quantifying a protein.").create();
    FormEntry feTopIons = mu.feb(uiSpinnerTopIons).name("ionquant.tp").label("Top N ions").tooltip("Only activate when Protein quant is Top-N.").create();
    FormEntry feMinFreq = mu.feb(uiSpinnerMinFreq).name("ionquant.minfreq").label("Min freq").tooltip("Only activate when Protein quant is Top-N.").create();
    FormEntry feMinExps = mu.feb(uiSpinnerMinExps).name("ionquant.minexps").label("Min exps").tooltip("Only activate when Protein quant is Top-N.").create();

    FormEntry feMbrMinCorr = mu.feb(uiSpinnerMbrMinCorr).name("ionquant.mbrmincorr").label("MBR min correlation").create();
    FormEntry feMbrRtTol = mu.feb(uiSpinnerMbrRtTol).name("ionquant.mbrrttol").label("MBR RT Window (minutes)").create();
    FormEntry feMbrImTol = mu.feb(uiSpinnerMbrImTol).name("ionquant.mbrimtol").label("MBR IM Window (1/k0)").create();

    FormEntry feMbrIonFdr = mu.feb(uiSpinnerMbrIonFdr).name("ionquant.ionfdr").label("MBR ion FDR").create();
    FormEntry feMbrPepFdr = mu.feb(uiSpinnerMbrPepFdr).name("ionquant.peptidefdr").label("MBR peptide FDR").create();
    FormEntry feMbrProtFdr = mu.feb(uiSpinnerMbrProtFdr).name("ionquant.proteinfdr").label("MBR protein FDR").create();

    FormEntry feMbrTopRuns = mu.feb(uiSpinnerMbrTopRuns).name("ionquant.mbrtoprun").label("MBR top runs").create();
    FormEntry feLight = mu.feb(uiTextLight).name("ionquant.light").label("Light")
        .tooltip("String description of mass deltas. E.g. for SILAC: K0;R0").create();
    FormEntry feMedium = mu.feb(uiTextMedium).name("ionquant.medium").label("Medium")
        .tooltip("String description of mass deltas. E.g. for SILAC: K4.02511;R6.02013").create();
    FormEntry feHeavy = mu.feb(uiTextHeavy).name("ionquant.heavy").label("Heavy")
        .tooltip("String description of mass deltas. E.g. for SILAC: K8.01420;R10.00827").create();

    UiCheck uiCheckNormalize = UiUtils.createUiCheck("Normalize", true);
    uiCheckNormalize.createToolTip().setTipText("Normalizing ion intensities among experiments.");
    uiCheckNormalize.setName("ionquant.normalization");
    FormEntry feMinIsotopes = mu.feb(uiComboMinIsotopes).name("ionquant.minisotopes").label("Min isotopes")
        .tooltip("Min number of isotopes for tracing.").create();
    UiCheck uiCheckWriteIndex = UiUtils.createUiCheck("Keep index on disk", true);
    uiCheckWriteIndex.setName("ionquant.writeindex");

    FormEntry feExcludemods = mu.feb(uiTextExcludemods).name("ionquant.excludemods").label("Excluded Mods").tooltip("String specifying excluded modifications in peptide and protein quantification. E.g. M15.9949;STY79.96633").create();

    SwingUtils.addItemSelectedListener(uiComboProtQuant, true, itemEvent -> {
      Object o = itemEvent.getItem();
      if (o == null)
        return;
      if (!(o instanceof String)) {
        log.warn(
            "Not a string received in item selection listener of 'ionquant.proteinquant' combo");
        return;
      }
      final boolean enabled = !"MaxLFQ".equalsIgnoreCase((String) itemEvent.getItem());
      updateEnabledStatus(uiSpinnerTopIons, enabled);
      updateEnabledStatus(uiSpinnerMinFreq, enabled);
      updateEnabledStatus(uiSpinnerMinExps, enabled);
    });


    mu.add(p, feRadioIonquant.comp).split().spanX();
    mu.add(p, uiCheckMbr);
    mu.add(p, feProtQuant.label(), mu.ccR());
    mu.add(p, feProtQuant.comp);
    mu.add(p, feMinIons.label(), mu.ccR());
    mu.add(p, feMinIons.comp).spanX().wrap();

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

    JPanel pa = mu.newPanel("Advanced options", mu.lcFillXNoInsetsTopBottom());

    mu.add(pa, new JLabel("Labels:"), mu.ccR());
    mu.add(pa, feLight.label(), mu.ccL()).split().spanX();
    mu.add(pa, feLight.comp);
    mu.add(pa, feMedium.label());
    mu.add(pa, feMedium.comp);
    mu.add(pa, feHeavy.label());
    mu.add(pa, feHeavy.comp).wrap();

    mu.add(pa, feExcludemods.label(), mu.ccR());
    mu.add(pa, feExcludemods.comp).growX().spanX().wrap();

    mu.add(pa, feTopIons.label(), mu.ccR());
    mu.add(pa, feTopIons.comp);
    mu.add(pa, uiCheckRequant);
    mu.add(pa, feMinIsotopes.label(), mu.ccR());
    mu.add(pa, feMinIsotopes.comp).wrap();

    mu.add(pa, feMinFreq.label(), mu.ccR());
    mu.add(pa, feMinFreq.comp);
    mu.add(pa, uiCheckNormalize).wrap();

    mu.add(pa, feMinExps.label(), mu.ccR());
    mu.add(pa, feMinExps.comp);
    mu.add(pa, uiCheckWriteIndex).wrap();

    mu.add(p, pa).spanX().growX().wrap();

    return p;
  }
}
