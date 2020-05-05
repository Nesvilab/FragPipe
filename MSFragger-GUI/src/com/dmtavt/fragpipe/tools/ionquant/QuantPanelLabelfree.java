package com.dmtavt.fragpipe.tools.ionquant;

import com.dmtavt.fragpipe.Version;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.messages.MessageIsUmpireRun;
import com.dmtavt.fragpipe.messages.MessageLoadQuantDefaults;
import com.github.chhh.utils.ProcessUtils;
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
    this.setBorder(new TitledBorder("Label-free Quantitation"));

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

    mu.add(p, pFreequant).wrap();
    mu.add(p, new JSeparator(SwingConstants.HORIZONTAL)).growX().spanX().wrap();
    mu.add(p, pIonquant).wrap();

    return p;
  }

  private JPanel createPanelFreequant(ButtonGroup buttonGroup) {
    JPanel p = mu.newPanel(null, true);
    uiRadioUseFreequant = new UiRadio("FreeQuant (for non ion mobility data)", null, true);
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

    UiCombo uiComboTimsTOF = UiUtils.createUiCombo(Arrays.asList("timsTOF", "Non-timsTOF"));
    UiCombo uiComboMbr = UiUtils.createUiCombo(Arrays.asList("No", "Yes"));
    UiCombo uiComboRequant = UiUtils.createUiCombo(Arrays.asList("Yes", "No"));

    UiCombo uiComboNormalize = UiUtils.createUiCombo(Arrays.asList("None", "Median"));
    UiCombo uiComboRequireIsotopes = UiUtils.createUiCombo(Arrays.asList("Min 2 isotopes", "Any"));

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
//    ionquant.peptidefdr :: MBR peptide FDR <=> 0.01 (0.001 - 1, step: 0.01)
//    ionquant.proteinfdr :: MBR protein FDR <=> 0.01 (0.001 - 1, step: 0.01)
//
//    ionquant.label :: Labels <=> <string>

    UiSpinnerDouble uiSpinnerMbrMinCorr = UiUtils.spinnerDouble(0.5, 0, 1, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerDouble uiSpinnerMbrRtTol = UiUtils.spinnerDouble(1.0, 0.01, 100, 0.1)
        .setCols(5).setFormat("#.##").create();
    UiSpinnerDouble uiSpinnerMbrImTol = UiUtils.spinnerDouble(0.05, 0.001, 0.5, 0.001)
        .setCols(5).setFormat("#.###").create();

    UiSpinnerDouble uiSpinnerMbrIonFdr = UiUtils.spinnerDouble(0.01, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrPepFdr = UiUtils.spinnerDouble(0.01, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();
    UiSpinnerDouble uiSpinnerMbrProtFdr = UiUtils.spinnerDouble(0.01, 0.001, 1, 0.01)
        .setCols(5).setFormat("#.###").create();

    UiSpinnerInt uiSpinnerMbrTopRuns = UiUtils.spinnerInt(3, 1, Integer.MAX_VALUE, 1).setCols(5).create();
    UiText uiTextLabels = UiUtils.uiTextBuilder().cols(30).ghost("<light mod>;<heavy mod>").create();

    FormEntry feDataType = mu.feb(uiComboTimsTOF).name("ionquant.noim").label("Data type").create();
    FormEntry feMbr = mu.feb(uiComboMbr).name("ionquant.mbr").label("Match between runs (MBR)").create();
    FormEntry feRequant = mu.feb(uiComboRequant).name("ionquant.requantify").label("Re-quantify").create();

    FormEntry feMzTol = mu.feb(uiSpinnerMzTol).name("ionquant.mztol").label("M/Z Window (ppm)").create();
    FormEntry feRtTol = mu.feb(uiSpinnerRtTol).name("ionquant.rttol").label("RT Window (minutes)").create();
    FormEntry feImTol = mu.feb(uiSpinnerImTol).name("ionquant.imtol").label("IM Window (1/k0)").create();


    FormEntry feMbrMinCorr = mu.feb(uiSpinnerMbrMinCorr).name("ionquant.mbrmincorr").label("MBR min correlation").create();
    FormEntry feMbrRtTol = mu.feb(uiSpinnerMbrRtTol).name("ionquant.mbrrttol").label("MBR RT Window (minutes)").create();
    FormEntry feMbrImTol = mu.feb(uiSpinnerMbrImTol).name("ionquant.mbrimtol").label("MBR IM Window (1/k0)").create();

    FormEntry feMbrIonFdr = mu.feb(uiSpinnerMbrIonFdr).name("ionquant.ionfdr").label("MBR ion FDR").create();
    FormEntry feMbrPepFdr = mu.feb(uiSpinnerMbrPepFdr).name("ionquant.peptidefdr").label("MBR peptide FDR").create();
    FormEntry feMbrProtFdr = mu.feb(uiSpinnerMbrProtFdr).name("ionquant.proteinfdr").label("MBR protein FDR").create();

    FormEntry feMbrTopRuns = mu.feb(uiSpinnerMbrTopRuns).name("ionquant.mbrtoprun").label("MBR top runs").create();
    FormEntry feLabel = mu.feb(uiTextLabels).name("ionquant.label").label("Label").create();

    FormEntry feNormalize = mu.feb(uiComboNormalize).name("ionquant.normalization").label("Normalize").create();
    FormEntry feRequireIsotopes = mu.feb(uiComboRequireIsotopes).name("ionquant.requireisotopes").label("Require isotopes")
        .tooltip("Min number of isotopes for tracing.").create();



    mu.add(p, feRadioIonquant.comp).split().spanX();
    mu.add(p, feDataType.label()).gapLeft("10px");
    mu.add(p, feDataType.comp).wrap();

    mu.add(p, feMzTol.label(), mu.ccR());
    mu.add(p, feMzTol.comp);
    mu.add(p, feRtTol.label(), mu.ccR());
    mu.add(p, feRtTol.comp);
    mu.add(p, feImTol.label(), mu.ccR());
    mu.add(p, feImTol.comp).spanX().wrap();

    mu.add(p, feMbr.label(), mu.ccR());
    mu.add(p, feMbr.comp).growX();
    mu.add(p, feMbrRtTol.label(), mu.ccR());
    mu.add(p, feMbrRtTol.comp);
    mu.add(p, feMbrImTol.label(), mu.ccR());
    mu.add(p, feMbrImTol.comp).spanX().wrap();

    mu.add(p, feRequant.label(), mu.ccR());
    mu.add(p, feRequant.comp);
    mu.add(p, feLabel.label(), mu.ccR());
    mu.add(p, feLabel.comp).spanX().growX().wrap();

    JPanel pa = mu.newPanel("Advanced options", mu.lcFillXNoInsetsTopBottom());

    mu.add(pa, feMbrMinCorr.label(), mu.ccR());
    mu.add(pa, feMbrMinCorr.comp);
    mu.add(pa, feMbrTopRuns.label(), mu.ccR());
    mu.add(pa, feMbrTopRuns.comp).spanX().wrap();

    mu.add(pa, feMbrIonFdr.label(), mu.ccR());
    mu.add(pa, feMbrIonFdr.comp);
    mu.add(pa, feMbrPepFdr.label(), mu.ccR());
    mu.add(pa, feMbrPepFdr.comp);
    mu.add(pa, feMbrProtFdr.label(), mu.ccR());
    mu.add(pa, feMbrProtFdr.comp).spanX().wrap();

    mu.add(pa, feNormalize.label(), mu.ccR());
    mu.add(pa, feNormalize.comp);
    mu.add(pa, feRequireIsotopes.label(), mu.ccR());
    mu.add(pa, feRequireIsotopes.comp).spanX().wrap();

    mu.add(p, pa).spanX().growX().wrap();

    return p;
  }
}
