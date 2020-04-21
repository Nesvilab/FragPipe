package com.dmtavt.fragpipe.tools.ionquant;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageIsUmpireRun;
import com.dmtavt.fragpipe.messages.MessageLoadQuantDefaults;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.text.DecimalFormat;
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
      SwingUtils.valuesFromMap(this, PropertiesUtils.toMap(props));
    } catch (Exception e) {
      log.error("Error loading quant defaults", e);
      SwingUtils.showErrorDialogWithStacktrace(e, this);
    }
  }

  public Map<String, String> toMap() {
    return SwingUtils.valuesToMap(this, (name) -> !name.startsWith("Spinner.formattedTextField"));
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
        .setFormat(new DecimalFormat("0.0#")).setNumCols(5).create();
    FormEntry feRtTol = new FormEntry("freequant.rt-tol", "RT Window (Minutes)",
        uiSpinnerRtTol);
    UiSpinnerDouble uiSpinnerMzTol = UiSpinnerDouble.builder(10.0, 0.1, 10000.0, 1)
        .setFormat(new DecimalFormat("0.#")).setNumCols(5).create();
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

    uiRadioUseIonquant = new UiRadio("IonQuant (for timsTOF ion mobility data)", null, false);
    buttonGroup.add(uiRadioUseIonquant);
    FormEntry feRadioIonquant = new FormEntry("ionquant.run-ionquant", "Not shown",
        uiRadioUseIonquant);
    UiSpinnerDouble uiSpinnerImTol = UiSpinnerDouble.builder(0.05, 0.01, 1.0, 0.01)
        .setFormat(new DecimalFormat("0.00")).setNumCols(5).create();
    FormEntry feImTol = new FormEntry("ionquant.im-tol", "IM Window (1/k0)", uiSpinnerImTol);
    UiSpinnerDouble uiSpinnerMzTol = UiSpinnerDouble.builder(10.0, 1.0, 1000.0, 1.0)
        .setFormat(new DecimalFormat("0.#")).setNumCols(5).create();
    FormEntry feMzTol = new FormEntry("ionquant.mz-tol", "M/Z Window (ppm)", uiSpinnerMzTol);
    UiCheck uiCheckIonquantPlot = new UiCheck("Plot (for debug)", null, false);
    FormEntry fePlot = new FormEntry("ionquant.is-plot", "Not shown", uiCheckIonquantPlot);

    UiSpinnerDouble uiSpinnerRtTol = UiSpinnerDouble.builder(0.4, 0.0, 10.0, 0.1)
        .setFormat(new DecimalFormat("0.0")).setNumCols(5).create();
    FormEntry feRtTol = new FormEntry("ionquant.rt-tol", "RT Window (minutes)", uiSpinnerRtTol);
    UiSpinnerDouble uiSpinnerMinFreq = UiSpinnerDouble.builder(0.5, 0.0, 1.0, 0.1)
        .setFormat(new DecimalFormat("0.00")).setNumCols(5).create();
    FormEntry feMinFreq = new FormEntry("ionquant.min-freq", "MinFreq", uiSpinnerMinFreq);

    mu.add(p, feRadioIonquant.comp);
    mu.add(p, feImTol.label(), mu.ccR());
    mu.add(p, feImTol.comp);
    mu.add(p, feMzTol.label(), mu.ccR());
    mu.add(p, feMzTol.comp);
    mu.add(p, fePlot.comp).wrap();
    mu.add(p, feRtTol.label(), mu.ccR()).span(2);
    mu.add(p, feRtTol.comp);
    mu.add(p, feMinFreq.label(), mu.ccR());
    mu.add(p, feMinFreq.comp);

    fePlot.comp.setVisible(false);

    return p;
  }
}
