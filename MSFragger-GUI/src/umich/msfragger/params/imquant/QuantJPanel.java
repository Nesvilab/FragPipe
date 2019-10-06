package umich.msfragger.params.imquant;

import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import java.awt.BorderLayout;
import java.text.DecimalFormat;
import java.util.Map;
import java.util.Properties;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.messages.MessageIsUmpireRun;
import umich.msfragger.messages.MessageLoadQuantDefaults;
import umich.msfragger.messages.MessageQuantRun;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.FormEntry;
import umich.msfragger.util.swing.JPanelWithEnablement;

public class QuantJPanel extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(QuantJPanel.class);
  private JPanel pTop;
  private UiCheck checkRun;
  private JPanel pContent;
//  private JPanel pImquant;
//  private JPanel pFreequant;
  private UiRadio uiRadioUseFreequant;
  private UiRadio uiRadioUseImquant;
  private ButtonGroup radioGroupQuant;


  public QuantJPanel() {
    initMore();
    initPostCreation();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
  }

  private void initPostCreation() {
    isRun(checkRun.isSelected());
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }
  public void isRun(boolean isEnabled) {
    checkRun.setSelected(isEnabled);
    updateEnabledStatus(pContent, isEnabled);
  }
  public boolean isImquant() {
    return isRun() && SwingUtils.isEnabledAndChecked(uiRadioUseImquant);
  }
  public boolean isFreequant() {
    return isRun() && SwingUtils.isEnabledAndChecked(uiRadioUseFreequant);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessageIsUmpireRun(MessageIsUmpireRun m) {
    if (m.isEnabled) {
      isRun(false); // disable quant when Umpire Runs
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessageQuantRun(MessageQuantRun m) {
    updateEnabledStatus(pContent, m.isRun);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessageLoadQuantDefaults(MessageLoadQuantDefaults m) {
    if (m.doAskUser) {
      int answer = SwingUtils.showConfirmDialog(this, new JLabel("<html>Load default quantitation options?"));
      if (JOptionPane.OK_OPTION != answer) {
        log.debug("User cancelled Loading Shepherd defaults");
        return;
      }
    }
    try {
      Properties props = PropertiesUtils.loadPropertiesLocal(QuantParams.class, QuantParams.DEFAULT_PROPERTIES_FN);
      SwingUtils.valuesFromMap(this, PropertiesUtils.to(props));
    } catch (Exception e) {
      log.error("Error loading quant defaults", e);
      SwingUtils.showErrorDialog(e, this);
    }
  }

  public Map<String, String> toMap() {
    return SwingUtils.valuesToMap(this, (name) -> !name.startsWith("Spinner.formattedTextField"));
  }

  public String getFreequantOptsAsText() {
    // report.labelfree.nonspecific=--ptw 0.4 --tol 10 --isolated
    Map<String, String> m = toMap();
    StringBuilder sb = new StringBuilder();
    sb.append("--ptw ").append(getOrThrow(m, "ui.freequant.rt-tol"));
    sb.append("--tol ").append(getOrThrow(m, "ui.freequant.mz-tol"));
    sb.append("--isolated");
    return sb.toString();
  }

  private String getOrThrow(Map<String, String> m, String key) {
    String s = m.get(key);
    if (s == null)
      throw new IllegalStateException("Could not get key: " + key);
    return s;
  }

  private void initMore() {
    this.setLayout(new BorderLayout());
//    this.setBorder(new EmptyBorder(0,0,0,0));
    this.setBorder(new TitledBorder("Quantitation"));

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px")));

      checkRun = new UiCheck("Run Qunatitation", null, false);
      checkRun.setName("ui.run-quantitation");
      checkRun.addActionListener(e -> {
        EventBus.getDefault().post(new MessageQuantRun(SwingUtils.isEnabledAndChecked(checkRun)));
      });
      pTop.add(checkRun, new CC().alignX("left"));
      JButton btnLoadDefaults = new JButton("Load Quant defaults");
      btnLoadDefaults.addActionListener((e) -> EventBus.getDefault().post(new MessageLoadQuantDefaults(true)));
      pTop.add(btnLoadDefaults, new CC().alignX("left"));

      pTop.setBorder(new EmptyBorder(0,0,0,0));
      this.add(pTop, BorderLayout.NORTH);
    }

    // Main content panel - container
    {
      pContent = new JPanel(new MigLayout(new LC().fillX()));
      pContent.setName("Quant Panel Content");
      pContent.setBorder(new EmptyBorder(0,0,0,0));

      pContent.addPropertyChangeListener("enabled", evt -> {
        log.debug("Quant pContent panel property '{}' changed from '{}' to '{}'", evt.getPropertyName(),
            evt.getOldValue(), evt.getNewValue());
        boolean newValue = (Boolean)evt.getNewValue();
        boolean isSwitchToEnabled = (Boolean) evt.getNewValue() && !(Boolean) evt.getOldValue();
        boolean pContentIsEnabled = newValue && checkRun.isSelected();
        log.debug("Quant pContent panel is switching to enabled? : {}, !checkRun.isSelected() : {}, final state should be: {}",
            isSwitchToEnabled, !checkRun.isSelected(), pContentIsEnabled);
        updateEnabledStatus(pContent, pContentIsEnabled);
      });
    }

    radioGroupQuant = new ButtonGroup();
    JPanel pBothQuants = new JPanel(new MigLayout(new LC()));
    {
//      pFreequant = new JPanel(new MigLayout(new LC()));
      //pPeakPicking.setBorder(new TitledBorder("FreeQuant options"));
      { // FreeQuant
        pBothQuants.setBorder(new EmptyBorder(0, 0, 0, 0));
        uiRadioUseFreequant = new UiRadio("FreeQuant (for non ion mobility data)", null, true);
        radioGroupQuant.add(uiRadioUseFreequant);
        FormEntry feRadioFreequant = new FormEntry("ui.freequant.is-run", "Not shown",
            uiRadioUseFreequant);
        UiSpinnerDouble uiSpinnerRtTol = UiSpinnerDouble.builder(0.4, 0.05, 1000.0, 0.1)
            .setFormat(new DecimalFormat("0.0#")).setNumCols(5).create();
        FormEntry feRtTol = new FormEntry("ui.freequant.rt-tol", "RT Window (Minutes)",
            uiSpinnerRtTol);
        UiSpinnerDouble uiSpinnerMzTol = UiSpinnerDouble.builder(10.0, 0.1, 10000.0, 1)
            .setFormat(new DecimalFormat("0.#")).setNumCols(5).create();
        FormEntry feMzTol = new FormEntry("ui.freequant.mz-tol", "M/z Window (ppm)",
            uiSpinnerMzTol);
        pBothQuants.add(feRadioFreequant.comp, new CC().alignX("left"));
        pBothQuants.add(feRtTol.label(), new CC().alignX("right"));
        pBothQuants.add(feRtTol.comp, new CC().alignX("left"));
        pBothQuants.add(feMzTol.label(), new CC().alignX("right"));
        pBothQuants.add(feMzTol.comp, new CC().alignX("left").spanX(2).wrap());
      }

      { // IMQuant
        uiRadioUseImquant = new UiRadio("IMQuant (use for timsTOF ion mobility data)", null, false);
        radioGroupQuant.add(uiRadioUseImquant);
        FormEntry feRadioImquant = new FormEntry("ui.imquant.is-run", "Not shown",
            uiRadioUseImquant);
        UiSpinnerDouble uiSpinnerImTol = UiSpinnerDouble.builder(0.1, 0.01, 1.0, 0.01)
            .setFormat(new DecimalFormat("0.00")).setNumCols(5).create();
        FormEntry feImTol = new FormEntry("ui.imquant.im-tol", "IM Window (1/k0)", uiSpinnerImTol);
        UiSpinnerDouble uiSpinnerMzTol = UiSpinnerDouble.builder(20.0, 1.0, 1000.0, 1.0)
            .setFormat(new DecimalFormat("0.#")).setNumCols(5).create();
        FormEntry feMzTol = new FormEntry("ui.imquant.mz-tol", "M/z Window (ppm)", uiSpinnerMzTol);
        UiCheck uiCheckImquantPlot = new UiCheck("Plot", null, false);
        FormEntry fePlot = new FormEntry("ui.imquant.is-plot", "Not shown", uiCheckImquantPlot);
        pBothQuants.add(feRadioImquant.comp, new CC().alignX("left"));
        pBothQuants.add(feImTol.label(), new CC().alignX("right"));
        pBothQuants.add(feImTol.comp, new CC().alignX("left"));
        pBothQuants.add(feMzTol.label(), new CC().alignX("right"));
        pBothQuants.add(feMzTol.comp, new CC().alignX("left"));
        pBothQuants.add(fePlot.comp, new CC().alignX("left").wrap());
      }
    }
    pContent.add(pBothQuants, new CC().wrap().growX());

    this.add(pContent, BorderLayout.CENTER);
  }

}
