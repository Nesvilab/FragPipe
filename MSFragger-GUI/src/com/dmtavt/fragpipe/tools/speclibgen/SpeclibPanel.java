package com.dmtavt.fragpipe.tools.speclibgen;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.NoteConfigPython;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SpeclibPanel extends JPanelBase {

  private static final Logger log = LoggerFactory.getLogger(SpeclibPanel.class);
  private static final String PREFIX = "speclibgen.";

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private UiRadio uiRadioUseSpectrast;
  private UiRadio uiRadioUseEasypqp;
  private ButtonGroup radioGroupTools;
  private List<String> pqpType;
  private List<String> pqpCal;
  private UiText uiTextPqpCalFile;
  private UiCombo uiComboPqpType;
  private UiCombo uiComboPqpCal;
  private UiSpinnerDouble uiSpinnerLowess;
  private UiSpinnerDouble uiSpinner_max_delta_unimod;
  private UiSpinnerDouble uiSpinner_max_delta_ppm;
  private JPanel panelEasypqp;
  private JPanel panelSpectrast;
  public static final String EASYPQP_TIMSTOF = "timsTOF";
  public static final String EASYPQP_EXTRAS_PREFIX = "easypqp.extras.";

  @Override
  protected void initMore() {
    super.initMore();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigSpeclibgen m) {
    log.debug("SpeclibPanel got NoteConfigSpeclibgen, instance not null? - {}", m.instance != null);
    if (m.instance == null || !m.isValid()) {
      updateEnabledStatus(this, false);
      return;
    }

    updateEnabledStatus(this, true);
    updateEnabledStatus(panelEasypqp, m.instance.isEasypqpOk());
    updateEnabledStatus(uiRadioUseSpectrast, m.instance.isSpectrastOk());
    uiRadioUseEasypqp.setToolTipText(m.instance.isEasypqpOk() ? "" : "Toolchain not initialized");
    uiRadioUseSpectrast.setToolTipText(m.instance.isSpectrastOk() ? "" : "Toolchain not initialized");
  }

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
    return PREFIX;
  }

  private JPanel createPanelTop() {
    // setting the insets allows the top panel to be shifted left of the options panel
    JPanel p = new JPanel(new MigLayout(new LC().insetsAll("0px")));
    mu.borderEmpty(p);

    checkRun = new UiCheck("Generate Spectral Library from search results", null, false);
    checkRun.setName("run-speclibgen");

    mu.add(p, checkRun);
    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    radioGroupTools = new ButtonGroup();
    panelEasypqp = createPanelEasypqp(radioGroupTools);
    panelSpectrast = createPanelSpectrast(radioGroupTools);

    mu.add(p, panelEasypqp).spanX().growX().wrap();
    mu.add(p, panelSpectrast).spanX().growX().wrap();

    return p;
  }

  private JPanel createPanelSpectrast(ButtonGroup radioGroupTools) {
    JPanel p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.border(p, 1);

    uiRadioUseSpectrast = new UiRadio("Use SpectraST (non-ion mobility data only)", null, false);
    radioGroupTools.add(uiRadioUseSpectrast);
    updateEnabledStatus(uiRadioUseSpectrast, false);
    FormEntry feRadioUseSpectrast = new FormEntry("use-spectrast", "Not shown",
        uiRadioUseSpectrast);

    mu.add(p, feRadioUseSpectrast.comp).pushX().growX().wrap();

    return p;
  }

  private JPanel createPanelEasypqp(ButtonGroup buttonGroup) {
    final JPanel p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.border(p, 1);

    uiRadioUseEasypqp = new UiRadio("Use EasyPQP", null, true);
    uiRadioUseEasypqp.setToolTipText("Enablement depends on proper python configuration");
    buttonGroup.add(uiRadioUseEasypqp);
    FormEntry feRadioUseEasypqp = new FormEntry("use-easypqp", "not-shown", uiRadioUseEasypqp);
    final String optionAuto = "Automatic selection of a run as reference RT";
    final String optionManual = "User provided RT calibration file";
    pqpCal = Arrays.asList(optionAuto, "iRT", "ciRT", optionManual);
    pqpType = Arrays.asList(EASYPQP_TIMSTOF, "non-timsTOF");

    uiComboPqpCal = UiUtils.createUiCombo(pqpCal);
    FormEntry fePqpCal = new FormEntry("easypqp.rt-cal",
        "RT Calibration", uiComboPqpCal);
    uiTextPqpCalFile = UiUtils.uiTextBuilder().create();
    FormEntry fePqpCalFile = mu.feb(uiTextPqpCalFile)
        .name("easypqp.select-file.text").label("Calibration file").create();
    JLabel labelPqpCalFile = fePqpCalFile.label();
    Fragpipe.renameNoCache(labelPqpCalFile, "easypqp.select-file.label");
    final JButton btnPqpCalFile = fePqpCalFile.browseButton("Browse",
        "Select calibration file", () -> {
          JFileChooser fc = FileChooserUtils
              .create("Calibration file", false, FcMode.FILES_ONLY);
          FileChooserUtils.setPath(fc, Stream.of(uiTextPqpCalFile.getNonGhostText()));
          return fc;
        },
        paths -> {
          log.debug("User selected PQP file: {}",
              paths.stream().map(Path::toString).collect(Collectors.joining(", ")));
          Path path = paths.get(0); // we only allowed selection of a single file in the file chooser
          try {
            validateCalFile(path);
          } catch (ValidationException e) {
            SwingUtils.showErrorDialog(this, SwingUtils.makeHtml(e.getMessage()), "Cal file error");
            return;
          }
          // validation went without exceptions
          uiTextPqpCalFile.setText(path.toString());
        });
    btnPqpCalFile.setName("easypqp.select-file.button." + Fragpipe.PROP_NOCACHE);

    uiComboPqpType = UiUtils.createUiCombo(pqpType);
//    FormEntry feDataType = new FormEntry("easypqp.data-type","Data type", uiComboPqpType);
    uiSpinnerLowess = UiUtils.spinnerDouble(0.01, 0.0, 1.0, 0.01)
        .setCols(5).setFormat("#.##").create();
    FormEntry feLowess = mu.feb(uiSpinnerLowess).name("easypqp.extras.rt_lowess_fraction")
        .label("RT Lowess Fraction")
        .tooltip("Fraction of data points to use for RT lowess regression. If set to 0, cross validation is used.").create();

    uiSpinner_max_delta_unimod = UiUtils.spinnerDouble(0.02, 0.0, 1.0, 0.01)
            .setCols(5).setFormat("#.##").create();
    FormEntry fe_max_delta_unimod = mu.feb(uiSpinner_max_delta_unimod).name("easypqp.extras.max_delta_unimod")
        .label("UniMod annotation tol (Da)")
        .tooltip("Maximum delta mass (Dalton) for UniMod annotation.  [default: 0.02]").create();

    uiSpinner_max_delta_ppm = UiUtils.spinnerDouble(15, 0.0, 100, 0.01)
            .setCols(5).setFormat("#.##").create();
    FormEntry fe_max_delta_ppm = mu.feb(uiSpinner_max_delta_ppm).name("easypqp.extras.max_delta_ppm")
        .label("Fragment annotation tol (ppm)")
        .tooltip("Maximum delta mass (PPM) for annotation. [default: 15]").create();

    mu.add(p, feRadioUseEasypqp.comp).wrap();
    mu.add(p, fePqpCal.label(), ccR());
    mu.add(p, fePqpCal.comp).split();
    mu.add(p, labelPqpCalFile);
    mu.add(p, btnPqpCalFile);
    mu.add(p, fePqpCalFile.comp).pushX().growX().wrap();
    mu.add(p, feLowess.label(), mu.ccR());
    mu.add(p, feLowess.comp).wrap();
    mu.add(p, fe_max_delta_unimod.label(), mu.ccR());
    mu.add(p, fe_max_delta_unimod.comp).wrap();
    mu.add(p, fe_max_delta_ppm.label(), mu.ccR());
    mu.add(p, fe_max_delta_ppm.comp).wrap();
//    mu.add(p, feDataType.label(), ccR());
//    mu.add(p, feDataType.comp, ccL().wrap());

    uiComboPqpCal.addItemListener(e -> {
      String selected = (String) e.getItem();
      final boolean show = optionManual.equals(selected);
      final AtomicBoolean visibilityChanged = new AtomicBoolean(false);
      SwingUtils.traverse(p, false, c -> {
        String name = c.getName();
        if (name != null && name.contains("easypqp.select-file.")) {
          log.debug("Traversing easyPQP options panel, found matching component: {}", name);
          if (c.isVisible() != show) {
            visibilityChanged.set(show);
            c.setVisible(show);
          }
        }
      });
      if (visibilityChanged.get()) {
        p.revalidate();
      }
    });
    uiComboPqpCal.setSelectedIndex(1);
    uiComboPqpCal.setSelectedIndex(0);

    updateEnabledStatus(p, false);
    return p;
  }

  /* Additional parameters for easypqp configuration that don't each need separate getters. */
  public Map<String, String> getMapArgsExtras() {
    return SwingUtils.valuesGet(this, name -> name.contains(EASYPQP_EXTRAS_PREFIX));
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("Spectral library generation"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  private void validateCalFile(Path path) throws ValidationException {
    NoteConfigPython configPython;
    try {
      configPython = Fragpipe.getSticky(NoteConfigPython.class);
    } catch (NoStickyException e) {
      throw new ValidationException(e);
    }
    final String s = configPython.pi.validateCalFile(path);
    if (s.trim().endsWith("ok"))
      return;
    throw new ValidationException(s);
  }

  private static CC ccL() {
    return new CC().alignX("left");
  }

  private static CC ccR() {
    return new CC().alignX("right");
  }

  public boolean isRunSpeclibgen() {
    return SwingUtils.isEnabledAndChecked(checkRun) && (useEasypqp() || useSpectrast());
  }

  public boolean useEasypqp() {
    return SwingUtils.isEnabledAndChecked(uiRadioUseEasypqp);
  }

  public String getEasypqpDataType() {
    return (String)uiComboPqpType.getSelectedItem();
  }

  public String getEasypqpCalOption() {
    return new String[]{"noiRT", "iRT", "ciRT", "a tsv file"}[uiComboPqpCal.getSelectedIndex()];
  }

  public Path getEasypqpCalFilePath() {
    return Paths.get(uiTextPqpCalFile.getNonGhostText());
  }

  public String getEasypqpFileType() {
    return (String) uiComboPqpType.getSelectedItem();
  }

  public double getEasypqpRTLowessFraction() {
    return uiSpinnerLowess.getActualValue();
  }

  public double getEasypqp_max_delta_unimod() {
    return uiSpinner_max_delta_unimod.getActualValue();
  }

  public double getEasypqp_max_delta_ppm() {
    return uiSpinner_max_delta_ppm.getActualValue();
  }

  public boolean useSpectrast() {
    return SwingUtils.isEnabledAndChecked(uiRadioUseSpectrast);
  }

}