package com.dmtavt.fragpipe.params.speclib;

import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.github.chhh.utils.PythonInfo;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
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
  private JPanel panelEasypqp;
  private JPanel panelSpectrast;

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

//      JButton btnLoadDefaults = new JButton("Load SpeclibGen defaults");
//      btnLoadDefaults.addActionListener((e) -> EventBus.getDefault().post(new MessageLoadShepherdDefaults(true)));
//      pTop.add(btnLoadDefaults, new CC().alignX("left"));

    mu.add(p, checkRun);
    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    radioGroupTools = new ButtonGroup();
    panelSpectrast = createPanelSpectrast(radioGroupTools);
    panelEasypqp = createPanelEasypqp(radioGroupTools);

    mu.add(p, panelSpectrast).spanX().growX().wrap();
    mu.add(p, panelEasypqp).spanX().growX().wrap();

    return p;
  }

  private JPanel createPanelSpectrast(ButtonGroup radioGroupTools) {
    JPanel p = mu.newPanel("SpectraST", mu.lcFillXNoInsetsTopBottom());

    uiRadioUseSpectrast = new UiRadio("Use SpectraST (for non-ion mobility data)", null, true);
    radioGroupTools.add(uiRadioUseSpectrast);
    updateEnabledStatus(uiRadioUseSpectrast, false);
    FormEntry feRadioUseSpectrast = new FormEntry("use-spectrast", "Not shown",
        uiRadioUseSpectrast);

    mu.add(p, feRadioUseSpectrast.comp).pushX().growX().wrap();

    return p;
  }

  private JPanel createPanelEasypqp(ButtonGroup buttonGroup) {
    final JPanel p = mu.newPanel("EasyPQP", mu.lcFillXNoInsetsTopBottom());

    uiRadioUseEasypqp = new UiRadio("Use EasyPQP", null, false);
    uiRadioUseEasypqp.setToolTipText("Enablement depends on proper python configuration");
    buttonGroup.add(uiRadioUseEasypqp);
    FormEntry feRadioUseEasypqp = new FormEntry("use-easypqp", "not-shown", uiRadioUseEasypqp);
    final String optionAuto = "Automatic selection of a run as reference RT";
    final String optionManual = "User provided RT calibration file";
    pqpCal = Arrays.asList(optionAuto, "iRT", "ciRT", optionManual);
    pqpType = Arrays.asList("timsTOF", "non-timsTOF");

    uiComboPqpCal = UiUtils.createUiCombo(pqpCal);
    FormEntry fePqpCal = new FormEntry("ui.name.report.speclibgen.easypqp.rt-cal",
        "RT Calibration", uiComboPqpCal);
    uiTextPqpCalFile = UiUtils.uiTextBuilder().create();
    FormEntry fePqpCalFile = new FormEntry(
        "ui.name.report.speclibgen.easypqp.select-file.text",
        "Calibration file", uiTextPqpCalFile);
    JLabel labelPqpCalFile = fePqpCalFile.label();
    labelPqpCalFile.setName("ui.name.report.speclibgen.easypqp.select-file.label");
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
    btnPqpCalFile.setName("ui.name.report.speclibgen.easypqp.select-file.button");

    uiComboPqpType = UiUtils.createUiCombo(pqpType);
    FormEntry feDataType = new FormEntry("ui.name.report.speclibgen.easypqp.data-type",
        "Data type", uiComboPqpType);

    mu.add(p, feRadioUseEasypqp.comp).wrap();
    p.add(fePqpCal.label(), ccR());
    p.add(fePqpCal.comp, ccL().split());
    p.add(labelPqpCalFile, ccL());
    p.add(fePqpCalFile.comp, ccL().pushX().growX());
    p.add(btnPqpCalFile, ccL().wrap());
    p.add(feDataType.label(), ccR());
    p.add(feDataType.comp, ccL().wrap());

    uiComboPqpCal.addItemListener(e -> {
      String selected = (String) e.getItem();
      final boolean show = optionManual.equals(selected);
      final AtomicBoolean visibilityChanged = new AtomicBoolean(false);
      SwingUtils.traverse(p, false, c -> {
        String name = c.getName();
        if (name != null && name.contains("ui.name.report.speclibgen.easypqp.select-file.")) {
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
    final String s = PythonInfo.get().validateCalFile(path);
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

  public String getEasypqpCalOption() {
    return new String[]{"noiRT", "iRT", "ciRT", "a tsv file"}[uiComboPqpCal.getSelectedIndex()];
  }

  public Path getEasypqpCalFilePath() {
    return Paths.get(uiTextPqpCalFile.getNonGhostText());
  }

  public String getEasypqpFileType() {
    return (String)uiComboPqpType.getSelectedItem();
  }

  public boolean useSpectrast() {
    return SwingUtils.isEnabledAndChecked(uiRadioUseSpectrast);
  }

}