package umich.msfragger.params.speclib;

import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiRadio;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.nio.file.Path;
import java.util.ArrayList;
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
import javax.swing.JScrollPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.java.balloontip.BalloonTip;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.messages.MessageEasypqpInit;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;


public class SpeclibPanel extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(SpeclibPanel.class);

  private final List<BalloonTip> balloonTips = new ArrayList<>();
  private JCheckBox checkRun;
  private JPanel pContent;
  private JScrollPane scroll;
  private JPanel pOptions;
  private JPanel pTop;
  private UiRadio uiRadioUseSpectrast;
  private UiRadio uiRadioUseEasypqp;
  private ButtonGroup radioGroupTools;
  private List<String> pqpType;
  private List<String> pqpCal;
  private UiText uiTextPqpCalFile;
  private UiCombo uiComboPqpType;
  private UiCombo uiComboPqpCal;


  public SpeclibPanel() {
    initMore();
    initPostCreation();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
  }

  private void initPostCreation() {
    this.addPropertyChangeListener("enabled", evt -> {
      log.debug("Shepherd panel property '{}' changed from '{}' to '{}'", evt.getPropertyName(),
          evt.getOldValue(), evt.getNewValue());
      boolean isSwitchToEnabled = (Boolean) evt.getNewValue() && !(Boolean) evt.getOldValue();
      log.debug("Shepherd panel is switching to enabled? : {}, !checkRun.isSelected() : {}",
          isSwitchToEnabled, !checkRun.isSelected());
      if (isSwitchToEnabled && !checkRun.isSelected()) {
        enablementMapping.put(pContent, false);
        updateEnabledStatus(pContent, false);
      }
    });
  }

  @Subscribe
  public void onMessageEasypqpInit(MessageEasypqpInit m) {
    log.debug("Got MessageEasypqpInit");
    updateEnabledStatus(uiRadioUseEasypqp, m.isPythonOk && m.isEasypqpPackageInstalled);
    if (!m.isPythonOk || !m.isEasypqpPackageInstalled) {
      uiRadioUseEasypqp
          .setToolTipText("Toolchain not initialized: " + (m.message == null ? "N/A" : m.message));
    }
  }

  @Subscribe
  public void onMessageSpeclibGenSpectrastInit(SpecLibGen.MessageInitDone m) {
    log.debug("Got MessageSpeclibGenSpectrastInit");
    updateEnabledStatus(uiRadioUseSpectrast, m.isSuccess);
    if (!m.isSuccess) {
      String reasons = "";
      if (m.reasons != null && !m.reasons.isEmpty()) {
        reasons = "Reasons: " + m.reasons.stream().map(reason -> reason.toString())
            .collect(Collectors.joining("<br/>"));
      }
      uiRadioUseSpectrast.setToolTipText("<html>Toolchain not initialized.<br/>" + reasons);
    }
  }

  private void initMore() {

    this.setLayout(new BorderLayout());
//    this.setBorder(new EmptyBorder(0,0,0,0));
    this.setBorder(new TitledBorder("Spectral Library"));

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px")));

      checkRun = new UiCheck("Generate Spectral Library from search results", null, false);
      checkRun.setName("ui.name.report.speclibgen.run");
      checkRun.addActionListener(e -> {
        final boolean isSelected = checkRun.isSelected();
        enablementMapping.put(pContent, isSelected);
        updateEnabledStatus(pContent, isSelected);
      });
      pTop.add(checkRun, new CC().alignX("left"));

//      JButton btnLoadDefaults = new JButton("Load SpeclibGen defaults");
//      btnLoadDefaults.addActionListener((e) -> EventBus.getDefault().post(new MessageLoadShepherdDefaults(true)));
//      pTop.add(btnLoadDefaults, new CC().alignX("left"));

      pTop.setBorder(new EmptyBorder(0, 0, 0, 0));
      this.add(pTop, BorderLayout.NORTH);
    }

    // Main content panel - container
    {
      pContent = new JPanel(new MigLayout(new LC().fillX()));
      pContent.setBorder(new EmptyBorder(0, 0, 0, 0));

      // when "Run Report" checkbox is switched, this panel can decide not to turn on,
      // if "Run Speclibgen" checkbox is off
      pContent.addPropertyChangeListener("enabled", evt -> {
        log.debug("Speclibgen pContent panel property '{}' changed from '{}' to '{}'",
            evt.getPropertyName(),
            evt.getOldValue(), evt.getNewValue());
        boolean newValue = (Boolean) evt.getNewValue();
        boolean isSwitchToEnabled = (Boolean) evt.getNewValue() && !(Boolean) evt.getOldValue();
        boolean pContentIsEnabled = newValue && checkRun.isSelected();
        log.debug(
            "Speclibgen pContent panel is switching to enabled? : {}, !checkRun.isSelected() : {}, final state should be: {}",
            isSwitchToEnabled, !checkRun.isSelected(), pContentIsEnabled);
        enablementMapping.put(pContent, pContentIsEnabled);
        updateEnabledStatus(pContent, pContentIsEnabled);
      });

//      scroll = new JScrollPane(pContent);
//      scroll.setBorder(new EmptyBorder(0, 0, 0, 0));
//      scroll.getVerticalScrollBar().setUnitIncrement(16);
    }

    {
      pOptions = new JPanel(new MigLayout(new LC().fillX()));
      //pPeakPicking.setBorder(new TitledBorder("PTMShepherd options"));
      pOptions.setBorder(new EmptyBorder(0, 0, 0, 0));

      radioGroupTools = new ButtonGroup();
      uiRadioUseSpectrast = new UiRadio("SpectraST (for non-ion mobility data)", null, true);
      radioGroupTools.add(uiRadioUseSpectrast);
      updateEnabledStatus(uiRadioUseSpectrast, false);
      FormEntry feRadioUseSpectrast = new FormEntry("ui.name.report.speclibgen.use-spectrast",
          "Not shown",
          uiRadioUseSpectrast);
      uiRadioUseEasypqp = new UiRadio("EasyPQP", null, false);
      radioGroupTools.add(uiRadioUseEasypqp);
      updateEnabledStatus(uiRadioUseEasypqp, false);
      FormEntry feRadioUseEasypqp = new FormEntry("ui.name.report.speclibgen.use-easypqp",
          "Not shown",
          uiRadioUseEasypqp);

      pOptions.add(feRadioUseSpectrast.comp, new CC().alignX("left").spanX().wrap());
      pOptions.add(feRadioUseEasypqp.comp, new CC().alignX("left").spanX().wrap());

      final JPanel pPqp = new JPanel(
          new MigLayout(new LC().fillX().alignX("left")));
      { // pqp options stuff
        final String optionAuto = "Automatic selection of a run as reference RT";
        final String optionManual = "User provided RT calibration file";
        pqpCal = Arrays.asList(optionAuto, "iRT", "ciRT", optionManual);
        pqpType = Arrays.asList("timsTOF", "non-timsTOF");

        pPqp.setBorder(new TitledBorder("EasyPQP Options"));
        uiComboPqpCal = UiUtils.createUiCombo(pqpCal);
        FormEntry fePqpCal = new FormEntry("ui.name.report.speclibgen.easypqp.rt-cal",
            "RT Calibration",
            uiComboPqpCal);
        uiTextPqpCalFile = UiUtils.uiTextBuilder().create();
        FormEntry fePqpCalFile = new FormEntry(
            "ui.name.report.speclibgen.easypqp.select-file.text",
            "Calibration file", uiTextPqpCalFile);
        JLabel labelPqpCalFile = fePqpCalFile.label();
        labelPqpCalFile.setName("ui.name.report.speclibgen.easypqp.select-file.label");
        final JButton btnPqpCalFile = fePqpCalFile.browseButton("Browse",
            () -> {
              JFileChooser fc = FileChooserUtils
                  .create("Calibration file", false, FcMode.FILES_ONLY);
              FileChooserUtils.setPath(fc, Stream.of(uiTextPqpCalFile.getNonGhostText()));
              return fc;
            },
            "Select calibration file", paths -> {
              log.debug("User selected PQP file: {}",
                  paths.stream().map(Path::toString).collect(Collectors.joining(", ")));
              Path path = paths
                  .get(0); // we only allowed selection of a single file in the file chooser
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

        pPqp.add(fePqpCal.label(), ccR());
        pPqp.add(fePqpCal.comp, ccL().split());
        pPqp.add(labelPqpCalFile, ccL());
        pPqp.add(fePqpCalFile.comp, ccL().pushX().growX());
        pPqp.add(btnPqpCalFile, ccL().wrap());
        pPqp.add(feDataType.label(), ccR());
        pPqp.add(feDataType.comp, ccL().wrap());

        uiComboPqpCal.addItemListener(e -> {
          String selected = (String) e.getItem();
          final boolean show = optionManual.equals(selected);
          final AtomicBoolean visibilityChanged = new AtomicBoolean(false);
          SwingUtils.traverse(pPqp, false, c -> {
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
            pPqp.revalidate();
          }
        });
        uiComboPqpCal.setSelectedIndex(1);
        uiComboPqpCal.setSelectedIndex(0);
      }

      pOptions.add(pPqp, new CC().spanX().growX().wrap());
      pContent.add(pOptions, new CC().wrap().growX());
    }

    this.add(pContent, BorderLayout.CENTER);

//    {
//      pPrecursorSpectrum = new JPanel(new MigLayout(new LC()));
//      pPrecursorSpectrum.setBorder(new TitledBorder("Peak Matching"));
//    }
  }

  private void validateCalFile(Path path) throws ValidationException {
    throw new ValidationException("GUOCI - implement"); // TODO: GUOCI - check cal file content
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

  public boolean useSpectrast() {
    return SwingUtils.isEnabledAndChecked(uiRadioUseSpectrast);
  }

  private void clearBalloonTips() {
    for (BalloonTip balloonTip : balloonTips) {
      if (balloonTip != null) {
        try {
          balloonTip.closeBalloon();
        } catch (Exception ignore) {
        }
      }
    }
    balloonTips.clear();
  }

  public boolean validateForm() {

//    Pattern reVarMods = Pattern.compile("[^\\s]+:-?\\d+(?:\\.\\d+)?(?:\\s*,\\s*[^\\s]+:-?\\d+(?:\\.\\d+)?)*");
//    String text = uiTextVarMods.getNonGhostText().trim();
//    boolean ok = true;
//    if (!StringUtils.isNullOrWhitespace(text) && !reVarMods.matcher(text).matches()) {
//      BalloonTip tip = new BalloonTip(uiTextVarMods,
//          "<html>Does not match allowed format \"&lt;name&gt;:&lt;mass&gt;\"");
//      tip.setVisible(true);
//      balloonTips.add(tip);
//      ok = false;
//    }
//    return ok;
    return true;
  }
}