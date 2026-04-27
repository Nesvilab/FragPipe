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

package org.nesvilab.fragpipe.tools.skyline;

import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;
import static org.nesvilab.utils.OsUtils.isWindows;

import org.nesvilab.fragpipe.messages.MessageUiRevalidate;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
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
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;


public class SkylinePanel extends JPanelBase {

  private static final String PREFIX = "skyline.";

  // Skyline mass analyzer display names (subset of Skyline's TransitionFullScan.MASS_ANALYZERS)
  static final String ANALYZER_CENTROIDED = "Centroided";
  static final String ANALYZER_QIT = "QIT";
  static final String[] MASS_ANALYZERS = {ANALYZER_CENTROIDED, ANALYZER_QIT};

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private JPanel panelBasic;
  private UiRadio uiRadioSkyline;
  private UiRadio uiRadioSkylineDaily;
  private UiRadio uiRadioSkylineCustom;
  private UiText uiTextSkylineCustom;
  private UiCombo uiComboModsMode;
  private UiCombo uiComboPrecursorMassAnalyzer;
  private UiCombo uiComboProductMassAnalyzer;
  private UiSpinnerDouble uiSpinnerPrecursorTolerance;
  private UiSpinnerDouble uiSpinnerFragmentTolerance;
  private JLabel labelPrecursorTolerance;
  private JLabel labelFragmentTolerance;
  private JLabel labelPrecursorTolUnit;
  private JLabel labelFragmentTolUnit;
  private UiSpinnerDouble uiSpinnerRtTolerance;
  private UiSpinnerInt uiSpinnerLibraryProductIons;
  private UiCheck uiCheckGenerateSkylineQuantReport;
  private UiCheck uiCheckSkipSkylineDocumentGeneration;
  private UiCheck uiCheckRunFragReporter;
  private JPanel panelSkylineQuant;
  private UiText uiTextModTag;
  private UiSpinnerDouble uiSpinnerSiteProb;
  private UiSpinnerDouble uiSpinnerFdr;

  @Override
  protected void initMore() {
    super.initMore();
    SwingUtils.setEnablementUpdater(this, pContent, checkRun);
    SwingUtils.setEnablementUpdater(this, panelSkylineQuant, checkRun);
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

    checkRun = new UiCheck("Run Skyline", null, false);
    checkRun.setName("run-skyline");

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/icon-skyline-48.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(p, checkRun).pushX();
    mu.add(p, imageLabel).gapRight("50").wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    panelBasic = createPanelBasic();

    mu.add(p, panelBasic).growX().wrap();

    return p;
  }

  private JPanel createPanelBasic() {
    panelBasic = mu.newPanel(mu.lcFillX());
    mu.border(panelBasic, 1);
    mu.border(panelBasic, "Basic settings");

    ButtonGroup radioGroup = new ButtonGroup();

    uiRadioSkyline = new UiRadio("Skyline", null, true);
    radioGroup.add(uiRadioSkyline);
    FormEntry feRadioSkyline = new FormEntry("skyline", "Not shown", uiRadioSkyline);

    uiRadioSkylineDaily = new UiRadio("Skyline Daily", null, false);
    radioGroup.add(uiRadioSkylineDaily);
    FormEntry feRadioSkylineDaily = new FormEntry("skyline-daily", "Not shown", uiRadioSkylineDaily);

    uiRadioSkylineCustom = new UiRadio("Custom", null, false);
    radioGroup.add(uiRadioSkylineCustom);
    FormEntry feRadioSkylineCustom = new FormEntry("skyline-custom", "Not shown", uiRadioSkylineCustom);

    uiTextSkylineCustom = UiUtils.uiTextBuilder().create();
    FormEntry feSkylineCustom = new FormEntry("skyline-custom-path", "Skyline custom path (optional)", uiTextSkylineCustom, "");
    JButton jButtonSkylineCustom = feSkylineCustom.browseButton("Browse", "Select library file", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Skyline", "exe");
      JFileChooser fc = FileChooserUtils.create("Skyline executable file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextSkylineCustom.getNonGhostText()));
      return fc;
    }, paths -> {
      Path path = paths.get(0); // we only allowed selection of a single file in the file chooser
      uiTextSkylineCustom.setText(path.toString());
    });

    uiRadioSkylineCustom.addItemListener(e -> {
      updateEnabledStatus(feSkylineCustom.comp, uiRadioSkylineCustom.isSelected());
      updateEnabledStatus(jButtonSkylineCustom, uiRadioSkylineCustom.isSelected());
    });

    uiComboModsMode = UiUtils.createUiCombo(Arrays.asList("Default", "O-glyco", "N-glyco"));
    uiComboModsMode.setSelectedIndex(0);
    FormEntry feComboModsMode = new FormEntry("skyline-mods-mode", "Special modifications mode", uiComboModsMode, "Special modification support.<br>"
        + "If O-glyco, uses O-Pair glycan database instead of mass offsets list.<br>"
        + "If N-glyco, uses Glycan Composition Assignment glycan database instead of mass offsets list.");

    String analyzerTooltip = "Mass analyzer used for the full-scan filter.<br>"
        + "Centroided: mass accuracy in ppm (1-1000).<br>"
        + "QIT: resolution in m/z (0.1-2.0).";
    String resolutionTooltip = "Resolution / mass accuracy for the selected mass analyzer.<br>"
        + "Centroided uses ppm, QIT uses m/z.";

    uiComboPrecursorMassAnalyzer = UiUtils.createUiCombo(MASS_ANALYZERS);
    uiComboPrecursorMassAnalyzer.setSelectedItem(ANALYZER_CENTROIDED);
    FormEntry fePrecursorMassAnalyzer = new FormEntry("skyline-precursor-mass-analyzer", "Precursor mass analyzer", uiComboPrecursorMassAnalyzer, analyzerTooltip);

    uiSpinnerPrecursorTolerance = UiUtils.spinnerDouble(10, 0.1, 1000, 1).setCols(8).setFormat("#.####").create();
    FormEntry fePrecursorTolerance = new FormEntry("skyline-precursor-tolerance", "Resolution", uiSpinnerPrecursorTolerance, resolutionTooltip);
    labelPrecursorTolerance = fePrecursorTolerance.label();
    labelPrecursorTolUnit = new JLabel("ppm");

    uiComboProductMassAnalyzer = UiUtils.createUiCombo(MASS_ANALYZERS);
    uiComboProductMassAnalyzer.setSelectedItem(ANALYZER_CENTROIDED);
    FormEntry feProductMassAnalyzer = new FormEntry("skyline-product-mass-analyzer", "Product mass analyzer", uiComboProductMassAnalyzer, analyzerTooltip);

    uiSpinnerFragmentTolerance = UiUtils.spinnerDouble(10, 0.1, 1000, 1).setCols(8).setFormat("#.####").create();
    FormEntry feFragmentTolerance = new FormEntry("skyline-fragment-tolerance", "Resolution", uiSpinnerFragmentTolerance, resolutionTooltip);
    labelFragmentTolerance = feFragmentTolerance.label();
    labelFragmentTolUnit = new JLabel("ppm");

    uiComboPrecursorMassAnalyzer.addItemListener(e -> {
      if (e.getStateChange() == ItemEvent.SELECTED) {
        applyMassAnalyzerSelection((String) e.getItem(), uiSpinnerPrecursorTolerance, labelPrecursorTolerance, labelPrecursorTolUnit);
      }
    });
    uiComboProductMassAnalyzer.addItemListener(e -> {
      if (e.getStateChange() == ItemEvent.SELECTED) {
        applyMassAnalyzerSelection((String) e.getItem(), uiSpinnerFragmentTolerance, labelFragmentTolerance, labelFragmentTolUnit);
      }
    });

    uiSpinnerRtTolerance = UiUtils.spinnerDouble(2.0, 0.1, 600.0, 0.1).setCols(5).setFormat("#.##").create();
    FormEntry feRtTolerance = new FormEntry("skyline-rt-tolerance", "RT tolerance (min)", uiSpinnerRtTolerance, "Retention time tolerance in minutes");

    uiSpinnerLibraryProductIons = new UiSpinnerInt(12, 1, 100, 1);
    FormEntry feLibraryProductIons = new FormEntry("skyline-library-product-ions", "Library product ions", uiSpinnerLibraryProductIons, "Number of library product ions");

    mu.add(panelBasic, feRadioSkyline.comp);
    mu.add(panelBasic, feRadioSkylineDaily.comp);
    mu.add(panelBasic, feRadioSkylineCustom.comp).split(3);
    mu.add(panelBasic, feSkylineCustom.comp).growX().pushX();
    mu.add(panelBasic, jButtonSkylineCustom).wrap();

    mu.add(panelBasic, fePrecursorMassAnalyzer.label(), mu.ccL()).split(2);
    mu.add(panelBasic, fePrecursorMassAnalyzer.comp);
    mu.add(panelBasic, labelPrecursorTolerance, mu.ccL()).split(3);
    mu.add(panelBasic, fePrecursorTolerance.comp);
    mu.add(panelBasic, labelPrecursorTolUnit).wrap();

    mu.add(panelBasic, feProductMassAnalyzer.label(), mu.ccL()).split(2);
    mu.add(panelBasic, feProductMassAnalyzer.comp);
    mu.add(panelBasic, labelFragmentTolerance, mu.ccL()).split(3);
    mu.add(panelBasic, feFragmentTolerance.comp);
    mu.add(panelBasic, labelFragmentTolUnit).wrap();

    mu.add(panelBasic, feRtTolerance.label(), mu.ccL()).split(2);
    mu.add(panelBasic, feRtTolerance.comp).wrap();

    mu.add(panelBasic, feLibraryProductIons.label(), mu.ccL()).split(2);
    mu.add(panelBasic, feLibraryProductIons.comp);

    mu.add(panelBasic, feComboModsMode.label(), mu.ccL()).split(2);
    mu.add(panelBasic, feComboModsMode.comp).wrap();

    // initialize labels and spinner ranges to match the default selection
    applyMassAnalyzerSelection(ANALYZER_CENTROIDED, uiSpinnerPrecursorTolerance, labelPrecursorTolerance, labelPrecursorTolUnit);
    applyMassAnalyzerSelection(ANALYZER_CENTROIDED, uiSpinnerFragmentTolerance, labelFragmentTolerance, labelFragmentTolUnit);

    updateEnabledStatus(feSkylineCustom.comp, uiRadioSkylineCustom.isSelected());
    updateEnabledStatus(jButtonSkylineCustom, uiRadioSkylineCustom.isSelected());
    updateEnabledStatus(panelBasic, true);

    adjustRadios();

    return panelBasic;
  }

  private JPanel createPanelSkylineQuant() {
    panelSkylineQuant = mu.newPanel(mu.lcFillX());
    mu.border(panelSkylineQuant, 1);
    mu.border(panelSkylineQuant, "Perform Skyline quantification");

    uiCheckGenerateSkylineQuantReport = UiUtils.createUiCheck("Build library, quantify, extract report", false);
    uiCheckGenerateSkylineQuantReport.setName("generate-skyline-quant-report");

    JLabel noteLabel = new JLabel("<html><b>Note: Do not enable this panel if you just want to use Skyline to visualize the results.<br>With this panel enabled, Skyline will build a new library and modify the traced peak boundaries.</b></html>");

    JPanel panelQuantOptions = mu.newPanel(mu.lcFillX());

    uiCheckSkipSkylineDocumentGeneration = UiUtils.createUiCheck("Use existing Skyline document", false);
    uiCheckSkipSkylineDocumentGeneration.setName("use-existing-skyline-document");
    uiCheckSkipSkylineDocumentGeneration.setToolTipText("If you already generated a Skyline document, you can skip the generation of a new one.");

    uiSpinnerFdr = UiUtils.spinnerDouble(1, 0, 1, 0.01).setCols(5).setFormat("#.###").create();
    FormEntry feFdr = mu.feb(uiSpinnerFdr).name("fdr").label("FDR").tooltip("False discovery rate threshold").create();

    JPanel panelFragReporter = mu.newPanel(mu.lcFillX());
    mu.border(panelFragReporter, 1);
    mu.border(panelFragReporter, "Reports");

    uiCheckRunFragReporter = UiUtils.createUiCheck("Run FragReporter to generate additional reports", true);
    uiCheckRunFragReporter.setName("run-fragreporter");

    uiTextModTag = UiUtils.uiTextBuilder().cols(40).create();
    FormEntry feModTag = new FormEntry("mod-tag", "Mod tag", uiTextModTag, "<html>Modification tag for generating modification-specific reports <br/>\n"
        + "STY:79.9663 for phospho<br/>\n"
        + "K:114.0429 for ubiquitin");

    uiSpinnerSiteProb = UiUtils.spinnerDouble(0.75, 0, 1, 0.01).setCols(5).setFormat("#.###").create();
    FormEntry feSiteProb = mu.feb(uiSpinnerSiteProb).name("min-site-prob").label("Min site probability").tooltip("Site localization confidence threshold").create();

    mu.add(panelFragReporter, uiCheckRunFragReporter).wrap();
    mu.add(panelFragReporter, feModTag.label(), mu.ccL()).split(2);
    mu.add(panelFragReporter, feModTag.comp).growX();
    mu.add(panelFragReporter, feSiteProb.label()).split(2);
    mu.add(panelFragReporter, feSiteProb.comp, mu.ccL());

    updateEnabledStatus(feModTag.label(), uiCheckRunFragReporter.isSelected());
    updateEnabledStatus(feModTag.comp, uiCheckRunFragReporter.isSelected());
    updateEnabledStatus(feSiteProb.label(), uiCheckRunFragReporter.isSelected());
    updateEnabledStatus(feSiteProb.comp, uiCheckRunFragReporter.isSelected());

    uiCheckRunFragReporter.addItemListener(e -> {
      boolean selected = SwingUtils.isEnabledAndChecked(uiCheckRunFragReporter);
      updateEnabledStatus(feModTag.label(), selected);
      updateEnabledStatus(feModTag.comp, selected);
      updateEnabledStatus(feSiteProb.label(), selected);
      updateEnabledStatus(feSiteProb.comp, selected);
    });

    mu.add(panelQuantOptions, uiCheckSkipSkylineDocumentGeneration).wrap();
    mu.add(panelQuantOptions, feFdr.label()).split(2);
    mu.add(panelQuantOptions, feFdr.comp, mu.ccL()).wrap();
    mu.add(panelQuantOptions, panelFragReporter).growX().spanX().wrap();

    updateEnabledStatus(panelQuantOptions, SwingUtils.isEnabledAndChecked(uiCheckGenerateSkylineQuantReport));

    uiCheckGenerateSkylineQuantReport.addItemListener(e -> {
      updateEnabledStatus(panelQuantOptions, SwingUtils.isEnabledAndChecked(uiCheckGenerateSkylineQuantReport));
    });

    mu.add(panelSkylineQuant, uiCheckGenerateSkylineQuantReport).wrap();
    mu.add(panelSkylineQuant, noteLabel).wrap();
    mu.add(panelSkylineQuant, panelQuantOptions).growX().spanX().wrap();

    updateEnabledStatus(panelSkylineQuant, true);
    return panelSkylineQuant;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("Skyline"));

    pTop = createPanelTop();
    pContent = createPanelContent();
    panelSkylineQuant = createPanelSkylineQuant();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
    this.add(panelSkylineQuant, BorderLayout.SOUTH);
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageUiRevalidate m) {
    adjustRadios();
  }

  private void adjustRadios() {
    DefaultArtifactVersion skylineVersion = Skyline.getSkylineVersion();
    DefaultArtifactVersion skylineDailyVersion = Skyline.getSkylineDailyVersion();

    if (skylineVersion != null && skylineDailyVersion != null) {
      updateEnabledStatus(uiRadioSkyline, true);
      updateEnabledStatus(uiRadioSkylineDaily, true);
    } else if (skylineVersion != null) {
      uiRadioSkyline.setSelected(true);
      updateEnabledStatus(uiRadioSkyline, true);
      uiRadioSkylineDaily.setSelected(false);
      updateEnabledStatus(uiRadioSkylineDaily, false);
    } else if (skylineDailyVersion != null) {
      uiRadioSkyline.setSelected(false);
      updateEnabledStatus(uiRadioSkyline, false);
      uiRadioSkylineDaily.setSelected(true);
      updateEnabledStatus(uiRadioSkylineDaily, true);
    } else {
      uiRadioSkyline.setSelected(false);
      updateEnabledStatus(uiRadioSkyline, false);
      uiRadioSkylineDaily.setSelected(false);
      updateEnabledStatus(uiRadioSkylineDaily, false);
    }
  }

  public String getSkylinePath() {
    if (!isWindows()) {
      return null;
    } else if (SwingUtils.isEnabledAndChecked(uiRadioSkyline)) {
      return Skyline.getSkylineRunnerPath();
    } else if (SwingUtils.isEnabledAndChecked(uiRadioSkylineDaily)) {
      return Skyline.getSkylineDailyRunnerPath();
    } else if (SwingUtils.isEnabledAndChecked(uiRadioSkylineCustom)) {
      return uiTextSkylineCustom.getNonGhostText();
    } else {
      return null;
    }
  }

  public String getSkylineVersion() {
    if (SwingUtils.isEnabledAndChecked(uiRadioSkyline)) {
      return Skyline.getSkylineVersion().toString();
    } else if (SwingUtils.isEnabledAndChecked(uiRadioSkylineDaily)) {
      return Skyline.getSkylineDailyVersion().toString();
    } else if (SwingUtils.isEnabledAndChecked(uiRadioSkylineCustom)) {
      try {
        return Skyline.sub2(uiTextSkylineCustom.getNonGhostText());
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    } else {
      return null;
    }
  }

  public int getModsMode() {
    return uiComboModsMode.getSelectedIndex();
  }

  public double getPrecursorTolerance() {
    return uiSpinnerPrecursorTolerance.getActualValue();
  }

  public double getFragmentTolerance() {
    return uiSpinnerFragmentTolerance.getActualValue();
  }

  public String getPrecursorMassAnalyzer() {
    return (String) uiComboPrecursorMassAnalyzer.getSelectedItem();
  }

  public String getProductMassAnalyzer() {
    return (String) uiComboProductMassAnalyzer.getSelectedItem();
  }

  public double getRtTolerance() {
    return uiSpinnerRtTolerance.getActualValue();
  }

  /**
   * Adapt the spinner range/default value, label text, and unit suffix to match the selected
   * Skyline mass analyzer. Resolution units per Skyline's TransitionFullScan:
   *   Centroided -> ppm (1-1000, default 10)
   *   QIT        -> m/z (0.1-2.0, default 0.7)
   */
  static void applyMassAnalyzerSelection(String analyzer, UiSpinnerDouble resSpinner, JLabel resLabel, JLabel unitLabel) {
    SpinnerNumberModel model = (SpinnerNumberModel) resSpinner.getModel();
    double current = ((Number) resSpinner.getValue()).doubleValue();
    if (ANALYZER_QIT.equals(analyzer)) {
      resLabel.setText("Resolution");
      unitLabel.setText("m/z");
      model.setMinimum(0.1);
      model.setMaximum(2.0);
      model.setStepSize(0.1);
      if (current < 0.1 || current > 2.0) {
        resSpinner.setValue(0.7);
      }
    } else {
      resLabel.setText("Mass accuracy");
      unitLabel.setText("ppm");
      model.setMinimum(1.0);
      model.setMaximum(1000.0);
      model.setStepSize(1.0);
      if (current < 1 || current > 1000) {
        resSpinner.setValue(10.0);
      }
    }
  }

  public int getLibraryProductIons() {
    return uiSpinnerLibraryProductIons.getActualValue();
  }

  public boolean isRunSkylineQuant() {
    return SwingUtils.isEnabledAndChecked(uiCheckGenerateSkylineQuantReport);
  }

  public boolean isSkipSkylineDocumentGeneration() {
    return SwingUtils.isEnabledAndChecked(uiCheckSkipSkylineDocumentGeneration);
  }

  public boolean isRunFragReporter() {
    return SwingUtils.isEnabledAndChecked(uiCheckRunFragReporter);
  }

  public String getModTag() {
    return uiTextModTag.getNonGhostText().trim();
  }

  public float getSiteProb() {
    return (float) uiSpinnerSiteProb.getActualValue();
  }

  public float getFdr() {
    return (float) uiSpinnerFdr.getActualValue();
  }
}