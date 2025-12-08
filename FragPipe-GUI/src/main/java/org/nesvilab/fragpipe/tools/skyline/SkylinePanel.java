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
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;


public class SkylinePanel extends JPanelBase {

  private static final String PREFIX = "skyline.";

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private JPanel panelBasic;
  private JPanel panelQuant;
  private UiRadio uiRadioSkyline;
  private UiRadio uiRadioSkylineDaily;
  private UiRadio uiRadioSkylineCustom;
  private UiText uiTextSkylineCustom;
  private UiCheck uiCheckUseSsl;
  private UiCombo uiComboModsMode;
  private UiSpinnerInt uiSpinnerPrecursorTolerance;
  private UiSpinnerInt uiSpinnerFragmentTolerance;
  private UiCheck uiCheckRunSkylineQuant;
  private UiCheck uiCheckSkipSkylineDocumentGeneration;
  private JPanel panelSiteReport;
  private UiText uiTextModTag;
  private UiSpinnerDouble uiSpinnerSiteProb;
  private UiSpinnerDouble uiSpinnerQValue;

  @Override
  protected void initMore() {
    super.initMore();
    SwingUtils.setEnablementUpdater(this, pContent, checkRun);
    SwingUtils.setEnablementUpdater(this, panelSiteReport, checkRun);
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

    checkRun = new UiCheck("Generate Skyline document", null, false);
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
    panelQuant = createPanelQuant();

    mu.add(p, panelBasic).growX().wrap();
    mu.add(p, panelQuant).growX().wrap();

    return p;
  }

  private JPanel createPanelBasic() {
    panelBasic = mu.newPanel(mu.lcFillX());
    mu.border(panelBasic, 1);

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

    String peakBoundsTooltip = "IonQuant and DIA-NN determine peak integration boundaries for DDA and DIA quant, respectively.<br>"
        + "If this box is not checked, those bounds will be displayed in the Skyline document.<br>"
        + "Check the box to override those bounds and have Skyline calculate its own peak boundaries<br>"
        + "(note: this does NOT affect the output tables from " + PROGRAM_TITLE + ").<br>"
        + "If IonQuant/DIANN are not run, Skyline bounds will be used regardless of this setting.";
    uiCheckUseSsl = UiUtils.createUiCheck("Let Skyline build the library and determine peak boundaries", false);
    uiCheckUseSsl.setName("use-ssl");
    uiCheckUseSsl.setToolTipText(SwingUtils.makeHtml(peakBoundsTooltip));

    uiComboModsMode = UiUtils.createUiCombo(Arrays.asList("Default", "O-glyco", "N-glyco"));
    uiComboModsMode.setSelectedIndex(0);
    FormEntry feComboModsMode = new FormEntry("skyline-mods-mode", "Special modifications mode", uiComboModsMode, "Special modification support.<br>"
        + "If O-glyco, uses O-Pair glycan database instead of mass offsets list.<br>"
        + "If N-glyco, uses Glycan Composition Assignment glycan database instead of mass offsets list.");

    uiSpinnerPrecursorTolerance = new UiSpinnerInt(10, 1, 1000, 1);
    FormEntry fePrecursorTolerance = new FormEntry("skyline-precursor-tolerance", "Precursor tolerance (ppm)", uiSpinnerPrecursorTolerance, "Precursor tolerance in ppm");

    uiSpinnerFragmentTolerance = new UiSpinnerInt(10, 1, 1000, 1);
    FormEntry feFragmentTolerance = new FormEntry("skyline-fragment-tolerance", "Fragment tolerance (ppm)", uiSpinnerFragmentTolerance, "Fragment tolerance in ppm");

    mu.add(panelBasic, feRadioSkyline.comp);
    mu.add(panelBasic, feRadioSkylineDaily.comp);
    mu.add(panelBasic, feRadioSkylineCustom.comp);
    mu.add(panelBasic, feSkylineCustom.comp).growX().pushX();
    mu.add(panelBasic, jButtonSkylineCustom).wrap();

    mu.add(panelBasic, uiCheckUseSsl).wrap();

    mu.add(panelBasic, feComboModsMode.label(), mu.ccL()).split(2);
    mu.add(panelBasic, feComboModsMode.comp).wrap();

    mu.add(panelBasic, fePrecursorTolerance.label(), mu.ccL()).split(2);
    mu.add(panelBasic, fePrecursorTolerance.comp);

    mu.add(panelBasic, feFragmentTolerance.label(), mu.ccL()).split(2);
    mu.add(panelBasic, feFragmentTolerance.comp).wrap();

    updateEnabledStatus(feSkylineCustom.comp, uiRadioSkylineCustom.isSelected());
    updateEnabledStatus(jButtonSkylineCustom, uiRadioSkylineCustom.isSelected());
    updateEnabledStatus(panelBasic, true);

    adjustRadios();

    return panelBasic;
  }

  private JPanel createPanelQuant() {
    panelQuant = mu.newPanel(mu.lcFillX());
    mu.border(panelQuant, 1);

    uiCheckRunSkylineQuant = UiUtils.createUiCheck("Run Skyline quant", false);
    uiCheckRunSkylineQuant.setName("run-skyline-quant");

    uiCheckSkipSkylineDocumentGeneration = UiUtils.createUiCheck("Skip Skyline document generation", false);
    uiCheckSkipSkylineDocumentGeneration.setName("skip-skyline-document-generation");
    uiCheckSkipSkylineDocumentGeneration.setToolTipText("If you already generated a Skyline document, you can skip the generation of a new one.");

    uiCheckRunSkylineQuant.addItemListener(e -> {
      updateEnabledStatus(uiCheckSkipSkylineDocumentGeneration, uiCheckRunSkylineQuant.isSelected());
    });

    mu.add(panelQuant, uiCheckRunSkylineQuant).wrap();
    mu.add(panelQuant, uiCheckSkipSkylineDocumentGeneration).wrap();
    
    updateEnabledStatus(panelQuant, true);

    return panelQuant;
  }

  private JPanel createPanelSiteReport() {
    panelSiteReport = mu.newPanel(mu.lcFillX());
    mu.border(panelSiteReport, 1);
    mu.border(panelSiteReport, "PTM site report (optional)");

    uiTextModTag = UiUtils.uiTextBuilder().cols(40).create();
    FormEntry feModTag = new FormEntry("mod-tag", "Mod tag", uiTextModTag, "<html>Modification tag for generating modification-specific reports <br/>\n"
        + "STY:79.9663 for phospho<br/>\n"
        + "K:114.0429 for ubiquitin");

    uiSpinnerSiteProb = UiUtils.spinnerDouble(0.75, 0, 1, 0.01).setCols(5).setFormat("#.###").create();
    FormEntry feSiteProb = mu.feb(uiSpinnerSiteProb).name("min-site-prob").label("Min site probability").tooltip("Site localization confidence threshold").create();

    uiSpinnerQValue = UiUtils.spinnerDouble(0.01, 0, 1, 0.01).setCols(5).setFormat("#.###").create();
    FormEntry feQValue = mu.feb(uiSpinnerQValue).name("q-value").label("Q-value").tooltip("Q-value threshold").create();

    mu.add(panelSiteReport, feQValue.label()).split(2);
    mu.add(panelSiteReport, feQValue.comp, mu.ccL());
    mu.add(panelSiteReport, feModTag.label(), mu.ccL()).split(2);
    mu.add(panelSiteReport, feModTag.comp).growX();
    mu.add(panelSiteReport, feSiteProb.label()).split(2);
    mu.add(panelSiteReport, feSiteProb.comp, mu.ccL());

    updateEnabledStatus(panelSiteReport, true);
    return panelSiteReport;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("Skyline"));

    pTop = createPanelTop();
    pContent = createPanelContent();
    panelSiteReport = createPanelSiteReport();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
    this.add(panelSiteReport, BorderLayout.SOUTH);
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

  public boolean isUseSsl() {
    return uiCheckUseSsl.isSelected();
  }

  public int getModsMode() {
    return uiComboModsMode.getSelectedIndex();
  }

  public int getPrecursorTolerance() {
    return uiSpinnerPrecursorTolerance.getActualValue();
  }

  public int getFragmentTolerance() {
    return uiSpinnerFragmentTolerance.getActualValue();
  }

  public boolean isRunSkylineQuant() {
    return SwingUtils.isEnabledAndChecked(uiCheckRunSkylineQuant);
  }

  public boolean isSkipSkylineDocumentGeneration() {
    return SwingUtils.isEnabledAndChecked(uiCheckSkipSkylineDocumentGeneration);
  }

  public String getModTag() {
    return uiTextModTag.getNonGhostText().trim();
  }

  public float getSiteProb() {
    return (float) uiSpinnerSiteProb.getActualValue();
  }

  public float getQValue() {
    return (float) uiSpinnerQValue.getActualValue();
  }
}