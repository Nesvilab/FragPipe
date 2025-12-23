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

package org.nesvilab.fragpipe.tools.diann;

import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;
import static org.nesvilab.utils.SwingUtils.createClickableHtml;
import static org.nesvilab.utils.SwingUtils.isEnabledAndChecked;

import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.messages.NoteConfigDiann;
import org.nesvilab.fragpipe.messages.NoteConfigTransferLearning;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.HtmlStyledJEditorPane;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;


public class DiannPanel extends JPanelBase {

  private static final String PREFIX = "diann.";

  private JCheckBox checkRun;
  private JCheckBox checkRunPlex;
  private JPanel pContent;
  private JPanel pTop;
  private UiCombo uiComboQuantificationStrategy;
  private UiCombo uiComboQuantificationStrategy2;
  private JLabel labelQuantificationStrategy;
  private JLabel labelQuantificationStrategy2;
  private UiCombo uiComboChannelNormalizationStrategy;
  private JLabel labelChannelNormalizationStrategy;
  private UiText uiTextCmdOpts;
  private UiSpinnerDouble uiSpinnerQvalue;
  private UiText uiTextLibrary;
  private JPanel panelBasic;
  private JPanel panelBasic2;
  private JPanel panelPlex;
  private JPanel panelFragReporter;
  private UiText uiTextModTag;
  private UiSpinnerDouble uiSpinnerSiteProb;
  private UiCheck uiCheckUseRunSpecificProteinQvalue;
  private UiCheck uiCheckUnrelatedRuns;
  private UiCheck uiCheckGenerateMsstats;
  private UiCheck uiCheckMbr;
  private UiCheck uiCheckRedoProteinInference;
  private UiText uiTextLight;
  private UiText uiTextMedium;
  private UiText uiTextHeavy;
  private UiCheck uiCheckGeneLevel;
  private UiCheck uiCheckProteinLevel;
  private UiCheck uiCheckPeptideLevel;
  private UiCheck uiCheckModifiedPeptideLevel;
  private UiCheck uiCheckSiteLevel;

  @Override
  protected void initMore() {
    super.initMore();
    SwingUtils.setEnablementUpdater(this, pContent, checkRun);
    SwingUtils.setEnablementUpdater(this, panelFragReporter, checkRun);
    SwingUtilities.invokeLater(() -> SwingUtilities.invokeLater(() -> {
      NoteConfigTransferLearning m = Bus.getStickyEvent(NoteConfigTransferLearning.class);
      if (m != null && panelFragReporter != null) {
        updateEnabledStatus(panelFragReporter, !m.isRunPrediction());
      }
    }));
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDiann m) {
    if (m.isValid()) {
      updateEnabledStatus(this, true);
      boolean isNewVersion = m.compareVersion("1.9") >= 0;
      uiComboQuantificationStrategy.setVisible(!isNewVersion);
      uiComboQuantificationStrategy2.setVisible(isNewVersion);
      labelQuantificationStrategy.setVisible(!isNewVersion);
      labelQuantificationStrategy2.setVisible(isNewVersion);
      uiComboChannelNormalizationStrategy.setVisible(isNewVersion);
      labelChannelNormalizationStrategy.setVisible(isNewVersion);
      uiCheckGenerateMsstats.setVisible(m.compareVersion("2.0") < 0);
    } else {
      updateEnabledStatus(this, false);
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.POSTING)
  public void on(NoteConfigTransferLearning m) {
    boolean isRunPrediction = m.isRunPrediction();
    if (uiCheckMbr != null && uiCheckRedoProteinInference != null) {
      uiCheckMbr.setSelected(isRunPrediction && (m.peptidesToPredict == 1));
      uiCheckRedoProteinInference.setSelected(isRunPrediction);
    }
    if (panelFragReporter != null) {
      updateEnabledStatus(panelFragReporter, !isRunPrediction);
    }
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

    checkRun = new UiCheck("Quantify with DIA-NN", null, false);
    checkRun.setName("run-dia-nn");

    String message = "The stand-alone DIA-NN program (with full functionality) can be downloaded from the <a href=\"https://github.com/vdemichev/DiaNN/releases\">DIA-NN GitHub repository</a>. <a href=\"https://doi.org/10.1038/s41592-019-0638-x\">Reference</a><br/>";

    HtmlStyledJEditorPane messagePane = createClickableHtml(message);

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/icon-diann-48.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    mu.add(p, checkRun);
    mu.add(p, imageLabel).gapRight("50").wrap();
    mu.add(p, messagePane).pushX();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    JPanel p2 = mu.newPanel("", mu.lcFillXNoInsetsTopBottom());
    p2.setLayout(new BoxLayout(p2, BoxLayout.X_AXIS));
    p2.setBorder(null);

    panelBasic = createPanelBasic();
    panelBasic2 = createPanelBasic2();
    panelPlex = createPanelPlex();

    mu.add(p2, panelBasic).spanX();
    p2.add(Box.createHorizontalStrut(10));
    mu.add(p2, panelPlex).spanX().wrap();

    mu.add(p, p2).growX().wrap();
    mu.add(p, panelBasic2).growX().wrap();

    return p;
  }

  private JPanel createPanelBasic() {
    panelBasic = mu.newPanel(mu.lcFillX());
    mu.border(panelBasic, 1);

    uiSpinnerQvalue = UiUtils.spinnerDouble(0.01, 0.001, 0.05, 0.01).setCols(5).setFormat("#.###").create();
    FormEntry feQvalue = mu.feb(uiSpinnerQvalue).name("q-value").label("FDR").tooltip("Control the global protein group FDR, global precursor FDR, and run-specific precursor FDR.").create();

    uiCheckUseRunSpecificProteinQvalue = UiUtils.createUiCheck("Apply run-specific protein FDR", false);
    FormEntry feUseRunSpecificProteinQvalue = mu.feb(uiCheckUseRunSpecificProteinQvalue).name("run-specific-protein-q-value").label("Apply run-specific protein FDR").tooltip("By default, the output matrices are filtered with 1% global protein group FDR, 1% global precursor FDR, and 1% run-specific precursor FDR.\nApply run specific protein FDR to have a more stringent filtering.").create();

    uiComboQuantificationStrategy = UiUtils.createUiCombo(Arrays.asList("Any LC (high accuracy)", "Any LC (high precision)", "Robust LC (high accuracy)", "Robust LC (high precision)"));
    FormEntry feQuantificationStrategy = new FormEntry("quantification-strategy", "Quantification strategy", uiComboQuantificationStrategy);
    uiComboQuantificationStrategy.setSelectedIndex(3);
    labelQuantificationStrategy = feQuantificationStrategy.label();

    uiComboQuantificationStrategy2 = UiUtils.createUiCombo(Arrays.asList("Legacy (direct)", "QuantUMS (high accuracy)", "QuantUMS (high precision)"));
    FormEntry feQuantificationStrategy2 = new FormEntry("quantification-strategy-2", "Quantification strategy", uiComboQuantificationStrategy2);
    uiComboQuantificationStrategy2.setSelectedIndex(2);
    labelQuantificationStrategy2 = feQuantificationStrategy2.label();

    uiCheckGenerateMsstats = UiUtils.createUiCheck("Generate MSstats input", true);
    FormEntry feGenerateMsstats = new FormEntry("generate-msstats", "Generate MSstats input", uiCheckGenerateMsstats, "Convert the DIA-NN output to MSstats format.");

    uiCheckUnrelatedRuns = UiUtils.createUiCheck("Unrelated runs", false);
    FormEntry feUnrelatedRuns = new FormEntry("unrelated-runs", "Unrelated runs", uiCheckUnrelatedRuns, "Different runs will be treated as unrelated, i.e. mass accuracy (when automatic) will be determined separately, as well as the retention time scan window.");

    uiCheckMbr = UiUtils.createUiCheck("MBR", false);
    FormEntry feMbr = new FormEntry("mbr", "MBR", uiCheckMbr, "Enable DIA-NN's MBR functionality. Only used when the input spectral library is from prediction of whole proteome.");

    uiCheckRedoProteinInference = UiUtils.createUiCheck("Redo protein inference", false);
    FormEntry feRedoProteinInference = new FormEntry("redo-protein-inference", "Redo protein inference", uiCheckRedoProteinInference, "Let DIA-NN redo the protein inference.");

    mu.add(panelBasic, feQvalue.label(), mu.ccL()).split(2);
    mu.add(panelBasic, feQvalue.comp).wrap();
    mu.add(panelBasic, feUseRunSpecificProteinQvalue.comp).wrap();
    mu.add(panelBasic, labelQuantificationStrategy, mu.ccL()).split(2);
    mu.add(panelBasic, feQuantificationStrategy.comp).wrap();
    mu.add(panelBasic, labelQuantificationStrategy2, mu.ccL()).split(2);
    mu.add(panelBasic, feQuantificationStrategy2.comp).wrap();
    mu.add(panelBasic, feUnrelatedRuns.comp).wrap();
    mu.add(panelBasic, feMbr.comp).split(3);
    mu.add(panelBasic, feRedoProteinInference.comp);
    mu.add(panelBasic, feGenerateMsstats.comp).wrap();

    updateEnabledStatus(panelBasic, true);
    return panelBasic;
  }

  private JPanel createPanelBasic2() {
    panelBasic2 = mu.newPanel(mu.lcFillX());
    mu.border(panelBasic2, 1);

    uiTextLibrary = UiUtils.uiTextBuilder().create();
    FormEntry feLibrary = new FormEntry("library", "Spectral library (optional)", uiTextLibrary, "Alternative spectral library file.\nIf blank, using the library.tsv built from " + PROGRAM_TITLE + ".");
    JButton jButtonLibrary = feLibrary.browseButton("Browse", "Select library file", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Library files", "csv", "tsv", "xls", "txt", "speclib", "sptxt", "msp");
      JFileChooser fc = FileChooserUtils.create("Library file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextLibrary.getNonGhostText()));
      return fc;
      }, paths -> {
      Path path = paths.get(0); // we only allowed selection of a single file in the file chooser
      uiTextLibrary.setText(path.toString());
    });

    uiTextCmdOpts = UiUtils.uiTextBuilder().cols(20).text("").create();
    FormEntry feCmdOpts = new FormEntry("cmd-opts", "Cmd line opts", uiTextCmdOpts, "These options will be passed on to DIA-NN.\n"
        + "This set will be merged with other options in this tab.\n"
        + "To set --threads, please adjust the Parallelism setting in the Workflow tab.\n"
        + "See output log (e.g. dry-run results) for the complete command.");

    mu.add(panelBasic2, feLibrary.label(), mu.ccL());
    mu.add(panelBasic2, feLibrary.comp).pushX().growX();
    mu.add(panelBasic2, jButtonLibrary).wrap();
    mu.add(panelBasic2, feCmdOpts.label(), mu.ccL());
    mu.add(panelBasic2, feCmdOpts.comp).growX().pushX().wrap();

    updateEnabledStatus(panelBasic2, true);
    return panelBasic2;
  }

  private JPanel createPanelPlex() {
    panelPlex = mu.newPanel(mu.lcFillX());
    mu.border(panelPlex, 1);

    checkRunPlex = new UiCheck("plex DIA", null, false);
    checkRunPlex.setName("run-dia-plex");

    uiTextLight = UiUtils.uiTextBuilder().cols(40).create();
    uiTextMedium = UiUtils.uiTextBuilder().cols(40).create();
    uiTextHeavy = UiUtils.uiTextBuilder().cols(40).create();

    uiComboChannelNormalizationStrategy = UiUtils.createUiCombo(new String[]{"Run specific", "Channel specific"});
    uiComboChannelNormalizationStrategy.setSelectedIndex(0);

    FormEntry feLight = new FormEntry("light", "Light    ", uiTextLight,
        "String description of mass deltas. <b>A-Z</b> for amino acids and <b>n</b> for N-terminus.<br>"
            + "E.g. (1) for SILAC: <b>K0;R0</b> (2) for dimethyl labeling: <b>Kn28.0313</b><br>"
            + "If the amino acid has both fixed and variable modifications, should sum up the <b>both</b> masses.");
    FormEntry feMedium = new FormEntry("medium", "Medium", uiTextMedium,
        "String description of mass deltas. <b>A-Z</b> for amino acids and <b>n</b> for N-terminus.<br>"
            + "E.g. for SILAC: <b>K4.025107;R6.020129</b><br>"
            + "If the amino acid has both fixed and variable modifications, should sum up the <b>both</b> masses.");
    FormEntry feHeavy = new FormEntry("heavy", "Heavy  ", uiTextHeavy,
        "String description of mass deltas. <b>A-Z</b> for amino acids and <b>n</b> for N-terminus.<br>"
            + "E.g. (1) for SILAC: <b>K8.014199;R10.008269</b> (2) for dimethyl labeling: <b>Kn36.075670</b><br>"
            + "If the amino acid has both fixed and variable modifications, should sum up the <b>both</b> masses.");

    FormEntry feChannelNormalizationStrategy = new FormEntry("channel-normalization-strategy", "Channel normalization strategy", uiComboChannelNormalizationStrategy,
        "Run specific: Normalization of multiplexed samples will be performed in run-specific manner.<br>"
            + "To perform normalization, for each precursor ion DIA-NN will sum the respective channels within each run and normalize these sums across runs.<br>"
            + "Used, for example, for protein turnover SILAC experiments.<br>"
            + "Channel specific: Normalization of multiplexed samples will be performed in channel-specific manner.<br>"
            + "Each channel in each run is treated as a separate sample to be normalised.<br>"
            + "Used, for example, to analyse experiments wherein multiplexing of independent samples is used to boost throughput.");
    labelChannelNormalizationStrategy = feChannelNormalizationStrategy.label();

    updateEnabledStatus(uiTextLibrary, !isRunPlex());
    updateEnabledStatus(uiTextLight, isRunPlex());
    updateEnabledStatus(uiTextMedium, isRunPlex());
    updateEnabledStatus(uiTextHeavy, isRunPlex());
    updateEnabledStatus(uiComboChannelNormalizationStrategy, isRunPlex());
    updateEnabledStatus(labelChannelNormalizationStrategy, isRunPlex());
    updateEnabledStatus(uiCheckGenerateMsstats, !isRunPlex());

    checkRunPlex.addItemListener(e -> {
      updateEnabledStatus(uiTextLibrary, !isRunPlex());
      updateEnabledStatus(uiTextLight, isRunPlex());
      updateEnabledStatus(uiTextMedium, isRunPlex());
      updateEnabledStatus(uiTextHeavy, isRunPlex());
      updateEnabledStatus(uiComboChannelNormalizationStrategy, isRunPlex());
      updateEnabledStatus(labelChannelNormalizationStrategy, isRunPlex());
      updateEnabledStatus(uiCheckGenerateMsstats, !isRunPlex());
    });

    mu.add(panelPlex, checkRunPlex).wrap();
    mu.add(panelPlex, feLight.label(), mu.ccL()).split(2);
    mu.add(panelPlex, feLight.comp).growX().wrap();
    mu.add(panelPlex, feMedium.label(), mu.ccL()).split(2);
    mu.add(panelPlex, feMedium.comp).growX().wrap();
    mu.add(panelPlex, feHeavy.label(), mu.ccL()).split(2);
    mu.add(panelPlex, feHeavy.comp).growX().wrap();
    mu.add(panelPlex, labelChannelNormalizationStrategy, mu.ccL()).split(2);
    mu.add(panelPlex, feChannelNormalizationStrategy.comp).wrap();

    updateEnabledStatus(panelPlex, true);
    return panelPlex;
  }

  private JPanel createPanelFragReporter() {
    panelFragReporter = mu.newPanel(mu.lcFillX());
    mu.border(panelFragReporter, 1);
    mu.border(panelFragReporter, "Reports");

    JLabel noteLabel = new JLabel("Run FragReporter to generate additional reports:");

    uiTextModTag = UiUtils.uiTextBuilder().cols(40).create();
    FormEntry feModTag = new FormEntry("mod-tag", "Mod tag", uiTextModTag, "<html>Modification tag for generating modification-specific reports <br/>\n"
        + "STY:79.9663 for phospho<br/>\n"
        + "K:114.0429 for ubiquitin");

    uiSpinnerSiteProb = UiUtils.spinnerDouble(0.75, 0, 1, 0.01).setCols(5).setFormat("#.###").create();
    FormEntry feSiteProb = mu.feb(uiSpinnerSiteProb).name("min-site-prob").label("Min site probability").tooltip("Site localization confidence threshold").create();

    uiCheckGeneLevel = UiUtils.createUiCheck("Gene", false);
    FormEntry feGeneLevel = new FormEntry("gene-level-report", "Gene", uiCheckGeneLevel, "Generate gene-level report");

    uiCheckProteinLevel = UiUtils.createUiCheck("Protein", false);
    FormEntry feProteinLevel = new FormEntry("protein-level-report", "Protein", uiCheckProteinLevel, "Generate protein-level report");

    uiCheckPeptideLevel = UiUtils.createUiCheck("Peptide", true);
    FormEntry fePeptideLevel = new FormEntry("peptide-level-report", "Peptide", uiCheckPeptideLevel, "Generate peptide-level report");

    uiCheckModifiedPeptideLevel = UiUtils.createUiCheck("Modified peptide", true);
    FormEntry feModifiedPeptideLevel = new FormEntry("modified-peptide-level-report", "Modified peptide", uiCheckModifiedPeptideLevel, "Generate modified peptide-level report");

    uiCheckSiteLevel = UiUtils.createUiCheck("Site", false);
    uiCheckSiteLevel.setEnabled(false);
    FormEntry feSiteLevel = new FormEntry("site-level-report", "Site", uiCheckSiteLevel, "Generate site-level report (multi-site and single-site)");

    mu.add(panelFragReporter, noteLabel).wrap();
    mu.add(panelFragReporter, feGeneLevel.comp).split(5);
    mu.add(panelFragReporter, feProteinLevel.comp);
    mu.add(panelFragReporter, fePeptideLevel.comp);
    mu.add(panelFragReporter, feModifiedPeptideLevel.comp);
    mu.add(panelFragReporter, feSiteLevel.comp).wrap();
    mu.add(panelFragReporter, feModTag.label(), mu.ccL()).split(2);
    mu.add(panelFragReporter, feModTag.comp).growX();
    mu.add(panelFragReporter, feSiteProb.label()).split(2);
    mu.add(panelFragReporter, feSiteProb.comp, mu.ccL()).wrap();

    updateEnabledStatus(panelFragReporter, true);
    return panelFragReporter;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("DIA Quantification"));

    pTop = createPanelTop();
    pContent = createPanelContent();
    panelFragReporter = createPanelFragReporter();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
    this.add(panelFragReporter, BorderLayout.SOUTH);
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isRunPlex() {
    return SwingUtils.isEnabledAndChecked(checkRunPlex);
  }

  public String getLight() {
    return uiTextLight.getNonGhostText().trim();
  }

  public String getMedium() {
    return uiTextMedium.getNonGhostText().trim();
  }

  public String getHeavy() {
    return uiTextHeavy.getNonGhostText().trim();
  }

  public float getDiannQvalue() {
    return (float) uiSpinnerQvalue.getActualValue();
  }

  public boolean useRunSpecificProteinQvalue() {
    return uiCheckUseRunSpecificProteinQvalue.isSelected();
  }

  public boolean generateMsstats() {
    return SwingUtils.isEnabledAndChecked(uiCheckGenerateMsstats);
  }

  public String getLibraryPath() {
    return uiTextLibrary.getNonGhostText().trim();
  }

  public String getCmdOpts() {
    return uiTextCmdOpts.getNonGhostText().trim();
  }

  public Set<String> getDiannQuantificationStrategy(NoteConfigDiann noteConfigDiann) {
    if (noteConfigDiann.compareVersion("1.9") >= 0) {
      switch (uiComboQuantificationStrategy2.getSelectedIndex()) {
        case 0:
          return Stream.of("--direct-quant").collect(Collectors.toSet());
        case 1:
          return Stream.of("--high-acc").collect(Collectors.toSet());
        default:
          return new HashSet<>();
      }
    } else {
      switch (uiComboQuantificationStrategy.getSelectedIndex()) {
        case 1:
          return Stream.of("--no-ifs-removal").collect(Collectors.toSet());
        case 2:
          return Stream.of("--peak-center").collect(Collectors.toSet());
        case 3:
          return Stream.of("--peak-center", "--no-ifs-removal").collect(Collectors.toSet());
        default:
          return new HashSet<>();
      }
    }
  }

  public String getDiannChannelNormalizationStrategy() {
    switch (uiComboChannelNormalizationStrategy.getSelectedIndex()) {
      case 0:
        return "--channel-run-norm";
      case 1:
        return "--channel-spec-norm";
      default:
        return "";
    }
  }

  public boolean unrelatedRuns() {
    return isEnabledAndChecked(uiCheckUnrelatedRuns);
  }

  public boolean useMbr() {
    return isEnabledAndChecked(uiCheckMbr);
  }

  public boolean redoProteinInference() {
    return isEnabledAndChecked(uiCheckRedoProteinInference);
  }

  public String getModTag() {
    return uiTextModTag.getNonGhostText().trim();
  }

  public float getSiteProb() {
    return (float) uiSpinnerSiteProb.getActualValue();
  }

  public boolean isGeneLevelReport() {
    return uiCheckGeneLevel.isSelected();
  }

  public boolean isProteinLevelReport() {
    return uiCheckProteinLevel.isSelected();
  }

  public boolean isPeptideLevelReport() {
    return uiCheckPeptideLevel.isSelected();
  }

  public boolean isModifiedPeptideLevelReport() {
    return uiCheckModifiedPeptideLevel.isSelected();
  }

  public boolean isSiteLevelReport() {
    return uiCheckSiteLevel.isSelected();
  }

  public String getReportLevels() {
    List<String> levels = new ArrayList<>();
    if (uiCheckGeneLevel.isSelected()) {
      levels.add("gene");
    }
    if (uiCheckProteinLevel.isSelected()) {
      levels.add("protein");
    }
    if (uiCheckPeptideLevel.isSelected()) {
      levels.add("peptide");
    }
    if (uiCheckModifiedPeptideLevel.isSelected()) {
      levels.add("modified-peptide");
    }
    if (uiCheckSiteLevel.isSelected() && !getModTag().isEmpty()) {
      levels.add("multi-site");
      levels.add("single-site");
    }
    return levels.isEmpty() ? "all" : String.join(";", levels);
  }
}