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

package com.dmtavt.fragpipe.tools.diann;

import static com.dmtavt.fragpipe.Fragpipe.fe;
import static com.github.chhh.utils.SwingUtils.createClickableHtml;
import static com.github.chhh.utils.SwingUtils.isEnabledAndChecked;

import com.dmtavt.fragpipe.messages.NoteConfigDiann;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.HtmlStyledJEditorPane;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.image.BufferedImage;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
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
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;


public class DiannPanel extends JPanelBase {

  private static final String PREFIX = "diann.";
  public static final String NEW_VERSION = "1.8.2 beta 27";

  private JCheckBox checkRun;
  private JCheckBox checkRunPlex;
  private JPanel pContent;
  private JPanel pTop;
  private UiCombo uiComboQuantificationStrategy;
  private UiCombo uiComboQuantificationStrategy2;
  private JLabel labelQuantificationStrategy;
  private JLabel labelQuantificationStrategy2;
  private UiText uiTextCmdOpts;
  private UiSpinnerDouble uiSpinnerQvalue;
  private UiText uiTextLibrary;
  private JPanel panelBasic;
  private JPanel panelPlex;
  private UiCheck uiCheckUsePredictedSpectra;
  private UiCheck uiCheckUseRunSpecificProteinQvalue;
  private UiCheck uiCheckUnrelatedRuns;
  private UiCheck uiCheckGenerateMsstats;
  private UiText uiTextLight;
  private UiText uiTextMedium;
  private UiText uiTextHeavy;

  @Override
  protected void initMore() {
    super.initMore();
    SwingUtils.setEnablementUpdater(this, pContent, checkRun);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDiann m) {
    if (m.isValid()) {
      updateEnabledStatus(this, true);

      if (m.compareVersion(NEW_VERSION) < 0) {
        uiComboQuantificationStrategy.setVisible(true);
        uiComboQuantificationStrategy2.setVisible(false);
        labelQuantificationStrategy.setVisible(true);
        labelQuantificationStrategy2.setVisible(false);
      } else {
        uiComboQuantificationStrategy.setVisible(false);
        uiComboQuantificationStrategy2.setVisible(true);
        labelQuantificationStrategy.setVisible(false);
        labelQuantificationStrategy2.setVisible(true);
      }
    } else {
      updateEnabledStatus(this, false);
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
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/com/dmtavt/fragpipe/icons/icon-diann-48.png")));
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

    panelBasic = createPanelBasic();
    panelPlex = createPanelPlex();

    mu.add(p, panelBasic).growX().wrap();
    mu.add(p, panelPlex).growX().wrap();

    return p;
  }

  private JPanel createPanelBasic() {
    panelBasic = mu.newPanel(mu.lcFillX());
    mu.border(panelBasic, 1);

    uiSpinnerQvalue = UiUtils.spinnerDouble(0.01, 0.001, 0.05, 0.01).setCols(5).setFormat("#.##").create();
    FormEntry feQvalue = mu.feb(uiSpinnerQvalue).name("q-value").label("FDR").tooltip("Control the global protein group FDR, global precursor FDR, and run-specific precursor FDR.").create();

    uiCheckUseRunSpecificProteinQvalue = UiUtils.createUiCheck("Apply run-specific protein FDR", false);
    FormEntry feUseRunSpecificProteinQvalue = mu.feb(uiCheckUseRunSpecificProteinQvalue).name("run-specific-protein-q-value").label("Apply run-specific protein FDR").tooltip("By default, the output matrices are filtered with 1% global protein group FDR, 1% global precursor FDR, and 1% run-specific precursor FDR.\nApply run specific protein FDR to have a more stringent filtering.").create();

    uiComboQuantificationStrategy = UiUtils.createUiCombo(Arrays.asList("Any LC (high accuracy)", "Any LC (high precision)", "Robust LC (high accuracy)", "Robust LC (high precision)"));
    FormEntry feQuantificationStrategy = new FormEntry("quantification-strategy", "Quantification strategy", uiComboQuantificationStrategy);
    uiComboQuantificationStrategy.setSelectedIndex(3);
    labelQuantificationStrategy = feQuantificationStrategy.label();

    uiComboQuantificationStrategy2 = UiUtils.createUiCombo(Arrays.asList("Legacy (direct)", "QuantUMS (high accuracy)", "QuantUMS (high precision)"));
    FormEntry feQuantificationStrategy2 = new FormEntry("quantification-strategy-2", "Quantification strategy (" + NEW_VERSION + ")+", uiComboQuantificationStrategy2);
    uiComboQuantificationStrategy2.setSelectedIndex(2);
    labelQuantificationStrategy2 = feQuantificationStrategy2.label();

    uiCheckUsePredictedSpectra = UiUtils.createUiCheck("Replace library spectra with predicted", false);
    FormEntry feUsePredictedSpectra = new FormEntry("use-predicted-spectra", "Replace library spectra with predicted", uiCheckUsePredictedSpectra);

    uiCheckGenerateMsstats = UiUtils.createUiCheck("Generate MSstats input", true);
    FormEntry feGenerateMsstats = new FormEntry("generate-msstats", "Generate MSstats input", uiCheckGenerateMsstats, "Convert the DIA-NN output to MSstats format.");

    uiCheckUnrelatedRuns = UiUtils.createUiCheck("Unrelated runs", false);
    FormEntry feUnrelatedRuns = new FormEntry("unrelated-runs", "Unrelated runs", uiCheckUnrelatedRuns, "Different runs will be treated as unrelated, i.e. mass accuracy (when automatic) will be determined separately, as well as the retention time scan window.");

    uiTextLibrary = UiUtils.uiTextBuilder().create();
    FormEntry feLibrary = new FormEntry("library", "Spectral library (optional)", uiTextLibrary, "Alternative spectral library file.\nIf blank, using the library.tsv built from FragPipe.");
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
    FormEntry feCmdOpts = fe(uiTextCmdOpts, "cmd-opts")
        .label("Cmd line opts:")
        .tooltip("These options will be passed on to DIA-NN.\n"
            + "This set will be merged with other options in this tab.\n"
            + "To set --threads, please adjust the Parallelism setting in the Workflow tab.\n"
            + "See output log (e.g. dry-run results) for the complete command.").create();

    mu.add(panelBasic, feQvalue.label(), mu.ccL());
    mu.add(panelBasic, feQvalue.comp).wrap();
    mu.add(panelBasic, feUseRunSpecificProteinQvalue.comp).wrap();
    mu.add(panelBasic, labelQuantificationStrategy, mu.ccL());
    mu.add(panelBasic, feQuantificationStrategy.comp).wrap();
    mu.add(panelBasic, labelQuantificationStrategy2, mu.ccL());
    mu.add(panelBasic, feQuantificationStrategy2.comp).wrap();
    mu.add(panelBasic, feUnrelatedRuns.comp).wrap();
    mu.add(panelBasic, feUsePredictedSpectra.comp).wrap();
    mu.add(panelBasic, feGenerateMsstats.comp).wrap();
    mu.add(panelBasic, feLibrary.label(), mu.ccL());
    mu.add(panelBasic, feLibrary.comp).pushX().growX();
    mu.add(panelBasic, jButtonLibrary).wrap();
    mu.add(panelBasic, feCmdOpts.label(), mu.ccL());
    mu.add(panelBasic, feCmdOpts.comp).growX().pushX().wrap();

    updateEnabledStatus(panelBasic, true);
    return panelBasic;
  }

  private JPanel createPanelPlex() {
    panelPlex = mu.newPanel(mu.lcFillX());
    mu.border(panelPlex, 1);

    checkRunPlex = new UiCheck("plex DIA", null, false);
    checkRunPlex.setName("run-dia-plex");

    uiTextLight = UiUtils.uiTextBuilder().cols(40).create();
    uiTextMedium = UiUtils.uiTextBuilder().cols(40).create();
    uiTextHeavy = UiUtils.uiTextBuilder().cols(40).create();

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

    updateEnabledStatus(uiTextLibrary, !isRunPlex());
    updateEnabledStatus(uiTextLight, isRunPlex());
    updateEnabledStatus(uiTextMedium, isRunPlex());
    updateEnabledStatus(uiTextHeavy, isRunPlex());

    checkRunPlex.addItemListener(e -> {
      updateEnabledStatus(uiTextLibrary, !isRunPlex());
      updateEnabledStatus(uiTextLight, isRunPlex());
      updateEnabledStatus(uiTextMedium, isRunPlex());
      updateEnabledStatus(uiTextHeavy, isRunPlex());
      updateEnabledStatus(uiCheckGenerateMsstats, !isRunPlex());
    });

    mu.add(panelPlex, checkRunPlex).wrap();
    mu.add(panelPlex, feLight.label(), mu.ccL()).split(2);
    mu.add(panelPlex, feLight.comp).growX().wrap();
    mu.add(panelPlex, feMedium.label(), mu.ccL()).split(2);
    mu.add(panelPlex, feMedium.comp).growX().wrap();
    mu.add(panelPlex, feHeavy.label(), mu.ccL()).split(2);
    mu.add(panelPlex, feHeavy.comp).growX().wrap();

    updateEnabledStatus(panelPlex, true);
    return panelPlex;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("DIA Quantification"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
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

  public Set<String> getDiannQuantificationStrategy(boolean isNew) {
    if (isNew) {
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

  public boolean usePredict() {
    return isEnabledAndChecked(uiCheckUsePredictedSpectra);
  }

  public boolean unrelatedRuns() {
    return isEnabledAndChecked(uiCheckUnrelatedRuns);
  }
}