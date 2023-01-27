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
import static com.dmtavt.fragpipe.cmd.CmdDiann.DIANN_VERSION;
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

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private UiCombo uiComboQuantificationStrategy;
  private UiText uiTextCmdOpts;
  private UiSpinnerDouble uiSpinnerQvalue;
  private UiText uiTextLibrary;
  private JPanel panelDiann;
  private UiCheck uiCheckUsePredictedSpectra;
  private UiCheck uiCheckUseRunSpecificProteinQvalue;
  private UiCheck uiCheckUnrelatedRuns;

  @Override
  protected void initMore() {
    super.initMore();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigDiann m) {
    if (m.isValid()) {
      updateEnabledStatus(this, true);
      checkRun.setEnabled(true);
      if (m.isChecked()) {
        checkRun.setSelected(true);
      }
      updateEnabledStatus(pContent, isRun());
    } else {
      updateEnabledStatus(this, false);
      checkRun.setSelected(false);
      updateEnabledStatus(pContent, isRun());
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

    checkRun = new UiCheck("Quantify with DIA-NN (version " + DIANN_VERSION + ")", null, false);
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
    panelDiann = mu.newPanel(mu.lcFillX());
    mu.border(panelDiann, 1);

    uiSpinnerQvalue = UiUtils.spinnerDouble(0.01, 0.001, 0.05, 0.01).setCols(5).setFormat("#.##").create();
    FormEntry feQvalue = mu.feb(uiSpinnerQvalue).name("q-value").label("FDR").tooltip("Control the global protein group FDR, global precursor FDR, and run-specific precursor FDR.").create();

    uiCheckUseRunSpecificProteinQvalue = UiUtils.createUiCheck("Apply run-specific protein FDR", false);
    FormEntry feUseRunSpecificProteinQvalue = mu.feb(uiCheckUseRunSpecificProteinQvalue).name("run-specific-protein-q-value").label("Apply run-specific protein FDR").tooltip("By default, the output matrices are filtered with 1% global protein group FDR, 1% global precursor FDR, and 1% run-specific precursor FDR.\nApply run specific protein FDR to have a more stringent filtering.").create();

    uiComboQuantificationStrategy = UiUtils.createUiCombo(Arrays.asList("Any LC (high accuracy)", "Any LC (high precision)", "Robust LC (high accuracy)", "Robust LC (high precision)"));
    FormEntry feQuantificationStrategy = new FormEntry("quantification-strategy", "Quantification strategy", uiComboQuantificationStrategy);
    uiComboQuantificationStrategy.setSelectedIndex(3);

    uiCheckUsePredictedSpectra = UiUtils.createUiCheck("Replace library spectra with predicted", true);
    FormEntry feUsePredictedSpectra = new FormEntry("use-predicted-spectra", "Replace library spectra with predicted", uiCheckUsePredictedSpectra);

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
            + "This set will be merged with precursor FDR and quantification strategy.\n"
            + "To set --threads, please adjust the Parallelism setting in the Workflow tab.\n"
            + "See output log (e.g. dry-run results) for the complete command.").create();

    mu.add(panelDiann, feQvalue.label(), mu.ccL());
    mu.add(panelDiann, feQvalue.comp).wrap();
    mu.add(panelDiann, feUseRunSpecificProteinQvalue.comp).wrap();
    mu.add(panelDiann, feQuantificationStrategy.label(), mu.ccL());
    mu.add(panelDiann, feQuantificationStrategy.comp).wrap();
    mu.add(panelDiann, feUnrelatedRuns.comp).wrap();
    mu.add(panelDiann, feUsePredictedSpectra.comp).wrap();
    mu.add(panelDiann, feLibrary.label(), mu.ccL());
    mu.add(panelDiann, feLibrary.comp).pushX().growX();
    mu.add(panelDiann, jButtonLibrary).wrap();
    mu.add(panelDiann, feCmdOpts.label(), mu.ccL());
    mu.add(panelDiann, feCmdOpts.comp).growX().pushX().wrap();

    updateEnabledStatus(panelDiann, true);
    return panelDiann;
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

  public float getDiannQvalue() {
    return (float) uiSpinnerQvalue.getActualValue();
  }

  public boolean useRunSpecificProteinQvalue() {
    return uiCheckUseRunSpecificProteinQvalue.isSelected();
  }

  public String getLibraryPath() {
    return uiTextLibrary.getNonGhostText().trim();
  }

  public String getCmdOpts() {
    return uiTextCmdOpts.getNonGhostText().trim();
  }

  public Set<String> getDiannQuantificationStrategy() {
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

  public boolean usePredict() {
    return isEnabledAndChecked(uiCheckUsePredictedSpectra);
  }

  public boolean unrelatedRuns() {
    return isEnabledAndChecked(uiCheckUnrelatedRuns);
  }
}