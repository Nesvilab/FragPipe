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

package org.nesvilab.fragpipe.tools.denovo;

import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.*;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.util.stream.Stream;


public class DeNovoPanel extends JPanelBase {

  private static final String PREFIX = "fragnovo.";

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private JPanel panelPrediction;
  private JPanel panelLoraPrediction;
  private UiText uiTextCredential;
  private UiCheck checkRunFineTuning;
  private UiCheck checkRunPrediction;
  private UiCheck checkRunLoraPrediction;
  private UiSpinnerInt uiSpinnerPrecursorMassTol;
  private UiSpinnerInt uiSpinnerIsotopeErrorMin;
  private UiSpinnerInt uiSpinnerIsotopeErrorMax;
  private UiCheck uiCheckUseIrt;
  private UiText uiTextNewTokens;
  private UiText uiTextCalFilePath;
  private FormEntry feCalFile;
  private JButton jButtonCalFile;
  private UiSpinnerInt uiSpinnerTimeout;
  private UiSpinnerDouble uiSpinnerScoreThreshold;
  private UiText uiTextLoraWeightsPath;
  private UiCombo uiComboModel;

  private static final String[] MODEL_OPTIONS = {
      "DDA_MassIVE",
      "ddaPASEF",
      "DIA_Orbitrap",
      "diaPASEF",
      "nonspecific"
  };

  @Override
  protected void initMore() {
    super.initMore();
    SwingUtils.setEnablementUpdater(this, panelPrediction, checkRunPrediction);
    SwingUtils.setEnablementUpdater(this, panelLoraPrediction, checkRunLoraPrediction);

    DocumentListener textFieldListener = new DocumentListener() {
      @Override
      public void insertUpdate(DocumentEvent e) {
        updateContentPanelEnablement();
      }

      @Override
      public void removeUpdate(DocumentEvent e) {
        updateContentPanelEnablement();
      }

      @Override
      public void changedUpdate(DocumentEvent e) {
        updateContentPanelEnablement();
      }
    };

    uiTextCredential.getDocument().addDocumentListener(textFieldListener);

    checkRun.addItemListener(e -> updateContentPanelEnablement());

    uiCheckUseIrt.addItemListener(e -> updateCalFileEnablement());

    updateContentPanelEnablement();
    updateCalFileEnablement();
  }

  private void updateContentPanelEnablement() {
    boolean checkRunSelected = checkRun.isSelected();

    if (pContent != null) {
      updateEnabledStatus(pContent, checkRunSelected);
    }
  }

  private void updateCalFileEnablement() {
    boolean enabled = uiCheckUseIrt.isSelected();
    updateEnabledStatus(feCalFile.label(), enabled);
    updateEnabledStatus(feCalFile.comp, enabled);
    updateEnabledStatus(jButtonCalFile, enabled);
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
    JPanel p = new JPanel(new MigLayout(new LC().insets("0", "0", "25", "0")));
    mu.borderEmpty(p);

    uiTextCredential = new UiText("", "");
    uiTextCredential.setColumns(20);
    FormEntry feCredential = mu.feb("credential", uiTextCredential)
        .label("Credential file: ")
        .tooltip("FragNovo requires a license key file to run.")
        .create();

    JButton jButtonCredential = feCredential.browseButton("Browse", "Select credential file", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Key files", "key");
      JFileChooser fc = FileChooserUtils.create("Credential file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextCredential.getNonGhostText()));
      return fc;
    }, paths -> {
      Path path = paths.get(0);
      uiTextCredential.setText(path.toString());
    });

    checkRun = new UiCheck("Run FragNovo de novo sequencing", null, false);
    checkRun.setName("run-fragnovo");

    JLabel availabilityLabel = new JLabel("<html><b>Note: Currently available to selected collaborators only.</b></html>");
    HtmlStyledJEditorPane documentationNote = SwingUtils.createClickableHtml(
        "<b>Please read the <a href=\"https://fragpipe.nesvilab.org/docs/tutorial_denovo.html\">documentation</a> carefully before proceeding.</b>");

    mu.add(p, checkRun).pushX().wrap();
    mu.add(p, feCredential.label()).split(3);
    mu.add(p, feCredential.comp).growX().pushX();
    mu.add(p, jButtonCredential).wrap();

    mu.add(p, availabilityLabel).wrap();
    mu.add(p, documentationNote).growX().wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    uiSpinnerPrecursorMassTol = new UiSpinnerInt(50, 1, 500, 1);
    FormEntry fePrecursorMassTol = mu.feb("precursor-mass-tol", uiSpinnerPrecursorMassTol)
        .label("Precursor mass tolerance (ppm): ")
        .tooltip("Precursor mass tolerance in ppm for de novo sequencing.")
        .create();

    uiSpinnerIsotopeErrorMin = new UiSpinnerInt(0, -5, 5, 1);
    FormEntry feIsotopeErrorMin = mu.feb("isotope-error-min", uiSpinnerIsotopeErrorMin)
        .label("Isotope error range: ")
        .tooltip("Minimum isotope error")
        .create();

    uiSpinnerIsotopeErrorMax = new UiSpinnerInt(1, -5, 5, 1);
    FormEntry feIsotopeErrorMax = mu.feb("isotope-error-max", uiSpinnerIsotopeErrorMax)
        .label(" to ")
        .tooltip("Maximum isotope error")
        .create();

    uiTextNewTokens = new UiText("", "S[79.9663]:166.998358; T[79.9663]:181.014000; Y[79.9663]:243.029659");
    uiTextNewTokens.setColumns(30);
    FormEntry feNewTokens = mu.feb("new-tokens", uiTextNewTokens)
        .label("Additional tokens: ")
        .tooltip("<html>Amino acid or modification tokens to add to the model vocabulary (optional).<br>"
            + "Only specify tokens that are <b>NOT</b> already in the base model.<br><br>"
            + "<b>Format:</b> token:mass, separated by semicolons.<br>"
            + "The mass is the <b>residue mass</b> (amino acid mass + modification mass shift).<br><br>"
            + "<b>Examples (phosphorylation, +79.96633 Da):</b><br>"
            + "&nbsp;&nbsp;<code>S[79.9663]:166.998358</code> (phospho-Ser: 87.032028 + 79.96633)<br>"
            + "&nbsp;&nbsp;<code>T[79.9663]:181.014000</code> (phospho-Thr: 101.04767 + 79.96633)<br>"
            + "&nbsp;&nbsp;<code>Y[79.9663]:243.029659</code> (phospho-Tyr: 163.063329 + 79.96633)<br><br>"
            + "<b>Other examples:</b><br>"
            + "&nbsp;&nbsp;<code>C:103.009185</code> (unmodified cysteine)<br>"
            + "&nbsp;&nbsp;<code>n[42.0106]:42.010565</code> (N-terminal acetylation)<br><br>"
            + "These extend the decoder vocabulary for LoRA fine-tuning and prediction.</html>")
        .create();

    uiSpinnerTimeout = new UiSpinnerInt(3600, 60, 86400, 60);
    FormEntry feTimeout = mu.feb("timeout", uiSpinnerTimeout)
        .label("Timeout (seconds): ")
        .tooltip("<html>Maximum time to wait for each server job to complete.<br>"
            + "Increase for large datasets or slow connections.</html>")
        .create();

    uiSpinnerScoreThreshold = new UiSpinnerDouble(0.8, 0.0, 1.1, 0.1, 1, new DecimalFormat("0.0"));
    uiSpinnerScoreThreshold.setColumns(3);
    FormEntry feScoreThreshold = mu.feb("score-threshold", uiSpinnerScoreThreshold)
        .label("Score threshold for FASTA file generation: ")
        .tooltip("<html>Minimum de novo score for peptides to be included in the generated FASTA file.<br>"
            + "Peptides with scores below this threshold are filtered out.</html>")
        .create();

    uiCheckUseIrt = new UiCheck("Use iRT", null, true);
    uiCheckUseIrt.setName("use-irt");

    uiTextCalFilePath = new UiText("", "");
    uiTextCalFilePath.setColumns(20);
    feCalFile = mu.feb("cal-file-path", uiTextCalFilePath)
        .label("RT calibration file (optional): ")
        .tooltip("<html>RT calibration file used for prediction, fine-tuning, and LoRA prediction.<br>"
            + "If not specified, FragPipe will automatically use the calibration file<br>"
            + "generated during the workflow analysis (<code>_RTcalibration.txt</code>).</html>")
        .create();

    jButtonCalFile = feCalFile.browseButton("Browse", "Select RT calibration file", () -> {
      final FileNameExtensionFilter filter = new FileNameExtensionFilter("Text files", "txt");
      JFileChooser fc = FileChooserUtils.create("RT calibration file", "Select", false, FcMode.FILES_ONLY, true, filter);
      fc.setFileFilter(filter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextCalFilePath.getNonGhostText()));
      return fc;
    }, paths -> uiTextCalFilePath.setText(paths.get(0).toString()));

    checkRunPrediction = new UiCheck("Perform de novo prediction", null, true);
    checkRunPrediction.setName("perform-prediction");
    panelPrediction = createPanelPrediction();

    JPanel panelLora = createPanelLora();

    mu.add(p, fePrecursorMassTol.label()).split(10);
    mu.add(p, fePrecursorMassTol.comp);
    mu.add(p, feIsotopeErrorMin.label()).gapLeft("30");
    mu.add(p, feIsotopeErrorMin.comp);
    mu.add(p, feIsotopeErrorMax.label());
    mu.add(p, feIsotopeErrorMax.comp);
    mu.add(p, feTimeout.label()).gapLeft("30");
    mu.add(p, feTimeout.comp);
    mu.add(p, feScoreThreshold.label()).gapLeft("30");
    mu.add(p, feScoreThreshold.comp).wrap();

    mu.add(p, feNewTokens.label()).split(2);
    mu.add(p, feNewTokens.comp).growX().wrap();

    mu.add(p, uiCheckUseIrt).wrap();

    mu.add(p, feCalFile.label()).split(3);
    mu.add(p, feCalFile.comp).growX().pushX();
    mu.add(p, jButtonCalFile).wrap();

    mu.add(p, checkRunPrediction).gapTop("10").wrap();
    mu.add(p, panelPrediction).growX().wrap();

    mu.add(p, panelLora).growX().gapTop("10").wrap();

    return p;
  }

  private JPanel createPanelPrediction() {
    panelPrediction = mu.newPanel(mu.lcFillX());
    mu.border(panelPrediction, 1);

    uiComboModel = UiUtils.createUiCombo(MODEL_OPTIONS);
    FormEntry feModel = mu.feb("model", uiComboModel)
        .label("Model: ")
        .tooltip("Select the base model checkpoint for prediction.")
        .create();

    mu.add(panelPrediction, feModel.label()).split(2);
    mu.add(panelPrediction, feModel.comp).wrap();

    return panelPrediction;
  }

  private JPanel createPanelLora() {
    JPanel panel = new JPanel(new MigLayout(new LC().fillX()));
    panel.setBorder(new TitledBorder("LoRA fine-tuning & prediction"));

    checkRunFineTuning = new UiCheck("Perform LoRa fine-tuning", null, false);
    checkRunFineTuning.setName("perform-fine-tuning");

    checkRunLoraPrediction = new UiCheck("Perform LoRA prediction", null, false);
    checkRunLoraPrediction.setName("perform-lora-prediction");
    panelLoraPrediction = createPanelLoraPrediction();

    mu.add(panel, checkRunFineTuning).wrap();

    mu.add(panel, checkRunLoraPrediction).gapTop("10").wrap();
    mu.add(panel, panelLoraPrediction).growX().wrap();

    return panel;
  }

  private JPanel createPanelLoraPrediction() {
    panelLoraPrediction = mu.newPanel(mu.lcFillX());
    mu.border(panelLoraPrediction, 1);

    uiTextLoraWeightsPath = new UiText("", "");
    uiTextLoraWeightsPath.setColumns(20);
    FormEntry feLoraWeights = mu.feb("lora-weights-path", uiTextLoraWeightsPath)
        .label("LoRA weights (optional): ")
        .tooltip("<html>Pre-trained LoRA weights for LoRA prediction.<br>"
            + "If not specified and fine-tuning is enabled, FragPipe will automatically<br>"
            + "use the LoRA weights generated during the fine-tuning step.</html>")
        .create();

    JButton jButtonLoraWeights = feLoraWeights.browseButton("Browse", "Select LoRA weights file", () -> {
      final FileNameExtensionFilter filter = new FileNameExtensionFilter("PyTorch files", "pt");
      JFileChooser fc = FileChooserUtils.create("LoRA weights file", "Select", false, FcMode.FILES_ONLY, true, filter);
      fc.setFileFilter(filter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextLoraWeightsPath.getNonGhostText()));
      return fc;
    }, paths -> uiTextLoraWeightsPath.setText(paths.get(0).toString()));

    mu.add(panelLoraPrediction, feLoraWeights.label()).split(3);
    mu.add(panelLoraPrediction, feLoraWeights.comp).growX().pushX();
    mu.add(panelLoraPrediction, jButtonLoraWeights).wrap();

    JLabel noteLabel = new JLabel("<html><i>If fine-tuning is enabled and no LoRA weights are specified, "
        + "weights from the fine-tuning step are used automatically.</i></html>");
    mu.add(panelLoraPrediction, noteLabel).growX().wrap();

    return panelLoraPrediction;
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("De Novo Sequencing"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isRunFineTuning() {
    return SwingUtils.isEnabledAndChecked(checkRunFineTuning);
  }

  public boolean isRunPrediction() {
    return SwingUtils.isEnabledAndChecked(checkRunPrediction);
  }

  public boolean isRunLoraPrediction() {
    return SwingUtils.isEnabledAndChecked(checkRunLoraPrediction);
  }

  public String getCredentialPath() {
    return uiTextCredential.getNonGhostText();
  }

  public int getPrecursorMassTol() {
    return uiSpinnerPrecursorMassTol.getActualValue();
  }

  public int getIsotopeErrorMin() {
    return uiSpinnerIsotopeErrorMin.getActualValue();
  }

  public int getIsotopeErrorMax() {
    return uiSpinnerIsotopeErrorMax.getActualValue();
  }

  public boolean isUseIrt() {
    return uiCheckUseIrt.isSelected();
  }

  public String getNewTokens() {
    return uiTextNewTokens.getNonGhostText();
  }

  public String getLoraWeightsPath() {
    return uiTextLoraWeightsPath.getNonGhostText();
  }

  public String getCalFilePath() {
    return uiTextCalFilePath.getNonGhostText();
  }

  public String getModelName() {
    return uiComboModel.getSelectedItem().toString();
  }

  public int getTimeout() {
    return uiSpinnerTimeout.getActualValue();
  }

  public float getScoreThreshold() {
    return (float) uiSpinnerScoreThreshold.getActualValue();
  }
}
