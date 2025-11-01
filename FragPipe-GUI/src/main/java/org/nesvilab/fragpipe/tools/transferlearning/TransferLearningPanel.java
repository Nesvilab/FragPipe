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

package org.nesvilab.fragpipe.tools.transferlearning;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiSpinnerInt;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;

import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;


public class TransferLearningPanel extends JPanelBase {

  private static final String PREFIX = "transfer-learning.";
  private static Map<String, String> instrumentMap = new TreeMap<>();

  private JCheckBox checkRun;
  private JPanel pContent;
  private JPanel pTop;
  private JPanel panelTraining;
  private JPanel panelPrediction;
  private UiText uiTextURL;
  private UiText uiTextAPIKey;
  private UiCheck checkRunTraining;
  private UiText uiTextLibrary;
  private UiText uiTextModelPath;
  private UiCheck checkRunPrediction;
  private UiCheck uiCheckPredictMS2;
  private UiCheck uiCheckPredictRT;
  private UiCheck uiCheckPredictIM;
  private UiCombo uiComboPeptidesToPredict;
  private UiText uiTextCustomPeptideList;
  private UiSpinnerInt uiSpinnerMinCharge;
  private UiSpinnerInt uiSpinnerMaxCharge;
  private UiCombo uiComboInstrument;
  private UiSpinnerInt uiSpinnerNce;
  private UiCombo uiComboOutputFormat;

  static {
    instrumentMap.put("QExactive", "QE");
    instrumentMap.put("Lumos", "LUMOS");
    instrumentMap.put("timsTOF", "TIMSTOF");
    instrumentMap.put("SCIEX TOF", "SCIEXTOF");
    instrumentMap.put("Astral", "THERMOTOF");
  }

  @Override
  protected void initMore() {
    super.initMore();
    SwingUtils.setEnablementUpdater(this, panelTraining, checkRunTraining);
    SwingUtils.setEnablementUpdater(this, panelPrediction, checkRunPrediction);
    
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
    
    uiTextURL.getDocument().addDocumentListener(textFieldListener);
    uiTextAPIKey.getDocument().addDocumentListener(textFieldListener);
    
    checkRun.addItemListener(e -> updateContentPanelEnablement());
    
    updateContentPanelEnablement();
  }
  
  private void updateContentPanelEnablement() {
    boolean urlNotBlank = !uiTextURL.getNonGhostText().trim().isEmpty();
    boolean apiKeyNotBlank = !uiTextAPIKey.getNonGhostText().trim().isEmpty();
    boolean checkRunSelected = checkRun.isSelected();
    boolean shouldEnable = checkRunSelected && urlNotBlank && apiKeyNotBlank;
    
    if (pContent != null) {
      updateEnabledStatus(pContent, shouldEnable);
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
    JPanel p = new JPanel(new MigLayout(new LC().insets("0", "0", "25", "0")));
    mu.borderEmpty(p);

    uiTextURL = new UiText("", "");
    uiTextURL.setColumns(20);
    FormEntry feURL = mu.feb("url", uiTextURL)
        .label("Transfer learning server URL: ")
        .tooltip("The public one is XXX")
        .create();

    uiTextAPIKey = new UiText("", "");
    uiTextAPIKey.setColumns(20);
    FormEntry feAPIKey = mu.feb("api-key", uiTextAPIKey)
        .label("Transfer learning API key: ")
        .create();

    checkRun = new UiCheck("Perform transfer learning and/or predict spectral library", null, false);
    checkRun.setName("run-transfer-learning");

    JLabel availabilityLabel = new JLabel("<html><b>Note: Currently available to selected collaborators only.</b></html>");

    mu.add(p, checkRun).pushX().wrap();
    mu.add(p, feURL.label()).split(2);
    mu.add(p, feURL.comp).growX().wrap();

    mu.add(p, feAPIKey.label()).split(2);
    mu.add(p, feAPIKey.comp).growX().wrap();

    mu.add(p, availabilityLabel).wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = new JPanel(new MigLayout(new LC().fillX()));
    mu.borderEmpty(p);

    checkRunTraining = new UiCheck("Perform transfer learning", null, true);
    checkRunTraining.setName("perform-transfer-learning");

    checkRunPrediction = new UiCheck("Predict spectral library", null, true);
    checkRunPrediction.setName("predict-spectral-library");

    panelTraining = createPanelTraining();
    panelPrediction = createPanelPrediction();

    mu.add(p, checkRunTraining).wrap();
    mu.add(p, panelTraining).growX().wrap();
    mu.add(p, checkRunPrediction).wrap();
    mu.add(p, panelPrediction).growX().wrap();

    return p;
  }

  private JPanel createPanelTraining() {
    panelTraining = mu.newPanel(mu.lcFillX());
    mu.border(panelTraining, 1);

    uiTextLibrary = new UiText("", "");
    uiTextLibrary.setColumns(20);
    FormEntry feLibrary = mu.feb("spectral-library-path", uiTextLibrary)
        .label("Load custom spectral library for training (optional): ")
        .tooltip("Load a custom spectral library for training. If not provided, a library will be generated by searching the input LC-MS data.")
        .create();

    JButton jButtonLibrary = feLibrary.browseButton("Browse", "Select library file. If not provided, a library will be generated by searching the input LC-MS data.", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Library files", "tsv");
      JFileChooser fc = FileChooserUtils.create("Library file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextLibrary.getNonGhostText()));
      return fc;
    }, paths -> {
      Path path = paths.get(0);
      uiTextLibrary.setText(path.toString());
    });

    mu.add(panelTraining, feLibrary.label()).split(3);
    mu.add(panelTraining, feLibrary.comp).growX().pushX();
    mu.add(panelTraining, jButtonLibrary).wrap();

    return panelTraining;
  }

  private JPanel createPanelPrediction() {
    panelPrediction = mu.newPanel(mu.lcFillX());
    mu.border(panelPrediction, 1);

    uiCheckPredictMS2 = new UiCheck("Predict MS2", null, true);
    uiCheckPredictMS2.setName("predict-ms2");

    uiCheckPredictRT = new UiCheck("Predict RT", null, true);
    uiCheckPredictRT.setName("predict-rt");

    uiCheckPredictIM = new UiCheck("Predict IM", null, true);
    uiCheckPredictIM.setName("predict-im");

    uiComboOutputFormat = UiUtils.createUiCombo(new String[]{"librarytsv", "parquet", "mgf"});
    uiComboOutputFormat.setSelectedIndex(0);
    FormEntry feOutputFormat = mu.feb("output-format", uiComboOutputFormat)
        .label("Output format")
        .tooltip("Output format for the predicted spectral library.")
        .create();

    uiTextModelPath = new UiText("", "");
    uiTextModelPath.setColumns(20);
    FormEntry feModelPath = mu.feb("model-path", uiTextModelPath)
        .label("Load custom model for prediction (optional): ")
        .tooltip("Load a custom model for prediction. If not provided, the model file in the output directory will be used.")
        .create();

    JButton jButtonModelPath = feModelPath.browseButton("Browse", "Select model file. If not provided, the model file in the output directory will be used.", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Model files", "zip");
      JFileChooser fc = FileChooserUtils.create("Model file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextModelPath.getNonGhostText()));
      return fc;
    }, paths -> {
      Path path = paths.get(0);
      uiTextModelPath.setText(path.toString());
    });

    uiComboPeptidesToPredict = UiUtils.createUiCombo(new String[]{"MSFragger search results", "Whole FASTA file (exclude decoys)", "Whole FASTA file (include decoys)", "Custom peptide list"});
    uiComboPeptidesToPredict.setSelectedIndex(1);
    FormEntry fePeptidesToPredict = mu.feb("peptides-to-predict", uiComboPeptidesToPredict)
        .label("Peptides to predict: ")
        .tooltip("MSFragger search results: use the peptides after MSFragger search to predict a spectral library.<br>"
            + "Whole FASTA file (exclude decoys): use the peptides digesed from the whole FASTA file (excluding decoys) to predict a spectral library.<br>"
            + "Whole FASTA file (include decoys): use the peptides digesed from the whole FASTA file (including decoys) to predict a spectral library.<br>"
            + "Custom peptide list: use a custom peptide list to predict a spectral library.")
        .create();

    uiTextCustomPeptideList = new UiText("", "");
    uiTextCustomPeptideList.setColumns(20);
    FormEntry feCustomPeptideList = mu.feb("custom-peptide-list", uiTextCustomPeptideList)
        .label("")
        .tooltip("Use a custom peptide list to predict a spectral library. Available when 'Custom peptide list' is selected.")
        .create();

    JButton jButtonCustomPeptideList = feCustomPeptideList.browseButton("Browse", "Select custom peptide list file", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Peptide list files", "csv");
      JFileChooser fc = FileChooserUtils.create("Custom peptide list file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextCustomPeptideList.getNonGhostText()));
      return fc;
    }, paths -> {
      Path path = paths.get(0);
      uiTextCustomPeptideList.setText(path.toString());
    });

    uiSpinnerMinCharge = new UiSpinnerInt(2, 1, 7, 1);
    FormEntry feMinCharge = mu.feb("min-charge", uiSpinnerMinCharge)
        .label("Min precursor charge")
        .tooltip("Min precursor charge when predicting spectral library. Available when 'Whole FASTA file (exclude decoys)', 'Whole FASTA file (include decoys)', or 'Custom peptide list' is selected.")
        .create();

    uiSpinnerMaxCharge = new UiSpinnerInt(3, 1, 7, 1);
    FormEntry feMaxCharge = mu.feb("max-charge", uiSpinnerMaxCharge)
        .label("Max precursor charge")
        .tooltip("Max precursor charge when predicting spectral library. Available when 'Whole FASTA file (exclude decoys)', 'Whole FASTA file (include decoys)', or 'Custom peptide list' is selected.")
        .create();

    uiComboInstrument = UiUtils.createUiCombo(instrumentMap.keySet().toArray(new String[0]));
    FormEntry feInstrument = mu.feb("instrument", uiComboInstrument)
        .label("Instrument")
        .tooltip("Instrument type when predicting spectral library. Available when 'Whole FASTA file (exclude decoys)', 'Whole FASTA file (include decoys)', or 'Custom peptide list' is selected.")
        .create();

    uiSpinnerNce = new UiSpinnerInt(30, 1, 100, 1);
    FormEntry feNce = mu.feb("nce", uiSpinnerNce)
        .label("NCE")
        .tooltip("NCE when predicting spectral library. Available when 'Whole FASTA file' or 'Custom peptide list' is selected.")
        .create();

    updateEnabledStatus(feCustomPeptideList.label(), isRunPrediction() && getPeptidesToPredict() == 3);
    updateEnabledStatus(feCustomPeptideList.comp, isRunPrediction() && getPeptidesToPredict() == 3);
    updateEnabledStatus(jButtonCustomPeptideList, isRunPrediction() && getPeptidesToPredict() == 3);
    updateEnabledStatus(feMinCharge.label(), isRunPrediction() && getPeptidesToPredict() > 0);
    updateEnabledStatus(feMinCharge.comp, isRunPrediction() && getPeptidesToPredict() > 0);
    updateEnabledStatus(feMaxCharge.label(), isRunPrediction() && getPeptidesToPredict() > 0);
    updateEnabledStatus(feMaxCharge.comp, isRunPrediction() && getPeptidesToPredict() > 0);
    updateEnabledStatus(feInstrument.label(), isRunPrediction() && getPeptidesToPredict() > 0);
    updateEnabledStatus(feInstrument.comp, isRunPrediction() && getPeptidesToPredict() > 0);
    updateEnabledStatus(feNce.label(), isRunPrediction() && getPeptidesToPredict() > 0);
    updateEnabledStatus(feNce.comp, isRunPrediction() && getPeptidesToPredict() > 0);

    checkRunPrediction.addItemListener(e -> {
      updateEnabledStatus(feCustomPeptideList.label(), isRunPrediction() && getPeptidesToPredict() == 3);
      updateEnabledStatus(feCustomPeptideList.comp, isRunPrediction() && getPeptidesToPredict() == 3);
      updateEnabledStatus(jButtonCustomPeptideList, isRunPrediction() && getPeptidesToPredict() == 3);
      updateEnabledStatus(feMinCharge.label(), isRunPrediction() && getPeptidesToPredict() > 0);
      updateEnabledStatus(feMinCharge.comp, isRunPrediction() && getPeptidesToPredict() > 0);
      updateEnabledStatus(feMaxCharge.label(), isRunPrediction() && getPeptidesToPredict() > 0);
      updateEnabledStatus(feMaxCharge.comp, isRunPrediction() && getPeptidesToPredict() > 0);
      updateEnabledStatus(feInstrument.label(), isRunPrediction() && getPeptidesToPredict() > 0);
      updateEnabledStatus(feInstrument.comp, isRunPrediction() && getPeptidesToPredict() > 0);
      updateEnabledStatus(feNce.label(), isRunPrediction() && getPeptidesToPredict() > 0);
      updateEnabledStatus(feNce.comp, isRunPrediction() && getPeptidesToPredict() > 0);
    });

    uiComboPeptidesToPredict.addItemListener(e -> {
      updateEnabledStatus(feCustomPeptideList.label(), getPeptidesToPredict() == 3);
      updateEnabledStatus(feCustomPeptideList.comp, getPeptidesToPredict() == 3);
      updateEnabledStatus(jButtonCustomPeptideList, getPeptidesToPredict() == 3);
      updateEnabledStatus(feMinCharge.label(), getPeptidesToPredict() > 0);
      updateEnabledStatus(feMinCharge.comp, getPeptidesToPredict() > 0);
      updateEnabledStatus(feMaxCharge.label(), getPeptidesToPredict() > 0);
      updateEnabledStatus(feMaxCharge.comp, getPeptidesToPredict() > 0);
      updateEnabledStatus(feInstrument.label(), getPeptidesToPredict() > 0);
      updateEnabledStatus(feInstrument.comp, getPeptidesToPredict() > 0);
      updateEnabledStatus(feNce.label(), getPeptidesToPredict() > 0);
      updateEnabledStatus(feNce.comp, getPeptidesToPredict() > 0);
    });

    mu.add(panelPrediction, feModelPath.label()).split(3);
    mu.add(panelPrediction, feModelPath.comp).growX().pushX();
    mu.add(panelPrediction, jButtonModelPath).wrap();

    mu.add(panelPrediction, uiCheckPredictMS2).split(5);
    mu.add(panelPrediction, uiCheckPredictRT);
    mu.add(panelPrediction, uiCheckPredictIM);
    mu.add(panelPrediction, feOutputFormat.label()).gapLeft("50");
    mu.add(panelPrediction, feOutputFormat.comp).wrap();

    mu.add(panelPrediction, fePeptidesToPredict.label()).split(5);
    mu.add(panelPrediction, fePeptidesToPredict.comp).growX().pushX();
    mu.add(panelPrediction, feCustomPeptideList.label());
    mu.add(panelPrediction, feCustomPeptideList.comp).growX().pushX();
    mu.add(panelPrediction, jButtonCustomPeptideList).wrap();

    mu.add(panelPrediction, feMinCharge.label()).split(4);
    mu.add(panelPrediction, feMinCharge.comp);
    mu.add(panelPrediction, feMaxCharge.label());
    mu.add(panelPrediction, feMaxCharge.comp).wrap();
    mu.add(panelPrediction, feInstrument.label()).split(4);
    mu.add(panelPrediction, feInstrument.comp);
    mu.add(panelPrediction, feNce.label());
    mu.add(panelPrediction, feNce.comp).wrap();

    updateEnabledStatus(panelPrediction, true);

    return panelPrediction;
  }


  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    this.setBorder(new TitledBorder("MSBooster Transfer Learning"));

    pTop = createPanelTop();
    pContent = createPanelContent();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  @Override
  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isRunTraining() {
    return SwingUtils.isEnabledAndChecked(checkRunTraining);
  }

  public boolean isRunPrediction() {
    return SwingUtils.isEnabledAndChecked(checkRunPrediction);
  }

  public String getURL() {
    return uiTextURL.getNonGhostText();
  }

  public String getAPIKey() {
    return uiTextAPIKey.getNonGhostText();
  }

  public String getLibraryPath() {
    return uiTextLibrary.getNonGhostText();
  }

  public String getModelPath() {
    return uiTextModelPath.getNonGhostText();
  }

  public boolean isPredictMS2() {
    return uiCheckPredictMS2.isSelected();
  }

  public boolean isPredictRT() {
    return uiCheckPredictRT.isSelected();
  }

  public boolean isPredictIM() {
    return uiCheckPredictIM.isSelected();
  }

  public int getPeptidesToPredict() {
    return uiComboPeptidesToPredict.getSelectedIndex();
  }

  public String getCustomPeptideListPath() {
    return uiTextCustomPeptideList.getNonGhostText();
  }

  public int getMinCharge() {
    return uiSpinnerMinCharge.getActualValue();
  }

  public int getMaxCharge() {
    return uiSpinnerMaxCharge.getActualValue();
  }

  public String getInstrument() {
    return instrumentMap.get(uiComboInstrument.getSelectedItem().toString());
  }

  public int getNce() {
    return uiSpinnerNce.getActualValue();
  }

  public String getOutputFormat() {
    return uiComboOutputFormat.getSelectedItem().toString();
  }
}
