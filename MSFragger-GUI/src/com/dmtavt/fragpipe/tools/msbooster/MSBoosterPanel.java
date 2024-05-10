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

package com.dmtavt.fragpipe.tools.msbooster;

import static com.github.chhh.utils.SwingUtils.createClickableHtml;
import static com.github.chhh.utils.swing.UiUtils.createUiCombo;

import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiText;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MSBoosterPanel extends JPanelBase {

  private static final Logger log = LoggerFactory.getLogger(MSBoosterPanel.class);
  private static final String PREFIX = "msbooster.";
  private static final Map<String, String> modelMap = new HashMap<>();

  private JPanel pTop;
  private JPanel pContent;
  private JPanel pp;
  private static final MigUtils mu = MigUtils.get();
  private UiCheck checkRun;
  private UiCheck uiCheckPredictRT;
  private UiCheck uiCheckPredictSpectra;
  private UiCheck uiUseCorrelatedFeatures;
  private UiCheck uiCheckFindBestRtModel;
  private UiCheck uiCheckFindBestSpectraModel;
  private UiCombo uiComboRTModel;
  private UiCombo uiComboSpectraModel;
  private UiText uiTextKoinaUrl;
  private JEditorPane uiLabelKoinaUrl;

  static {
    modelMap.put("DIA-NN", "DIA-NN");
    modelMap.put("DeepLC HeLa HF", "Deeplc_hela_hf");
    modelMap.put("AlphaPept RT Generic", "AlphaPept_rt_generic");
    modelMap.put("Prosit 2019 iRT", "Prosit_2019_irt");
    modelMap.put("Prosit 2020 iRT TMT", "Prosit_2020_irt_TMT");
    modelMap.put("MS2PIP 2021 HCD", "ms2pip_2021_hcd");
    modelMap.put("AlphaPept MS2 Generic", "AlphaPept_ms2_generic");
    modelMap.put("Prosit 2019 Intensity", "Prosit_2019_intensity");
    modelMap.put("Prosit 2023 Intensity timsTOF", "Prosit_2023_intensity_timsTOF");
    modelMap.put("Prosit 2020 Intensity CID", "Prosit_2020_intensity_CID");
    modelMap.put("Prosit 2020 Intensity TMT", "Prosit_2020_intensity_TMT");
    modelMap.put("Prosit 2020 Intensity HCD", "Prosit_2020_intensity_HCD");
  }

  public MSBoosterPanel() {
    super();
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

  public void setRunStatus(boolean status) {
    checkRun.setSelected(status);
  }

  @Override
  protected void init() {
    mu.layout(this, mu.lcFillXNoInsetsTopBottom());
    mu.border(this, "Rescoring Using Deep Learning Prediction");

    pTop = createPanelTop();

    uiCheckPredictRT = new UiCheck("Predict RT", null, true);
    uiCheckPredictRT.setName("predict-rt");

    uiCheckPredictSpectra = new UiCheck("Predict spectra", null, true);
    uiCheckPredictSpectra.setName("predict-spectra");

    uiUseCorrelatedFeatures = new UiCheck("Use correlated features", null, false);
    uiUseCorrelatedFeatures.setName("use-correlated-features");

    uiCheckFindBestRtModel = new UiCheck("Find best RT model", null, false);
    uiCheckFindBestRtModel.setName("find-best-rt-model");

    uiCheckFindBestSpectraModel = new UiCheck("Find best spectra model", null, false);
    uiCheckFindBestSpectraModel.setName("find-best-spectra-model");

    uiTextKoinaUrl = new UiText("", "put your Koina server URL");
    uiTextKoinaUrl.setColumns(20);
    FormEntry feKoinaUrl = mu.feb("koina-url", uiTextKoinaUrl)
        .label("Koina server URL: ")
        .tooltip("Fill in your Koina server URL if you want to use the models in Koina.\nThe public one is https://koina.proteomicsdb.org/v2/models/")
        .create();

    uiLabelKoinaUrl = createClickableHtml("Fill in your Koina server URL if you want to use the models in <a href=\"https://koina.wilhelmlab.org/\">Koina</a>. The public one is https://koina.proteomicsdb.org/v2/models/");

    uiComboRTModel = createUiCombo(new String[]{
        "DIA-NN",
        "DeepLC HeLa HF",
        "AlphaPept RT Generic",
        "Prosit 2019 iRT",
        "Prosit 2020 iRT TMT"
    });
    FormEntry feRTModel = mu.feb("rt-model", uiComboRTModel)
        .label("Model: ")
        .tooltip("If using the model other than 'DIA-NN', MSBooster will query the Koina server to get the prediction.")
        .create();

    uiComboSpectraModel = createUiCombo(new String[]{
        "DIA-NN",
        "MS2PIP 2021 HCD",
        "AlphaPept MS2 Generic",
        "Prosit 2019 Intensity",
        "Prosit 2023 Intensity timsTOF",
        "Prosit 2020 Intensity CID",
        "Prosit 2020 Intensity TMT",
        "Prosit 2020 Intensity HCD"
    });
    FormEntry feSpectraModel = mu.feb("spectra-model", uiComboSpectraModel)
        .label("Model: ")
        .tooltip("If using the model other than 'DIA-NN', MSBooster will query the Koina server to get the prediction.")
        .create();

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, uiCheckPredictRT);
    mu.add(pContent, feRTModel.label()).split(2);
    mu.add(pContent, feRTModel.comp);
    mu.add(pContent, uiCheckFindBestRtModel).wrap();

    mu.add(pContent, uiCheckPredictSpectra);
    mu.add(pContent, feSpectraModel.label()).split(2);
    mu.add(pContent, feSpectraModel.comp);
    mu.add(pContent, uiCheckFindBestSpectraModel);
    mu.add(pContent, uiUseCorrelatedFeatures).wrap();

    pp = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pp, feKoinaUrl.label()).split(2);
    mu.add(pp, feKoinaUrl.comp).growX().wrap();
    mu.add(pp, uiLabelKoinaUrl).wrap();

    mu.add(this, pTop).growX().wrap();
    mu.add(this, pContent).growX().wrap();
    mu.add(this, pp).growX().wrap();
  }

  private JPanel createPanelTop() {

    JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());

    checkRun = new UiCheck("Run MSBooster", null, false);
    checkRun.setName("run-msbooster");
    JLabel info = new JLabel("<html>Rescoring using deep learning prediction. Require <b>Run Percolator</b> in PSM validation panel.");

    mu.add(p, checkRun).split();
    mu.add(p, info).gapLeft("80px").wrap();
    return p;
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean predictRt() {
    return uiCheckPredictRT.isSelected();
  }

  public boolean predictSpectra() {
    return uiCheckPredictSpectra.isSelected();
  }

  public boolean findBestRtModel() {
    return uiCheckFindBestRtModel.isSelected();
  }

  public boolean findBestSpectraModel() {
    return uiCheckFindBestSpectraModel.isSelected();
  }

  public String koinaUrl() {
    return uiTextKoinaUrl.getNonGhostText().trim();
  }

  public String rtModel() {
    if (uiComboRTModel.getSelectedItem() == null) {
      return "DIA-NN";
    } else {
      return modelMap.get((String) uiComboRTModel.getSelectedItem());
    }
  }

  public String spectraModel() {
    if (uiComboSpectraModel.getSelectedItem() == null) {
      return "DIA-NN";
    } else {
      return modelMap.get((String) uiComboSpectraModel.getSelectedItem());
    }
  }

  public boolean useCorrelatedFeatures() {
    return uiUseCorrelatedFeatures.isSelected();
  }

  @Override
  public void initMore() {
    updateEnabledStatus(this, true);
    super.initMore();
  }
}
