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

package org.nesvilab.fragpipe.tools.msbooster;

import static org.nesvilab.fragpipe.tabs.TabMsfragger.setJTableColSize;
import static org.nesvilab.utils.SwingUtils.createClickableHtml;
import static org.nesvilab.utils.swing.UiUtils.createUiCombo;

import java.awt.Component;
import java.awt.ItemSelectable;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.miginfocom.layout.CC;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.ModelsTable;
import org.nesvilab.fragpipe.api.ModelsTableModel;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelBase;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.renderers.TableCellDoubleRenderer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MSBoosterPanel extends JPanelBase {

  private static final Logger log = LoggerFactory.getLogger(MSBoosterPanel.class);
  private static final String PREFIX = "msbooster.";
  private static final Map<String, String> modelMap = new HashMap<>();

  private JPanel pTop;
  private JPanel pContent;
  private JPanel pp;
  private JPanel pp2;
  private static final MigUtils mu = MigUtils.get();
  private UiCheck checkRun;
  private UiCheck uiCheckPredictRt;
  private UiCheck uiCheckPredictSpectra;
  private UiCheck uiCheckPredictIm;
  private UiCheck uiCheckFindBestRtModel;
  private UiCheck uiCheckFindBestSpectraModel;
  private UiCheck uiCheckFindBestImModel;
  private UiCombo uiComboRtModel;
  private UiCombo uiComboSpectraModel;
  private UiCombo uiComboImModel;
  private UiCombo uiComboFragmentationType;
  private UiText uiTextKoinaUrl;
  private UiText uiTextLibrary;
  private JEditorPane uiLabelKoinaUrl;
  private ModelsTable tableRtModels;
  private ModelsTable tableSpectraModels;
  private ModelsTable tableImModels;

  static {
    modelMap.put("DIA-NN", "DIA-NN");
    modelMap.put("DeepLC HeLa HF", "Deeplc_hela_hf");
    modelMap.put("AlphaPept RT Generic", "AlphaPept_rt_generic");
    modelMap.put("Prosit 2019 iRT", "Prosit_2019_irt");
    modelMap.put("Prosit 2020 iRT TMT", "Prosit_2020_irt_TMT");
    modelMap.put("MS2PIP 2021 HCD", "ms2pip_2021_hcd");
    modelMap.put("MS2PIP timsTOF 2024", "ms2pip_timsTOF2024");
    modelMap.put("MS2PIP TTOF 5600", "ms2pip_ttof5600");
    modelMap.put("MS2PIP Immuno HCD", "ms2pip_immuno_hcd");
    modelMap.put("MS2PIP CID TMT", "ms2pip_CID_TMT");
    modelMap.put("MS2PIP iTRAQ Phospho", "ms2pip_iTRAQphospho");
    modelMap.put("AlphaPept MS2 Generic", "AlphaPept_ms2_generic");
    modelMap.put("Prosit 2019 Intensity", "Prosit_2019_intensity");
    modelMap.put("Prosit 2023 Intensity timsTOF", "Prosit_2023_intensity_timsTOF");
    modelMap.put("Prosit 2020 Intensity CID", "Prosit_2020_intensity_CID");
    modelMap.put("Prosit 2020 Intensity TMT", "Prosit_2020_intensity_TMT");
    modelMap.put("Prosit 2020 Intensity HCD", "Prosit_2020_intensity_HCD");
    modelMap.put("AlphaPept CCS Generic", "AlphaPept_ccs_generic");
    modelMap.put("IM2Deep", "IM2Deep");
    modelMap.put("UniSpec", "UniSpec");
    modelMap.put("Prosit 2024 Intensity CIT", "Prosit_2024_intensity_cit");
    modelMap.put("Prosit 2025 Intensity MultiFrag", "Prosit_2025_intensity_MultiFrag");
    modelMap.put("Prosit 2024 iRT CIT", "Prosit_2024_irt_cit");
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
    mu.border(this, "Rescoring using deep learning prediction");

    pTop = createPanelTop();

    uiCheckPredictRt = new UiCheck("Predict RT", null, true);
    uiCheckPredictRt.setName("predict-rt");

    uiCheckPredictSpectra = new UiCheck("Predict spectra", null, true);
    uiCheckPredictSpectra.setName("predict-spectra");

    uiCheckPredictIm = new UiCheck("Predict IM (if applicable)", null, true);
    uiCheckPredictIm.setName("predict-im");

    uiTextKoinaUrl = new UiText("", "put your Koina server URL");
    uiTextKoinaUrl.setColumns(20);
    FormEntry feKoinaUrl = mu.feb("koina-url", uiTextKoinaUrl)
        .label("Koina server URL (optional): ")
        .tooltip("Fill in your Koina server URL if you want to use the models in Koina.\nThe public one is https://koina.wilhelmlab.org:443/v2/models/")
        .create();

    uiLabelKoinaUrl = createClickableHtml("Fill in your Koina server URL if you want to use the models in <a href=\"https://koina.wilhelmlab.org/\">Koina</a>. The public one is https://koina.wilhelmlab.org:443/v2/models/");

    uiTextLibrary = new UiText("", "put your library path (optional, experimental)");
    uiTextLibrary.setColumns(20);
    FormEntry feLibrary = mu.feb("spectral-library-path", uiTextLibrary)
        .label("Spectral library (optional, experimental): ")
        .tooltip("Fill in the path to the spectral library if you want to use the library during the rescoring.")
        .create();

    JButton jButtonLibrary = feLibrary.browseButton("Browse", "Select library file", () -> {
      final FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Library files", "speclib", "tsv", "parquet", "mgf");
      JFileChooser fc = FileChooserUtils.create("Library file", "Select", false, FcMode.FILES_ONLY, true, fileNameExtensionFilter);
      fc.setFileFilter(fileNameExtensionFilter);
      FileChooserUtils.setPath(fc, Stream.of(uiTextLibrary.getNonGhostText()));
      return fc;
    }, paths -> {
      Path path = paths.get(0); // we only allowed selection of a single file in the file chooser
      uiTextLibrary.setText(path.toString());
    });

    uiComboRtModel = createUiCombo(new String[]{
        "DIA-NN",
        "DeepLC HeLa HF",
        "AlphaPept RT Generic",
        "Prosit 2019 iRT",
        "Prosit 2020 iRT TMT",
        "Prosit 2024 iRT CIT"
    });
    FormEntry feRtModel = mu.feb("rt-model", uiComboRtModel)
        .label("Model: ")
        .tooltip("If using the model other than 'DIA-NN', MSBooster will query the Koina server to get the prediction.")
        .create();

    uiComboSpectraModel = createUiCombo(new String[]{
        "DIA-NN",
        "MS2PIP 2021 HCD",
        "MS2PIP timsTOF 2024",
        "MS2PIP TTOF 5600",
        "MS2PIP Immuno HCD",
        "MS2PIP CID TMT",
        "MS2PIP iTRAQ Phospho",
        "AlphaPept MS2 Generic",
        "Prosit 2019 Intensity",
        "Prosit 2023 Intensity timsTOF",
        "Prosit 2020 Intensity CID",
        "Prosit 2020 Intensity TMT",
        "Prosit 2020 Intensity HCD",
        "UniSpec",
        "Prosit 2024 Intensity CIT",
        "Prosit 2025 Intensity MultiFrag"
    });
    FormEntry feSpectraModel = mu.feb("spectra-model", uiComboSpectraModel)
        .label("Model: ")
        .tooltip("If using the model other than 'DIA-NN', MSBooster will query the Koina server to get the prediction.")
        .create();

    uiComboImModel = createUiCombo(new String[]{
        "DIA-NN",
        "AlphaPept CCS Generic",
        "IM2Deep"
    });
    FormEntry feImModel = mu.feb("im-model", uiComboImModel)
        .label("Model: ")
        .tooltip("If using the model other than 'DIA-NN', MSBooster will query the Koina server to get the prediction.")
        .create();

    uiComboFragmentationType = createUiCombo(new String[]{
      "auto",
      "HCD",
      "CID",
      "ETD",
      "UVPD",
      "ECD",
      "EID",
      "ETCID"
    });
    FormEntry feFragmentationType = mu.feb("fragmentation-type", uiComboFragmentationType)
        .label("Fragmentation type: ")
        .tooltip("Select the fragmentation type for the MS2 spectra.")
        .create();

    JPanel uiRtTable = createRtModelsPanel();
    JPanel uiSpectraTable = createSpectraModelsPanel();
    JPanel uiImTable = createImModelsPanel();

    pContent = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pContent, uiCheckPredictRt);
    mu.add(pContent, feRtModel.label()).split(2);
    mu.add(pContent, feRtModel.comp);
    mu.add(pContent, uiCheckFindBestRtModel).wrap();

    mu.add(pContent, uiCheckPredictSpectra);
    mu.add(pContent, feSpectraModel.label()).split(2);
    mu.add(pContent, feSpectraModel.comp);
    mu.add(pContent, uiCheckFindBestSpectraModel).wrap();

    mu.add(pContent, uiCheckPredictIm);
    mu.add(pContent, feImModel.label()).split(2);
    mu.add(pContent, feImModel.comp);
    mu.add(pContent, uiCheckFindBestImModel).wrap();

    mu.add(pContent, feFragmentationType.label()).split(2);
    mu.add(pContent, feFragmentationType.comp).wrap();

    pp = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pp, feKoinaUrl.label()).split(2);
    mu.add(pp, feKoinaUrl.comp).growX().wrap();
    mu.add(pp, uiLabelKoinaUrl).wrap();

    pp2 = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.add(pp2, feLibrary.label()).split(2);
    mu.add(pp2, feLibrary.comp).growX();
    mu.add(pp2, jButtonLibrary).wrap();

    mu.add(pContent, pp).split().growX().spanX().wrap();
    mu.add(pContent, pp2).split().growX().spanX().wrap();
    mu.add(pTop, pContent).growX().wrap();
    mu.add(this, pTop).growX().wrap();
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

  private JPanel createRtModelsPanel() {
    uiCheckFindBestRtModel = new UiCheck("Find best RT model (requires Koina server)", null, false);
    uiCheckFindBestRtModel.setName("find-best-rt-model");

    JPanel uiTableRtModels = mu.newPanel(null, mu.lcNoInsetsTopBottom());

    List<Model> models = List.of(
        new Model("DIA-NN", true),
        new Model("DeepLC HeLa HF", true),
        new Model("AlphaPept RT Generic", true),
        new Model("Prosit 2019 iRT", true),
        new Model("Prosit 2020 iRT TMT", true),
        new Model("Prosit 2024 iRT CIT", true)
    );

    tableRtModels = createTableModels(models, "table.rt-models");
    SwingUtilities.invokeLater(() -> {
      setJTableColSize(tableRtModels, 0, 20, 150, 50);
    });

    JScrollPane tableScroll = new JScrollPane(tableRtModels, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    mu.add(uiTableRtModels, uiCheckFindBestRtModel).wrap();
    uiTableRtModels.add(tableScroll, new CC().minHeight("50px").maxHeight("100px").growX().spanX().wrap());

    return uiTableRtModels;
  }

  private JPanel createSpectraModelsPanel() {
    uiCheckFindBestSpectraModel = new UiCheck("Find best spectral model (requires Koina server)", null, false);
    uiCheckFindBestSpectraModel.setName("find-best-spectra-model");

    JPanel uiTableSpectraModels = mu.newPanel(null, mu.lcNoInsetsTopBottom());

    List<Model> models = List.of(
        new Model("DIA-NN", true),
        new Model("MS2PIP 2021 HCD", true),
        new Model("MS2PIP timsTOF 2024", true),
        new Model("MS2PIP TTOF 5600", true),
        new Model("MS2PIP Immuno HCD", true),
        new Model("MS2PIP CID TMT", true),
        new Model("MS2PIP iTRAQ Phospho", true),
        new Model("AlphaPept MS2 Generic", true),
        new Model("Prosit 2019 Intensity", true),
        new Model("Prosit 2023 Intensity timsTOF", true),
        new Model("Prosit 2020 Intensity CID", true),
        new Model("Prosit 2020 Intensity TMT", true),
        new Model("Prosit 2020 Intensity HCD", true),
        new Model("UniSpec", true),
        new Model("Prosit 2024 Intensity CIT", true),
        new Model("Prosit 2025 Intensity MultiFrag", true)
    );

    tableSpectraModels = createTableModels(models, "table.spectra-models");
    SwingUtilities.invokeLater(() -> {
      setJTableColSize(tableSpectraModels, 0, 20, 150, 50);
    });

    JScrollPane tableScroll = new JScrollPane(tableSpectraModels, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    mu.add(uiTableSpectraModels, uiCheckFindBestSpectraModel).wrap();
    uiTableSpectraModels.add(tableScroll, new CC().minHeight("50px").maxHeight("100px").growX().spanX().wrap());

    return uiTableSpectraModels;
  }

  private JPanel createImModelsPanel() {
    uiCheckFindBestImModel = new UiCheck("Find best IM model if applicable (requires Koina server)", null, false);
    uiCheckFindBestImModel.setName("find-best-im-model");

    JPanel uiTableImModels = mu.newPanel(null, mu.lcNoInsetsTopBottom());

    List<Model> models = List.of(
        new Model("DIA-NN", true),
        new Model("AlphaPept CCS Generic", true),
        new Model("IM2Deep", true)
    );

    tableImModels = createTableModels(models, "table.im-models");
    SwingUtilities.invokeLater(() -> {
      setJTableColSize(tableImModels, 0, 20, 150, 50);
    });

    JScrollPane tableScroll = new JScrollPane(tableImModels, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    mu.add(uiTableImModels, uiCheckFindBestImModel).wrap();
    uiTableImModels.add(tableScroll, new CC().minHeight("50px").maxHeight("100px").growX().spanX().wrap());

    return uiTableImModels;
  }


  public boolean predictRt() {
    return uiCheckPredictRt.isSelected();
  }

  public boolean predictSpectra() {
    return uiCheckPredictSpectra.isSelected();
  }

  public boolean predictIm() {
    return uiCheckPredictIm.isSelected();
  }

  public boolean findBestRtModel() {
    return uiCheckFindBestRtModel.isSelected();
  }

  public boolean findBestSpectraModel() {
    return uiCheckFindBestSpectraModel.isSelected();
  }

  public boolean findBestImModel() {
    return uiCheckFindBestImModel.isSelected();
  }

  public String koinaUrl() {
    return uiTextKoinaUrl.getNonGhostText().trim();
  }

  public String libraryPath() {
    return uiTextLibrary.getNonGhostText().trim();
  }

  public String rtModel() {
    if (uiComboRtModel.getSelectedItem() == null) {
      return "DIA-NN";
    } else {
      return modelMap.get((String) uiComboRtModel.getSelectedItem());
    }
  }

  public String spectraModel() {
    if (uiComboSpectraModel.getSelectedItem() == null) {
      return "DIA-NN";
    } else {
      return modelMap.get((String) uiComboSpectraModel.getSelectedItem());
    }
  }

  public String imModel() {
    if (uiComboImModel.getSelectedItem() == null) {
      return "DIA-NN";
    } else {
      return modelMap.get((String) uiComboImModel.getSelectedItem());
    }
  }

  public String rtTestModels() {
    List<String> t = new ArrayList<>(modelMap.size());
    for (Model m : tableRtModels.model.getModels()) {
      if (m.isEnabled) {
        t.add(modelMap.get(m.name));
      }
    }
    return String.join(",", t);
  }

  public String spectraTestModels() {
    List<String> t = new ArrayList<>(modelMap.size());
    for (Model m : tableSpectraModels.model.getModels()) {
      if (m.isEnabled) {
        t.add(modelMap.get(m.name));
      }
    }
    return String.join(",", t);
  }

  public String imTestModels() {
    List<String> t = new ArrayList<>(modelMap.size());
    for (Model m : tableImModels.model.getModels()) {
      if (m.isEnabled) {
        t.add(modelMap.get(m.name));
      }
    }
    return String.join(",", t);
  }

  public String fragmentationType() {
    return (String) uiComboFragmentationType.getSelectedItem();
  }

  @Override
  public void initMore() {
    super.initMore();
    SwingUtilities.invokeLater(() -> SwingUtilities.invokeLater(() -> updateEnabledStatus(pContent, checkRun.isSelected())));
  }

  private ModelsTable createTableModels(List<Model> models, String paramName) {
    Object[][] data = f(models);
    String[] columnNames = new String[]{"Enabled", "Model"};

    ModelsTableModel m = new ModelsTableModel(
        columnNames,
        new Class<?>[]{Boolean.class, String.class},
        new boolean[]{true, false},
        data);

    ModelsTable t = new ModelsTable(m, columnNames, MSBoosterPanel::f);
    Fragpipe.rename(t, paramName, PREFIX);

    t.setDefaultRenderer(Double.class, new TableCellDoubleRenderer());
    t.setFillsViewportHeight(true);

    return t;
  }

  private static Object[][] f(List<Model> models) {
    Object[][] data = new Object[models.size()][3];
    for (int i = 0; i < data.length; i++) {
      data[i][0] = true;
      data[i][1] = models.get(i).name;
    }
    return data;
  }
}
