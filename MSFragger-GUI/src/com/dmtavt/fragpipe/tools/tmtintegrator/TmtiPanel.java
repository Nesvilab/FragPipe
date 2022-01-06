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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.tools.tmtintegrator;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.dialogs.QuantLabelAnnotationDialog;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesList;
import com.dmtavt.fragpipe.messages.MessageLoadTmtIntegratorDefaults;
import com.dmtavt.fragpipe.messages.MessageType;
import com.dmtavt.fragpipe.messages.NoteConfigTmtI;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtAnnotationTable.ExpNameToAnnotationFile;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import com.github.chhh.utils.swing.renderers.ButtonColumn;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.mozilla.universalchardet.UniversalDetector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TmtiPanel extends JPanelBase {
  private static final Logger log = LoggerFactory.getLogger(TmtiPanel.class);
  private static final MigUtils mu = MigUtils.get();
  private static final String STRING_NO_PATH_SET = "No path set yet";

  private JPanel pTop;
  private UiCheck checkRun;
  private JPanel pContent;
  private JPanel pTable;
  private TmtAnnotationTable tmtAnnotationTable;
  private JScrollPane scrollPaneTmtTable;
  private JPanel pOptsBasic;
  private Action actionBrowse;
  private Action actionCreate;
  private ButtonColumn colBrowse;
  private ButtonColumn colCreate;
  private UiCombo uiComboQuantLevel;
  private UiCombo uiComboLabelNames;
  public static final String PREFIX = "tmtintegrator.";
  public static final String PROP_LAST_ANNOTATION_PATH = "fragpipe.tmt.last-annotation-path";
  private static final Map<String, Function<String, String>> CONVERT_TO_FILE;
  private static final Map<String, Function<String, String>> CONVERT_TO_GUI;

  static {
    CONVERT_TO_FILE = new HashMap<>();
    CONVERT_TO_GUI = new HashMap<>();

    CONVERT_TO_FILE.put(TmtiConfProps.PROP_channel_num, s -> {
      QuantLabel label = QuantLabel.LABELS.stream()
          .filter(ql -> ql.getName().equalsIgnoreCase(s))
          .findFirst().orElseThrow(supplyRunEx("No matching quant label"));
      return Integer.toString(label.getReagentNames().size());
    });
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_groupby, s -> findOrThrow(false, TmtiConfProps.COMBO_GROUP_BY,
        s,supplyRunEx("No matching groupby value")).getValInConfig());
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_prot_norm, s -> findOrThrow(false, TmtiConfProps.COMBO_NORM,
        s,supplyRunEx("No matching prot_norm value")).getValInConfig());
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_unique_gene, s -> findOrThrow(false, TmtiConfProps.COMBO_UNIQUE_GENE,
        s,supplyRunEx("No matching unique_gene value")).getValInConfig());
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_add_Ref, s -> findOrThrow(false, TmtiConfProps.COMBO_ADD_REF,
        s,supplyRunEx("No matching add_Ref value")).getValInConfig());
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_unique_pep, s -> findOrThrow(false, TmtiConfProps.COMBO_PEPTIDE_PROTEIN_UNIQUENESS,
        s,supplyRunEx("No matching prot_norm value")).getValInConfig());
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_aggregation_method, s -> findOrThrow(false, TmtiConfProps.COMBO_AGGREGATION_METHOD,
        s,supplyRunEx("No matching prot_norm value")).getValInConfig());

    CONVERT_TO_GUI.put(TmtiConfProps.PROP_channel_num, s -> {
      int numChannels = Integer.parseInt(s);
      QuantLabel label = QuantLabel.LABELS.stream()
          .filter(ql -> ql.getReagentNames().size() == numChannels)
          .findFirst().orElseThrow(supplyRunEx("No matching quant label"));
      return label.getName();
    });
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_groupby, s -> findOrThrow(true, TmtiConfProps.COMBO_GROUP_BY,
        s,supplyRunEx("No matching groupby value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_prot_norm, s -> findOrThrow(true, TmtiConfProps.COMBO_NORM,
        s,supplyRunEx("No matching prot_norm value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_unique_gene, s -> findOrThrow(true, TmtiConfProps.COMBO_UNIQUE_GENE,
        s,supplyRunEx("No matching unique_gene value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_unique_pep, s -> findOrThrow(true, TmtiConfProps.COMBO_PEPTIDE_PROTEIN_UNIQUENESS,
        s,supplyRunEx("No matching unique_gene value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_add_Ref, s -> findOrThrow(true, TmtiConfProps.COMBO_ADD_REF,
        s,supplyRunEx("No matching add_Ref value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_aggregation_method, s -> findOrThrow(true, TmtiConfProps.COMBO_AGGREGATION_METHOD,
        s,supplyRunEx("No matching add_Ref value")).getValInUi());
  }

  private JPanel pOptsAdvanced;
  private UiCombo uiComboAddRef;
  private UiCheck uiCheckDontRunFqLq;

  private static Supplier<? extends RuntimeException> supplyRunEx(String message) {
    return () -> new RuntimeException(message);
  }

  private static ComboValue findOrThrow(boolean toUi, List<ComboValue> cvs, String searchVal, Supplier<? extends RuntimeException> exceptionSupplier) {
    return cvs.stream()
        .filter(cv -> {
          if (toUi) {
            return cv.valInConfig.equalsIgnoreCase(searchVal);
          } else {
            return cv.valInUi.equalsIgnoreCase(searchVal);
          }
        }).findFirst()
        .orElseThrow(exceptionSupplier);
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

  @Override
  protected void initMore() {
    super.initMore();
    Bus.post(new MessageLcmsFilesList(MessageType.REQUEST, null));
  }

  @Override
  protected void init() {
    this.setLayout(new BorderLayout());
    mu.border(this, "Isobaric Labeling-Based Quantification");

    pTop = createPanelTop();
    pContent = createPanelContent();
    pTable = createPanelTable();
    pOptsBasic = createPanelOptsBasic();
    JPanel pOptsAdvancedPtm = createPanelOptsAdvancedPtm();
    pOptsAdvanced = createPanelOptsAdvanced();

    mu.add(pContent, pTable).growX();

    JPanel p = new JPanel(new MigLayout(new LC()));
    mu.add(p, pOptsBasic).alignY("top").growX().growY().wrap();
    mu.add(p, pOptsAdvancedPtm).alignY("top").growX().growY().wrap();

    mu.add(pContent, p).alignY("top").growX().growY().wrap();
    mu.add(pContent, pOptsAdvanced).spanX().growX().wrap();

    this.add(pTop, BorderLayout.NORTH);
    this.add(pContent, BorderLayout.CENTER);
  }

  private JPanel createPanelTop() {
    JPanel p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(p);

    checkRun = new UiCheck("Run TMT-Integrator", null, false);
    checkRun.setName("run-tmtintegrator");
    JButton btnLoadDefaults = UiUtils.createButton("Load TMT-Integrator defaults",
        (e) -> Bus.post(new MessageLoadTmtIntegratorDefaults(true)));

    uiCheckDontRunFqLq = UiUtils.createUiCheck("Skip PSM quantification (rerun TMT-Integrator only)", false);
    FormEntry feDontRunFqLq = mu.feb(uiCheckDontRunFqLq).name("dont-run-fq-lq")
        .tooltip("Only use in rare situations when you need to re-run TMT-Integrator separately")
        .create();

    mu.add(p, checkRun);
    mu.add(p, btnLoadDefaults).pushX().wrap();
    mu.add(p, feDontRunFqLq.comp).spanX().growX().wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = mu.newPanel(mu.lcFillX());
    mu.borderEmpty(p);
    return p;
  }

  private JPanel createPanelTable() {
    JPanel p = mu.newPanel(null, mu.lcFillXNoInsetsTopBottom());
    mu.borderEmpty(p);

    tmtAnnotationTable = new TmtAnnotationTable();
    actionBrowse = new AbstractAction() {
      @Override
      public void actionPerformed(ActionEvent e) {
        TmtiPanel.this.actionBrowse(e);
      }
    };
    actionCreate = new AbstractAction() {
      @Override
      public void actionPerformed(ActionEvent e) {
        TmtiPanel.this.actionEditCreate(e);
      }
    };
    colBrowse = new ButtonColumn(tmtAnnotationTable, actionBrowse, 2);
    colCreate = new ButtonColumn(tmtAnnotationTable, actionCreate, 3);

    p.add(new JLabel("Sample/Channel Annotation (rows will be filled when you assign LC-MS files to experiments)"), BorderLayout.NORTH);
    tmtAnnotationTable.fireInitialization();
    tmtAnnotationTable.setFillsViewportHeight(false);
    scrollPaneTmtTable = new JScrollPane();
    scrollPaneTmtTable.setViewportView(tmtAnnotationTable);
    scrollPaneTmtTable.setPreferredSize(new Dimension(640, 300));
    p.add(scrollPaneTmtTable, BorderLayout.CENTER);

    return p;
  }

  private JPanel createPanelOptsBasic() {
    JPanel p = new JPanel(new MigLayout(new LC()));//.debug()));
    mu.border(p, "Basic Options");

    uiComboLabelNames = UiUtils.createUiCombo(
        QuantLabel.LABELS.stream().map(QuantLabel::getName).collect(Collectors.toList()));
    FormEntry feLabelType = fe(TmtiConfProps.PROP_channel_num,
        "Label type", uiComboLabelNames, null);

    uiComboQuantLevel = UiUtils.createUiCombo(new String[]{"2", "3", "2-lowres"});
    FormEntry feQuantLevel = fe("quant_level", "Quant level", uiComboQuantLevel, "MS level of quantification (2: MS2; 3: MS3; 2-lowres: MS2 low-resolution)");

    UiText uiTextRefTag = UiUtils.uiTextBuilder().cols(10).text("Bridge").create();
    FormEntry feRefTag = fe(TmtiConfProps.PROP_ref_tag,
        "Ref sample tag", uiTextRefTag,
        "<html>Unique tag to identify reference (bridge) channels");

    UiCombo uiComboGroupBy = UiUtils.createUiCombo(TmtiConfProps.COMBO_GROUP_BY.stream()
        .map(ComboValue::getValInUi).collect(Collectors.toList()));
    FormEntry feGroupBy = fe(TmtiConfProps.PROP_groupby,
        "Group by", uiComboGroupBy,
        "<html>Level of summarization <br/>\n"
            + "0: PSM aggregation to the gene level <br/>\n"
            + "1: protein <br/>\n"
            + "2: peptide sequence <br/>\n"
            + "3: multiple PTM sites <br/>\n"
            + "4: single PTM sites <br/>\n"
            + "-1: generate reports at all levels");

    UiCombo uiComboNormalization = UiUtils.createUiCombo(TmtiConfProps.COMBO_NORM.stream()
        .map(ComboValue::getValInUi).collect(Collectors.toList()));
    FormEntry feProtNorm = fe(TmtiConfProps.PROP_prot_norm,
        "Normalization", uiComboNormalization,
        "<html>Normalization <br/>\n"
            + "0: None <br/>\n"
            + "1: MD (median centering) <br/>\n"
            + "2: GN (median centering variance scaling) <br/>\n"
            + "-1: generate reports with all normalization options)");

    uiComboAddRef = UiUtils.createUiCombo(TmtiConfProps.COMBO_ADD_REF.stream()
        .map(ComboValue::getValInUi).collect(Collectors.toList()));
    FormEntry feAddRef = fe(TmtiConfProps.PROP_add_Ref,
        "Define reference", uiComboAddRef,
        "<html>Add an artificial reference channel if<br/>\n"
            + "there is no reference channel in the sample");
    uiComboAddRef.addItemListener(e -> {
      final String selected = (String) uiComboAddRef.getSelectedItem();
      boolean enabled = TmtiConfProps.COMBO_ADD_REF_CHANNEL.equalsIgnoreCase(selected);
      updateEnabledStatus(uiTextRefTag, enabled);
    });
    uiComboAddRef.setSelectedItem(null);
    uiComboAddRef.setSelectedItem(TmtiConfProps.COMBO_ADD_REF_CHANNEL);

    addRowLabelComp(p, feLabelType);
    addRowLabelComp(p, feQuantLevel);
    addRowLabelComp(p, feAddRef);
    addRowLabelComp(p, feRefTag);
    addRowLabelComp(p, feGroupBy);
    addRowLabelComp(p, feProtNorm);

    return p;
  }

  public String getDefineReference() {
    return (String)uiComboAddRef.getSelectedItem();
  }

  private JPanel createPanelOptsAdvanced() {
    JPanel p = mu.newPanel(new LC().fillX());
    mu.border(p, "Advanced Options");

    JPanel[] t = createPanelOptsAdvancedGeneral();

    mu.add(p, t[0]).growX().pushX().wrap();
    mu.add(p, t[1]).growX().pushX().wrap();

    return p;
  }

  private JPanel[] createPanelOptsAdvancedGeneral() {
    UiCombo uiComboUniqueGene = UiUtils.createUiCombo(TmtiConfProps.COMBO_UNIQUE_GENE.stream()
        .map(ComboValue::getValInUi).collect(Collectors.toList()));
    FormEntry feUniqueGene = fe(TmtiConfProps.PROP_unique_gene,
        "Peptide-Gene uniqueness", uiComboUniqueGene,
        "<html>0: allow all PSMs <br/>"
            + "1: remove PSMs mapping to more than one gene with evidence of expression in the dataset <br/>\n"
            + "2: remove all PSMs mapping to more than one gene in the FASTA database\n");

    DecimalFormat df2 = new DecimalFormat("#.##");
    DecimalFormat df3 = new DecimalFormat("#.###");

    UiSpinnerDouble uiSpinnerMinPsmProb = UiSpinnerDouble
        .builder(0.9, 0.0, 1.0, 0.05).setFormat(df3).setCols(5).create();
    FormEntry feMinPsmProb = fe(TmtiConfProps.PROP_min_pep_prob,
        "Min PSM probability", uiSpinnerMinPsmProb,
        "<html>Minimum PSM probability threshold <br/>\n"
            + "(on top of FDR filtering by Philosopher)");

    UiSpinnerDouble uiSpinnerMinPurity = UiSpinnerDouble
        .builder(0.5, 0.0, 1.0, 0.05).setFormat(df3).setCols(5).create();
    FormEntry feMinPurity = fe(TmtiConfProps.PROP_min_purity,
        "Min purity", uiSpinnerMinPurity,
        "<html>Ion purity score threshold");

    UiSpinnerDouble uiSpinnerMinPercent = UiSpinnerDouble
        .builder(0.05, 0.0, 1.0, 0.05).setFormat(df3).setCols(5).create();
    FormEntry feMinPercent = fe(TmtiConfProps.PROP_min_percent,
        "Min Intensity (percent)", uiSpinnerMinPercent,
        "<html>Remove low intensity PSMs (e.g. value of 0.05 indicates removal <br/>\n"
            + "of PSMs with the summed TMT reporter ions intensity in the lowest 5% of <br/>\n"
            + "all PSMs)");

    UiSpinnerDouble uiSpinnerGlycoFilter = UiSpinnerDouble
            .builder(-1, -1, 1.0, 0.01).setFormat(df3).setCols(5).create();
    FormEntry feGlycoFilter = fe(TmtiConfProps.PROP_glyco_qval,
            "Glycan FDR filter", uiSpinnerGlycoFilter,
            "<html>(optional) Remove PSMs not passing glycan FDR at specified level (q-value) <br/>\n"
                    + "Set to -1 to ignore. Requires glycan assignment from PTM-Shepherd. <br/>\n");

    UiCheck uiCheckGlycanComposition = new UiCheck("Index Glycan Composition", null, false);
    FormEntry feGlycoComposition = fe(TmtiConfProps.PROP_use_glycan_composition,
            "not-shown", uiCheckGlycanComposition,
            "<html>For multi-mass report, index by glycan composition instead of mass <br/>\n"
                    + "to separate isomeric glycan compositions. Requires glycan assignment from PTM-Shepherd. <br/>\n");

    UiSpinnerInt uiSpinnerMinNtt = UiUtils.spinnerInt(0, 0, 1000, 1).setCols(5).create();
    FormEntry feMinNtt = mu.feb(uiSpinnerMinNtt).name(TmtiConfProps.PROP_min_ntt).label("Min NTT")
        .tooltip("Minimum allowed number of enzymatic termini").create();

    UiText uiTextProtExclude = UiUtils.uiTextBuilder().cols(10).text("none").create();
    FormEntry feProtExclude = fe(TmtiConfProps.PROP_prot_exclude,
        "Exclude proteins", uiTextProtExclude,
        "<html>Exclude proteins with the specified tag(s) at the beginning of the accession <br/>\n"
            + "number (e.g. none: no exclusion; sp|,tr| : exclude protein with sp| or tr|)");

    UiCombo uiComboUniquePep = UiUtils.createUiCombo(TmtiConfProps.COMBO_PEPTIDE_PROTEIN_UNIQUENESS.stream()
        .map(ComboValue::getValInUi).collect(Collectors.toList()));
    FormEntry feUniquePep = fe(TmtiConfProps.PROP_unique_pep,
        "Peptide-Protein uniqueness", uiComboUniquePep,
        "<html>Unique only: use peptides that are unique or confidently assigned to a<br/>\n"
            + "single protein (or a single indistinguishable protein group) <br/>\n"
            + "<br/>\n"
            + "Unique+Razor: use 'unique plus razor' approach, with each shared <br/>\n"
            + "peptide assigned as razor to one protein (as classified by Philosopher <br/>\n"
            + "and defined in psm.tsv file) <br/>\n");

    UiCombo uiComboAggregationMethod = UiUtils.createUiCombo(TmtiConfProps.COMBO_AGGREGATION_METHOD.stream().map(ComboValue::getValInUi).collect(Collectors.toList()));
    FormEntry feAggregationMethod = fe(TmtiConfProps.PROP_aggregation_method, 
        "Aggregation method", uiComboAggregationMethod,
        "<html>Method to aggregate PSM abundances<br/>\n"
            + "Median: use median<br/>\n"
            + "Weighted: use precursor intensity-weighted abundances <br/>\n");

    UiCheck uiCheckBestPsm = new UiCheck("Best PSM", null, true);
    FormEntry feBestPsm = fe(TmtiConfProps.PROP_best_psm,
        "not-shown", uiCheckBestPsm,
        "<html>Keep the best PSM only (highest summed TMT intensity) among all <br/>\n"
            + "redundant PSMs within the same LC-MS run");

    UiCheck uiCheckPsmNorm = new UiCheck("PSM norm", null, false);
    FormEntry fePsmNorm = fe(TmtiConfProps.PROP_psm_norm, "not-shown", uiCheckPsmNorm,
        "Perform additional retention time-based normalization at the PSM level");

    UiCheck uiCheckOutlierRemoval = UiCheck.of("Outlier removal", true);
    FormEntry feOutlierRemoval = fe(TmtiConfProps.PROP_outlier_removal,
        "not-shown", uiCheckOutlierRemoval,
        "<html>Perform additional retention time-based normalization at the PSM level");

    UiCheck uiCheckAllowOverlabel = UiCheck.of("Allow overlabel", true);
    FormEntry feAllowOverlabel = fe(TmtiConfProps.PROP_allow_overlabel,
        "not-shown", uiCheckAllowOverlabel,
        "<html>Allow PSMs with TMT on S (when overlabeling on S was allowed in the database search)");

    UiCheck uiCheckAllowUnlabeled = UiCheck.of("Allow unlabeled", true);
    FormEntry feAllowUnlabeled = fe(TmtiConfProps.PROP_allow_unlabeled,
        "not-shown", uiCheckAllowUnlabeled,
        "<html>Allow peptides with unlabeled n-term (i.e. no TMT/iTRAQ label and no Acetyl at n-terminus)");

    UiCheck uiCheckMs1Int = UiCheck.of("Use MS1 intensity", true);
    FormEntry feMs1Int = fe(TmtiConfProps.PROP_ms1_int,
        "not-shown", uiCheckMs1Int,
        "<html>Use MS1 precursor ion intensity (if true) or MS2 summed TMT <br/>\n"
            + "reporter ion intensity (if false) as part of the reference <br/>\n"
            + "sample abundance estimation");

    UiCheck uiCheckTop3 = UiCheck.of("Top 3 ions", true);
    FormEntry feTop3 = fe(TmtiConfProps.PROP_top3_pep,
        "not-shown", uiCheckTop3,
        "<html>Use top 3 most intense peptide ions as part of the reference <br/>\n"
            + "sample abundance estimation");

    UiCheck uiCheckPrintRef = UiCheck.of("Print reference intensity", false);
    FormEntry fePrintRefInt = fe(TmtiConfProps.PROP_print_RefInt,
        "not-shown", uiCheckPrintRef,
        "<html>Print individual reference sample intensities");

    UiSpinnerDouble uiSpinnerMinBestPepProb = UiSpinnerDouble
        .builder(0, 0, 1.0, 0.1).setFormat(df2).setCols(5).create();
    FormEntry feMaxPepProb = mu
        .feb(TmtiConfProps.PROP_max_pep_prob_thres, uiSpinnerMinBestPepProb)
        .label("Min best peptide probability").create();

    JPanel p = mu.newPanel(mu.lcNoInsetsTopBottom());
    mu.border(p, "Filtering and normalization");

    mu.add(p, feUniqueGene.label(), mu.ccR());
    mu.add(p, feUniqueGene.comp).split().spanX();
    mu.add(p, feUniquePep.label(), mu.ccR());
    mu.add(p, feUniquePep.comp).wrap();
    mu.add(p, feMinPsmProb.label(), mu.ccR());
    mu.add(p, feMinPsmProb.comp);
    mu.add(p, feMinPurity.label(), mu.ccR());
    mu.add(p, feMinPurity.comp);
    mu.add(p, feMinPercent.label(), mu.ccR());
    mu.add(p, feMinPercent.comp);
    mu.add(p, feGlycoFilter.label(), mu.ccR());
    mu.add(p, feGlycoFilter.comp).wrap();
    mu.add(p, feMaxPepProb.label(), mu.ccR());
    mu.add(p, feMaxPepProb.comp);
    mu.add(p, feMinNtt.label(), mu.ccR());
    mu.add(p, feMinNtt.comp);
    mu.add(p, feAggregationMethod.label(), mu.ccR());
    mu.add(p, feAggregationMethod.comp).spanX().wrap();

    JPanel pChecks = mu.newPanel(mu.lcNoInsetsTopBottom());
    mu.add(pChecks, feBestPsm.comp);
    mu.add(pChecks, fePsmNorm.comp);
    mu.add(pChecks, feAllowOverlabel.comp);
    mu.add(pChecks, feAllowUnlabeled.comp);
    mu.add(pChecks, feOutlierRemoval.comp);
    mu.add(pChecks, feGlycoComposition.comp).wrap();

    mu.add(p, pChecks).spanX().wrap();

    mu.add(p, feProtExclude.label(), mu.ccR());
    mu.add(p, feProtExclude.comp).spanX().growX().wrap();

    JPanel p2 = mu.newPanel(mu.lcNoInsetsTopBottom());
    mu.border(p2, "Ratio to Abundance conversion");
    mu.add(p2, feMs1Int.comp);
    mu.add(p2, feTop3.comp);
    mu.add(p2, fePrintRefInt.comp).spanX().wrap();

    return new JPanel[]{p, p2};
  }

  private JPanel createPanelOptsAdvancedPtm() {
    JPanel p = new JPanel(new MigLayout(new LC()));//.debug()));
    mu.border(p, "PTMs");

    DecimalFormat df2 = new DecimalFormat("#.##");

    UiSpinnerDouble uiSpinnerMinSiteProb = UiSpinnerDouble
        .builder(-1, -1, 1.0, 0.1).setFormat(df2).setCols(5).create();
    FormEntry feMinSiteProb = fe(TmtiConfProps.PROP_min_site_prob,
        "Min site probability", uiSpinnerMinSiteProb,
        "<html>site localization confidence threshold <br/>\n"
            + "-1: global <br/>\n"
            + "0: as determined by the search engine <br/>\n"
            + "above 0 (e.g., 0.75): min PTMProphet site probability");

    UiText uiTextModTag = UiUtils.uiTextBuilder().cols(10).text("none").create();
    FormEntry feModTag = fe(TmtiConfProps.PROP_mod_tag,
        "Mod tag", uiTextModTag,
        "<html>PTM tag for generating PTM-specific reports <br/>\n"
            + "none: global data<br/>\n"
            + "S(79.9663),T(79.9663),Y(79.9663): phospho<br/>\n"
            + "K(114.0429),K(343.2059): ubiquitin");

    mu.add(p, feModTag.label());
    mu.add(p, feModTag.comp).pushX().spanX().wrap();
    mu.add(p, feMinSiteProb.label(), mu.ccR());
    mu.add(p, feMinSiteProb.comp).pushX().spanX().wrap();

    return p;
  }

  private static FormEntry fe(String name, String label, JComponent comp, String tooltip) {
    return new FormEntry(name, label, comp, tooltip);
  }

  private static void addRowLabelCompComp(JPanel p, FormEntry fe1, FormEntry fe2) {
    if (fe1 != null) {
      mu.add(p, fe1.label(), mu.ccR());
      mu.add(p, fe1.comp);
      mu.add(p, fe2.comp).alignX("left").wrap();
    } else {
      mu.add(p, fe2.comp).skip(2).wrap();
    }
  }

  private static void addRowLabelComp(JPanel p, FormEntry fe1) {
    mu.add(p, fe1.label(), mu.ccR());
    mu.add(p, fe1.comp).wrap();
  }

  public QuantLabel getSelectedLabel() {
    String name  = (String) uiComboLabelNames.getSelectedItem();
    Optional<QuantLabel> label = QuantLabel.LABELS.stream()
        .filter(ql -> ql.getName().equalsIgnoreCase(name)).findFirst();
    if (!label.isPresent()) {
      throw new IllegalStateException("Unknown label name in combo box: " + name);
    }
    return label.get();
  }

  private void updateUiOnCheckRunStateChange(boolean isRun) {
    enablementMapping.put(pContent, isRun);
    updateEnabledStatus(pContent, isRun);
  }

  public int getNumChannels() {
    return getSelectedLabel().getReagentNames().size();
  }

  public Map<LcmsFileGroup, Path> getAnnotations() {
    ArrayList<ExpNameToAnnotationFile> annotations = tmtAnnotationTable.fetchModel()
        .dataCopy();
    Map<LcmsFileGroup, Path> map = new TreeMap<>();
    for (ExpNameToAnnotationFile row : annotations) {
      map.put(new LcmsFileGroup(row.expName, new ArrayList<>(row.lcmsFiles)), Paths.get(row.getPath()));
    }
    return map;
  }

  public String getQuantLevel() {
    return uiComboQuantLevel.asString();
  }

  public static class TmtAnnotationValidationException extends Exception {

    public TmtAnnotationValidationException(String message) {
      super(message);
    }

    public TmtAnnotationValidationException(String message, Throwable cause) {
      super(message, cause);
    }

    public TmtAnnotationValidationException(Throwable cause) {
      super(cause);
    }
  }

  public static List<QuantLabelAnnotation> parseTmtAnnotationFile(File file)
      throws IOException {
    String encoding = UniversalDetector.detectCharset(file);
    if (encoding == null) {
      throw new IOException("Cannot detect the encoding of " + file.getAbsolutePath());
    }

    Charset charset;
    try {
      charset = Charset.forName(encoding);
    } catch (Exception ex) {
      throw new IOException("Do not support the encoding (" + encoding + ") of  " + file.getAbsolutePath());
    }

    List<String> lines = Files.readAllLines(file.toPath(), charset).stream()
        .filter(l -> !StringUtils.isNullOrWhitespace(l)).collect(Collectors.toList());

    List<QuantLabelAnnotation> annotations = new ArrayList<>();
    for (String line : lines) {
      line = line.trim().replaceAll("[^a-zA-Z0-9\\s]", "");
      if (StringUtils.isNullOrWhitespace(line))
        continue;
      String[] split = line.split("\\s+", 2);
      if (split.length > 1) {
        annotations.add(new QuantLabelAnnotation(split[0].trim(), split[1].trim()));
      } else {
        annotations.add(new QuantLabelAnnotation(split[0].trim(), "NA"));
      }
    }
    return annotations;
  }

  private static QuantLabel validateAnnotations(List<QuantLabelAnnotation> annoations)
      throws TmtAnnotationValidationException {

    List<QuantLabel> matching = QuantLabel.LABELS.stream()
        .filter(ql -> ql.getReagentNames().size() == annoations.size())
        .filter(ql -> ql.getReagentNames().containsAll(annoations.stream()
            .map(QuantLabelAnnotation::getLabel).collect(Collectors.toList())))
        .collect(Collectors.toList());
    if (matching.isEmpty()) {
      Set<String> knownLabelNames = QuantLabel.LABELS.stream()
          .flatMap(ql -> ql.getReagentNames().stream())
          .collect(Collectors.toSet());
      String unknownLabelNames = annoations.stream()
          .filter(qla -> !knownLabelNames.contains(qla.getLabel()))
          .map(QuantLabelAnnotation::getLabel).distinct().collect(Collectors.joining(", "));
      throw new TmtAnnotationValidationException("No known quant label types match labeling\n"
          + "reagent names in given annotations."
          + (unknownLabelNames.isEmpty() ? "" : ("\nUnknown label names: " + unknownLabelNames)));
    }
    if (matching.size() > 1) {
      throw new TmtAnnotationValidationException("Multiple known quant label types match labeling\n"
          + "reagent names in given annotations:\n" + matching.stream()
          .map(QuantLabel::getName).collect(Collectors.joining(", ")));
    }
    return matching.get(0);
  }

  public boolean isRun() {
    return SwingUtils.isEnabledAndChecked(checkRun);
  }

  public boolean isRunFqLq() {
    return isRun() && !uiCheckDontRunFqLq.isSelected();
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteConfigTmtI m) {
    updateEnabledStatus(this, m.isValid());
  }

  @Subscribe
  public void on(MessageLoadTmtIntegratorDefaults m) {
    log.debug("Got MessageLoadTmtIntegratorDefaults, it's an empty marker message");
    final Map<String, String> map = TmtiConfig.getDefaultAsMap();
    log.debug("Read tmt-i default props props: {}", map);
    HashMap<String, String> prefixed = new HashMap<>();
    map.forEach((k, v) -> {
      String vConvertedToUi = CONVERT_TO_GUI.getOrDefault(k, Function.identity()).apply(v);
      prefixed.put(StringUtils.prependOnce(k, PREFIX), vConvertedToUi);
    });
    SwingUtils.valuesSet(this, prefixed);
  }

  @Subscribe(threadMode =  ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsFilesList m) {
    if (m.type == MessageType.REQUEST)
      return;

    final Map<String, ExpNameToAnnotationFile> oldRows = tmtAnnotationTable.fetchModel().dataCopy().stream()
        .collect(Collectors.toMap(row -> row.expName, row -> row));

    // newly added files need to be added to corresponding rows
    Map<String, List<InputLcmsFile>> filesByExp = m.files.stream()
        .collect(Collectors.groupingBy(InputLcmsFile::getExperiment));
    List<ExpNameToAnnotationFile> newRows = new ArrayList<>();

    try {
      for (Entry<String, List<InputLcmsFile>> e : filesByExp.entrySet()) {
        String expName = e.getKey();
        List<InputLcmsFile> files = e.getValue();
        ExpNameToAnnotationFile newRow = new ExpNameToAnnotationFile(expName, files,
            STRING_NO_PATH_SET);
        ExpNameToAnnotationFile oldRow = oldRows.get(expName);
        if (oldRow != null) {
          newRow.setPath(oldRow.getPath());
        } else {
          // maybe there already is annotation.txt file?
          List<Path> fileDirs = Seq.seq(e.getValue())
              .map(lcms -> lcms.getPath().getParent()).distinct().toList();
          if (fileDirs.size() == 1) {
            // only if all files are in the same directory, we'll try to auto-detect annotations file
            List<Path> annotations = PathUtils.findFilesQuietly(fileDirs.get(0),
                p -> p.getFileName().toString().endsWith("annotation.txt"))
                .collect(Collectors.toList());
            if (annotations.size()
                == 1) {// this is in case the predicate is changed such that it can match multiple files
              newRow.setPath(annotations.get(0).toString());
            }
          }
        }
        newRows.add(newRow);
      }
    } catch (Exception e) {
      log.debug("Something happened while processing LCMS files events in TMTi", e);
    }

    tmtAnnotationTable.fetchModel().dataClear();
    tmtAnnotationTable.fetchModel().dataAddAll(newRows);

  }

  private Path computePlexDir(String expName, Set<InputLcmsFile> lcmsFiles) {
    List<Path> dirs = lcmsFiles.stream().map(f -> f.getPath().getParent()).distinct()
        .collect(Collectors.toList());
    if (dirs.size() > 1) {
      String m = String
          .format("Not all LCMS files in experiment '%s' are in the same folder.\n"
                  + "All LCMS files from the same plex should be in one directory:\n"
                  + "%s",
              expName,
              lcmsFiles.stream().map(f -> f.getPath().toString()).collect(Collectors.joining("\n")));
      SwingUtils.showWarningDialog(this, m, "Not all LCMS files in same dir");
      return null;
    }
    Path saveDir = null;
    if (dirs.size() == 1) {
      saveDir = dirs.get(0);
    } else {
      log.error("There were no LCMS files in the experiment for TMT annotation file selection. Should not happen, report to developers.");
      return null;
    }
    return saveDir;
  }

  public void actionBrowse(ActionEvent e) {
    int modelRow = Integer.parseInt( e.getActionCommand() );
    log.debug("Browse action running in TMT, model row number: {}", modelRow);
    final ExpNameToAnnotationFile row = tmtAnnotationTable.fetchModel().dataGet(modelRow);

    Path expDir = computePlexDir(row.getExpName(), row.lcmsFiles);
    if (expDir == null) {
      log.error("No LCMS files in experiment '{}' when browsing for TMT annotation file. Should not happen", row.getExpName());
      return;
    }

    JFileChooser fc = new JFileChooser();
    fc.setAcceptAllFileFilterUsed(true);
    fc.setMultiSelectionEnabled(false);
    Optional<Path> exisitngFile = Stream
        .of(row.path,
            expDir.toString(),
            ThisAppProps.load(PROP_LAST_ANNOTATION_PATH),
            ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN))
        .map(path -> {
          try {
            return StringUtils.isNullOrWhitespace(path) ? null : Paths.get(path);
          } catch (Exception ignore) {
            return null;
          }
        })
        .filter(Objects::nonNull).filter(path -> Files.exists(path))
        .findFirst();

    exisitngFile.ifPresent(path -> fc.setCurrentDirectory(path.toFile()));
    if (exisitngFile.isPresent() && Files.isRegularFile(exisitngFile.get())) {
      fc.setSelectedFile(exisitngFile.get().toFile());
    }

    fc.setFileFilter(new javax.swing.filechooser.FileFilter() {
      @Override
      public boolean accept(File f) {
        return Files.isDirectory(f.toPath()) || f.toPath().getFileName().toString().endsWith("annotation.txt");
      }

      @Override
      public String getDescription() {
        return "Only *annotation.txt files are allowed";
      }
    });

    final int answer = fc.showDialog(TmtiPanel.this.scrollPaneTmtTable, "Select");
    if (JOptionPane.OK_OPTION == answer) {
      File selectedFile = fc.getSelectedFile();
      List<QuantLabelAnnotation> annotations;
      QuantLabel matchingLabel;
      try {
        annotations = parseTmtAnnotationFile(selectedFile);
        matchingLabel = validateAnnotations(annotations);
      } catch (TmtAnnotationValidationException | IOException ex) {
        SwingUtils.showErrorDialogWithStacktrace(ex, TmtiPanel.this.scrollPaneTmtTable, false);
        return;
      }

      if (!expDir.equals(selectedFile.toPath().getParent())) {
        String m = String.format(
            "Current implementation requires the annotation file to be\n"
                + "in the same directory as corresponding LCMS files.\n"
                + "Please select or create a file in:\n\n%s", expDir);
        SwingUtils.showWarningDialog(this, m, "Bad annotation file path");
        return;
      }

      // maybe update selected Label Type (aka number of channels)?
      if (!getSelectedLabel().getName().equalsIgnoreCase(matchingLabel.getName())) {
        int confirmation = SwingUtils.showConfirmDialog(TmtiPanel.this,
            new JLabel(String.format("<html>Loaded file looks to be for %s.<br/>\n"
                + "Load configuration preset for that label?", matchingLabel.getName())));
        if (JOptionPane.OK_OPTION == confirmation) {
          log.debug("User selected to load config preset for known label");
          uiComboLabelNames.setSelectedItem(matchingLabel.getName());
        }
      }

      // copy the file
      Path selectedPath = selectedFile.toPath();
      Path dest = expDir.resolve(selectedPath.getFileName());
      if (!selectedPath.getParent().equals(expDir)) {
        try {
          Files.copy(selectedPath, dest, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ex) {
          throw new IllegalStateException(ex);
        }
      }

      // save the path
      ThisAppProps.save(PROP_LAST_ANNOTATION_PATH, selectedFile);

      row.setPath(dest.toString());
      tmtAnnotationTable.fetchModel().fireTableDataChanged();
    }
  }

  public void actionEditCreate(ActionEvent e) {
    int modelRowIndex = Integer.parseInt( e.getActionCommand() );
    log.debug("Create action running in TMT, model row number: {}", modelRowIndex);
    ExpNameToAnnotationFile row = tmtAnnotationTable.fetchModel().dataGet(modelRowIndex);
    String pathInRow = row.getPath();

    Path saveDir = computePlexDir(row.getExpName(), row.lcmsFiles);
    if (saveDir == null) {
      log.error("computePlexDir returned null");
      return;
    }

    Path existingPath = null;
    try {
      if (Files.exists(Paths.get(pathInRow))) existingPath = Paths.get(pathInRow);
    } catch (Exception ignore) {}

    List<QuantLabelAnnotation> quantLabelAnnotations = null;
    if (existingPath != null) {
      try {
        quantLabelAnnotations = parseTmtAnnotationFile(existingPath.toFile());
      } catch (IOException ex) {
        log.warn("Could not parse annotation file", ex);
      }
    }

    JFrame parent = SwingUtils.findParentFrame(TmtiPanel.this);
    String labelName = (String) uiComboLabelNames.getSelectedItem();
    QuantLabelAnnotationDialog d = new QuantLabelAnnotationDialog(parent, row, labelName, quantLabelAnnotations);
    log.debug("Dialog table model:\n{}", d.getModel().dataCopy().stream()
        .map(QuantLabelAnnotation::toString).collect(Collectors.joining("\n")));
    d.setVisible(true);
    if (d.getDialogResult() != JOptionPane.OK_OPTION) {
      return;
    }

    // get new data
    ArrayList<QuantLabelAnnotation> userAnnotations = d.getModel().dataCopy();

    // select path to save file to
    Path selectedPath = null;
    Path savePath = null;

    // ask user
    while (selectedPath == null) {
      JFileChooser fc = new JFileChooser();
      fc.setDialogTitle("Save annotation to a file");
      String suggestedPath = Stream
          .of(existingPath != null ? saveDir.resolve(existingPath.getFileName()).toString() : saveDir.toString(),
              ThisAppProps.load(TmtiPanel.PROP_LAST_ANNOTATION_PATH),
              ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN))
          .filter(Objects::nonNull).findFirst().orElse(null);
      FileChooserUtils.setPath(fc, suggestedPath);
      fc.setCurrentDirectory(Paths.get(suggestedPath).toFile());

      fc.setFileFilter(new javax.swing.filechooser.FileFilter() {
        @Override
        public boolean accept(File f) {
          return Files.isDirectory(f.toPath()) || f.toPath().getFileName().toString().endsWith("annotation.txt");
        }

        @Override
        public String getDescription() {
          return "Only *annotation.txt files are allowed";
        }
      });

      int userSelection = fc.showSaveDialog(parent);
      if (JFileChooser.APPROVE_OPTION == userSelection) {
        selectedPath = fc.getSelectedFile().toPath();
        if (!selectedPath.getParent().equals(saveDir)) {
          String msg = "<html>Current implementation requires annotation files to be saved<br/>\n"
              + "in the same directory as LCMS files for that plex. Please save the file in:<br/>\n<br/>\n" + saveDir;
          String htmlMsg = SwingUtils.makeHtml(msg);

          SwingUtils.showWarningDialog(this,
              htmlMsg,
              "Select different location");
          selectedPath = null;
          continue;
        }
        log.debug("User selected to save annotattion in file: {}", selectedPath);
      } else {
        log.debug("User selected NOT to save annotattion in file");
        return; // user chose not to save file
      }

      if (!selectedPath.toString().endsWith("annotation.txt")) {
        selectedPath = Paths.get(selectedPath.toAbsolutePath() + "_annotation.txt");
      }

      // ask about overwriting
      if (Files.exists(selectedPath)) {
        int answer = SwingUtils.showConfirmDialog(parent, new JLabel("<html>Overwrite existing file?<br/>\n"
            + selectedPath));
        if (JOptionPane.NO_OPTION == answer) {
          continue;
        }
        if (JOptionPane.CANCEL_OPTION == answer) {
          break;
        }
        if (JOptionPane.OK_OPTION == answer) {
          try {
            Files.deleteIfExists(selectedPath);
            savePath = selectedPath;
          } catch (IOException ex) {
            SwingUtils.showErrorDialogWithStacktrace(ex, parent);
            log.warn("Something happened while deleting file", ex);
          }

        }
      } else {
        savePath = selectedPath;
      }
    }

    if (savePath == null) {
      log.debug("User chose not to select or not to overwrite annotation file");
      return;
    }

    // write file
    List<String> userAnnotationsList = userAnnotations.stream()
        .map(qla -> qla.getLabel() + " " + qla.getSample())
        .collect(Collectors.toList());
    try {
      Files.write(savePath, userAnnotationsList, StandardOpenOption.CREATE_NEW);
      ThisAppProps.save(PROP_LAST_ANNOTATION_PATH, savePath.toFile());
    } catch (IOException ex) {
      SwingUtils.showErrorDialogWithStacktrace(ex, parent);
    }

    row.setPath(savePath.toString());
    //tmtAnnotationTable.fetchModel().dataSet(modelRowIndex, row);
    tmtAnnotationTable.fetchModel().fireTableDataChanged();
  }

  private static List<String> tryReadFileFully(String path) {
    try {
      return Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8);
    } catch (Exception e) {
      log.warn("Could not fully read file", e);
    }
    return null;
  }

  public Map<String, String> formToConfig(int ramGb, String pathTmtiJar, String pathFasta, String pathOutput) {
    Map<String, String> map = SwingUtils.valuesGet(this, null);
    final Map<String, String> mapConv = new HashMap<>();
    map.forEach((k, v) ->
    {
      String prop = StringUtils.afterLastDot(k);
      if (!TmtiConfProps.PROPS.contains(prop)) {
        return; // skip values that are not officially supported in config file
      }
      if (prop.contentEquals("mod_tag")) {
        String t = CONVERT_TO_FILE.getOrDefault(prop, Function.identity()).apply(v);
        if (t.isEmpty()) {
          mapConv.put(prop, "none");
        } else {
          mapConv.put(prop, t);
        }
      } else {
        mapConv.put(prop, CONVERT_TO_FILE.getOrDefault(prop, Function.identity()).apply(v));
      }
    });

    mapConv.put("path", pathTmtiJar);
    mapConv.put("memory", Integer.toString(ramGb));
    mapConv.put("protein_database", pathFasta);
    mapConv.put("output", pathOutput);

    return mapConv;
  }

  public void writeConfig(Writer w, Map<String, String> map) {
    try {
      TmtiConfig.write(map, w);
    } catch (IOException e) {
      log.error("Error writing TMT-Integrator config", e);
      throw new IllegalStateException(e);
    }
  }
}
