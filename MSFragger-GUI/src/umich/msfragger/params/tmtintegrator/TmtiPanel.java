package umich.msfragger.params.tmtintegrator;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.JPanelBase;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
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
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.gui.dialogs.QuantLabelAnnotationDialog;
import umich.msfragger.gui.renderers.ButtonColumn;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesList;
import com.dmtavt.fragpipe.messages.MessageLoadTmtIntegratorDefaults;
import com.dmtavt.fragpipe.messages.MessageTmtIntegratorRun;
import com.dmtavt.fragpipe.messages.MessageType;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.tmtintegrator.TmtAnnotationTable.ExpNameToAnnotationFile;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;

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
  private JPanel pOpts;
  private Action actionBrowse;
  private Action actionCreate;
  private ButtonColumn colBrowse;
  private ButtonColumn colCreate;
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
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_prot_norm, s -> findOrThrow(false, TmtiConfProps.COMBO_PROT_NORM,
        s,supplyRunEx("No matching prot_norm value")).getValInConfig());
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_unique_gene, s -> findOrThrow(false, TmtiConfProps.COMBO_UNIQUE_GENE,
        s,supplyRunEx("No matching unique_gene value")).getValInConfig());
    CONVERT_TO_FILE.put(TmtiConfProps.PROP_add_Ref, s -> findOrThrow(false, TmtiConfProps.COMBO_ADD_REF,
        s,supplyRunEx("No matching add_Ref value")).getValInConfig());

    CONVERT_TO_GUI.put(TmtiConfProps.PROP_channel_num, s -> {
      int numChannels = Integer.parseInt(s);
      QuantLabel label = QuantLabel.LABELS.stream()
          .filter(ql -> ql.getReagentNames().size() == numChannels)
          .findFirst().orElseThrow(supplyRunEx("No matching quant label"));
      return label.getName();
    });
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_groupby, s -> findOrThrow(true, TmtiConfProps.COMBO_GROUP_BY,
        s,supplyRunEx("No matching groupby value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_prot_norm, s -> findOrThrow(true, TmtiConfProps.COMBO_PROT_NORM,
        s,supplyRunEx("No matching prot_norm value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_unique_gene, s -> findOrThrow(true, TmtiConfProps.COMBO_UNIQUE_GENE,
        s,supplyRunEx("No matching unique_gene value")).getValInUi());
    CONVERT_TO_GUI.put(TmtiConfProps.PROP_add_Ref, s -> findOrThrow(true, TmtiConfProps.COMBO_ADD_REF,
        s,supplyRunEx("No matching add_Ref value")).getValInUi());
  }

  private UiText uiTextLabelquant;
  private UiText uiTextFreequant;

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
    mu.border(this, "TMT Qunatitation");

    pTop = createPanelTop();
    pContent = createPanelContent();
    pTable = createPanelTable();
    pOpts = createPanelOpts();

    mu.add(pContent, pTable).growX();
    mu.add(pContent, pOpts).alignY("top").growX().wrap();

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

    mu.add(p, checkRun);
    mu.add(p, btnLoadDefaults).pushX().wrap();

    return p;
  }

  private JPanel createPanelContent() {
    JPanel p = mu.newPanel(mu.lcFillX());
    mu.borderEmpty(p);
    return p;
  }

  private JPanel createPanelTable() {
    JPanel p = new JPanel(new BorderLayout());
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

    p.add(new JLabel("TMT Annotations (rows will be filled when you assign LCMS files to experiments)"), BorderLayout.NORTH);
    tmtAnnotationTable.fireInitialization();
    tmtAnnotationTable.setFillsViewportHeight(false);
    scrollPaneTmtTable = new JScrollPane();
    scrollPaneTmtTable.setViewportView(tmtAnnotationTable);

    p.add(scrollPaneTmtTable, BorderLayout.CENTER);

    return p;
  }


  private JPanel createPanelOpts() {
    JPanel p = new JPanel(new MigLayout(new LC()));//.debug()));
    mu.borderEmpty(p);

    // row 1
    {
      uiComboLabelNames = UiUtils.createUiCombo(
          QuantLabel.LABELS.stream().map(QuantLabel::getName).collect(Collectors.toList()));
      FormEntry feLabelType = fe(TmtiConfProps.PROP_channel_num,
          "Label type", uiComboLabelNames, null);

      UiCheck uiCheckUniquePep = new UiCheck("Unique pep", null, false);
      FormEntry feUniquePep = fe(TmtiConfProps.PROP_unique_pep,
          "not-shown", uiCheckUniquePep,
          "<html>Allow PSMs with unique peptides only (if true) or unique plus <br/>\n"
              + "razor peptides (if false), as classified by Philosopher and defined in PSM.tsv files");

      addPoptsRow(p, feLabelType, feUniquePep);
    }

    // row 2
    {
      UiText uiTextRefTag = UiUtils.uiTextBuilder().cols(10).text("Bridge").create();
      FormEntry feRefTag = fe(TmtiConfProps.PROP_ref_tag,
          "Ref sample tag", uiTextRefTag,
          "<html>Unique tag for identifying the reference channel (Bridge sample added to <br/>\n"
              + "each multiplex)");

      UiCheck uiCheckBestPsm = new UiCheck("Best PSM", null, true);
      FormEntry feBestPsm = fe(TmtiConfProps.PROP_best_psm,
          "not-shown", uiCheckBestPsm,
          "<html>Keep the best PSM only (highest summed TMT intensity) among all redundant PSMs <br/>\n"
              + "within the same LC-MS run");

      addPoptsRow(p, feRefTag, feBestPsm);
    }

    // row 3
    {
      UiCombo uiComboGroupBy = UiUtils.createUiCombo(TmtiConfProps.COMBO_GROUP_BY.stream()
          .map(ComboValue::getValInUi).collect(Collectors.toList()));
      FormEntry feGroupBy = fe(TmtiConfProps.PROP_groupby,
          "Group by", uiComboGroupBy,
          "<html>Level of data summarization(0: PSM aggregation to the gene level; 1: protein; <br/>\n"
              + "2: peptide sequence; 3: multiple PTM sites; 4: single PTM site; <br/>\n"
              + "-1: generate reports at all levels)");

      UiCheck uiCheckPsmNorm = new UiCheck("PSM norm", null, false);
      FormEntry fePsmNorm = fe(TmtiConfProps.PROP_psm_norm, "not-shown", uiCheckPsmNorm,
          "Perform additional retention time-based normalization at the PSM level");

      addPoptsRow(p, feGroupBy, fePsmNorm);
    }

    // row 4
    {
      UiCombo uiComboProtNorm = UiUtils.createUiCombo(TmtiConfProps.COMBO_PROT_NORM.stream()
          .map(ComboValue::getValInUi).collect(Collectors.toList()));
      FormEntry feProtNorm = fe(TmtiConfProps.PROP_prot_norm,
          "Prot norm", uiComboProtNorm,
          "<html>Normalization (0: None; 1: MD (median centering); 2: GN (median centering + <br/>\n"
              + "variance scaling); -1: generate reports with all normalization options)");

      UiCheck uiCheckOutlierRemoval = UiCheck.of("Outlier removal", true);
      FormEntry feOutlierRemoval = fe(TmtiConfProps.PROP_outlier_removal,
          "not-shown", uiCheckOutlierRemoval,
          "<html>Perform additional retention time-based normalization at the PSM level");

      addPoptsRow(p, feProtNorm, feOutlierRemoval);
    }

    // row 5
    {
      UiCombo uiComboUniqueGene = UiUtils.createUiCombo(TmtiConfProps.COMBO_UNIQUE_GENE.stream()
          .map(ComboValue::getValInUi).collect(Collectors.toList()));
      FormEntry feUniqueGene = fe(TmtiConfProps.PROP_unique_gene,
          "Unique gene", uiComboUniqueGene,
          "<html>0: allow all PSMs; 1: remove PSMs mapping to more than one GENE with evidence <br/>\n"
              + "of expression in the dataset; 2:remove all PSMs mapping to more than one GENE <br/>\n"
              + "in the fasta file");

      UiCheck uiCheckAllowOverlabel = UiCheck.of("Allow overlabel", true);
      FormEntry feAllowOverlabel = fe(TmtiConfProps.PROP_allow_overlabel,
          "not-shown", uiCheckAllowOverlabel,
          "<html>allow PSMs with TMT on S (when overlabeling on S was allowed in the database search)");

      addPoptsRow(p, feUniqueGene, feAllowOverlabel);
    }

    // row 6
    {
      UiCombo uiComboAddRef = UiUtils.createUiCombo(TmtiConfProps.COMBO_ADD_REF.stream()
          .map(ComboValue::getValInUi).collect(Collectors.toList()));
      FormEntry feAddRef = fe(TmtiConfProps.PROP_add_Ref,
          "Define reference", uiComboAddRef,
          "<html>add an artificial reference channel if there is no reference channel");

      UiCheck uiCheckAllowUnlabeled = UiCheck.of("Allow unlabeled", true);
      FormEntry feAllowUnlabeled = fe(TmtiConfProps.PROP_allow_unlabeled,
          "not-shown", uiCheckAllowUnlabeled,
          "<html>allow PSMs with TMT on S (when overlabeling on S was allowed <br/>\n"
              + "in the database search)");

      addPoptsRow(p, feAddRef, feAllowUnlabeled);
    }

    DecimalFormat df2 = new DecimalFormat("#.##");

    // row 7
    {
      UiSpinnerDouble uiSpinnerMinPepProb = UiSpinnerDouble
          .builder(0.9, 0.0, 1.0, 0.05).setFormat(df2).setNumCols(5).create();
      FormEntry feMinPepProb = fe(TmtiConfProps.PROP_min_pep_prob,
          "Min PSM probability", uiSpinnerMinPepProb,
          "<html>minimum PSM probability threshold (in addition to FDR-based <br/>\n"
              + "filtering by Philosopher)");

      UiCheck uiCheckMs1Int = UiCheck.of("Use MS1 intensity", true);
      FormEntry feMs1Int = fe(TmtiConfProps.PROP_ms1_int,
          "not-shown", uiCheckMs1Int,
          "<html>use MS1 precursor ion intensity (if true) or MS2 summed TMT <br/>\n"
              + "reporter ion intensity (if false) as part of the reference <br/>\n"
              + "sample abundance estimation");

      addPoptsRow(p, feMinPepProb, feMs1Int);
    }

    // row 8
    {
      UiSpinnerDouble uiSpinnerMinPurity = UiSpinnerDouble
          .builder(0.5, 0.0, 1.0, 0.05).setFormat(df2).setNumCols(5).create();
      FormEntry feMinPurity = fe(TmtiConfProps.PROP_min_purity,
          "Min purity", uiSpinnerMinPurity,
          "<html>ion purity score threshold");

      UiCheck uiCheckTop3 = UiCheck.of("Top 3 ions", true);
      FormEntry feTop3 = fe(TmtiConfProps.PROP_top3_pep,
          "not-shown", uiCheckTop3,
          "<html>use top 3 most intense peptide ions as part of the reference <br/>\n"
              + "sample abundance estimation");

      addPoptsRow(p, feMinPurity, feTop3);
    }

    // row 9
    {
      UiSpinnerDouble uiSpinnerMinPercent = UiSpinnerDouble
          .builder(0.5, 0.0, 1.0, 0.05).setFormat(df2).setNumCols(5).create();
      FormEntry feMinPercent = fe(TmtiConfProps.PROP_min_percent,
          "Min percent", uiSpinnerMinPercent,
          "<html>remove low intensity PSMs (e.g. value of 0.05 indicates removal <br/>\n"
              + "of PSMs with the summed TMT reporter ions intensity in the lowest 5% of <br/>\n"
              + "all PSMs)");

      UiCheck uiCheckPrintRef = UiCheck.of("Print ref int", false);
      FormEntry fePrintRefInt = fe(TmtiConfProps.PROP_print_RefInt,
          "not-shown", uiCheckPrintRef,
          "<html>print individual reference sample abundance estimates for each <br/>\n"
              + "multiplex in the final reports (in addition to the combined reference <br/>\n"
              + "sample abundance estimate)");

      addPoptsRow(p, feMinPercent, fePrintRefInt);
    }

    // rest of rows
    UiSpinnerDouble uiSpinnerMinSiteProb = UiSpinnerDouble
        .builder(-1, -1, 1.0, 0.1).setFormat(df2).setNumCols(5).create();
    FormEntry feMinSiteProb = fe(TmtiConfProps.PROP_min_site_prob,
        "Min site probability", uiSpinnerMinSiteProb,
        "<html>site localization confidence threshold (-1: for Global; <br/>\n"
            + "0: as determined by the search engine; above 0 (e.g. 0.75): PTMProphet <br/>\n"
            + "probability, to be used with phosphorylation only)");
    p.add(feMinSiteProb.label(), new CC().alignX("right"));
    p.add(feMinSiteProb.comp, new CC().alignX("left").spanX().wrap());

    UiText uiTextProtExclude = UiUtils.uiTextBuilder().cols(10).text("none").create();
    FormEntry feProtExclude = fe(TmtiConfProps.PROP_prot_exclude,
        "Exclude proteins", uiTextProtExclude,
        "exclude proteins with specified tags at the beginning of the accession <br/>\n"
            + "number (e.g. none: no exclusion; sp|,tr| : exclude protein with sp| or tr|)");
    p.add(feProtExclude.label(), new CC().alignX("right"));
    p.add(feProtExclude.comp, new CC().alignX("left").spanX().wrap());

    UiText uiTextModTag = UiUtils.uiTextBuilder().cols(10).text("none").create();
    FormEntry feModTag = fe(TmtiConfProps.PROP_mod_tag,
        "Mod tag", uiTextModTag,
        "<html>TM info for generation of PTM-specific reports <br/>\n"
            + "none: for Global data<br/>\n"
            + "S[167],T[181],Y[243]: for Phospho<br/>\n"
            + "K[170]: for K-Acetyl");
    p.add(feModTag.label(), new CC().alignX("right"));
    p.add(feModTag.comp, new CC().alignX("left").spanX().wrap());

    uiTextFreequant = new UiText("--ptw 0.4 --tol 10 --isolated");
    FormEntry feFreequant = fe("freequant", "Freequant opts", uiTextFreequant,
        "Command line options for Philosopher Freequant command");
    uiTextLabelquant = new UiText("--purity 0.5 --tol 10");
    FormEntry feLabelquant = fe("labelquant", "Labelquant opts", uiTextLabelquant,
        "Command line options for Philosopher Labelquant command");
    p.add(feLabelquant.label(), new CC().alignX("right"));
    p.add(feLabelquant.comp, new CC().alignX("left").spanX().growX().wrap());
    p.add(feFreequant.label(), new CC().alignX("right"));
    p.add(feFreequant.comp, new CC().alignX("left").spanX().growX().wrap());

    return p;
  }

  private static FormEntry fe(String name, String label, JComponent comp, String tooltip) {
    return new FormEntry(name, label, comp, tooltip);
  }

  private static void addPoptsRow(JPanel pOpts, FormEntry fe1, FormEntry fe2) {
    pOpts.add(fe1.label(), new CC().alignX("right"));
    pOpts.add(fe1.comp, new CC().alignX("left"));
    pOpts.add(fe2.comp, new CC().alignX("left").wrap());
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
    Map<LcmsFileGroup, Path> map = new HashMap<>();
    for (ExpNameToAnnotationFile row : annotations) {
      map.put(new LcmsFileGroup(row.expName, new ArrayList<>(row.lcmsFiles)), Paths.get(row.getPath()));
    }
    return map;
  }

  public String getLabelquantOptsAsText() {
    return uiTextLabelquant.getNonGhostText();
  }

  public String getFreequantOptsAsText() {
    return uiTextFreequant.getNonGhostText();
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
    List<String> lines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8).stream()
        .filter(l -> !StringUtils.isNullOrWhitespace(l)).collect(Collectors.toList());

    List<QuantLabelAnnotation> annotations = new ArrayList<>();
    for (String line : lines) {
      if (StringUtils.isNullOrWhitespace(line))
        continue;
      String[] split = line.split("[ ]+", 2);
      annotations.add(new QuantLabelAnnotation(split[0].trim(), split[1].trim()));
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
    return checkRun.isEnabled() && checkRun.isSelected();
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
    SwingUtils.valuesFromMap(this, prefixed);
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
    for (Entry<String, List<InputLcmsFile>> e : filesByExp.entrySet()) {
      String expName = e.getKey();
      List<InputLcmsFile> files = e.getValue();
      ExpNameToAnnotationFile newRow = new ExpNameToAnnotationFile(expName, files, STRING_NO_PATH_SET);
      ExpNameToAnnotationFile oldRow = oldRows.get(expName);
      if (oldRow != null) {
        newRow.setPath(oldRow.getPath());
      }
      newRows.add(newRow);
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

  public void actionBrowse(ActionEvent e)
  {
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

//      if (!expDir.equals(selectedFile.toPath().getParent())) {
//        String m = String.format(
//            "Current implementation requires the annotation file to be\n"
//                + "in the same directory as corresponding LCMS files.\n"
//                + "Please select or create a file in:\n%s", expDir);
//        SwingUtils.showWarningDialog(this, m, "Bad annotation file path");
//        return;
//      }

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
    while (selectedPath == null) {
      JFileChooser fc = new JFileChooser();
      fc.setDialogTitle("Specify a file to save");
      String suggestedPath = Stream
          .of(existingPath != null ? saveDir.resolve(existingPath.getFileName()).toString() : saveDir.toString(),
              ThisAppProps.load(TmtiPanel.PROP_LAST_ANNOTATION_PATH),
              ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN))
          .filter(Objects::nonNull).findFirst().orElse(null);
      FileChooserUtils.setPath(fc, suggestedPath);
      fc.setCurrentDirectory(Paths.get(suggestedPath).toFile());
      int userSelection = fc.showSaveDialog(parent);
      if (JFileChooser.APPROVE_OPTION == userSelection) {
        selectedPath = fc.getSelectedFile().toPath();
//        if (!selectedPath.getParent().equals(saveDir)) {
//          String msg = "<html>Current implementation requires annotation files to be saved<br/>\n"
//              + "in the same directory as LCMS files for that plex. Please save the<br/>\n"
//              + "file in:<br/>\n<br/>\n" + saveDir.toString();
//          String htmlMsg = SwingUtils.makeHtml(msg);
//
//          SwingUtils.showWarningDialog(this,
//              htmlMsg,
//              "Select different location");
//          selectedPath = null;
//          continue;
//        }
        log.debug("User selected to save annotattion in file: {}", selectedPath.toString());
      } else {
        log.debug("User selected NOT to save annotattion in file");
        return; // user chose not to save file
      }

      // ask about overwriting
      if (Files.exists(selectedPath)) {
        int answer = SwingUtils.showConfirmDialog(parent, new JLabel("<html>Overwrite existing file?<br/>\n"
            + selectedPath.toString()));
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
    Map<String, String> map = SwingUtils.valuesToMap(this);
    final Map<String, String> mapConv = new HashMap<>();
    map.forEach((k, v) ->
    {
      String prop = StringUtils.afterLastDot(k);
      if (!TmtiConfProps.PROPS.contains(prop)) {
        return; // skip values that are not officially supported in config file
      }
      mapConv.put(prop, CONVERT_TO_FILE.getOrDefault(prop, Function.identity()).apply(v));
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
