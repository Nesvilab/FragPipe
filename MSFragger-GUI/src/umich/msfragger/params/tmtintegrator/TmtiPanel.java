package umich.msfragger.params.tmtintegrator;

import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import com.github.chhh.utils.swing.UiUtils.UiTextBuilder;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
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
import umich.msfragger.gui.dialogs.QuantLabelAnnotationDialog;
import umich.msfragger.gui.renderers.ButtonColumn;
import umich.msfragger.messages.MessageLcmsFilesList;
import umich.msfragger.messages.MessageLoadTmtIntegratorDefaults;
import umich.msfragger.messages.MessageTmtIntegratorRun;
import umich.msfragger.messages.MessageType;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.tmtintegrator.TmtAnnotationTable.ExpNameToAnnotationFile;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.FormEntry;
import umich.msfragger.util.swing.JPanelWithEnablement;

public class TmtiPanel extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TmtiPanel.class);
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
  public static final String PREFIX = "ui.tmtintegrator.configfile.";
  public static final String PROP_LAST_ANNOTATION_PATH = "fragpipe.tmt.last-annotation-path";

  public TmtiPanel() {
    init();
    // register on the bus only after all the components have been created to avoid NPEs
    EventBus.getDefault().register(this);
    initPostCreation();
  }

  private void initPostCreation() {
    EventBus.getDefault().post(new MessageLcmsFilesList(MessageType.REQUEST, null));
    this.addHierarchyListener(e -> {
      log.debug("TMT-I addHierarchyListener - make sure it's selected / deselected when component shown");
      TmtiPanel.this.updateUiOnCheckRunStateChange(checkRun.isSelected());
    });
  }

  private void init() {
    this.setLayout(new BorderLayout());
//    this.setBorder(new EmptyBorder(0,0,0,0));
    this.setBorder(new TitledBorder("TMT Qunatitation"));

    // Top panel with run checkbox
    {
      // setting the insets allows the top panel to be shifted left of the options panel
      pTop = new JPanel(new MigLayout(new LC().insetsAll("0px")));//.debug()));

      checkRun = new UiCheck("Run TMT-Integrator", null, true);
      checkRun.setName("ui.name.downstream.run-tmtintegrator");
      checkRun.addActionListener(this::actionOnCheckboxIsRun);
      pTop.add(checkRun, new CC().alignX("left"));
      JButton btnLoadDefaults = new JButton("Load TMT-Integrator defaults");
      btnLoadDefaults.addActionListener((e) -> EventBus.getDefault().post(new MessageLoadTmtIntegratorDefaults(true)));
      pTop.add(btnLoadDefaults, new CC().alignX("left"));

      pTop.setBorder(new EmptyBorder(0,0,0,0));
      this.add(pTop, BorderLayout.NORTH);
    }

    // Main content panel - container
    {
      pContent = new JPanel(new MigLayout(new LC().fillX()));
      pContent.setBorder(new EmptyBorder(0,0,0,0));

      pContent.addPropertyChangeListener("enabled", evt -> {
        log.debug("Tmt pContent panel property '{}' changed from '{}' to '{}'", evt.getPropertyName(),
            evt.getOldValue(), evt.getNewValue());
        boolean newValue = (Boolean)evt.getNewValue();
        boolean isSwitchToEnabled = (Boolean) evt.getNewValue() && !(Boolean) evt.getOldValue();
        boolean pContentIsEnabled = newValue && checkRun.isSelected();
        log.debug("Tmt pContent panel is switching to enabled? : {}, !checkRun.isSelected() : {}, final state should be: {}",
            isSwitchToEnabled, !checkRun.isSelected(), pContentIsEnabled);
        enablementMapping.put(pContent, pContentIsEnabled);
        updateEnabledStatus(pContent, pContentIsEnabled);
      });

//      scroll = new JScrollPane(pContent);
//      scroll.setBorder(new EmptyBorder(0, 0, 0, 0));
//      scroll.getVerticalScrollBar().setUnitIncrement(16);
    }

    {
      //pTable = new JPanel(new MigLayout(new LC()));
      pTable = new JPanel(new BorderLayout());
      //pPeakPicking.setBorder(new TitledBorder("PTMShepherd options"));
      pTable.setBorder(new EmptyBorder(0, 0, 0, 0));

      tmtAnnotationTable = new TmtAnnotationTable();
      actionBrowse = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
          TmtiPanel.this.actionPerformedBrowse(e);
        }
      };

      actionCreate = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
          TmtiPanel.this.actionPerformedEditCreate(e);
        }
      };
      colBrowse = new ButtonColumn(tmtAnnotationTable, actionBrowse, 2);
      colCreate = new ButtonColumn(tmtAnnotationTable, actionCreate, 3);

      pTable.add(new JLabel("TMT Annotations"), BorderLayout.NORTH);
      tmtAnnotationTable.fireInitialization();
      tmtAnnotationTable.setFillsViewportHeight(false);
      scrollPaneTmtTable = new JScrollPane();
      scrollPaneTmtTable.setViewportView(tmtAnnotationTable);

      pTable.add(scrollPaneTmtTable, BorderLayout.CENTER);
      pContent.add(pTable, new CC().growX());
    }

    {
      pOpts = new JPanel(new MigLayout(new LC()));//.debug()));
//      pOpts = new JPanel(new BorderLayout());
      //pOpts.setBorder(new TitledBorder("TMT options"));
      pOpts.setBorder(new EmptyBorder(0, 0, 0, 0));


      // row 1
      {
        uiComboLabelNames = UiUtils.createUiCombo(
            QuantLabel.LABELS.stream().map(QuantLabel::getName).collect(Collectors.toList()));
        FormEntry feLabelType = fe(TmtiConfProps.PROP_channel_num,
            "Label type", uiComboLabelNames);

        UiCheck uiCheckUniquePep = new UiCheck("Unique pep", null, false);
        FormEntry feUniquePep = fe(TmtiConfProps.PROP_unique_pep,
            "not-shown", uiCheckUniquePep,
            "<html>Allow PSMs with unique peptides only (if true) or unique plus <br/>\n"
                + "razor peptides (if false), as classified by Philosopher and defined in PSM.tsv files");

        addPoptsRow(pOpts, feLabelType, feUniquePep);
      }

      // row 2
      {
        UiText uiTextRefTag = UiUtils.uiTextBuilder().cols(10).create();
        FormEntry feRefTag = fe(TmtiConfProps.PROP_ref_tag,
            "Ref tag", uiTextRefTag,
            "<html>Unique tag for identifying the reference channel (Bridge sample added to <br/>\n"
                + "each multiplex)");

        UiCheck uiCheckBestPsm = new UiCheck("Best PSM", null, true);
        FormEntry feBestPsm = fe(TmtiConfProps.PROP_best_psm,
            "not-shown", uiCheckBestPsm,
            "<html>Keep the best PSM only (highest summed TMT intensity) among all redundant PSMs <br/>\n"
                + "within the same LC-MS run");

        addPoptsRow(pOpts, feRefTag, feBestPsm);
      }

      // row 3
      {
        UiCombo uiComboGroupBy = UiUtils.createUiCombo(TmtiConfProps.GROUP_BY.stream()
            .map(ComboValue::getValInUi).collect(Collectors.toList()));
        FormEntry feGroupBy = fe(TmtiConfProps.PROP_groupby,
            "Group by", uiComboGroupBy,
            "<html>Level of data summarization(0: PSM aggregation to the gene level; 1: protein; <br/>\n"
                + "2: peptide sequence; 3: multiple PTM sites; 4: single PTM site; <br/>\n"
                + "-1: generate reports at all levels)");

        UiCheck uiCheckPsmNorm = new UiCheck("PSM norm", null, false);
        FormEntry fePsmNorm = fe(TmtiConfProps.PROP_psm_norm, "not-shown", uiCheckPsmNorm,
            "Perform additional retention time-based normalization at the PSM level");

        addPoptsRow(pOpts, feGroupBy, fePsmNorm);
      }

      // row 4
      {
        UiCombo uiComboProtNorm = UiUtils.createUiCombo(TmtiConfProps.PROT_NORM.stream()
            .map(ComboValue::getValInUi).collect(Collectors.toList()));
        FormEntry feProtNorm = fe(TmtiConfProps.PROP_prot_norm,
            "Prot norm", uiComboProtNorm,
            "<html>Normalization (0: None; 1: MD (median centering); 2: GN (median centering + <br/>\n"
                + "variance scaling); -1: generate reports with all normalization options)");

        UiCheck uiCheckOutlierRemoval = UiCheck.of("Outlier removal", true);
        FormEntry feOutlierRemoval = fe(TmtiConfProps.PROP_outlier_removal,
            "not-shown", uiCheckOutlierRemoval,
            "<html>Perform additional retention time-based normalization at the PSM level");

        addPoptsRow(pOpts, feProtNorm, feOutlierRemoval);
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

        addPoptsRow(pOpts, feUniqueGene, feAllowOverlabel);
      }

      // row 6
      {
        UiCombo uiComboAddRef = UiUtils.createUiCombo(TmtiConfProps.COMBO_ADD_REF.stream()
            .map(ComboValue::getValInUi).collect(Collectors.toList()));
        FormEntry feAddRef = fe(TmtiConfProps.PROP_add_Ref,
            "Add Ref", uiComboAddRef,
            "<html>add an artificial reference channel if there is no reference channel");

        UiCheck uiCheckAllowUnlabeled = UiCheck.of("Allow unlabeled", true);
        FormEntry feAllowUnlabeled = fe(TmtiConfProps.PROP_allow_unlabeled,
            "not-shown", uiCheckAllowUnlabeled,
            "<html>allow PSMs with TMT on S (when overlabeling on S was allowed <br/>\n"
                + "in the database search)");

        addPoptsRow(pOpts, feAddRef, feAllowUnlabeled);
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

        UiCheck uiCheckMs1Int = UiCheck.of("Allow unlabeled", true);
        FormEntry feMs1Int = fe(TmtiConfProps.PROP_ms1_int,
            "not-shown", uiCheckMs1Int,
            "<html>use MS1 precursor ion intensity (if true) or MS2 summed TMT <br/>\n"
                + "reporter ion intensity (if false) as part of the reference <br/>\n"
                + "sample abundance estimation");

        addPoptsRow(pOpts, feMinPepProb, feMs1Int);
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

        addPoptsRow(pOpts, feMinPurity, feTop3);
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

        addPoptsRow(pOpts, feMinPercent, fePrintRefInt);
      }

      // rest of rows
      UiSpinnerDouble uiSpinnerMinSiteProb = UiSpinnerDouble
          .builder(-1, -1, 1.0, 0.1).setFormat(df2).setNumCols(5).create();
      FormEntry feMinSiteProb = fe(TmtiConfProps.PROP_min_site_prob,
          "Min site probability", uiSpinnerMinSiteProb,
          "<html>site localization confidence threshold (-1: for Global; <br/>\n"
              + "0: as determined by the search engine; above 0 (e.g. 0.75): PTMProphet <br/>\n"
              + "probability, to be used with phosphorylation only)");
      pOpts.add(feMinSiteProb.label(), new CC().alignX("right"));
      pOpts.add(feMinSiteProb.comp, new CC().alignX("left").spanX().wrap());

      UiText uiTextProtExclude = UiUtils.uiTextBuilder().cols(10).text("none").create();
      FormEntry feProtExclude = fe(TmtiConfProps.PROP_prot_exclude,
          "Exclude proteins", uiTextProtExclude,
          "exclude proteins with specified tags at the beginning of the accession <br/>\n"
              + "number (e.g. none: no exclusion; sp|,tr| : exclude protein with sp| or tr|)");
      pOpts.add(feProtExclude.label(), new CC().alignX("right"));
      pOpts.add(feProtExclude.comp, new CC().alignX("left").spanX().wrap());

      UiText uiTextModTag = UiUtils.uiTextBuilder().cols(10).text("none").create();
      FormEntry feModTag = fe(TmtiConfProps.PROP_mod_tag,
          "Mod tag", uiTextModTag,
          "<html>TM info for generation of PTM-specific reports <br/>\n"
              + "none: for Global data<br/>\n"
              + "S[167],T[181],Y[243]: for Phospho<br/>\n"
              + "K[170]: for K-Acetyl");
      pOpts.add(feModTag.label(), new CC().alignX("right"));
      pOpts.add(feModTag.comp, new CC().alignX("left").spanX().wrap());


      // add oOpts to main panel
      pContent.add(pOpts, new CC().alignY("top").growX().wrap());
    }

    this.add(pContent, BorderLayout.CENTER);
  }

  private static FormEntry fe(String name, String label, JComponent comp, String tooltip) {
    return new FormEntry(PREFIX + name, label, comp, tooltip);
  }

  private static FormEntry fe(String name, String label, JComponent comp) {
    return new FormEntry(PREFIX + name, label, comp, null);
  }

  private static void addPoptsRow(JPanel pOpts, FormEntry fe1, FormEntry fe2) {
    pOpts.add(fe1.label(), new CC().alignX("right"));
    pOpts.add(fe1.comp, new CC().alignX("left"));
    pOpts.add(fe2.comp, new CC().alignX("left").wrap());
  }

  private QuantLabel getSelectedLabel() {
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

  private void actionOnCheckboxIsRun(ActionEvent e) {
    final boolean isRun = checkRun.isSelected();
    updateUiOnCheckRunStateChange(isRun);
    EventBus.getDefault().post(new MessageTmtIntegratorRun(isRun));
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

  private static List<QuantLabelAnnotation> parseTmtAnnotationFile(File file)
      throws TmtAnnotationValidationException {
    if (file == null || !Files.exists(file.toPath()))
      throw new TmtAnnotationValidationException("File does not exist");
    List<String> lines;
    try {
      lines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new TmtAnnotationValidationException(e);
    }
    final long nonEmptyLinesCount = lines.stream()
        .filter(line -> !StringUtils.isNullOrWhitespace(line)).count();
    Optional<QuantLabel> label = QuantLabel.LABELS.stream()
        .filter(ql -> ql.getReagentNames().size() == nonEmptyLinesCount).findFirst();
    if (!label.isPresent()) {
      throw new TmtAnnotationValidationException("No known labels with " + nonEmptyLinesCount + " reagents.");
    }

    List<QuantLabelAnnotation> annotations = new ArrayList<>();
    for (int i = 0; i < lines.size(); i++) {
      String line = lines.get(i);
      if (StringUtils.isNullOrWhitespace(line))
        continue;
      String[] split = line.split("[ ]+", 2);
      if (!label.get().getReagentNames().contains(split[0])) {
        throw new TmtAnnotationValidationException(
            String.format("Line %d contains unknown labeling reagent name: %s", i + 1, split[0]));
      }
      annotations.add(new QuantLabelAnnotation(split[0], split[1]));
    }
    return annotations;
  }

  public boolean isRun() {
    return checkRun.isEnabled() && checkRun.isSelected();
  }

  @Subscribe
  public void onMessageLoadTmtIntegratorDefaults(MessageLoadTmtIntegratorDefaults m) {
    log.debug("Got MessageLoadTmtIntegratorDefaults, it's an empty marker message");
    // TODO: implement
  }

  @Subscribe(threadMode =  ThreadMode.MAIN_ORDERED)
  public void onMessageLcmsFilesList(MessageLcmsFilesList m) {
    if (m.type == MessageType.REQUEST)
      return;

    final Map<String, ExpNameToAnnotationFile> curRows = tmtAnnotationTable.fetchModel().dataCopy().stream()
        .collect(Collectors.toMap(row -> row.expName, row -> row));
    List<String> expNames = m.files.stream().map(InputLcmsFile::getExperiment)
        .distinct().sorted().collect(Collectors.toList());
    List<ExpNameToAnnotationFile> newRows = m.files.stream().map(InputLcmsFile::getExperiment)
        .distinct().sorted()
        .map(name -> curRows.getOrDefault(name, new ExpNameToAnnotationFile(name, STRING_NO_PATH_SET)))
        .collect(Collectors.toList());

    tmtAnnotationTable.fetchModel().dataClear();
    tmtAnnotationTable.fetchModel().dataAddAll(newRows);

  }

  public void actionPerformedBrowse(ActionEvent e)
  {
    int modelRow = Integer.parseInt( e.getActionCommand() );
    log.debug("Browse action running in TMT, model row number: {}", modelRow);
    ExpNameToAnnotationFile row = tmtAnnotationTable.fetchModel().dataGet(modelRow);
    JFileChooser fc = new JFileChooser();
    fc.setAcceptAllFileFilterUsed(true);
    fc.setMultiSelectionEnabled(false);
    Optional<Path> exisitngFile = Stream
        .of(row.path,
            ThisAppProps.load(PROP_LAST_ANNOTATION_PATH),
            ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN))
        .map(path -> {
          try {
            return path == null ? null : Paths.get(path);
          } catch (Exception ignore) {
            return null;
          }
        })
        .filter(Objects::nonNull)
        .filter(path -> Files.exists(path))
        .findFirst();

    exisitngFile.ifPresent(path -> fc.setSelectedFile(path.toFile()));
    final int answer = fc.showDialog(TmtiPanel.this.scrollPaneTmtTable, "Select");
    if (JOptionPane.OK_OPTION == answer) {
      File selectedFile = fc.getSelectedFile();
      List<QuantLabelAnnotation> annotations;
      try {
        annotations = parseTmtAnnotationFile(selectedFile);
      } catch (TmtAnnotationValidationException ex) {
        SwingUtils.showErrorDialog(ex, TmtiPanel.this.scrollPaneTmtTable, false);
        return;
      }

      // do the annotations match anything known to us?
      Optional<QuantLabel> known = QuantLabel.LABELS.stream()
          .filter(ql -> ql.getReagentNames().size() == annotations.size()).findFirst();
      if (!known.isPresent()) throw new IllegalStateException("No known labels match");

      // maybe update selected Label Type (aka number of channels)?
      if (!getSelectedLabel().getName().equalsIgnoreCase(known.get().getName())) {
        int confirmation = SwingUtils.showConfirmDialog(TmtiPanel.this,
            new JLabel(String.format("<html>Loaded file looks to be for %s.<br/>\n"
                + "Load configuration preset for that label?", known.get().getName())));
        if (JOptionPane.OK_OPTION == confirmation) {
          log.debug("User selected to load config preset for known label");
          uiComboLabelNames.setSelectedItem(known.get().getName());
        }
      }

      row.setPath(selectedFile.toString());
      tmtAnnotationTable.fetchModel().fireTableDataChanged();

      // save the path
      ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, selectedFile);
    }
  }

  public void actionPerformedEditCreate(ActionEvent e) {
    int modelRowIndex = Integer.parseInt( e.getActionCommand() );
    log.debug("Create action running in TMT, model row number: {}", modelRowIndex);
    ExpNameToAnnotationFile row = tmtAnnotationTable.fetchModel().dataGet(modelRowIndex);
    String pathInRow = row.getPath();

    Path existingPath = null;
    try {
      if (Files.exists(Paths.get(pathInRow))) existingPath = Paths.get(pathInRow);
    } catch (Exception ignore) {}

    List<QuantLabelAnnotation> quantLabelAnnotations = null;
    if (existingPath != null) {
      try {
        quantLabelAnnotations = parseTmtAnnotationFile(
            existingPath.toFile());
      } catch (TmtAnnotationValidationException ex) {
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
      String suggestedPaths = Stream
          .of(existingPath != null ? existingPath.toString() : null,
              ThisAppProps.load(TmtiPanel.PROP_LAST_ANNOTATION_PATH),
              ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN))
          .filter(Objects::nonNull).findFirst().orElse(null);
      SwingUtils.setFileChooserPath(fc, suggestedPaths);
      int userSelection = fc.showSaveDialog(parent);
      if (JFileChooser.APPROVE_OPTION == userSelection) {
        selectedPath = fc.getSelectedFile().toPath();
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
            SwingUtils.showErrorDialog(ex, parent);
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
      SwingUtils.showErrorDialog(ex, parent);
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
}
