package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.api.PropsFile;
import com.dmtavt.fragpipe.messages.MessageLcmsAddFiles;
import com.dmtavt.fragpipe.messages.MessageLcmsAddFolder;
import com.dmtavt.fragpipe.messages.MessageLcmsClearFiles;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesAdded;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesList;
import com.dmtavt.fragpipe.messages.MessageLcmsGroupAction;
import com.dmtavt.fragpipe.messages.MessageLcmsGroupAction.Type;
import com.dmtavt.fragpipe.messages.MessageLcmsRemoveSelected;
import com.dmtavt.fragpipe.messages.MessageLoadUi;
import com.dmtavt.fragpipe.messages.MessageSaveAsWorkflow;
import com.dmtavt.fragpipe.messages.MessageType;
import com.dmtavt.fragpipe.messages.MessageUpdateWorkflows;
import com.github.chhh.utils.FileDrop;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.RandomUtils;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.jooq.lambda.Unchecked;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.gui.LcmsInputFileTable;
import umich.msfragger.gui.MsfraggerGuiFrameUtils;
import umich.msfragger.gui.MsfraggerGuiFrameUtils.LcmsFileAddition;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.UniqueLcmsFilesTableModel;
import umich.msfragger.gui.dialogs.ExperimentNameDialog;
import umich.msfragger.params.ThisAppProps;

public class TabWorkflow extends JPanelWithEnablement {

  private static final Logger log = LoggerFactory.getLogger(TabWorkflow.class);
  private final MigUtils mu = MigUtils.get();
  private JButton btnFilesRemove;
  private JButton btnFilesClear;
  public static final String TAB_PREFIX = "workflow.";

  private SimpleETable tableRawFiles;
  private UniqueLcmsFilesTableModel tableModelRawFiles;
  private FileDrop tableRawFilesFileDrop;
  private JScrollPane scrollPaneRawFiles;
  private JButton btnGroupsConsecutive;
  private JButton btnGroupsByParentDir;
  private JButton btnGroupsByFilename;
  private JButton btnGroupsAssignToSelected;
  private JButton btnGroupsClear;
  private JEditorPane epWorkflowsInfo;
  private UiSpinnerInt uiSpinnerRam;
  private UiSpinnerInt uiSpinnerThreads;
  private Map<String, PropsFile> workflows;
  private UiCombo uiComboWorkflows;
  public static final String PROP_WORKFLOW_DESC = "workflow.description";
  private static final String DEFAULT_WORKFLOW = "Defaults";

  public TabWorkflow() {
    init();
    initMore();
  }

  private void initMore() {
    tableRawFilesFileDrop = makeFileDrop(); // file drop is registered after all components are created
    Bus.register(this);
    Bus.postSticky(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    add(createPanelWorkflows(), mu.ccGx().wrap());
    add(createPanelOptions(), mu.ccGx().wrap());
    add(createPanelLcmsFiles(), mu.ccGx().wrap());
  }

  private String genSentence() {
    int numWords = RandomUtils.nextInt(10, 120);
    return IntStream.range(0, numWords)
        .mapToObj(i -> genWord())
        .collect(StringBuilder::new, (s1, s2) -> s1.append(" ").append(s2),
            (s1, s2) -> s1.append(" ").append(s2))
        .toString();
  }

  private String genWord() {
    int lo = 97;  // letter 'a'
    int hi = 122; // letter 'z'
    int targetStringLength = RandomUtils.nextInt(2, 12);
    Random random = new Random();
    return random.ints(lo, hi + 1)
        .limit(targetStringLength)
        .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
        .toString();
  }

  private FormEntry.Builder fe(JComponent comp, String name) {
    return Fragpipe.fe(comp, name, TAB_PREFIX);
  }

  private JPanel createPanelOptions() {
    JPanel p = mu.newPanel("Global settings", true);

    uiSpinnerRam = new UiSpinnerInt(0, 0, 1024, 1, 3);
    FormEntry feRam = fe(uiSpinnerRam, "ram").label("RAM (GB)").create();
    uiSpinnerThreads = new UiSpinnerInt(Runtime.getRuntime().availableProcessors() - 1, 0, 128, 1);
    FormEntry feThreads = fe(uiSpinnerThreads, "threads").label("Threads").create();

    mu.add(p, feRam.label()).split();
    mu.add(p, feRam.comp);
    mu.add(p, feThreads.label());
    mu.add(p, feThreads.comp).pushX().wrap();

    return p;
  }

  public int getRamGb() {
    return uiSpinnerRam.getActualValue();
  }

  public int getThreads() {
    return uiSpinnerThreads.getActualValue();
  }

  private Map<String, PropsFile> loadWorkflowFiles() {
    Path dir = FragpipeLocations.get().getDirWorkflows();
    try {
      final Map<String, PropsFile> files = new HashMap<>();
      Files.walk(dir).filter(Files::isRegularFile)
          .filter(p -> "workflow"
              .equalsIgnoreCase(StringUtils.afterLastDot(p.getFileName().toString())))
          .forEach(Unchecked.consumer(path -> {
            PropsFile f = new PropsFile(path, "Loaded from: " + path.toString());
            f.load();
            if (f.size() > 0) {
              String s = f.getPath().getFileName().toString();
              String name = StringUtils.upToLastDot(s);
              files.put(StringUtils.isBlank(name) ? s : name, f);
            }
          }, throwable -> {
            log.error("Error while reading alleged workflow file", throwable);
          }));
      return files;
    } catch (IOException e) {
      SwingUtils.showErrorDialogWithStacktrace(e, this);
      return Collections.emptyMap();
    }
  }

  private JPanel createPanelWorkflows() {
    JPanel p = mu.newPanel("Workflows", true);

    epWorkflowsInfo = SwingUtils.createClickableHtml(true,
        "FragPipe and its collection of tools support multiple proteomic workflows.\n"
            + "Select an option in the dropdown menu below to configure "
            + "all the tools. You can tweak the options yourself after loading.\n"
            + "Also, <a href=\"https://google.com\">see the tutorial</a>");

    workflows = loadWorkflowFiles();
    List<String> names = createNamesForWorkflowsCombo(workflows);
    uiComboWorkflows = UiUtils.createUiCombo(names);
    final JEditorPane epWorkflowsDesc = SwingUtils.createClickableHtml(SwingUtils.makeHtml(""));
    epWorkflowsDesc.setPreferredSize(new Dimension(400, 50));
    uiComboWorkflows.addItemListener(e -> {
      String name = (String) e.getItem();
      PropsFile propsFile = workflows.get(name);
      if (propsFile == null) {
        throw new IllegalStateException("Workflows map is not synchronized with the dropdown");
      }
      SwingUtils.setJEditorPaneContent(epWorkflowsDesc, propsFile.getProperty(PROP_WORKFLOW_DESC, "Description not present"));
    });
    JButton btnWorkflowLoad = UiUtils.createButton("Load", e -> {
      String workflow = (String) uiComboWorkflows.getSelectedItem();
      log.debug("Load workflow button clicked: {}", workflow);
      int confirmation = SwingUtils
          .showConfirmDialog(this, new JLabel("Do you want to load workflow: " + workflow + "?"),
              "Confirmation");
      if (JOptionPane.OK_OPTION == confirmation) {
        log.debug("Loading workflow/ui state: {}", workflow);
        PropsFile propsFile = workflows.get(workflow);
        if (propsFile == null) {
          if (!DEFAULT_WORKFLOW.equalsIgnoreCase(workflow)) {
            throw new IllegalStateException("Workflows map is not synchronized with the dropdown");
          } else {
            throw new UnsupportedOperationException("Not implemented loading defaults from jar without having the defaults file in workflows/ folder");
          }
        }
        if (propsFile.containsKey("workflow.workflow-option")) {
          propsFile.setProperty("workflow.workflow-option", workflow);
        }
        Bus.post(new MessageLoadUi(propsFile));
      }
    });
    FormEntry feComboWorkflow = fe(uiComboWorkflows, "workflow-option")
        .label("Select an option to load config for:")
        .tooltip("This is purely for convenience of loading appropriate defaults\n"
            + "for various standard workflows.\n"
            + "You can totally just set up all the options yourself.").create();

    mu.add(p, epWorkflowsInfo).growX().spanX().wrap();
    mu.add(p, feComboWorkflow.label()).split();
    mu.add(p, feComboWorkflow.comp);
    mu.add(p, btnWorkflowLoad);
    mu.add(p, new JLabel("or save current settings as workflow")).gapLeft("15px");
    mu.add(p, UiUtils.createButton("Save", e -> Bus.post(new MessageSaveAsWorkflow(false)))).wrap();

    mu.add(p, epWorkflowsDesc).growX().spanX().wrap();

    return p;
  }

  private void logObjectType(Object m) {
    log.debug("Got {}", m.getClass().getSimpleName());
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsAddFiles m) {
    logObjectType(m);

    List<Path> searchPaths = Fragpipe.getExtBinSearchPaths();
    final javax.swing.filechooser.FileFilter ff = CmdMsfragger.getFileChooserFilter(searchPaths);
    Predicate<File> supportedFilePredicate = CmdMsfragger.getSupportedFilePredicate(searchPaths);
    JFileChooser fc = FileChooserUtils
        .create("Choose raw data files", "Select", true, FcMode.ANY, true,
            ff);
    tableModelRawFiles.dataCopy();
    FileChooserUtils.setPath(fc, Stream.of(ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN)));

    int result = fc.showDialog(this, "Select");
    if (JFileChooser.APPROVE_OPTION != result) {
      return;
    }
    final List<Path> paths = Arrays.stream(fc.getSelectedFiles())
        .filter(supportedFilePredicate)
        .map(File::toPath).collect(Collectors.toList());
    if (paths.isEmpty()) {
      JOptionPane.showMessageDialog(this,
          "None of selected files/folders are supported", "Warning", JOptionPane.WARNING_MESSAGE);
      return;
    } else {
      Bus.post(new MessageLcmsFilesAdded(paths));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsFilesAdded m) {
    // save locations
    String savePath = m.recursiveAdditionRoot != null ? m.recursiveAdditionRoot.toString()
        : m.paths.get(0).toString();
    ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, savePath);

    LcmsFileAddition lfa = new LcmsFileAddition(m.paths, new ArrayList<>(m.paths));
    MsfraggerGuiFrameUtils.processAddedLcmsPaths(lfa, this, Fragpipe::getExtBinSearchPaths);

    // add the files
    List<InputLcmsFile> toAdd = lfa.toAdd.stream()
        .map(p -> new InputLcmsFile(p, ThisAppProps.DEFAULT_LCMS_EXP_NAME))
        .collect(Collectors.toList());
    if (!toAdd.isEmpty()) {
      tableModelRawFiles.dataAddAll(toAdd);
      postFileListUpdate();
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsAddFolder m) {
    logObjectType(m);

    final javax.swing.filechooser.FileFilter ff = CmdMsfragger
        .getFileChooserFilter(Fragpipe.getExtBinSearchPaths());
    JFileChooser fc = FileChooserUtils
        .create("Select a folder with LC/MS files (searched recursively)", "Select",
            true, FcMode.ANY, true);
    FileChooserUtils.setPath(fc, Stream.of(ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN)));

    String lastPath = ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN);
    if (!StringUtils.isNullOrWhitespace(lastPath)) {
      try {
        Path p = Paths.get(lastPath);
        fc.setSelectedFile(p.toFile());
      } catch (Exception ignore) {
      }
    }

    int confirmation = fc.showOpenDialog(this);

    if (confirmation != JFileChooser.APPROVE_OPTION) {
      return;
    }

    final Predicate<File> pred = CmdMsfragger
        .getSupportedFilePredicate(Fragpipe.getExtBinSearchPaths());
    File[] selectedFiles = fc.getSelectedFiles();
    List<Path> paths = new ArrayList<>();
    for (File f : selectedFiles) {
      PathUtils.traverseDirectoriesAcceptingFiles(f, pred, paths, false);
    }

    if (selectedFiles.length > 0 && !paths.isEmpty()) {
      EventBus.getDefault().post(new MessageLcmsFilesAdded(paths, selectedFiles[0].toPath()));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsRemoveSelected m) {
    logObjectType(m);

    final List<InputLcmsFile> toRemove = new ArrayList<>();
    Arrays.stream(this.tableRawFiles.getSelectedRows())
        .map(tableRawFiles::convertRowIndexToModel)
        .boxed()
        .forEach(i -> toRemove.add(tableModelRawFiles.dataGet(i)));
    tableRawFiles.getSelectionModel().clearSelection();

    if (!toRemove.isEmpty()) {
      tableModelRawFiles.dataRemoveAll(toRemove);
      postFileListUpdate();
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsClearFiles m) {
    logObjectType(m);
    tableModelRawFiles.dataClear();
    postFileListUpdate();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsFilesList m) {
    if (m.type == MessageType.REQUEST) {
      Bus.post(new MessageLcmsFilesList(MessageType.RESPONSE, tableModelRawFiles.dataCopy()));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageSaveAsWorkflow m) {
    Fragpipe fp = Fragpipe.getStickyStrict(Fragpipe.class);
    Properties uiProps = FragpipeCacheUtils.tabsSave(fp.tabs);
    Map<String, String> vetted = Seq.seq(PropertiesUtils.toMap(uiProps)).filter(kv -> {
      String k = kv.v1().toLowerCase();
      if (k.startsWith(TabConfig.TAB_PREFIX)) {
        return false;
      }
      return !k.contains("workdir") && !k.contains("db-path");
    }).toMap(kv -> kv.v1, kv -> kv.v2);

    Path saveDir;
    if (!m.toCustomDir) {
      saveDir = FragpipeLocations.get().getDirWorkflows();
    } else {
      // save to custom dir
      final String propWorkflowDir = "workflow.last-save-dir";
      JFileChooser fc = FileChooserUtils
          .builder("Select folder to save workflow file to")
          .multi(false).mode(FcMode.DIRS_ONLY).acceptAll(true).approveButton("Select folder")
          .paths(Stream.of(Fragpipe.propsVarGet(propWorkflowDir),
              FragpipeLocations.get().getDirWorkflows().toString()))
          .create();
      if (fc.showOpenDialog(fp) != JFileChooser.APPROVE_OPTION) {
        log.debug("User cancelled dir selection");
        return;
      }
      saveDir = fc.getSelectedFile().toPath();
    }

    // ask about name and description
    MigUtils mu = MigUtils.get();
    final JPanel p = mu.newPanel(null, true);
    UiText uiTextName = UiUtils.uiTextBuilder().cols(20).ghost("Name is required").create();
    uiTextName.setName("file-name");
    final JEditorPane ep = new JEditorPane("text/html", SwingUtils.wrapInStyledHtml(""));
    ep.setPreferredSize(new Dimension(320, 240));
    ep.setName("file-desc");
    mu.add(p, new JLabel("Name")).split();
    mu.add(p, uiTextName).growX().wrap();
    mu.add(p, new JLabel("Description (optional)")).wrap();
    mu.add(p, ep).spanX().wrap();

    int answer = SwingUtils
        .showConfirmDialog(fp, p, "Assign workflow name");
    if (JOptionPane.OK_OPTION != answer) {
      return;
    }
    String text = uiTextName.getNonGhostText().trim();
    if (StringUtils.isBlank(text)) {
      SwingUtils
          .showErrorDialog(fp, "Workflow name can't be left empty", "Error saving workflow");
      return;
    }
    text = StringUtils.appendOnce(text, ".workflow");
    Path savePath;
    try {
      savePath = saveDir.resolve(text).normalize().toAbsolutePath();
    } catch (Exception e) {
      SwingUtils.showErrorDialog(fp, "Not a valid path", "Error saving workflow");
      return;
    }
    if (PathUtils.existing(savePath.toString()) != null) {
      int ans = SwingUtils.showConfirmDialog(fp,
          new JLabel(SwingUtils.makeHtml("Overwrite existing file?\n" + savePath.toString())),
          "Overwrite?");
      if (JOptionPane.OK_OPTION != ans) {
        log.debug("user chose not to overwrite file");
        return;
      }
    }
    String desc = SwingUtils.tryExtractHtmlBody(ep.getText());
    if (StringUtils.isNotBlank(desc)) {
      vetted.put(PROP_WORKFLOW_DESC, desc);
    }

    // save
    FragpipeCacheUtils.saveToFileSorted(PropertiesUtils.from(vetted), savePath,
        "Workflow: " + StringUtils.upToLastDot(savePath.getFileName().toString()));
    SwingUtils.showInfoDialog(fp, "Saved to: " + savePath, "Error saving workflow");

    if (FragpipeLocations.get().getDirWorkflows().equals(saveDir)) {
      Bus.post(new MessageUpdateWorkflows());
    }
  }

  private List<String> createNamesForWorkflowsCombo(Map<String, PropsFile> fileMap) {
    return Seq.seq(fileMap.keySet()).filter(s -> !DEFAULT_WORKFLOW.equalsIgnoreCase(s)).sorted().prepend(
        DEFAULT_WORKFLOW).toList();
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageUpdateWorkflows m) {
    String previouslySelected = (String) uiComboWorkflows.getSelectedItem();
    List<String> previousOptions = new ArrayList<>();
    for (int i = 0; i < uiComboWorkflows.getModel().getSize(); i++) {
      previousOptions.add(uiComboWorkflows.getModel().getElementAt(i));
    }
    log.debug("Previously selected: {}, Previous options: {}", previouslySelected, previousOptions);

    workflows = loadWorkflowFiles();
    List<String> names = createNamesForWorkflowsCombo(workflows);
    uiComboWorkflows.setModel(new DefaultComboBoxModel<>(names.toArray(new String[0])));
    Optional<String> newName = names.stream().filter(name -> !previousOptions.contains(name))
        .findFirst();
    if (newName.isPresent()) {
      log.debug("Figured that '{}' workflow was added", newName.get());
      uiComboWorkflows.setSelectedItem(newName.get());
    } else {
      log.debug("Cant figure which workflow name was added");
    }
  }

  private void postFileListUpdate() {
    ArrayList<InputLcmsFile> data = tableModelRawFiles.dataCopy();
    if (!data.isEmpty()) {
      ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, data.get(0).getPath().toString());
    }
    Bus.post(new MessageLcmsFilesList(MessageType.UPDATE, data));
  }

  private JPanel createPanelLcmsFiles() {
    JPanel p = mu.newPanel("Input LC/MS Files", true);

    JButton btnFilesAddFiles = button("Add files", MessageLcmsAddFiles::new);
    JButton btnFilesAddFolder = button("Add folder recursively", MessageLcmsAddFolder::new);
    btnFilesRemove = button("Remove selected files", MessageLcmsRemoveSelected::new);
    btnFilesRemove.setEnabled(false);
    btnFilesClear = button("Clear files", MessageLcmsClearFiles::new);
    btnFilesClear.setEnabled(false);

    btnGroupsConsecutive = button("Consecutive",
        () -> new MessageLcmsGroupAction(Type.CONSECUTIVE));
    btnGroupsByParentDir = button("By parent directory",
        () -> new MessageLcmsGroupAction(Type.BY_PARENT_DIR));
    btnGroupsByFilename = button("By file name",
        () -> new MessageLcmsGroupAction(Type.BY_FILE_NAME));
    btnGroupsAssignToSelected = button("Set experiment/replicate",
        () -> new MessageLcmsGroupAction(Type.SET_EXP));
    btnGroupsClear = button("Clear groups", () -> new MessageLcmsGroupAction(Type.CLEAR_GROUPS));

    createFileTable();

    mu.add(p, btnFilesAddFiles).split();
    mu.add(p, btnFilesAddFolder);
    mu.add(p, btnFilesRemove);
    mu.add(p, btnFilesClear).wrap();
    mu.add(p,
        new JLabel("Assign files to Experiments/Groups (select rows to activate action buttons):"))
        .spanX().wrap();
    mu.add(p, btnGroupsConsecutive).split();
    mu.add(p, btnGroupsByParentDir);
    mu.add(p, btnGroupsByFilename);
    mu.add(p, btnGroupsAssignToSelected);
    mu.add(p, btnGroupsClear).wrap();

    p.add(scrollPaneRawFiles, mu.ccGx().wrap());

    return p;
  }

  private JButton button(String text, Supplier<Object> message) {
    return UiUtils.createButton(text, e -> Bus.post(message.get()));
  }

  private void createFileTable() {
    tableModelRawFiles = MsfraggerGuiFrameUtils.createTableModelRawFiles();
    tableModelRawFiles.addTableModelListener(e -> {
      List<InputLcmsFile> files = tableModelRawFiles.dataCopy();
      EventBus.getDefault().post(new MessageLcmsFilesList(MessageType.UPDATE, files));
    });
    tableRawFiles = new LcmsInputFileTable(tableModelRawFiles);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnFilesClear);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsConsecutive);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByParentDir);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByFilename);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsClear);

    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnFilesRemove);
    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnGroupsAssignToSelected);
    tableRawFiles.fireInitialization();
    tableRawFiles.setFillsViewportHeight(true);
    scrollPaneRawFiles = new JScrollPane();
    scrollPaneRawFiles.setViewportView(tableRawFiles);
  }

  private FileDrop makeFileDrop() {
    return new FileDrop(this, true, files -> {
      Predicate<File> pred = CmdMsfragger
          .getSupportedFilePredicate(Fragpipe.getExtBinSearchPaths());
      List<Path> accepted = new ArrayList<>(files.length);
      for (File f : files) {
        PathUtils.traverseDirectoriesAcceptingFiles(f, pred, accepted, false);
      }
      if (!accepted.isEmpty()) {
        Bus.post(new MessageLcmsFilesAdded(accepted));
      }
    });
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLcmsGroupAction m) {
    log.debug("Got MessageLcmsGroupAction of type: {}", m.type.toString());

    switch (m.type) {

      case CONSECUTIVE:
        this.actionConsecutive();
        break;
      case BY_PARENT_DIR:
        this.actionByParentDir();
        break;
      case BY_FILE_NAME:
        this.actionByFileName();
        break;
      case SET_EXP:
        this.actionSetExt();
        break;
      case CLEAR_GROUPS:
        this.actionClearGroups();
        break;
      default:
        throw new IllegalStateException("Unknown enum option: " + m.type.toString());
    }

    postFileListUpdate();
  }

  private void actionClearGroups() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), ThisAppProps.DEFAULT_LCMS_EXP_NAME));
    }
  }

  private void actionSetExt() {
    final UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    final ArrayList<InputLcmsFile> data = m.dataCopy();
    List<Integer> selectedRows = Arrays.stream(this.tableRawFiles.getSelectedRows())
        .mapToObj(tableRawFiles::convertRowIndexToModel).collect(Collectors.toList());

    final List<String> paths = selectedRows.stream()
        .map(i -> data.get(i).getPath().toString())
        .collect(Collectors.toList());

    final Set<String> exps = selectedRows.stream()
        .flatMap(i -> data.get(i).getExperiment() == null ? Stream.empty()
            : Stream.of(data.get(i).getExperiment()))
        .collect(Collectors.toSet());
    final Set<Integer> reps = selectedRows.stream()
        .flatMap(i -> data.get(i).getReplicate() == null ? Stream.empty()
            : Stream.of(data.get(i).getReplicate()))
        .collect(Collectors.toSet());
    final String defaultExp = exps.size() == 1 ? exps.iterator().next() : null;
    final Integer defaultRep = reps.size() == 1 ? reps.iterator().next() : null;

    ExperimentNameDialog d = new ExperimentNameDialog(SwingUtils.findParentFrame(this), true, paths,
        defaultExp, defaultRep);
    d.setVisible(true);
    if (d.isOk()) {
      for (int selectedRow : selectedRows) {
        int i = tableRawFiles.convertRowIndexToModel(selectedRow);
        InputLcmsFile f = m.dataGet(i);
        m.dataSet(i, new InputLcmsFile(f.getPath(), d.getExperimentName(), d.getReplicateNumber()));
      }
    }
  }

  private void actionByFileName() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      String group = StringUtils.upToLastDot(f.getPath().getFileName().toString());
      m.dataSet(i, new InputLcmsFile(f.getPath(), group));
    }
  }

  private void actionByParentDir() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      int count = f.getPath().getNameCount();
      String group = count - 2 >= 0
          ? f.getPath().getName(count - 2).toString()
          : f.getPath().getName(count - 1).toString();
      m.dataSet(i, new InputLcmsFile(f.getPath(), group));
    }
  }

  private void actionConsecutive() {
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    final int groupNumMaxLen = (int) Math.ceil(Math.log(m.dataSize()));
    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), "exp", i + 1));
    }
  }

  public Map<String, LcmsFileGroup> getLcmsFileGroups() {
    List<InputLcmsFile> lcmsInputs = tableModelRawFiles.dataCopy();
    Map<String, List<InputLcmsFile>> mapGroup2Files = lcmsInputs.stream()
        .collect(Collectors.groupingBy(InputLcmsFile::getGroup));

    Map<String, LcmsFileGroup> result = new TreeMap<>();
    for (Entry<String, List<InputLcmsFile>> e : mapGroup2Files.entrySet()) {
      result.put(e.getKey(), new LcmsFileGroup(e.getKey(), e.getValue()));
    }

    return result;
  }

  public List<InputLcmsFile> getLcmsFiles() {
    return tableModelRawFiles.dataCopy();
  }
}
