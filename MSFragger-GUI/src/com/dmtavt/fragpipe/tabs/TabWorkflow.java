package com.dmtavt.fragpipe.tabs;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.Version;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.api.IPathsProvider;
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
import com.github.chhh.utils.MapUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiSpinnerInt;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
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
import com.dmtavt.fragpipe.cmd.CmdMsfragger;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.api.LcmsInputFileTable;
import com.dmtavt.fragpipe.api.SimpleETable;
import com.dmtavt.fragpipe.api.TableModelColumn;
import com.dmtavt.fragpipe.api.UniqueLcmsFilesTableModel;
import com.dmtavt.fragpipe.dialogs.ExperimentNameDialog;
import com.dmtavt.fragpipe.params.ThisAppProps;

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
  private JEditorPane epWorkflowsDesc;
  private UiCheck uiCheckProcessEachExperimentSeparately;
  private UiText uiTextLastAddedLcmsDir;

  public TabWorkflow() {
    init();
    initMore();
  }

  public static UniqueLcmsFilesTableModel createTableModelRawFiles() {
    List<TableModelColumn<InputLcmsFile, ?>> cols = new ArrayList<>();

    TableModelColumn<InputLcmsFile, String> colPath = new TableModelColumn<>(
        "Path (can drag & drop from Explorer)",
        String.class, false, data -> data.getPath().toString());
    TableModelColumn<InputLcmsFile, String> colExp = new TableModelColumn<>(
        "Experiment (can be empty)", String.class, true, InputLcmsFile::getExperiment);
    TableModelColumn<InputLcmsFile, Integer> colRep = new TableModelColumn<>(
        "Replicate (can be empty)", Integer.class, true, InputLcmsFile::getReplicate);
    cols.add(colPath);
    cols.add(colExp);
    cols.add(colRep);

    UniqueLcmsFilesTableModel m = new UniqueLcmsFilesTableModel(cols, 0);

    return m;
  }

  public static void processAddedLcmsPaths(LcmsFileAddition files, Component parent, IPathsProvider extBinSearchPaths) {
    // vet/check input LCMS files for bad naming
    final javax.swing.filechooser.FileFilter ff = CmdMsfragger.getFileChooserFilter(extBinSearchPaths.get());
    final HashMap<Path, Set<String>> reasonsDir = new HashMap<>();
    final HashMap<Path, Set<String>> reasonsFn = new HashMap<>();
    //final HashMap<String, List<Path>> reasonsRev = new HashMap<>();
    final String allowedChars = "[A-Za-z0-9-_+.\\[\\]()]";
    Pattern re = Pattern.compile(allowedChars + "+");
    final String REASON_NON_ASCII = "Non-ASCII chars";
    final String REASON_PATH_SPACES = "Path contains spaces";
    final String REASON_FN_DOTS = "Filename contains dots";
    final String REASON_UNSUPPORTED = "Not supported";
    final String REASON_DISALLOWED_CHARS = "Contains characters other than: " + allowedChars;

    for (Path path : files.paths) {
      Set<String> why = InputLcmsFile.validatePath(path.getParent().toString());
      if (!why.isEmpty()) {
        reasonsDir.put(path, why);
      }
    }

    for (Path path : files.paths) {
      Set<String> why = InputLcmsFile.validateFilename(path.getFileName().toString());
      if (!why.isEmpty()) {
        reasonsFn.put(path, why);
      }
    }

    // in case there were suspicious paths
    if (!reasonsDir.isEmpty() || !reasonsFn.isEmpty()) {
      HashMap<Path, String> path2reasons = new HashMap<>();
      for (Entry<Path, Set<String>> kv : reasonsDir.entrySet()) {
        for (String reason : kv.getValue()) {
          path2reasons.compute(kv.getKey(), (path, s) -> s == null ? "Direcotry " + reason : s.concat(", Direcotry " + reason));
        }
      }
      for (Entry<Path, Set<String>> kv : reasonsFn.entrySet()) {
        for (String reason : kv.getValue()) {
          path2reasons.compute(kv.getKey(), (path, s) -> s == null ? "File name " + reason : s.concat(", File name " + reason));
        }
      }
      String[] columns = {"Reasons", "Path"};
      String[][] data = new String[path2reasons.size()][2];
      int index = -1;
      for (Entry<Path, String> kv : path2reasons.entrySet()) {
        data[++index][0] = kv.getValue();
        data[index][1] = kv.getKey().toString();
      }

      DefaultTableModel model = new DefaultTableModel(data, columns);
      JTable table = new JTable(model);
      table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
      JPanel panel = new JPanel(new BorderLayout());
      panel.add(new JLabel("<html>Found problems with some files (" + path2reasons.size() + " of " + files.paths.size() + ").<br/>"
          + "This <b>will likely cause trouble</b> with some of processing tools.<br/><br/>"
          + "What do you want to do with these files?<br/>"), BorderLayout.NORTH);
      panel.add(Box.createVerticalStrut(100), BorderLayout.CENTER);
      panel.add(new JScrollPane(table), BorderLayout.CENTER);
      SwingUtils.makeDialogResizable(panel);

      String[] options;
      if (!reasonsFn.isEmpty()) {
        options = new String[]{"Cancel", "Add anyway", "Only add well-behaved files", "Try rename files"};
      } else {
        options = new String[]{"Cancel", "Add anyway", "Only add well-behaved files"};
      }

      int confirmation = JOptionPane
          .showOptionDialog(parent, panel, "Add these files?",
              JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);

      switch (confirmation) {
        case 0:
          return;
        case 1:
          break;
        case 2:
          files.toAdd = files.toAdd.stream().filter(path -> !path2reasons.containsKey(path)).collect(Collectors.toList());
          break;
        case 3: // rename files
          int confirm1 = SwingUtils.showConfirmDialog(parent, new JLabel(
              "<html>Attempt to rename files without moving them.<br/>\n" +
                  "This is a non-reversible operation.<br/><br/>\n" +
                  "We'll show you a preview before proceeding with actual renaming.<br/>\n" +
                  "Do you want to continue?"));
          if (JOptionPane.YES_OPTION != confirm1) {
            return;
          }
          final Map<Path, Path> toRename = reasonsFn.keySet().stream()
              .collect(Collectors.toMap(Function.identity(), InputLcmsFile::renameBadFile));
          Set<Path> uniqueRenamed = new HashSet<>(toRename.values());
          if (uniqueRenamed.size() != reasonsFn.size()) {
            SwingUtils.showDialog(parent, new JLabel(
                    "<html>Renaming given files according to our scheme would result<br/>\n" +
                        "in clashing file paths. Renaming cancelled. Consider renaming manually.<br/>\n" +
                        "It is preferable to not have spaces in file names, not have more than one dot<br/>\n" +
                        "and to not use non-latin characters."),
                "Not safe to rename files", JOptionPane.WARNING_MESSAGE);
            return;
          }

          final Map<Path, Path> existingRenames = new HashMap<>();
          for (Entry<Path, Path> kv : toRename.entrySet()) {
            if (Files.exists(kv.getValue())) {
              existingRenames.put(kv.getKey(), kv.getValue());
            }
          }
          if (!existingRenames.isEmpty()) {
            JPanel pane = new JPanel(new BorderLayout());
            pane.add(new JLabel("<html>Renaming given files according to our scheme would result<br/>\n" +
                "in file paths that already exist on your computer.<br/>\n" +
                "Renaming cancelled."), BorderLayout.NORTH);

            pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(existingRenames)));
            SwingUtils.showDialog(parent, pane,
                "Not safe to rename files", JOptionPane.WARNING_MESSAGE);
            return;
          }

        {
          JPanel pane = new JPanel(new BorderLayout());
          pane.add(new JLabel("<html>Proposed renaming scheme, do you agree?<br/>\n"));
          pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(toRename)));
          int confirm2 = SwingUtils.showConfirmDialog(parent, pane);
          if (JOptionPane.YES_OPTION != confirm2) {
            return;
          }
        }

        final Map<Path, Path> couldNotRename = new HashMap<>();
        final Map<Path, Path> renamedOk = new HashMap<>();
        Runnable runnable = () -> {
          for (Entry<Path, Path> kv : toRename.entrySet()) {
            try {
              Files.move(kv.getKey(), kv.getValue());
              renamedOk.put(kv.getKey(), kv.getValue());
            } catch (Exception e) {
              log.error(String.format("From '%s' to '%s' at '%s'",
                  kv.getKey().getFileName(), kv.getValue().getFileName(), kv.getKey().getParent()));
              couldNotRename.put(kv.getKey(), kv.getValue());
            }
          }
        };

        SwingUtils.DialogAndThread dat = SwingUtils.runThreadWithProgressBar("Renaming files", parent, runnable);
        dat.thread.start();
        dat.dialog.setVisible(true);

        if (!couldNotRename.isEmpty()) {
          JPanel pane = new JPanel(new BorderLayout());
          pane.add(new JLabel("<html>Unfortunately could not rename some of the files:<br/>"), BorderLayout.NORTH);
          pane.add(new JScrollPane(SwingUtils.tableFromTwoSiblingFiles(couldNotRename)), BorderLayout.CENTER);
          SwingUtils.showDialog(parent, pane, "Renaming failed", JOptionPane.WARNING_MESSAGE);
          return;
        }

        // renaming succeeded, change paths to renamed ones
        files.toAdd = files.toAdd.stream().map(path -> renamedOk.getOrDefault(path, path)).collect(Collectors.toList());

        break;
      }
    }
  }

  private void initMore() {
    SwingUtils.renameDeep(this, false, TAB_PREFIX, null);
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
    FormEntry feRam = fe(uiSpinnerRam, "ram").label("RAM (GB, 0=auto)").tooltip("Leave at zero to use a reasonable amount automatically").create();
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

  private Map<String, PropsFile> findPropsFiles(Path startDir) throws IOException {
    final Map<String, PropsFile> files = new HashMap<>();
    final Predicate<Path> filter = p -> "workflow".equalsIgnoreCase(StringUtils.afterLastDot(p.getFileName().toString()));
    Files.walk(startDir).filter(Files::isRegularFile)
        .filter(filter)
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
  }

  private Map<String, PropsFile> loadWorkflowFiles() {
    try {
      Map<String, PropsFile> files;
      Map<String, PropsFile> filesLocal = findPropsFiles(FragpipeLocations.get().getDirWorkflows());
      Map<String, PropsFile> filesStored = findPropsFiles(FragpipeLocations.get().getPathLongTermStorage());
      files = filesLocal;
      List<String> diffNames = MapUtils.keysDiffRight(filesLocal, filesStored).collect(Collectors.toList());
      List<PropsFile> diffPropFiles = Seq.seq(filesStored).filter(kv -> diffNames.contains(kv.v1))
          .map(kv -> kv.v2).toList();

      if (!diffNames.isEmpty()) {
        JLabel message = new JLabel(
            SwingUtils.makeHtml("Found workflows from previous FragPipe sessions:\n - "+Seq.seq(diffNames).toString("\n - ")));
        final String[] choices = {"Copy", "Ignore", "Delete"};
        int choice = SwingUtils
            .showChoiceDialog(this, "Load workflows?", message, choices, 0);
        switch (choice) {
          case 0:

            Path dirWorkflows = FragpipeLocations.get().getDirWorkflows();
            for (PropsFile propsFile : diffPropFiles) {
              Path p = dirWorkflows.resolve(propsFile.getPath().getFileName());
              propsFile.setPath(p);
              propsFile.save();
            }
            files = findPropsFiles(FragpipeLocations.get().getDirWorkflows());
            break;
          case 1:
            break; // do nothing
          case 2:
            if (SwingUtils.showConfirmDialogShort(this, "Sure you want to delete stored workflows?")) {
              for (PropsFile diffPropFile : diffPropFiles) {
                Files.deleteIfExists(diffPropFile.getPath());
              }
            }
            break;
          default:
            throw new IllegalStateException("Unknown option, probably forgot to add code branch for a newly added option");
        }
      }
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
    epWorkflowsDesc = SwingUtils.createClickableHtml(SwingUtils.makeHtml(""));
    epWorkflowsDesc.setPreferredSize(new Dimension(400, 50));
    uiComboWorkflows.addItemListener(e -> {
      String name = (String) uiComboWorkflows.getSelectedItem();
      PropsFile propsFile = workflows.get(name);
      if (propsFile == null) {
        if (!DEFAULT_WORKFLOW.equalsIgnoreCase(name)) {
          throw new IllegalStateException("Workflows map is not synchronized with the dropdown");
        }
        log.debug("Default workflow was selected, we don't have a file for it in the workflows/ folder");
        SwingUtils.setJEditorPaneContent(epWorkflowsDesc,
            "Defaults file is not present yet.\n"
                + "That message will be gone when we fill workflows/ folder.");
      } else {
        SwingUtils.setJEditorPaneContent(epWorkflowsDesc,
            propsFile.getProperty(PROP_WORKFLOW_DESC, "Description not present"));
      }
    });
    JButton btnWorkflowLoad = UiUtils.createButton("Load", this::actionLoadSelectedWorkflow);
    FormEntry feComboWorkflow = fe(uiComboWorkflows, "workflow-option")
        .label("Select an option to load config for:")
        .tooltip("This is purely for convenience of loading appropriate defaults\n"
            + "for various standard workflows.\n"
            + "You can totally just set up all the options yourself.").create();
    JButton btnOpenInExplorer = SwingUtils
        .createButtonOpenInFileManager(this, "Open in File Manager",
            () -> FragpipeLocations.get().getDirWorkflows());

    mu.add(p, epWorkflowsInfo).growX().spanX().wrap();
    mu.add(p, feComboWorkflow.label()).split();
    mu.add(p, feComboWorkflow.comp);
    mu.add(p, btnWorkflowLoad);
    mu.add(p, new JLabel("or save current settings as workflow")).gapLeft("15px");
    mu.add(p, UiUtils.createButton("Save", e -> Bus.post(new MessageSaveAsWorkflow(false))));
    if (Version.isDevBuild()) {
      mu.add(p, UiUtils.createButton("Save Dev", e -> Bus.post(new MessageSaveAsWorkflow(false, true))));
    }
    mu.add(p, btnOpenInExplorer).wrap();

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
    String saveDir = null;
    if (m.recursiveAdditionRoot != null) {
      saveDir = m.recursiveAdditionRoot.toString();
    } else if (!m.paths.isEmpty()) {
      saveDir = m.paths.get(0).getParent().toString();
    }
    if (saveDir != null) {
      Fragpipe.propsVarSet(ThisAppProps.PROP_LCMS_FILES_IN, saveDir);
      if (uiTextLastAddedLcmsDir != null) {
        uiTextLastAddedLcmsDir.setText(saveDir);
      }
    }

    LcmsFileAddition lfa = new LcmsFileAddition(m.paths, new ArrayList<>(m.paths));
    processAddedLcmsPaths(lfa, this, Fragpipe::getExtBinSearchPaths);

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
    List<Path> accepted = new ArrayList<>();
    List<Path> inputPaths;

    if (!m.dirs.isEmpty()) {
      inputPaths = m.dirs;
    }
    else {
      final javax.swing.filechooser.FileFilter ff = CmdMsfragger
          .getFileChooserFilter(Fragpipe.getExtBinSearchPaths());
      JFileChooser fc = FileChooserUtils
          .builder("Select a folder with LC/MS files (searched recursively)")
          .approveButton("Select").mode(FcMode.ANY).multi(true)
          .acceptAll(true).filters(Seq.of(ff).toList())
          .paths(Stream.of(Fragpipe.propsVarGet(ThisAppProps.PROP_LCMS_FILES_IN)))
          .create();

      if (JFileChooser.APPROVE_OPTION != fc.showOpenDialog(this)) {
        return;
      }
      File[] selectedFiles = fc.getSelectedFiles();
      inputPaths = Seq.of(selectedFiles).map(File::toPath).toList();
    }

    if (inputPaths.isEmpty()) {
      log.warn("No input paths were given");
      return;
    }
    Fragpipe.propsVarSet(ThisAppProps.LAST_RECURSIVE_FOLDER_ADDED, inputPaths.get(0).toString());

    for (Path p : inputPaths) {
      final Predicate<File> pred = CmdMsfragger.getSupportedFilePredicate(Fragpipe.getExtBinSearchPaths());
      PathUtils.traverseDirectoriesAcceptingFiles(p.toFile(), pred, accepted, false);
    }

    List<Path> lessGenerated = Seq.seq(accepted)
        .filter(p -> {
          final String end = "_calibrated.mgf";
          final String fnLo = p.getFileName().toString().toLowerCase();
          if (!fnLo.endsWith(end)) {
            return true;
          }

          final String fnBaseLo = StringUtils.upToLastSubstr(fnLo, end, false);
          final Path dir = p.getParent();
          long count = Seq.seq(accepted).filter(p2 -> dir.equals(p2.getParent()))
              .map(p2 -> p2.getFileName().toString().toLowerCase())
              .filter(fnLo2 -> fnLo2.startsWith(fnBaseLo))
              .count();
          if (count == 1) {
            log.warn("Not filtering out LCMS file ending with '_calibrated.mgf' as no possible parent file found:\n\t{}", p);
          }
          return count == 1; // _calibrated.mgf is the only file with that base-name, add it
        }).toList();

    if (!accepted.isEmpty()) {
      Bus.post(new MessageLcmsFilesAdded(lessGenerated, inputPaths.get(0)));
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


    Properties uiProps = FragpipeCacheUtils.tabsSave(fp.tabs, m.saveWithFieldTypes);


    Map<String, String> vetted = Seq.seq(PropertiesUtils.toMap(uiProps)).filter(kv -> {
      String k = kv.v1().toLowerCase();
      if (k.startsWith(TabConfig.TAB_PREFIX)) { // nothing from tab config goes into a workflow
        return false;
      }
      return !k.contains("workdir") && !k.contains("db-path") // no workdir or fasta file
      && !k.endsWith(".ram") && !k.endsWith(".threads"); // no ram and threads from Wrokflow tab
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

    String curName = (String)uiComboWorkflows.getSelectedItem();
    String curDesc = epWorkflowsDesc.getText();

    MigUtils mu = MigUtils.get();
    final JPanel p = mu.newPanel(null, true);
    UiText uiTextName = UiUtils.uiTextBuilder().cols(20).ghost("Name is required").text(curName).create();
    uiTextName.setName("file-name");
    final JEditorPane ep = new JEditorPane("text/html", SwingUtils.wrapInStyledHtml(SwingUtils.tryExtractHtmlBody(curDesc)));
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

    uiCheckProcessEachExperimentSeparately = UiUtils
        .createUiCheck("Process each experiment separately", false);
    FormEntry feProcExpSep = mu.feb(uiCheckProcessEachExperimentSeparately)
        .name("process-exps-separately")
        .tooltip("Process each experiment in isolation. Don't make combined protxml files.\n").create();

    mu.add(p, btnFilesAddFiles).split();
    mu.add(p, btnFilesAddFolder);
    mu.add(p, btnFilesRemove);

    final boolean addDebugBtn = true;
    if (!addDebugBtn) {
      mu.add(p, btnFilesClear).wrap();
    } else {
      uiTextLastAddedLcmsDir = UiUtils.uiTextBuilder().cols(20)
          .text(Fragpipe.propsVarGet(ThisAppProps.LAST_RECURSIVE_FOLDER_ADDED, "")).create();
      JButton btnDebugFolderAdd = UiUtils.createButton("Add recent", e -> {
        //String add = "D:\\ms-data\\TMTIntegrator_v1.1.4\\TMT-I-Test\\tmti-test-data_5-min-cuts";
        Path existing = PathUtils.existing(uiTextLastAddedLcmsDir.getNonGhostText());
        if (existing == null) {
          SwingUtils.showInfoDialog(this, "Path not exists:\n" + uiTextLastAddedLcmsDir.getNonGhostText(), "Warning");
        } else {
          Bus.post(new MessageLcmsAddFolder(Seq.of(existing).toList()));
        }
      });
      btnDebugFolderAdd.setBackground(Color.PINK);
      mu.add(p, btnFilesClear);
      mu.add(p, btnDebugFolderAdd).gapLeft("20px");
      mu.add(p, uiTextLastAddedLcmsDir).growX().pushX().wrap();
    }

    mu.add(p,
        new JLabel("Assign files to Experiments/Groups (select rows to activate action buttons):"))
        .spanX().wrap();
    mu.add(p, btnGroupsConsecutive).split();
    mu.add(p, btnGroupsByParentDir);
    mu.add(p, btnGroupsByFilename);
    mu.add(p, btnGroupsAssignToSelected);
    mu.add(p, btnGroupsClear);
    mu.add(p, feProcExpSep.comp).wrap();

    p.add(scrollPaneRawFiles, mu.ccGx().wrap());

    return p;
  }

  public boolean isProcessEachExpSeparately() {
    return uiCheckProcessEachExperimentSeparately.isSelected();
  }

  private JButton button(String text, Supplier<Object> message) {
    return UiUtils.createButton(text, e -> Bus.post(message.get()));
  }

  private void createFileTable() {
    tableModelRawFiles = createTableModelRawFiles();
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

  private void actionLoadSelectedWorkflow(ActionEvent e) {
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
          Fragpipe fp = Fragpipe.createDummy();
          Properties defaults = FragpipeCacheUtils.tabsSave(fp.tabs);
          fp.dispose();
          Bus.post(new MessageLoadUi(defaults));
          return;
//          throw new UnsupportedOperationException(
//              "Not implemented loading defaults from jar without having the defaults file in workflows/ folder");
        }
      }

      log.debug("Reloading file from disk, in case it has changed: {}", propsFile.getPath());
      try {
        propsFile.load();
      } catch (IOException ex) {
        SwingUtils.showErrorDialogWithStacktrace(ex, this, true);
        log.error("Error re-loading workflow file", ex);
      }

      if (propsFile.containsKey("workflow.workflow-option")) {
        propsFile.setProperty("workflow.workflow-option", workflow);
      }
      Bus.post(new MessageLoadUi(propsFile));
    }
  }

  public static class LcmsFileAddition {
    public List<Path> paths;
    public List<Path> toAdd;

    public LcmsFileAddition(List<Path> paths, List<Path> toAdd) {
      this.paths = paths;
      this.toAdd = toAdd;
    }
  }
}
