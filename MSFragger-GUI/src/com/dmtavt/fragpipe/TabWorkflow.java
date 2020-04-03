package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageLcmsAddFiles;
import com.dmtavt.fragpipe.messages.MessageLcmsAddFolder;
import com.dmtavt.fragpipe.messages.MessageLcmsClearFiles;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesAdded;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesList;
import com.dmtavt.fragpipe.messages.MessageLcmsGroupAction;
import com.dmtavt.fragpipe.messages.MessageLcmsGroupAction.Type;
import com.dmtavt.fragpipe.messages.MessageLcmsRemoveSelected;
import com.dmtavt.fragpipe.messages.MessageType;
import com.github.chhh.utils.FileDrop;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCombo;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Dimension;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.RandomUtils;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsInputFileTable;
import umich.msfragger.gui.MsfraggerGuiFrameUtils;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.UniqueLcmsFilesTableModel;

public class TabWorkflow extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabWorkflow.class);
  private final MigUtils mu = MigUtils.get();
  private JButton btnFilesRemove;
  private JButton btnFilesClear;

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
  private JPanel pWorkflows;
  private JPanel pLcmsFiles;
  private JPanel pContent;

  public static final String TAB_PREFIX = "workflow.";

  public TabWorkflow() {
    init();
    initMore();
  }

  private void initMore() {
    Bus.register(this);
  }

  private void init() {
    this.setLayout(new MigLayout(new LC().fillX()));

    add(createPanalWorkflows(), mu.ccGx().wrap());
    add(createPanalLcmsFiles(), mu.ccGx().wrap());
  }

  private String genSentence() {
    int numWords = RandomUtils.nextInt(10, 120);
    return IntStream.range(0, numWords)
        .mapToObj(i -> genWord())
        .collect(StringBuilder::new, (s1, s2) -> s1.append(" ").append(s2), (s1, s2) -> s1.append(" ").append(s2))
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

  private JPanel createPanalWorkflows() {
    JPanel p = mu.panel(false, "Workflows");

    epWorkflowsInfo = SwingUtils.createClickableHtml(true,
        "FragPipe and its collection of tools support multiple proteomic workflows.\n"
        + "Select an option in the dropdown menu below to configure "
        + "all the tools. You can tweak the options yourself after loading.\n"
        + "Also, <a href=\"https://google.com\">see the tutorial</a>");
    final UiCombo uiCombo = UiUtils.createUiCombo(Arrays
        .asList("Open Search", "Closed Search", "Non-specific Search", "Speclib generation",
            "TMT-10 Quant"));
    final JEditorPane epWorkflowsDesc = SwingUtils.createClickableHtml(SwingUtils.makeHtml(genSentence()));
    epWorkflowsDesc.setPreferredSize(new Dimension(400, 50));
    uiCombo.addItemListener(e -> SwingUtils.setJEditorPaneContent(epWorkflowsDesc, genSentence()));
    JButton btnWorkflowLoad = UiUtils.createButton("Load", e -> {
      log.debug("Load workflow button clicked");
      SwingUtils.showInfoDialog(this, "User clicked " + (String)uiCombo.getSelectedItem(), "Loading");
    });


    p.add(epWorkflowsInfo, mu.ccL().growX().spanX().wrap());
    p.add(btnWorkflowLoad, mu.ccL().split());
    p.add(uiCombo, mu.ccL().wrap());
    p.add(epWorkflowsDesc, mu.ccL().growX().spanX().wrap());

    return p;
  }

  private JPanel createPanalLcmsFiles() {
    JPanel p = mu.panel(false, "Input LC/MS Files");

    JButton btnFilesAddFiles = button("Add files", MessageLcmsAddFiles::new);
    JButton btnFilesAddFolder = button("Add folder recursively", MessageLcmsAddFolder::new);
    btnFilesRemove = button("Remove selected files", MessageLcmsRemoveSelected::new);
    btnFilesRemove.setEnabled(false);
    btnFilesClear = button("Clear files", MessageLcmsClearFiles::new);
    btnFilesClear.setEnabled(false);

    btnGroupsConsecutive = button("Consecutive", () -> new MessageLcmsGroupAction(Type.CONSECUTIVE));
    btnGroupsByParentDir = button("By parent directory", () -> new MessageLcmsGroupAction(Type.BY_PARENT_DIR));
    btnGroupsByFilename = button("By file name", () -> new MessageLcmsGroupAction(Type.BY_FILE_NAME));
    btnGroupsAssignToSelected = button("Set experiment/replicate", () -> new MessageLcmsGroupAction(Type.SET_EXP));
    btnGroupsClear = button("Clear groups", () -> new MessageLcmsGroupAction(Type.CLEAR_GROUPS));

    tableRawFilesFileDrop = makeFileDrop();

    createFileTable();

    mu.add(p, btnFilesAddFiles).split();
    mu.add(p, btnFilesAddFolder);
    mu.add(p, btnFilesRemove);
    mu.add(p, btnFilesClear).wrap();
    mu.add(p, new JLabel("Assign files to Experiments/Groups (select rows to activate action buttons):")).spanX().wrap();
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
      Predicate<File> pred = CmdMsfragger.getSupportedFilePredicate(Fragpipe.getExtBinSearchPaths());
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
  public void onLcmsGroupAction(MessageLcmsGroupAction m) {
    log.debug("Got MessageLcmsGroupAction of type: {}", m.type.toString());
  }
}
