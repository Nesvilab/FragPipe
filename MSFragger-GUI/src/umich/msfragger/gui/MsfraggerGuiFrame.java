/*
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package umich.msfragger.gui;

import static com.dmtavt.fragpipe.TabConfig.createFraggerCitationHtml;
import static umich.msfragger.gui.MsfraggerGuiFrameUtils.createPhilosopherCitationHtml;
import static umich.msfragger.gui.MsfraggerGuiFrameUtils.initEditorPaneSeqDb;

import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.JTextComponent;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.SubscriberExceptionEvent;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.Version;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.cmd.ToolingUtils;
import umich.msfragger.gui.MsfraggerGuiFrameUtils.LcmsFileAddition;
import umich.msfragger.gui.api.LogbackJTextPaneAppender;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.UniqueLcmsFilesTableModel;
import umich.msfragger.gui.dialogs.ExperimentNameDialog;
import com.dmtavt.fragpipe.messages.MessageAppendToConsole;
import com.dmtavt.fragpipe.messages.MessageDbUpdate;
import com.dmtavt.fragpipe.messages.MessageDecoyTag;
import com.dmtavt.fragpipe.messages.MessageExternalProcessOutput;
import com.dmtavt.fragpipe.messages.MessageIsUmpireRun;
import com.dmtavt.fragpipe.messages.MessageKillAll;
import com.dmtavt.fragpipe.messages.MessageKillAll.REASON;
import com.dmtavt.fragpipe.messages.MessageLastRunWorkDir;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesAdded;
import com.dmtavt.fragpipe.messages.MessageLcmsFilesList;
import com.dmtavt.fragpipe.messages.MessageLoadAllForms;
import com.dmtavt.fragpipe.messages.MessagePythonBinSelectedByUser;
import com.dmtavt.fragpipe.messages.MessageReportEnablement;
import com.dmtavt.fragpipe.messages.MessageRun;
import com.dmtavt.fragpipe.messages.MessageSaveUiState;
import com.dmtavt.fragpipe.messages.MessageSaveCache;
import com.dmtavt.fragpipe.messages.MessageSaveLog;
import com.dmtavt.fragpipe.messages.MessageSearchType;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageTipNotification;
import com.dmtavt.fragpipe.messages.MessageType;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcPanel;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.dbslice.DbSlice;
import umich.msfragger.params.fragger.FraggerMigPanel;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.params.fragger.MsfraggerProps;
import umich.msfragger.params.philosopher.ReportPanel;
import umich.msfragger.params.speclib.SpecLibGen;
import umich.msfragger.params.tmtintegrator.TmtiPanel;
import umich.msfragger.params.umpire.UmpirePanel;
import com.github.chhh.utils.FastaUtils.FastaDecoyPrefixSearchResult;
import com.github.chhh.utils.FileDrop;
import com.github.chhh.utils.GhostText;
import com.github.chhh.utils.LogUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.PythonInfo;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.ISimpleTextComponent;
import com.github.chhh.utils.swing.TextConsole;

public class MsfraggerGuiFrame extends javax.swing.JFrame {

  private static final org.slf4j.Logger log = LoggerFactory.getLogger(MsfraggerGuiFrame.class);
  private final Object procRunLock = new Object();

  public FraggerMigPanel fraggerMigPanel;
  TextConsole console;
  ExecutorService exec = Executors.newFixedThreadPool(1);;


  //e static final String TEXT_SAME_SEQ_DB = "<Same as in MSFragger>";
  Color defTextColor;
  GhostText ghostTextPepProph;
  GhostText ghostTextProtProp;
  BalloonTip balloonMsfragger;
  BalloonTip balloonPhilosopher;
  Color balloonBgColor = Color.WHITE;

  HashMap<String, BalloonTip> tipMap = new HashMap<>();
  static final String TIP_NAME_FRAGGER_JAVA_VER = "msfragger.java.min.ver";

  SimpleETable tableRawFiles;
  UniqueLcmsFilesTableModel tableModelRawFiles;
  FileDrop tableRawFilesFileDrop;

  public static final SearchTypeProp DEFAULT_TYPE = SearchTypeProp.closed;

  String textPepProphetFocusGained = null;
  String textReportAnnotateFocusGained = null;
  String textReportFilterFocusGained = null;
  String textReportAbacusFocusGained = null;
  String textDecoyTagFocusGained = null;
  String textLabelfreeFocusGained = null;

  Pattern reDecoyTagReportAnnotate = Pattern.compile("--prefix\\s+([^\\s]+)");
  Pattern reDecoyTagReportFilter = Pattern.compile("--tag\\s+([^\\s]+)");
  Pattern reDecoyTagReportAbacus = Pattern.compile("--tag\\s+([^\\s]+)");
  Pattern reDecoyTagPepProphCmd = Pattern.compile("--decoy\\s+([^\\s]+)");
  Pattern reDecoyTagSequenceDb = Pattern.compile("([^\\s]+)");

  static final String UNKNOWN_VERSION = "Unknown";
  String fraggerVer = UNKNOWN_VERSION;
  String philosopherVer = UNKNOWN_VERSION;

  UmpirePanel umpirePanel = null;
  private JScrollPane umpireScroll = null;

  public static final Color COLOR_GREEN = new Color(105, 193, 38);
  public static final Color COLOR_GREEN_DARKER = new Color(104, 184, 55);
  public static final Color COLOR_GREEN_DARKEST = new Color(82, 140, 26);
  public static final Color COLOR_RED = new Color(236, 99, 80);
  public static final Color COLOR_RED_DARKER = new Color(166, 56, 68);
  public static final Color COLOR_RED_DARKEST = new Color(155, 35, 29);
  public static final Color COLOR_BLACK = new Color(0, 0, 0);

  final Color COLOR_TOOL = new Color(140, 3, 89);
  final Color COLOR_WORKDIR = new Color(6, 2, 140);
  final Color COLOR_CMDLINE = new Color(0, 107, 109);

  private static final String ACTION_EXPORT_LOG = "Export-Log";

  public MsfraggerGuiFrame() {
    EventBus.getDefault().register(this);
    initComponents();
    ProcessManager.get().init();
    initMore();
  }

  private void initActions() {
    AbstractAction exportToTextFile = new AbstractAction(ACTION_EXPORT_LOG) {

      {
        putValue(NAME, ACTION_EXPORT_LOG);
        putValue(ACTION_COMMAND_KEY, ACTION_EXPORT_LOG);
      }

      @Override
      public void actionPerformed(ActionEvent e) {
        MsfraggerGuiFrameUtils.exportLogToFile(MsfraggerGuiFrame.this);
      }
    };

    panelRun.getActionMap().put(exportToTextFile.getValue(Action.NAME), exportToTextFile);
  }

  public JTextField getTxtWorkingDir() {
    return txtWorkingDir;
  }

  public JCheckBox getChkRunPeptideProphet() {
    return chkRunPeptideProphet;
  }

  public JCheckBox getChkRunProteinProphet() {
    return chkRunProteinProphet;
  }

  public ReportPanel getPanelReportOptions() {
    return panelReportOptions;
  }

  public JTextField getTextSequenceDbPath() {
    return textSequenceDbPath;
  }

  public JTextField getTextBinMsfragger() {
    return textBinMsfragger;
  }

  public JButton getBtnRun() {
    return btnRun;
  }

  public JButton getBtnStop() {
    return btnStop;
  }

  public JTextField getTextDecoyTagSeqDb() {
    return textDecoyTagSeqDb;
  }

  public CrystalcPanel getPanelCrystalc() {
    return panelCrystalc;
  }

  public JCheckBox getCheckCombinedPepxml() {
    return checkCombinedPepxml;
  }

  public JTextArea getTextPepProphCmd() {
    return textPepProphCmd;
  }

  public JCheckBox getCheckProcessGroupsSeparately() {
    return checkProcessGroupsSeparately;
  }

  public JTextArea getTxtProteinProphetCmdLineOpts() {
    return txtProteinProphetCmdLineOpts;
  }

  public umich.msfragger.params.ionquant.QuantJPanel getPanelQuant() {
    return panelQuant;
  }

  public umich.msfragger.params.ptmshepherd.PtmshepherdJPanel getPtmshepherdPanel() {
    return ptmshepherdPanel;
  }

  public umich.msfragger.params.speclib.SpeclibPanel getSpeclibPanel1() {
    return speclibPanel1;
  }

  public JLabel getLblFraggerJavaVer() {
    return lblFraggerJavaVer;
  }

  public JLabel getLblFastaCount() {
    return lblFastaCount;
  }

  public JTextField getTextBinPhilosopher() {
    return textBinPhilosopher;
  }

  public JLabel getLabelPhilosopherInfo() {
    return lblPhilosopherInfo;
  }

  public TmtiPanel getTmtPanel() {
    return tmtiPanel1;
  }

  private void initMore() {

    setTitle(Version.PROGRAM_TITLE + " (v" + Version.version() + ")");
    setLocale(Locale.ROOT);

    {
      JLabel c = new JLabel();
      c.setFont(lblFraggerJavaVer.getFont());
      c.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      c.setAlignmentX(Component.CENTER_ALIGNMENT);
      c.setText(
          "<html>Tabs on top represent processing steps and will be performed sequentially.<br/>\n"
              + "Tabs will become enabled once the tools on this panel are configured.");
      panelBottomHints.add(c);
    }
    {
      Properties props = ThisAppProps.getRemotePropertiesWithLocalDefaults();
//      Properties p = ThisAppProps.getLocalProperties(); // for testing
      String linkUrl = props.getProperty(ThisAppProps.PROP_SETUP_TUTORIAL_URL,
          "https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html");

      JEditorPane c = SwingUtils.createClickableHtml(
          "<a href='" + linkUrl + "'>Configuration Help</a>");
      c.setFont(lblFraggerJavaVer.getFont());
      c.setAlignmentX(Component.CENTER_ALIGNMENT);
      JPanel p = new JPanel();
      p.setAlignmentX(Component.CENTER_ALIGNMENT);
      p.add(c);
      panelBottomHints.add(p);
    }

    console = new TextConsole();
    final Font currentFont = console.getFont();
    console.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    console.setContentType("text/plain; charset=UTF-8");
    console.addMouseListener(new MouseAdapter() {

      @Override
      public void mouseReleased(MouseEvent e) {
        if (e.isPopupTrigger()) {
          doPop(e);
        }
      }

      private void doPop(MouseEvent e) {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem ctxItemExport = new JMenuItem(panelRun.getActionMap().get(ACTION_EXPORT_LOG));
        ctxItemExport.setText("Export to text file");
        menu.add(ctxItemExport);
        menu.show(e.getComponent(), e.getX(), e.getY());

      }
    });
    consoleScrollPane.setViewportView(console);

    defTextColor = UIManager.getColor("TextField.foreground");
    if (defTextColor == null) {
      defTextColor = Color.BLACK;
    }

    // check if fragger jar points to a correct location
    if (!MsfraggerGuiFrameUtils.validateMsfraggerJarContents(textBinMsfragger.getText())) {
      log.debug("Msfragger jar is not valid");
    }

    if (MsfraggerGuiFrameUtils.validatePhilosopherPath(textBinPhilosopher.getText()) == null) {
      enablePhilosopherPanels(false);
    }

    tableModelRawFiles = MsfraggerGuiFrameUtils.createTableModelRawFiles();
    tableModelRawFiles.addTableModelListener(e -> {
      List<InputLcmsFile> files = tableModelRawFiles.dataCopy();
      EventBus.getDefault().post(new MessageLcmsFilesList(MessageType.UPDATE, files));
    });
    tableRawFiles = new LcmsInputFileTable(tableModelRawFiles);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnRawClear);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsConsecutive);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByParentDir);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByFilename);
    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsClear);
    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnRawRemove);
    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnGroupsAssignToSelected);
    tableRawFiles.fireInitialization();
    tableRawFiles.setFillsViewportHeight(true);
    scrollPaneRawFiles.setViewportView(tableRawFiles);

    // Drag and drop support for files from Explorer to the Application
    // this works only when tableRawFiles.setFillsViewportHeight(true) is called.
//        tableRawFiles.setDragEnabled(true);
//        tableRawFiles.setDropMode(DropMode.ON);
//        tableRawFiles.setFillsViewportHeight(true);
//        TransferHandler origHandler = tableRawFiles.getTransferHandler();
//        SimpleETableTransferHandler newHandler = new SimpleETableTransferHandler();
//        tableRawFiles.setTransferHandler(newHandler);
    // dropping onto enclosing JPanel works.
    tableRawFilesFileDrop = new FileDrop(panelSelectedFiles, true, files -> {
      Predicate<File> pred = CmdMsfragger.getSupportedFilePredicate(getExtBinSearchPaths());
      List<Path> accepted = new ArrayList<>(files.length);
      for (File f : files) {
        PathUtils.traverseDirectoriesAcceptingFiles(f, pred, accepted, false);
      }
      if (!accepted.isEmpty()) {
        EventBus.getDefault().post(new MessageLcmsFilesAdded(accepted));
      }
    });

    textBinPython.addFocusListener(new FocusAdapter() {
      @Override
      public void focusLost(FocusEvent e) {
        EventBus.getDefault().post(new MessagePythonBinSelectedByUser(textBinPython.getText()));
      }
    });

    fraggerMigPanel = new FraggerMigPanel();
    final int fraggerTabIndex = 3;
    final String fraggerTabName = "MSFragger";
    tabPane.add(fraggerMigPanel, fraggerTabIndex);
    tabPane.setTitleAt(fraggerTabIndex, fraggerTabName);


    // set icons for tabs
    Map<String, Integer> mapTabNameToIdx = new HashMap<>();
    for (int i = 0, tabCount = tabPane.getTabCount(); i < tabCount; i++) {
      mapTabNameToIdx.put(tabPane.getTitleAt(i), i);
    }
    setTabIcon(mapTabNameToIdx, "Config", "/umich/msfragger/gui/icons/146-wrench.png");
    setTabIcon(mapTabNameToIdx, "Select LC/MS Files", "/umich/msfragger/gui/icons/198-download2.png");
    setTabIcon(mapTabNameToIdx, "Database", "/umich/msfragger/gui/icons/093-drawer.png");
    setTabIcon(mapTabNameToIdx, "Downstream", "/umich/msfragger/gui/icons/328-move-down.png");
    setTabIcon(mapTabNameToIdx, "Report", "/umich/msfragger/gui/icons/185-clipboard.png");
    setTabIcon(mapTabNameToIdx, fraggerTabName, "/umich/msfragger/gui/icons/bolt-16.png");
    //setTabIcon(mapTabNameToIdx, "", "");

    exec.submit(() -> MsfraggerGuiFrameUtils.validateAndSaveMsfraggerPath(this, textBinMsfragger.getText()));
    exec.submit(() -> MsfraggerGuiFrameUtils.validateAndSavePhilosopherPath(this, textBinPhilosopher.getText()));
    exec.submit(() -> Version.checkUpdates());
    exec.submit(() -> MsfraggerGuiFrameUtils.checkPreviouslySavedParams(MsfraggerGuiFrame.this));

    // The python check must be run before DbSlice and SpecLibGen.
    // Don't run these checks asynchronously
    exec.submit(() -> MsfraggerGuiFrameUtils.checkPython(MsfraggerGuiFrame.this));
    exec.submit(() -> MsfraggerGuiFrameUtils.validateDbslicing(fraggerVer));
    exec.submit(MsfraggerGuiFrameUtils::validateSpeclibgen);


    exec.submit(() -> MsfraggerGuiFrameUtils.validateMsadjusterEligibility(fraggerVer));
    exec.submit(() -> MsfraggerGuiFrameUtils.validateMsfraggerMassCalibrationEligibility(fraggerVer));


    // submitting all "loadLast" methods for invocation
    for (Method method : this.getClass().getDeclaredMethods()) {
      // TODO: Old 'loadLast' mechanism is mostly replaced by auto save/load of components by 'name'
      if (method.getName().startsWith("loadLast") && method.getParameterCount() == 0) {
        exec.submit(() -> method.invoke(this));
      }
    }

    // Force loading form caches
    EventBus.getDefault().post(MessageLoadAllForms.newForCaching());

    initActions();
  }

  public Path getBinMsfragger() {
    String text = textBinMsfragger.getText();
    if (StringUtils.isNullOrWhitespace(text)) {
      return null;
    }
    try {
      return Paths.get(text);
    } catch (Exception e) {
      throw new IllegalArgumentException(e);
    }
  }

  private void setTabIcon(Map<String, Integer> mapTabNameToIndex, String name, String iconPathInJar) {
    Integer index = mapTabNameToIndex.get(name);
    if (name != null) {
      ImageIcon icon = new ImageIcon(
          getClass().getResource(iconPathInJar));
      if (tabPane != null && index != null && icon != null) {
        tabPane.setIconAt(index, icon);
      }
    } else {
      throw new IllegalStateException("Tab with name '" + name + "' does not exist.");
    }
  }

  //region EventBus listeners
  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onPythonBinSelectedByUser(MessagePythonBinSelectedByUser m) {
    MsfraggerGuiFrameUtils.validateAndSavePython(m.path, true, this);
  }


  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessagePythonInfoChanged(PythonInfo.MessageInfoChanged m) {
    PythonInfo pi = PythonInfo.get();
    if (pi.isInitialized()) {
      textBinPython.setText(pi.getCommand());
      lblPythonInfo.setText("Version: " + pi.getVersion());
    } else {
      textBinPython.setText("");
      lblPythonInfo.setText("N/A");
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onDecoyTagChanged(MessageDecoyTag m) {
    updateDecoyTagSeqDb(m.tag, false);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessageReportEnablement(MessageReportEnablement m) {
    final boolean selected = m.isReportEnabled;
    Container[] comps = new Container[]{
        panelReportOptions
    };
    for (Container c : comps) {
      SwingUtils.enableComponents(c, selected);
    }
  }

  @Subscribe
  public void onSubscriberException(SubscriberExceptionEvent msg) {
    SwingUtils.showErrorDialogWithStacktrace(msg.throwable, this);
  }

  @Subscribe
  public void onMessageSearchType(MessageSearchType m) {
    final SearchTypeProp t = m.type;

    for (Method method : this.getClass().getDeclaredMethods()) {
      // method name starts with "loadDefaults" and has MessageSearchType as the only parameter
      if (method.getName().startsWith("loadDefaults") && method.getParameterCount() == 1 &&
          SearchTypeProp.class.equals(method.getParameterTypes()[0])) {
        if (method.getName().toLowerCase().contains("decoytag")) {
          log.debug("Skipping 'loadDefaults' method with 1 param of SearchTypeProp.class: {}", method.getName());
        } else {
          log.debug("Invoking 'loadDefaults' method with 1 param of SearchTypeProp.class: {}",
              method.getName());
          exec.submit(() -> method.invoke(MsfraggerGuiFrame.this, t));
        }
      }
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onDbsliceMessage1(DbSlice.Message1 m) {
    log.debug("Got DbSlice.Message1 m = {} [append={}, error={}]", m.text, m.append, m.isError);
    FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(lblDbsliceInfo1), m);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onDbsliceMessage2(DbSlice.Message2 m) {
    log.debug("Got DbSlice.Message2 m = {} [append={}, error={}]", m.text, m.append, m.isError);
    FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(epDbsliceInfo), m);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN)
  public void onDbsliceInitDone(DbSlice.MessageInitDone m) {
    log.debug("Got DbSlice.MessageInitDone m [success={}]", m.isSuccess);
    MsfraggerGuiFrameUtils.actionDbspliceInitDone(epDbsliceInfo, m);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onSpeclibgenMessage1(SpecLibGen.Message1 m) {
    FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(lblSpeclibInfo1), m);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onSpeclibgenMessage2(SpecLibGen.Message2 m) {
    FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(epSpeclibInfo2), m);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN)
  public void onSpeclibgenInitDone(SpecLibGen.MessageInitDone m) {
    log.debug("Got SpecLibGen.MessageInitDone m [success={}]", m.isSuccess);
//    FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(epSpeclibInfo2),
//        new SpecLibGen.Message2(true, !m.isSuccess, text));

    final Map<SpecLibGen.MessageInitDone.REASON, String> map = new HashMap<>();
    map.put(SpecLibGen.MessageInitDone.REASON.PY_VER, "Python 3 is required.");
    map.put(SpecLibGen.MessageInitDone.REASON.WRONG_FRAGGER,
        "Latest version of MSFragger is required.");
    map.put(SpecLibGen.MessageInitDone.REASON.PY_MODULES, "Python modules required.");
    map.put(SpecLibGen.MessageInitDone.REASON.NOT_UNPACKED, "Error unpacking.");
    StringBuilder sb = new StringBuilder();
    sb.append("Spectral library generation ");
    sb.append("<b>");
    sb.append(m.isSuccess ? "enabled" : "disabled");
    sb.append("</b>");
    sb.append(" for non-ion mobility data (with SpectraST).");
    if (!m.isSuccess) {
      String reasons = m.reasons.stream().flatMap(reason ->
          map.containsKey(reason) ? Stream.of(map.get(reason)) : Stream.empty())
          .collect(Collectors.joining(" <br/>"));
      if (reasons.length() > 0) {
        sb.append(" <br/>").append(reasons);
      }
      sb.append(" <br/>").append("FragPipe will work fine without this functionality.");
    }

    sb.append("<br/><br/>Spectral library generation ");
    sb.append("<b>");
    sb.append(SpecLibGen.get().checkPythonErrorModulesEasypqp().isSuccess ? "enabled" : "disabled");
    sb.append("</b>");
    sb.append(" for ion mobility data (with EasyPQP).");
    if (!SpecLibGen.get().checkPythonErrorModulesEasypqp().isSuccess) {
      sb.append(" <br/>").append("EasyPQP not installed");
      sb.append(" <br/>").append("follow instructions at: <a href=\"https://github.com/grosenberger/easypqp\">https://github.com/grosenberger/easypqp</a>");
      sb.append(" <br/>").append("FragPipe will work fine without this functionality.");
    }

    FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(epSpeclibInfo2),
        new DbSlice.Message2(true, false, sb.toString()));

    if (!m.isSuccess) {
      // attach link with instructions
      Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
//      Properties p = ThisAppProps.getLocalProperties(); // for testing
      String linkUrl = p.getProperty(MsfraggerProps.PROP_SPECLIBGEN_INSTRUCTIONS_URL,
          "https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html");
      String instructions = String.format(
          "<br/>See <a href='%s'>configuration help</a> online for instructions how to enable.",
          linkUrl);
      FragpipeUiHelpers.messageToTextComponent(ISimpleTextComponent.from(epSpeclibInfo2),
          new SpecLibGen.Message2(true, false, instructions));
    }
  }

  @Subscribe
  public void onLcmsFilesAdded(MessageLcmsFilesAdded m) {
    if (m.paths == null || m.paths.isEmpty()) {
      log.warn("Got MessageLcmsFilesAdded with empty paths");
      return;
    }

    // save locations
    String savePath = m.recursiveAdditionRoot != null ? m.recursiveAdditionRoot.toString() : m.paths.get(0).toString();
    ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, savePath);

    LcmsFileAddition lfa = new LcmsFileAddition(m.paths, new ArrayList<>(m.paths));
    MsfraggerGuiFrameUtils.processAddedLcmsPaths(lfa, this);

    // add the files
    tableModelRawFiles.dataAddAll(
            lfa.toAdd.stream().map(p -> new InputLcmsFile(p, ThisAppProps.DEFAULT_LCMS_EXP_NAME)).collect(Collectors.toList())
    );
  }



  @Subscribe
  public void onTipNotification(MessageTipNotification m) {
    SwingUtilities.invokeLater(() -> {
      tipMap.computeIfPresent(m.key, (key, tip) -> {
        tip.closeBalloon();
        return null;
      });
      BalloonTip tip = new BalloonTip(btnAboutInConfig, SwingUtils.createClickableHtml(m.text, balloonBgColor),
          new RoundedBalloonStyle(5, 5, balloonBgColor, Color.BLACK), true);
      tip.setVisible(true);
      tipMap.put(m.key, tip);
    });
  }

  @Subscribe
  public void onShowAbout(MessageShowAboutDialog m) {
    MsfraggerGuiFrameUtils.onShowAboutDialog(this, m);
  }

  //endregion

  public boolean isRunUmpireSe() {
    return checkEnableDiaumpire.isSelected() && umpirePanel != null && umpirePanel.checkRunUmpireSe
        .isSelected();
  }

  void enablePhilosopherPanels(boolean enabled) {
    SwingUtils.enableComponents(panelPeptideProphet, enabled);
    chkRunPeptideProphet.setSelected(enabled);
    SwingUtils.enableComponents(panelProteinProphet, enabled);
    chkRunProteinProphet.setSelected(enabled);
    SwingUtils.enableComponents(panelReport, enabled);
    panelReportOptions.activate(enabled);

    btnDbDownload.setEnabled(enabled);
    final String tooltip = enabled ?
        "Download protein database and add decoys + contaminants" :
        "<html>Automatic downloading of database requires Philospher<br/>\n"
            + "binary to be selected on Config tab";
    btnDbDownload.setToolTipText(tooltip);
  }

  /**
   * This method is called from within the constructor to initialize the form. WARNING: Do NOT
   * modify this code. The content of this method is always regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel2 = new javax.swing.JLabel();
        tabPane = new javax.swing.JTabbedPane();
        scrollConfig = new javax.swing.JScrollPane();
        panelConfig = new javax.swing.JPanel();
        panelMsfraggerConfig = new javax.swing.JPanel();
        btnMsfraggerBinDownload = new javax.swing.JButton();
        btnMsfraggerBinBrowse = new javax.swing.JButton();
        textBinMsfragger = new javax.swing.JTextField();
        jScrollPane1 = new javax.swing.JScrollPane();
        editorMsfraggerCitation = new javax.swing.JEditorPane();
        lblFraggerJavaVer = new javax.swing.JLabel();
        btnMsfraggerUpdate = new javax.swing.JButton();
        panelPhilosopherConfig = new javax.swing.JPanel();
        btnPhilosopherBinDownload = new javax.swing.JButton();
        btnPhilosopherBinBrowse = new javax.swing.JButton();
        textBinPhilosopher = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        lblPhilosopherInfo = new javax.swing.JLabel();
        jScrollPane3 = new javax.swing.JScrollPane();
        editorPhilosopherLink = new javax.swing.JEditorPane();
        btnFindTools = new javax.swing.JButton();
        lblFindAutomatically = new javax.swing.JLabel();
        btnClearCache = new javax.swing.JButton();
        btnAboutInConfig = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        lblDbsliceInfo1 = new javax.swing.JLabel();
        jScrollPane6 = new javax.swing.JScrollPane();
        epDbsliceInfo = new javax.swing.JEditorPane();
        checkEnableDiaumpire = new javax.swing.JCheckBox();
        jPanel1 = new javax.swing.JPanel();
        lblSpeclibInfo1 = new javax.swing.JLabel();
        scrollEpSpeclibInfo2 = new javax.swing.JScrollPane();
        epSpeclibInfo2 = new javax.swing.JEditorPane();
        jPanel3 = new javax.swing.JPanel();
        btnBrowseBinPython = new javax.swing.JButton();
        textBinPython = new javax.swing.JTextField();
        lblPythonInfo = new javax.swing.JLabel();
        panelBottomHints = new javax.swing.JPanel();
        panelSelectFiles = new javax.swing.JPanel();
        panelSelectedFiles = new javax.swing.JPanel();
        btnRawAddFiles = new javax.swing.JButton();
        btnRawClear = new javax.swing.JButton();
        scrollPaneRawFiles = new javax.swing.JScrollPane();
        btnRawAddFolder = new javax.swing.JButton();
        btnRawRemove = new javax.swing.JButton();
        btnGroupsConsecutive = new javax.swing.JButton();
        jLabel10 = new javax.swing.JLabel();
        btnGroupsByParentDir = new javax.swing.JButton();
        btnGroupsByFilename = new javax.swing.JButton();
        btnGroupsClear = new javax.swing.JButton();
        btnGroupsAssignToSelected = new javax.swing.JButton();
        panelSequenceDb = new javax.swing.JPanel();
        panelDbInfo = new javax.swing.JPanel();
        textSequenceDbPath = new javax.swing.JTextField();
        btnBrowse = new javax.swing.JButton();
        jLabel5 = new javax.swing.JLabel();
        textDecoyTagSeqDb = new javax.swing.JTextField();
        btnTryDetectDecoyTag = new javax.swing.JButton();
        lblFastaCount = new javax.swing.JLabel();
        jScrollPane5 = new javax.swing.JScrollPane();
        editorSequenceDb = new javax.swing.JEditorPane();
        btnDbDownload = new javax.swing.JButton();
        scrollDownstream = new javax.swing.JScrollPane();
        panelDownstream = new javax.swing.JPanel();
        panelPeptideProphet = new javax.swing.JPanel();
        chkRunPeptideProphet = new javax.swing.JCheckBox();
        btnPepProphDefaultsOpen = new javax.swing.JButton();
        btnPepProphDefaultsClosed = new javax.swing.JButton();
        panelPeptideProphetOptions = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        textPepProphCmd = new javax.swing.JTextArea();
        jLabel34 = new javax.swing.JLabel();
        checkCombinedPepxml = new javax.swing.JCheckBox();
        btnPepProphDefaultsNonspecific = new javax.swing.JButton();
        jLabel11 = new javax.swing.JLabel();
        panelProteinProphet = new javax.swing.JPanel();
        chkRunProteinProphet = new javax.swing.JCheckBox();
        btnProtProphDefaultsClosed = new javax.swing.JButton();
        btnProtProphDefaultsOpen = new javax.swing.JButton();
        panelProteinProphetOptions = new javax.swing.JPanel();
        checkProcessGroupsSeparately = new javax.swing.JCheckBox();
        jScrollPane4 = new javax.swing.JScrollPane();
        txtProteinProphetCmdLineOpts = new javax.swing.JTextArea();
        jLabel40 = new javax.swing.JLabel();
        panelCrystalc = new umich.msfragger.params.crystalc.CrystalcPanel();
        tmtiPanel1 = new TmtiPanel();
        panelReport = new javax.swing.JPanel();
        ptmshepherdPanel = new umich.msfragger.params.ptmshepherd.PtmshepherdJPanel();
        panelQuant = new umich.msfragger.params.ionquant.QuantJPanel();
        panelReportOptions = new umich.msfragger.params.philosopher.ReportPanel();
        speclibPanel1 = new umich.msfragger.params.speclib.SpeclibPanel();
        panelRun = new javax.swing.JPanel();
        btnStop = new javax.swing.JButton();
        btnClearConsole = new javax.swing.JButton();
        consoleScrollPane = new javax.swing.JScrollPane();
        lblOutputDir = new javax.swing.JLabel();
        btnSelectWrkingDir = new javax.swing.JButton();
        txtWorkingDir = new javax.swing.JTextField();
        btnAbout = new javax.swing.JButton();
        checkDryRun = new javax.swing.JCheckBox();
        btnReportErrors = new javax.swing.JButton();
        btnRun = new javax.swing.JButton();
        btnExportLog = new javax.swing.JButton();
        btnOpenInExplorer = new javax.swing.JButton();
        btnPrintCommands = new javax.swing.JButton();
        btnSaveAllToolsConfig = new javax.swing.JButton();
        btnLoadAllToolsConfig = new javax.swing.JButton();

        jLabel2.setText("jLabel2");

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("MSFragger");
        setIconImages(ToolingUtils.loadIcon());
        setMinimumSize(new java.awt.Dimension(640, 480));
        setName("frameMain"); // NOI18N
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowOpened(java.awt.event.WindowEvent evt) {
                formWindowOpened(evt);
            }
        });

        tabPane.setMaximumSize(new java.awt.Dimension(1920, 1080));
        tabPane.setMinimumSize(new java.awt.Dimension(640, 480));
        tabPane.setName(""); // NOI18N

        scrollConfig.setBorder(null);

        panelMsfraggerConfig.setBorder(javax.swing.BorderFactory.createTitledBorder("MSFragger"));

        btnMsfraggerBinDownload.setText("Download");
        btnMsfraggerBinDownload.setToolTipText("<html>Open the download web-page for MSFragger in browser.<br/>\nYou need to agree to the license terms."); // NOI18N
        btnMsfraggerBinDownload.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnMsfraggerBinDownloadActionPerformed(evt);
            }
        });

        btnMsfraggerBinBrowse.setText("Browse");
        btnMsfraggerBinBrowse.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnMsfraggerBinBrowseActionPerformed(evt);
            }
        });

        textBinMsfragger.setText(ToolingUtils.getDefaultBinMsfragger());
        textBinMsfragger.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent evt) {
                textBinMsfraggerFocusLost(evt);
            }
        });
        textBinMsfragger.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                textBinMsfraggerActionPerformed(evt);
            }
        });

        jScrollPane1.setBorder(null);

        editorMsfraggerCitation.setEditable(false);
        editorMsfraggerCitation.setBackground(lblFraggerJavaVer.getBackground());
        editorMsfraggerCitation.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        editorMsfraggerCitation.setContentType("text/html"); // NOI18N
        editorMsfraggerCitation.setFont(lblFraggerJavaVer.getFont());
        editorMsfraggerCitation.setText(createFraggerCitationHtml(lblFraggerJavaVer.getFont()));
        editorMsfraggerCitation.setAutoscrolls(false);
        editorMsfraggerCitation.addHyperlinkListener(new javax.swing.event.HyperlinkListener() {
            public void hyperlinkUpdate(javax.swing.event.HyperlinkEvent evt) {
                urlHandlerViaSystemBrowser(evt);
            }
        });
        jScrollPane1.setViewportView(editorMsfraggerCitation);

        lblFraggerJavaVer.setText(OsUtils.JavaInfo());

        btnMsfraggerUpdate.setText("Update");
        btnMsfraggerUpdate.setToolTipText("<html>Open MSFragger upgrader tool in browser.<br>\nIn order to update you <b>must</b> download an<br>\noriginal copy from the <b>download</b> website once.\n");
        btnMsfraggerUpdate.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnMsfraggerUpdateActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelMsfraggerConfigLayout = new javax.swing.GroupLayout(panelMsfraggerConfig);
        panelMsfraggerConfig.setLayout(panelMsfraggerConfigLayout);
        panelMsfraggerConfigLayout.setHorizontalGroup(
            panelMsfraggerConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelMsfraggerConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelMsfraggerConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(lblFraggerJavaVer, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane1)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelMsfraggerConfigLayout.createSequentialGroup()
                        .addComponent(textBinMsfragger)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnMsfraggerBinBrowse)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnMsfraggerBinDownload)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnMsfraggerUpdate)))
                .addContainerGap())
        );
        panelMsfraggerConfigLayout.setVerticalGroup(
            panelMsfraggerConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelMsfraggerConfigLayout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(panelMsfraggerConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(textBinMsfragger, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnMsfraggerBinDownload)
                    .addComponent(btnMsfraggerBinBrowse)
                    .addComponent(btnMsfraggerUpdate))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(lblFraggerJavaVer)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 68, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        panelPhilosopherConfig.setBorder(javax.swing.BorderFactory.createTitledBorder("Philosopher"));

        btnPhilosopherBinDownload.setText("Download");
        btnPhilosopherBinDownload.setToolTipText("<html>Opens the web-page for Philosopher download.<br/>\nChoose the right version for your platform.");
        btnPhilosopherBinDownload.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPhilosopherBinDownloadActionPerformed(evt);
            }
        });

        btnPhilosopherBinBrowse.setText("Browse");
        btnPhilosopherBinBrowse.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPhilosopherBinBrowseActionPerformed(evt);
            }
        });

        textBinPhilosopher.setText(ToolingUtils.getDefaultBinPhilosopher());
        textBinPhilosopher.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent evt) {
                textBinPhilosopherFocusLost(evt);
            }
        });
        textBinPhilosopher.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                textBinPhilosopherActionPerformed(evt);
            }
        });

        jLabel3.setText("If provided, philosopher binary will be used for Peptide and Protein Prophets and Report");

        lblPhilosopherInfo.setText(OsUtils.OsInfo());

        jScrollPane3.setBorder(null);

        editorPhilosopherLink.setEditable(false);
        editorPhilosopherLink.setBackground(lblFraggerJavaVer.getBackground());
        editorPhilosopherLink.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        editorPhilosopherLink.setContentType("text/html"); // NOI18N
        editorPhilosopherLink.setFont(lblFraggerJavaVer.getFont());
        editorPhilosopherLink.setText(createPhilosopherCitationHtml(lblFraggerJavaVer));
        editorPhilosopherLink.addHyperlinkListener(new javax.swing.event.HyperlinkListener() {
            public void hyperlinkUpdate(javax.swing.event.HyperlinkEvent evt) {
                urlHandlerViaSystemBrowser(evt);
            }
        });
        jScrollPane3.setViewportView(editorPhilosopherLink);

        javax.swing.GroupLayout panelPhilosopherConfigLayout = new javax.swing.GroupLayout(panelPhilosopherConfig);
        panelPhilosopherConfig.setLayout(panelPhilosopherConfigLayout);
        panelPhilosopherConfigLayout.setHorizontalGroup(
            panelPhilosopherConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPhilosopherConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelPhilosopherConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblPhilosopherInfo, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelPhilosopherConfigLayout.createSequentialGroup()
                        .addComponent(textBinPhilosopher)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnPhilosopherBinBrowse)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnPhilosopherBinDownload))
                    .addComponent(jLabel3, javax.swing.GroupLayout.DEFAULT_SIZE, 748, Short.MAX_VALUE)
                    .addComponent(jScrollPane3))
                .addContainerGap())
        );
        panelPhilosopherConfigLayout.setVerticalGroup(
            panelPhilosopherConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPhilosopherConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelPhilosopherConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(textBinPhilosopher, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnPhilosopherBinDownload)
                    .addComponent(btnPhilosopherBinBrowse))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(lblPhilosopherInfo)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 33, Short.MAX_VALUE))
        );

        btnFindTools.setText("Search tools automatically");
        btnFindTools.setToolTipText(lblFindAutomatically.getToolTipText());
        btnFindTools.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnFindToolsActionPerformed(evt);
            }
        });

        lblFindAutomatically.setLabelFor(lblFindAutomatically);
        lblFindAutomatically.setText("Recursively search for tools in a directory (e.g. Downloads)");
        lblFindAutomatically.setToolTipText("<html>If you have the tools downloaded somewhere already, you can<br/>\nuse this button to automatically look for them.<br/>\nIt's probably simpler to just set them manually though.");

        btnClearCache.setText("Clear Cache");
        btnClearCache.setToolTipText("<html>Forget all the stored text-field information.<br/>\nAfter you relaunch the application everything will reset<br/>\nto default values."); // NOI18N
        btnClearCache.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnClearCacheActionPerformed(evt);
            }
        });

        btnAboutInConfig.setText("About");
        btnAboutInConfig.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAboutInConfigActionPerformed(evt);
            }
        });

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("DB Splitting"));
        jPanel2.setToolTipText("<html>Requires <b>Python 3</b> with packages <b>Numpy, Pandas</b>\nWays to get everything set up:<br>\n<ul>\n<li>Install Python 3 if you don't yet have it.</li>\n<li>Install required python modules using <i>pip</i>, the python package manager, with command:</li>\n<ul>\n<li>pip install numpy pandas</li>\n</ul>\n</ul>\n");

        lblDbsliceInfo1.setText(DbSlice.DEFAULT_MESSAGE);

        jScrollPane6.setBorder(null);

        epDbsliceInfo.setEditable(false);
        epDbsliceInfo.setBackground(lblFraggerJavaVer.getBackground());
        epDbsliceInfo.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        epDbsliceInfo.setContentType("text/html"); // NOI18N
        epDbsliceInfo.setText("");
        epDbsliceInfo.addHyperlinkListener(new javax.swing.event.HyperlinkListener() {
            public void hyperlinkUpdate(javax.swing.event.HyperlinkEvent evt) {
                urlHandlerViaSystemBrowser(evt);
            }
        });
        jScrollPane6.setViewportView(epDbsliceInfo);

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane6)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(lblDbsliceInfo1)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addComponent(lblDbsliceInfo1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        checkEnableDiaumpire.setText("Enable DIA-Umpire");
        checkEnableDiaumpire.setToolTipText("<html>\nOnly use this if you have DIA data and need to pre-process it to make compatible to MSFragger.");
        checkEnableDiaumpire.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                checkEnableDiaumpireStateChanged(evt);
            }
        });

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Spectral Lib generation"));
        jPanel1.setToolTipText("<html>Requires <b>Python 3</b> with packages <b>Cython, Msproteomicstools, matplotlib</b>.<br/>\nWays to get everything set up:<br>\n<ul>\n<li>Install Python 3 if you don't yet have it.</li>\n<li>Install required python modules using <i>pip</i>, the python package manager, with commands:</li>\n<ul>\n<li>pip install numpy pandas cython</li>\n<li>pip install msproteomicstools matplotlib</li>\n</ul>\n</ul>\nFor timsTOF ion mobility data, EasyPQP can be used. Requires separate installation, follow instructions at: <a href=\"https://github.com/grosenberger/easypqp\">https://github.com/grosenberger/easypqp</a>");

        lblSpeclibInfo1.setText(SpecLibGen.DEFAULT_MESSAGE);

        scrollEpSpeclibInfo2.setBorder(null);

        epSpeclibInfo2.setEditable(false);
        epSpeclibInfo2.setBackground(lblFraggerJavaVer.getBackground());
        epSpeclibInfo2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        epSpeclibInfo2.setContentType("text/html"); // NOI18N
        epSpeclibInfo2.setFont(lblFraggerJavaVer.getFont());
        epSpeclibInfo2.setText("");
        epSpeclibInfo2.addHyperlinkListener(new javax.swing.event.HyperlinkListener() {
            public void hyperlinkUpdate(javax.swing.event.HyperlinkEvent evt) {
                urlHandlerViaSystemBrowser(evt);
            }
        });
        scrollEpSpeclibInfo2.setViewportView(epSpeclibInfo2);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(lblSpeclibInfo1)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(scrollEpSpeclibInfo2, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(lblSpeclibInfo1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(scrollEpSpeclibInfo2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Python"));

        btnBrowseBinPython.setText("Browse");
        btnBrowseBinPython.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnBrowseBinPythonActionPerformed(evt);
            }
        });

        lblPythonInfo.setText("");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(textBinPython)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnBrowseBinPython))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(lblPythonInfo)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnBrowseBinPython)
                    .addComponent(textBinPython, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(lblPythonInfo)
                .addGap(0, 12, Short.MAX_VALUE))
        );

        panelBottomHints.setLayout(new javax.swing.BoxLayout(panelBottomHints, javax.swing.BoxLayout.Y_AXIS));

        javax.swing.GroupLayout panelConfigLayout = new javax.swing.GroupLayout(panelConfig);
        panelConfig.setLayout(panelConfigLayout);
        panelConfigLayout.setHorizontalGroup(
            panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelConfigLayout.createSequentialGroup()
                        .addComponent(btnAboutInConfig)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelConfigLayout.createSequentialGroup()
                        .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jPanel2, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jPanel3, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(panelPhilosopherConfig, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(panelMsfraggerConfig, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelConfigLayout.createSequentialGroup()
                                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelConfigLayout.createSequentialGroup()
                                        .addComponent(btnFindTools)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(lblFindAutomatically))
                                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelConfigLayout.createSequentialGroup()
                                        .addGap(67, 67, 67)
                                        .addComponent(btnClearCache)
                                        .addGap(18, 18, 18)
                                        .addComponent(checkEnableDiaumpire)))
                                .addGap(0, 0, Short.MAX_VALUE))
                            .addComponent(panelBottomHints, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addContainerGap())))
        );
        panelConfigLayout.setVerticalGroup(
            panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnAboutInConfig)
                    .addComponent(btnClearCache)
                    .addComponent(checkEnableDiaumpire))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnFindTools)
                    .addComponent(lblFindAutomatically))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelMsfraggerConfig, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelPhilosopherConfig, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelBottomHints, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(146, Short.MAX_VALUE))
        );

        scrollConfig.setViewportView(panelConfig);

        tabPane.addTab("Config", scrollConfig);

        panelSelectedFiles.setBorder(javax.swing.BorderFactory.createTitledBorder("Selected files (Drag & Drop files or folders here, it's OK if they contain non LC/MS files)"));

        btnRawAddFiles.setText("Add files");
        btnRawAddFiles.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawAddFilesActionPerformed(evt);
            }
        });

        btnRawClear.setText("Clear Files");
        btnRawClear.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawClearActionPerformed(evt);
            }
        });

        btnRawAddFolder.setText("Add Folder Recursively");
        btnRawAddFolder.setToolTipText("<html>Recursively search a directory, importing<br/>\nall compatible LC/MS files.");
        btnRawAddFolder.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawAddFolderActionPerformed(evt);
            }
        });

        btnRawRemove.setText("Remove Selected");
        btnRawRemove.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawRemoveActionPerformed(evt);
            }
        });

        btnGroupsConsecutive.setText("Consecutive");
        btnGroupsConsecutive.setToolTipText("<html>Assign each run to its own experiment.<br/> <b>Names like \"exp_1\"</b> will be assgined.");
        btnGroupsConsecutive.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnGroupsConsecutiveActionPerformed(evt);
            }
        });

        jLabel10.setText("Assign files to Experiments/Groups:");

        btnGroupsByParentDir.setText("By parent directory");
        btnGroupsByParentDir.setToolTipText("<html>Assign each run to an experiment.<br/>\nLCMS file's <b>parent directory name<b> will be used<br/>\nas the experiment name.");
        btnGroupsByParentDir.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnGroupsByParentDirActionPerformed(evt);
            }
        });

        btnGroupsByFilename.setText("By file name");
        btnGroupsByFilename.setToolTipText("<html>Each file is assigned to an experiment with<br/>\nthe <b>same name as the file itself</b>.");
        btnGroupsByFilename.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnGroupsByFilenameActionPerformed(evt);
            }
        });

        btnGroupsClear.setText("Clear Groups");
        btnGroupsClear.setToolTipText("<html>Each file is assigned to the <b>default</b> experiment.");
        btnGroupsClear.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnGroupsClearActionPerformed(evt);
            }
        });

        btnGroupsAssignToSelected.setText("Set experiment/replicate");
        btnGroupsAssignToSelected.setToolTipText(Tooltips.tipBtnAssignToSelected());
        btnGroupsAssignToSelected.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnGroupsAssignToSelectedActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelSelectedFilesLayout = new javax.swing.GroupLayout(panelSelectedFiles);
        panelSelectedFiles.setLayout(panelSelectedFilesLayout);
        panelSelectedFilesLayout.setHorizontalGroup(
            panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(scrollPaneRawFiles)
                    .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                        .addGroup(panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel10)
                            .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                                .addComponent(btnRawAddFiles)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnRawAddFolder)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnRawRemove)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnRawClear))
                            .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                                .addComponent(btnGroupsConsecutive)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnGroupsByParentDir)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnGroupsByFilename)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnGroupsAssignToSelected)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnGroupsClear)))
                        .addGap(0, 171, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelSelectedFilesLayout.setVerticalGroup(
            panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnRawRemove)
                    .addComponent(btnRawAddFiles)
                    .addComponent(btnRawAddFolder)
                    .addComponent(btnRawClear))
                .addGap(11, 11, 11)
                .addComponent(jLabel10)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnGroupsConsecutive)
                    .addComponent(btnGroupsByParentDir)
                    .addComponent(btnGroupsByFilename)
                    .addComponent(btnGroupsAssignToSelected)
                    .addComponent(btnGroupsClear))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(scrollPaneRawFiles, javax.swing.GroupLayout.DEFAULT_SIZE, 550, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout panelSelectFilesLayout = new javax.swing.GroupLayout(panelSelectFiles);
        panelSelectFiles.setLayout(panelSelectFilesLayout);
        panelSelectFilesLayout.setHorizontalGroup(
            panelSelectFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelSelectFilesLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(panelSelectedFiles, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        panelSelectFilesLayout.setVerticalGroup(
            panelSelectFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelSelectFilesLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(panelSelectedFiles, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        tabPane.addTab("Select LC/MS Files", null, panelSelectFiles, "Input mzML or mzXML files");

        panelDbInfo.setBorder(javax.swing.BorderFactory.createTitledBorder("FASTA sequence database"));

        textSequenceDbPath.setToolTipText("Path to fasta file");
        textSequenceDbPath.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent evt) {
                textSequenceDbPathFocusLost(evt);
            }
        });

        btnBrowse.setText("Browse");
        btnBrowse.setToolTipText("Path to fasta file");
        btnBrowse.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnBrowseActionPerformed(evt);
            }
        });

        jLabel5.setText("Decoy tag");

        textDecoyTagSeqDb.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                textDecoyTagSeqDbFocusGained(evt);
            }
            public void focusLost(java.awt.event.FocusEvent evt) {
                textDecoyTagSeqDbFocusLost(evt);
            }
        });

        btnTryDetectDecoyTag.setText("Try Auto-Detect");
        btnTryDetectDecoyTag.setToolTipText("Try to auto-detect decoy tag used in the database");
        btnTryDetectDecoyTag.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnTryDetectDecoyTagActionPerformed(evt);
            }
        });

        lblFastaCount.setToolTipText("Number of proteins in fasta file");

        jScrollPane5.setViewportView(editorSequenceDb);
        initEditorPaneSeqDb(editorSequenceDb);

        btnDbDownload.setText("Download");
        btnDbDownload.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnDbDownloadActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelDbInfoLayout = new javax.swing.GroupLayout(panelDbInfo);
        panelDbInfo.setLayout(panelDbInfoLayout);
        panelDbInfoLayout.setHorizontalGroup(
            panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelDbInfoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 748, Short.MAX_VALUE)
                    .addGroup(panelDbInfoLayout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(textDecoyTagSeqDb, javax.swing.GroupLayout.PREFERRED_SIZE, 131, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(btnTryDetectDecoyTag)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(lblFastaCount)
                        .addGap(71, 71, 71))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelDbInfoLayout.createSequentialGroup()
                        .addComponent(textSequenceDbPath)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(btnBrowse)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(btnDbDownload)))
                .addContainerGap())
        );
        panelDbInfoLayout.setVerticalGroup(
            panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelDbInfoLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(textSequenceDbPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnBrowse)
                    .addComponent(btnDbDownload))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(textDecoyTagSeqDb, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnTryDetectDecoyTag)
                    .addComponent(lblFastaCount))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 570, Short.MAX_VALUE)
                .addContainerGap())
        );

        loadLastSequenceDb();
        loadLastDecoyTag();

        javax.swing.GroupLayout panelSequenceDbLayout = new javax.swing.GroupLayout(panelSequenceDb);
        panelSequenceDb.setLayout(panelSequenceDbLayout);
        panelSequenceDbLayout.setHorizontalGroup(
            panelSequenceDbLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelSequenceDbLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(panelDbInfo, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        panelSequenceDbLayout.setVerticalGroup(
            panelSequenceDbLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelSequenceDbLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(panelDbInfo, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        tabPane.addTab("Database", panelSequenceDb);

        chkRunPeptideProphet.setSelected(true);
        chkRunPeptideProphet.setText("Run PeptideProphet");
        chkRunPeptideProphet.setName("ui.name.downstream.check.run-pep-proph"); // NOI18N
        chkRunPeptideProphet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkRunPeptideProphetActionPerformed(evt);
            }
        });

        btnPepProphDefaultsOpen.setText("Open Search");
        btnPepProphDefaultsOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPepProphDefaultsOpenActionPerformed(evt);
            }
        });

        btnPepProphDefaultsClosed.setText("Closed Search");
        btnPepProphDefaultsClosed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPepProphDefaultsClosedActionPerformed(evt);
            }
        });

        panelPeptideProphetOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("PeptideProphet Options"));

        textPepProphCmd.setColumns(20);
        textPepProphCmd.setLineWrap(true);
        textPepProphCmd.setRows(5);
        textPepProphCmd.setWrapStyleWord(true);
        textPepProphCmd.setName("ui.name.downstream.text.pep-proph-cmd"); // NOI18N
        textPepProphCmd.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                textPepProphCmdFocusGained(evt);
            }
            public void focusLost(java.awt.event.FocusEvent evt) {
                textPepProphCmdFocusLost(evt);
            }
        });
        jScrollPane2.setViewportView(textPepProphCmd);
        loadLastPeptideProphet();

        jLabel34.setText("Cmd Line Options");

        checkCombinedPepxml.setText("<html>Single <b>combined</b> pep-xml file per experiment/group");
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N
        checkCombinedPepxml.setName(bundle.getString("ui.name.check.combine-pepxml")); // NOI18N
        checkCombinedPepxml.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkCombinedPepxmlActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelPeptideProphetOptionsLayout = new javax.swing.GroupLayout(panelPeptideProphetOptions);
        panelPeptideProphetOptions.setLayout(panelPeptideProphetOptionsLayout);
        panelPeptideProphetOptionsLayout.setHorizontalGroup(
            panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelPeptideProphetOptionsLayout.createSequentialGroup()
                        .addComponent(jLabel34)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 642, Short.MAX_VALUE))
                    .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                        .addComponent(checkCombinedPepxml, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelPeptideProphetOptionsLayout.setVerticalGroup(
            panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                .addGroup(panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jLabel34))
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 57, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(checkCombinedPepxml, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(0, 5, Short.MAX_VALUE))
        );

        btnPepProphDefaultsNonspecific.setText("Non-specific Search");
        btnPepProphDefaultsNonspecific.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPepProphDefaultsNonspecificActionPerformed(evt);
            }
        });

        jLabel11.setText("Load defaults for:");

        javax.swing.GroupLayout panelPeptideProphetLayout = new javax.swing.GroupLayout(panelPeptideProphet);
        panelPeptideProphet.setLayout(panelPeptideProphetLayout);
        panelPeptideProphetLayout.setHorizontalGroup(
            panelPeptideProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetLayout.createSequentialGroup()
                .addComponent(chkRunPeptideProphet)
                .addGap(18, 18, 18)
                .addComponent(jLabel11)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnPepProphDefaultsClosed)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnPepProphDefaultsOpen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnPepProphDefaultsNonspecific)
                .addContainerGap(197, Short.MAX_VALUE))
            .addComponent(panelPeptideProphetOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        panelPeptideProphetLayout.setVerticalGroup(
            panelPeptideProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetLayout.createSequentialGroup()
                .addGroup(panelPeptideProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(chkRunPeptideProphet)
                    .addComponent(btnPepProphDefaultsOpen)
                    .addComponent(btnPepProphDefaultsClosed)
                    .addComponent(btnPepProphDefaultsNonspecific)
                    .addComponent(jLabel11))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelPeptideProphetOptions, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        chkRunProteinProphet.setSelected(true);
        chkRunProteinProphet.setText("Run ProteinProphet");
        chkRunProteinProphet.setName("ui.name.downstream.check.run-prot-proph"); // NOI18N
        chkRunProteinProphet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkRunProteinProphetActionPerformed(evt);
            }
        });

        btnProtProphDefaultsClosed.setText("Allow mass shifted peptides");
        btnProtProphDefaultsClosed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnProtProphDefaultsClosedActionPerformed(evt);
            }
        });

        btnProtProphDefaultsOpen.setText("Do NOT allow mass shifted peptides");
        btnProtProphDefaultsOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnProtProphDefaultsOpenActionPerformed(evt);
            }
        });

        panelProteinProphetOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("ProteinProphet Options"));

        checkProcessGroupsSeparately.setSelected(loadLastProcessGroupsSeparately());
        checkProcessGroupsSeparately.setText("<html>Separate ProteinProphet prot.xml file per group/experiment");
        checkProcessGroupsSeparately.setToolTipText("<html><b>Uncheck</b> if you want a report comparing protein abundances across<br/>\nexperiments or just want a single protein identification result from all<br/>\nthe runs.<br/>\n<b>Only check</b> if you want peptide/protein ID results<br/>\nfor each experiment separately. E.g. this might be useful if you have<br/>\n100 files on hand and use the \"assign to experiments\" feature to quickly<br/>\nrun MSFragger + downstream processing on each of those and get a pepxml<br/>\nand/or protxml files.");
        checkProcessGroupsSeparately.setName("ui.name.downstream.check.separate-protxml"); // NOI18N
        checkProcessGroupsSeparately.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkProcessGroupsSeparatelyActionPerformed(evt);
            }
        });

        txtProteinProphetCmdLineOpts.setColumns(20);
        txtProteinProphetCmdLineOpts.setLineWrap(true);
        txtProteinProphetCmdLineOpts.setRows(5);
        txtProteinProphetCmdLineOpts.setWrapStyleWord(true);
        txtProteinProphetCmdLineOpts.setName("ui.name.downstream.text.prot-proph-cmd"); // NOI18N
        txtProteinProphetCmdLineOpts.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent evt) {
                txtProteinProphetCmdLineOptsFocusLost(evt);
            }
        });
        jScrollPane4.setViewportView(txtProteinProphetCmdLineOpts);
        loadLastProteinProphet();

        jLabel40.setText("Cmd Line Options");

        javax.swing.GroupLayout panelProteinProphetOptionsLayout = new javax.swing.GroupLayout(panelProteinProphetOptions);
        panelProteinProphetOptions.setLayout(panelProteinProphetOptionsLayout);
        panelProteinProphetOptionsLayout.setHorizontalGroup(
            panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addComponent(checkProcessGroupsSeparately, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 416, Short.MAX_VALUE))
                    .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addComponent(jLabel40)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 642, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelProteinProphetOptionsLayout.setVerticalGroup(
            panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jLabel40))
                    .addComponent(jScrollPane4, javax.swing.GroupLayout.PREFERRED_SIZE, 57, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(checkProcessGroupsSeparately, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(156, 156, 156))
        );

        javax.swing.GroupLayout panelProteinProphetLayout = new javax.swing.GroupLayout(panelProteinProphet);
        panelProteinProphet.setLayout(panelProteinProphetLayout);
        panelProteinProphetLayout.setHorizontalGroup(
            panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetLayout.createSequentialGroup()
                .addComponent(chkRunProteinProphet)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnProtProphDefaultsClosed)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnProtProphDefaultsOpen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            .addComponent(panelProteinProphetOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        panelProteinProphetLayout.setVerticalGroup(
            panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetLayout.createSequentialGroup()
                .addGroup(panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(chkRunProteinProphet)
                    .addComponent(btnProtProphDefaultsClosed)
                    .addComponent(btnProtProphDefaultsOpen))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelProteinProphetOptions, javax.swing.GroupLayout.PREFERRED_SIZE, 112, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        javax.swing.GroupLayout panelDownstreamLayout = new javax.swing.GroupLayout(panelDownstream);
        panelDownstream.setLayout(panelDownstreamLayout);
        panelDownstreamLayout.setHorizontalGroup(
            panelDownstreamLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelDownstreamLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelDownstreamLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(panelPeptideProphet, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(panelProteinProphet, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(panelCrystalc, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(tmtiPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        panelDownstreamLayout.setVerticalGroup(
            panelDownstreamLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelDownstreamLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(panelPeptideProphet, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(panelProteinProphet, javax.swing.GroupLayout.PREFERRED_SIZE, 145, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelCrystalc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(tmtiPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 288, Short.MAX_VALUE)
                .addContainerGap())
        );

        scrollDownstream.setViewportView(panelDownstream);

        tabPane.addTab("Downstream", scrollDownstream);

        ptmshepherdPanel.setBorder(javax.swing.BorderFactory.createTitledBorder("PTM Analysis"));

        panelQuant.setBorder(javax.swing.BorderFactory.createTitledBorder("Quantitation"));

        javax.swing.GroupLayout panelReportLayout = new javax.swing.GroupLayout(panelReport);
        panelReport.setLayout(panelReportLayout);
        panelReportLayout.setHorizontalGroup(
            panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelReportLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(ptmshepherdPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(panelQuant, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(panelReportOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(speclibPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        panelReportLayout.setVerticalGroup(
            panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelReportLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(panelReportOptions, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelQuant, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(ptmshepherdPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(speclibPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(72, Short.MAX_VALUE))
        );

        tabPane.addTab("Report", null, panelReport, "");

        btnStop.setText("Stop");
        btnStop.setEnabled(false);
        btnStop.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnStopActionPerformed(evt);
            }
        });

        btnClearConsole.setText("Clear console");
        btnClearConsole.setToolTipText("Clear text in the console below");
        btnClearConsole.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnClearConsoleActionPerformed(evt);
            }
        });

        lblOutputDir.setText("Output dir");
        lblOutputDir.setToolTipText("<html>All the output will be placed into this directory.<br/>\nSome of the tools might produce output side by side with<br/>\noriginal input files, and if you Stop processing prematurely,<br/>\nthose intermediate files might have not been moved/deleted yet.<br/>"); // NOI18N

        btnSelectWrkingDir.setText("Browse");
        btnSelectWrkingDir.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSelectWrkingDirActionPerformed(evt);
            }
        });

        txtWorkingDir.setToolTipText(lblOutputDir.getToolTipText());
        txtWorkingDir.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent evt) {
                txtWorkingDirFocusLost(evt);
            }
        });

        btnAbout.setText("About");
        btnAbout.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAboutActionPerformed(evt);
            }
        });

        checkDryRun.setText("Dry Run");
        checkDryRun.setToolTipText("<html>Only print the commands to execute, <br/>\nbut don't actually execute them.");

        btnReportErrors.setText("Report Erorrs");
        btnReportErrors.setToolTipText("<html>Submit an issue ticket to the bug tracker.<br/>\nPlease attach the following:\n<ol>\n<li>Run log. Use the button \"Export Log\", or copy paste the contents of the log <br/>\nto the ticket text using <b>inside triple tilde block, like this: ```{your-log-text-here}```</b></li>\n<li>fragger.params file. You can find it in the output directory you specified.</li>\n<li>Any other relevant details, like what you were trying to do, which database you used, etc</li>\n</ol>");
        btnReportErrors.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnReportErrorsActionPerformed(evt);
            }
        });

        btnRun.setBackground(new java.awt.Color(203, 234, 210));
        btnRun.setText("<html><b>RUN<b>");
        btnRun.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRunActionPerformed(evt);
            }
        });

        btnExportLog.setText("Export Log");
        btnExportLog.setToolTipText("<html>Save the content of the text console to a file.<br/>\nYou can also just copy-paste text from it.");
        btnExportLog.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnExportLogActionPerformed(evt);
            }
        });

        btnOpenInExplorer.setText("Open in Explorer");
        btnOpenInExplorer.setToolTipText("<html>Open the <b>output dir</b> in system file explorer.");
        btnOpenInExplorer.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnOpenInExplorerActionPerformed(evt);
            }
        });

        btnPrintCommands.setText("Print Commands");
        btnPrintCommands.setToolTipText("<html>This button has exactly the same effect as<br/>\nchecking 'Dry Run' checkbox and clicking 'Run'");
        btnPrintCommands.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPrintCommandsActionPerformed(evt);
            }
        });

        btnSaveAllToolsConfig.setText("Save All Tools Config");
        btnSaveAllToolsConfig.setToolTipText("<html>Save parameters for the whole pipeline, including MSFragger,<br/>\nProtein and Peptide Prophets, Reports and all other to a file<br/>\nwhich can be loaded back using the `Load All Tools Config`<br/>\nbutton on the `Run` tab.<br/>\nDuring each run parameters are also saved to `fragpipe.config` file<br/>\nin the output directory.");
        btnSaveAllToolsConfig.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSaveAllToolsConfigActionPerformed(evt);
            }
        });

        btnLoadAllToolsConfig.setText("Load All Tools Config");
        btnLoadAllToolsConfig.setToolTipText("<html>Load previously saved parameters for the whole pipeline.<br/>\nParameters can be saved using the `Save All Tools Config` button<br/>\non the `Run` tab.<br/>\nDuring each run parameters are also saved to `fragpipe.config` file<br/>\nin the output directory.");
        btnLoadAllToolsConfig.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnLoadAllToolsConfigActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelRunLayout = new javax.swing.GroupLayout(panelRun);
        panelRun.setLayout(panelRunLayout);
        panelRunLayout.setHorizontalGroup(
            panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelRunLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(consoleScrollPane)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelRunLayout.createSequentialGroup()
                        .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelRunLayout.createSequentialGroup()
                                .addComponent(btnRun, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnStop)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(checkDryRun)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 175, Short.MAX_VALUE)
                                .addComponent(btnPrintCommands)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnExportLog)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnReportErrors))
                            .addGroup(panelRunLayout.createSequentialGroup()
                                .addComponent(lblOutputDir)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(txtWorkingDir)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnSelectWrkingDir))
                            .addGroup(panelRunLayout.createSequentialGroup()
                                .addComponent(btnSaveAllToolsConfig)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnLoadAllToolsConfig)
                                .addGap(0, 0, Short.MAX_VALUE)))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(btnClearConsole, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(btnOpenInExplorer, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(btnAbout, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
                .addContainerGap())
        );
        panelRunLayout.setVerticalGroup(
            panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelRunLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnAbout)
                    .addComponent(btnSaveAllToolsConfig)
                    .addComponent(btnLoadAllToolsConfig))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblOutputDir)
                    .addComponent(btnSelectWrkingDir)
                    .addComponent(txtWorkingDir, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnOpenInExplorer))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnStop)
                    .addComponent(btnClearConsole)
                    .addComponent(btnReportErrors)
                    .addComponent(btnRun, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnExportLog)
                    .addComponent(checkDryRun)
                    .addComponent(btnPrintCommands))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(consoleScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 586, Short.MAX_VALUE)
                .addContainerGap())
        );

        tabPane.addTab("<html><b>Run</b>", new javax.swing.ImageIcon(getClass().getResource("/umich/msfragger/gui/icons/video-play-16.png")), panelRun); // NOI18N

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addGap(0, 0, 0)
                .addComponent(tabPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(tabPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );

        tabPane.getAccessibleContext().setAccessibleName("FragPipe");
        tabPane.getAccessibleContext().setAccessibleDescription("Run MSFragger pipeline");
        addChangeListenerTextSequenceDb();

        pack();
    }// </editor-fold>//GEN-END:initComponents

  private void btnAboutActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAboutActionPerformed
    EventBus.getDefault().post(new MessageShowAboutDialog());
  }//GEN-LAST:event_btnAboutActionPerformed

  private void btnSelectWrkingDirActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSelectWrkingDirActionPerformed
    MsfraggerGuiFrameUtils.actionSelectWorkingDir(this);
  }//GEN-LAST:event_btnSelectWrkingDirActionPerformed

  void clearConsole() {
    console.setText("");
  }
  
  private void btnClearConsoleActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearConsoleActionPerformed
    clearConsole();
  }//GEN-LAST:event_btnClearConsoleActionPerformed

  private void btnStopActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnStopActionPerformed
    btnRun.setEnabled(true);
    btnStop.setEnabled(false);
    EventBus.getDefault().post(new MessageKillAll(REASON.USER_ACTION));
  }//GEN-LAST:event_btnStopActionPerformed

  @Subscribe
  public void onMessageKillAll(MessageKillAll m) {
    // try saving log
    MessageLastRunWorkDir wdMsg = EventBus.getDefault().getStickyEvent(MessageLastRunWorkDir.class);
    Path workDir = null;
    if (wdMsg != null) {
      workDir = wdMsg.workDir;
    } else {
      try {
        Path p = Paths.get(txtWorkingDir.getText());
        if (Files.exists(p)) {
          workDir = p;
        }
      } catch (Exception ignored) {}
    }
    if (workDir != null) {
      EventBus.getDefault().post(new MessageSaveLog(workDir));
    }
    resetRunButtons(true);
  }
  private void btnRawClearActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawClearActionPerformed
    tableModelRawFiles.dataClear();
  }//GEN-LAST:event_btnRawClearActionPerformed

  List<Path> getExtBinSearchPaths() {
    List<Path> searchPaths = new ArrayList<>();
    Path binMsfragger = getBinMsfragger();
    if (binMsfragger != null) {
      searchPaths.add(binMsfragger);
    }
    return searchPaths;
  }

  private void btnRawAddFilesActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawAddFilesActionPerformed
    // button add raw lcms files
    if (btnRawAddFiles == evt.getSource()) {
      List<Path> searchPaths = getExtBinSearchPaths();
      final javax.swing.filechooser.FileFilter ff = CmdMsfragger.getFileChooserFilter(searchPaths);
      Predicate<File> supportedFilePredicate = CmdMsfragger.getSupportedFilePredicate(searchPaths);
      String approveText = "Select";
      JFileChooser fc = new JFileChooser();
      fc.setAcceptAllFileFilterUsed(true);
      fc.setFileFilter(ff);
      fc.setApproveButtonText(approveText);
      fc.setDialogTitle("Choose raw data files");
      fc.setMultiSelectionEnabled(true);
      fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

      ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN, fc);

      int retVal = fc.showDialog(this, approveText);
      if (retVal != JFileChooser.APPROVE_OPTION)
        return;
      final List<Path> paths = Arrays.stream(fc.getSelectedFiles())
          .filter(supportedFilePredicate)
          .map(File::toPath)
          .collect(Collectors.toList());
      if (paths.isEmpty()) {
        JOptionPane.showMessageDialog(this,
            "None of selected files/folders are supported", "Warning", JOptionPane.WARNING_MESSAGE);
        return;
      } else {
        EventBus.getDefault().post(new MessageLcmsFilesAdded(paths));
      }
    }
  }//GEN-LAST:event_btnRawAddFilesActionPerformed

  private void btnRawRemoveActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawRemoveActionPerformed

    final List<InputLcmsFile> toRemove = new ArrayList<>();
    Arrays.stream(this.tableRawFiles.getSelectedRows())
        .map(tableRawFiles::convertRowIndexToModel)
        .boxed()
        .forEach(i -> toRemove.add(tableModelRawFiles.dataGet(i)));

    // TODO: convert to selected model

    tableRawFiles.getSelectionModel().clearSelection();
    tableModelRawFiles.dataRemoveAll(toRemove);
  }//GEN-LAST:event_btnRawRemoveActionPerformed

  private void btnRawAddFolderActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawAddFolderActionPerformed
    final javax.swing.filechooser.FileFilter ff = CmdMsfragger.getFileChooserFilter(getExtBinSearchPaths());

//    final FileFilter ff = new FileFilter() {
//      final Predicate<String> rawLcmsFnPredicate = CmdMsfragger
//          .getRawLcmsFnPredicate(Arrays.asList(getBinMsfragger()));
//      @Override
//      public boolean accept(File pathname) {
//        return rawLcmsFnPredicate.test(pathname.getName().toLowerCase());
//      }
//    };

    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Select");
    fc.setApproveButtonToolTipText("Select folder to import");
    fc.setDialogTitle("Select a folder with LC/MS files (searched recursively)");


    fc.setAcceptAllFileFilterUsed(true);
    fc.setFileFilter(ff);
    fc.setMultiSelectionEnabled(true);
    fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

    String lastPath = ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN);
    if (!StringUtils.isNullOrWhitespace(lastPath)) {
      try {
        Path p = Paths.get(lastPath);
        fc.setSelectedFile(p.toFile());
//        Path dir = Files.isDirectory(p) ? p.getParent() : p;
//        SwingUtils.setFileChooserPath(fc, dir);
      } catch (Exception ignore) {}
    }

    int confirmation = fc.showOpenDialog(this);

    if (confirmation != JFileChooser.APPROVE_OPTION)
      return;

    final Predicate<File> pred = CmdMsfragger
        .getSupportedFilePredicate(Arrays.asList(getBinMsfragger()));
    File[] selectedFiles = fc.getSelectedFiles();
    List<Path> paths = new ArrayList<>();
    for (File f : selectedFiles) {
      ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, f);
      PathUtils.traverseDirectoriesAcceptingFiles(f, pred, paths, false);
    }

    if (selectedFiles != null && selectedFiles.length > 0  && !paths.isEmpty()) {
      EventBus.getDefault().post(new MessageLcmsFilesAdded(paths, selectedFiles[0].toPath()));
    }
  }//GEN-LAST:event_btnRawAddFolderActionPerformed

  private void btnReportErrorsActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReportErrorsActionPerformed
    final String issueTrackerAddress = ThisAppProps.getLocalProperties().getProperty(Version.PROP_ISSUE_TRACKER_URL);
    try {
      Desktop.getDesktop().browse(URI.create(issueTrackerAddress));
    } catch (IOException ex) {
      log.warn("Exception while trying to open default browser: {}", ex.getMessage());
    }
  }//GEN-LAST:event_btnReportErrorsActionPerformed

  private void btnMsfraggerBinBrowseActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerBinBrowseActionPerformed
    JFileChooser fileChooser = new JFileChooser();
    fileChooser.setApproveButtonText("Select");
    fileChooser.setDialogTitle("Select MSFragger jar");
    fileChooser.setMultiSelectionEnabled(false);
    FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("JAR files",
        "jar");
    fileChooser.setFileFilter(fileNameExtensionFilter);

    fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

    Path curExistingPath = PathUtils.existing(textBinMsfragger.getText().trim());
    if (curExistingPath != null) {
      FileChooserUtils.setPath(fileChooser, curExistingPath);
    } else {
      List<String> props = Arrays
          .asList(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, ThisAppProps.PROP_BINARIES_IN);
      String fcPath = ThisAppProps.tryFindPath(props, true);
      FileChooserUtils.setPath(fileChooser, fcPath);
    }

    int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File foundFile = fileChooser.getSelectedFile();
        if (MsfraggerGuiFrameUtils.validateAndSaveMsfraggerPath(this, foundFile.getAbsolutePath())) {
          ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, foundFile.getAbsolutePath());
        }
        break;
    }

  }//GEN-LAST:event_btnMsfraggerBinBrowseActionPerformed

  private void btnMsfraggerBinDownloadActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerBinDownloadActionPerformed
    try {
      final String downloadUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_DOWNLOAD_URL, "");
      Desktop.getDesktop().browse(URI.create(downloadUrl));
    } catch (IOException ex) {
      throw new IllegalStateException("Could not open MSFragger download link in browser.", ex);
    }
  }//GEN-LAST:event_btnMsfraggerBinDownloadActionPerformed

  private static void urlHandlerViaSystemBrowser(
      javax.swing.event.HyperlinkEvent evt) {//GEN-FIRST:event_urlHandlerViaSystemBrowser
    MsfraggerGuiFrameUtils.urlEventHandle(evt);
  }//GEN-LAST:event_urlHandlerViaSystemBrowser

  private void btnPhilosopherBinDownloadActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPhilosopherBinDownloadActionPerformed
    MsfraggerGuiFrameUtils.downloadPhilosopher();
  }//GEN-LAST:event_btnPhilosopherBinDownloadActionPerformed

  private void btnFindToolsActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnFindToolsActionPerformed

    MsfraggerGuiFrameUtils.findToolsAction(this);
  }//GEN-LAST:event_btnFindToolsActionPerformed

  private void btnPhilosopherBinBrowseActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPhilosopherBinBrowseActionPerformed
    MsfraggerGuiFrameUtils.userBrowsePhilosopherBin(this);

  }//GEN-LAST:event_btnPhilosopherBinBrowseActionPerformed

  private void textBinMsfraggerFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textBinMsfraggerFocusLost
    MsfraggerGuiFrameUtils.validateAndSaveMsfraggerPath(this, textBinMsfragger.getText());
  }//GEN-LAST:event_textBinMsfraggerFocusLost

  private void textBinPhilosopherFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textBinPhilosopherFocusLost
    MsfraggerGuiFrameUtils.validateAndSavePhilosopherPath(this, textBinPhilosopher.getText());
  }//GEN-LAST:event_textBinPhilosopherFocusLost

  private void textBinPhilosopherActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textBinPhilosopherActionPerformed

  }//GEN-LAST:event_textBinPhilosopherActionPerformed

  private void btnClearCacheActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearCacheActionPerformed
    ThisAppProps.clearCache();
    new MsfraggerParams().clearCache();
    new CrystalcParams().clearCache();
    Path p = MessageSaveUiState.newForCache().path;
    try {
      Files.deleteIfExists(p);
    } catch (IOException e) {
      log.error("Could not delete fragpipe form cache file: {}", p.toString());
    }
  }//GEN-LAST:event_btnClearCacheActionPerformed

  public String getFastaPath() {
    return textSequenceDbPath.getText().trim();
  }


  public void formWrite(OutputStream os) throws IOException {
    Map<String, String> map = MsfraggerGuiFrameUtils.tabPaneToMap(tabPane);
    Properties props = PropertiesUtils.from(map);
    try (BufferedOutputStream bos = new BufferedOutputStream(os)) {
      props.store(bos, ThisAppProps.cacheComments());
    }
  }

  public void formRead(InputStream is) throws IOException {
    Properties props = new Properties();
    try (BufferedInputStream bis = new BufferedInputStream(is)) {
      props.load(bis);
    }
    Map<String, String> map = PropertiesUtils.to(props);
    MsfraggerGuiFrameUtils.tabPaneFromMap(tabPane, map);
  }

  @Subscribe
  public void onMessageSaveFormCaches(MessageSaveUiState m) {
    log.debug("Writing form caches to: {}", m.path.toString());
    try (OutputStream os = Files.newOutputStream(m.path)) {
      formWrite(os);
    } catch (IOException e) {
      log.error("Could not write fragpipe form cache to: {}", m.path.toString());
    }
  }

  @Subscribe
  public void onMessageLoadFormCaches(MessageLoadAllForms m) {
    log.debug("Loading form caches from: {}", m.path.toString());
    try (InputStream is = Files.newInputStream(m.path)) {
      formRead(is);
    } catch (IOException e) {
      log.error("Could not read fragpipe form cache from: {}", m.path.toString());
    }
  }

  @Subscribe
  public void onMessageRun(MessageRun m) {
    FragpipeOnMessages.onMessageRun(this, m);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onMessageExternalProcessOutput(MessageExternalProcessOutput m) {
    if (m.output == null) {
      log.warn("MessageExternalProcessOutput with null text, this is a bug, report to devs");
      return;
    }

    // special case, colorize output from MSFragger
    if (CmdMsfragger.NAME.equals(m.procName)) {
      if (m.isError) {
        LogUtils.print(COLOR_RED_DARKEST, console, true, m.output, false);
      } else {
        LogUtils.printWithAnsiColorCodes(console, true, m.output, false);
      }
      return;
    }

    LogUtils.printWithAnsiColorCodes(console, true, m.output, false);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onMessageAppendToConsole(MessageAppendToConsole m) {
    Color c = m.color == null ? COLOR_BLACK : m.color;
    LogUtils.print(c, console, true, m.text, true);
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onMessageLastRunWorkDir(MessageLastRunWorkDir m) {
    ThisAppProps.save(ThisAppProps.PROP_FILE_OUT, m.workDir.toAbsolutePath().toString());
  }

  @Subscribe(threadMode =  ThreadMode.MAIN_ORDERED)
  public void onMessageLcmsFilesList(MessageLcmsFilesList m) {
    if (m.type == MessageType.REQUEST) {
      List<InputLcmsFile> files = tableModelRawFiles != null ? tableModelRawFiles.dataCopy() : Collections.emptyList();
      EventBus.getDefault().post(new MessageLcmsFilesList(MessageType.RESPONSE, files));
    }
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onMessageSaveLog(MessageSaveLog m) {
    final Path dir = m.workDir;
    final int numAttempts = 20;
    int attempt = 0;
    while (++attempt <= numAttempts) {
      LocalDateTime time = LocalDateTime.now();
      String timestamp = time.format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss"));
      Path path = dir.resolve(ThisAppProps.LOG_FILE_NAME + "_" + timestamp + ThisAppProps.LOG_FILE_EXT);
      if (!Files.exists(path)) {
        MsfraggerGuiFrameUtils.saveLogToFile(console, path);
        return;
      }
      try {
        Thread.sleep(200L);
      } catch (InterruptedException ignore) {}
    }
    log.error("Did not save log file, number of attempts exceeded");
  }

  private void btnRunActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRunActionPerformed
    final boolean isDryRun = checkDryRun.isSelected();
    EventBus.getDefault().post(new MessageRun(isDryRun));
  }//GEN-LAST:event_btnRunActionPerformed

  private void btnPepProphDefaults(SearchTypeProp t) {
    int confirm1 = JOptionPane.showConfirmDialog(this,
        "<html>Load " + t + " search defaults?");
    if (JOptionPane.YES_OPTION != confirm1) {
      return;
    }

    loadDefaultsPeptideProphet(t);
  }

  private void btnAboutInConfigActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAboutInConfigActionPerformed
    EventBus.getDefault().post(new MessageShowAboutDialog());
  }//GEN-LAST:event_btnAboutInConfigActionPerformed

  private void btnExportLogActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnExportLogActionPerformed
    Action action = panelRun.getActionMap().get(ACTION_EXPORT_LOG);
    if (action != null) {
      action.actionPerformed(null);
    }
  }//GEN-LAST:event_btnExportLogActionPerformed

  private void textSequenceDbPathFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textSequenceDbPathFocusLost
    MsfraggerGuiFrameUtils.validateAndSaveFastaPath(this, textSequenceDbPath.getText());
  }//GEN-LAST:event_textSequenceDbPathFocusLost

  private void btnBrowseActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnBrowseActionPerformed
    JFileChooser fileChooser = new JFileChooser();
    fileChooser.setApproveButtonText("Select");
    fileChooser.setDialogTitle("Select FASTA file");
    fileChooser.setMultiSelectionEnabled(false);
    FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("FASTA DB",
        "fasta", "fa", "fas", "fast");
    fileChooser.setFileFilter(fileNameExtensionFilter);

    fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

    final String propName = ThisAppProps.PROP_DB_FILE_IN;
    String oldPath = textSequenceDbPath.getText().trim();
    String fcPath;

    if (!StringUtils.isNullOrWhitespace(oldPath)) {
      fcPath = oldPath;
    } else {
      fcPath = ThisAppProps.tryFindPath(Arrays.asList(propName), true);
    }

    FileChooserUtils.setPath(fileChooser, fcPath);

    int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File foundFile = fileChooser.getSelectedFile();
        if (MsfraggerGuiFrameUtils.validateAndSaveFastaPath(this, foundFile.getAbsolutePath())) {
          ThisAppProps.save(propName, foundFile.getAbsolutePath());
        }
        break;
    }
  }//GEN-LAST:event_btnBrowseActionPerformed

  private void textDecoyTagSeqDbFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textDecoyTagSeqDbFocusLost
    MsfraggerGuiFrameUtils
        .validateAndSaveDecoyTagSeqDb(textDecoyTagFocusGained, textDecoyTagSeqDb, tipMap, null, true);
  }//GEN-LAST:event_textDecoyTagSeqDbFocusLost

  private void textDecoyTagSeqDbFocusGained(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textDecoyTagSeqDbFocusGained
    textDecoyTagFocusGained = textDecoyTagSeqDb.getText().trim();
  }//GEN-LAST:event_textDecoyTagSeqDbFocusGained

  private void btnTryDetectDecoyTagActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnTryDetectDecoyTagActionPerformed
    Path p;
    try {
      p = Paths.get(textSequenceDbPath.getText());
      if (!Files.exists(p)) {
        throw new FileNotFoundException("File doesn't exist: " + p.toAbsolutePath().toString());
      }

    } catch (Exception e) {
      JOptionPane.showMessageDialog(btnTryDetectDecoyTag,
          "<html>Could not open sequence database file", "File not found",
          JOptionPane.ERROR_MESSAGE);
      return;
    }

    FastaDecoyPrefixSearchResult fastaDecoyPrefixSearchResult = new FastaDecoyPrefixSearchResult(p, this)
        .invoke();
    if (fastaDecoyPrefixSearchResult.isError()) {
      return;
    }
    String selectedPrefix = fastaDecoyPrefixSearchResult.getSelectedPrefix();
    if (selectedPrefix != null) {
      updateDecoyTagSeqDb(selectedPrefix, false);
    }
  }//GEN-LAST:event_btnTryDetectDecoyTagActionPerformed

  private void formWindowOpened(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowOpened

  }//GEN-LAST:event_formWindowOpened

  private void btnMsfraggerUpdateActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerUpdateActionPerformed
    try {
      String url = MsfraggerProps.getProperties()
          .getProperty(MsfraggerProps.PROP_UPDATESERVER_WEBSITE_URL);
      Desktop.getDesktop().browse(URI.create(url));
    } catch (IOException ex) {
      throw new IllegalStateException("Could not open MSFragger update link in browser.", ex);
    }
  }//GEN-LAST:event_btnMsfraggerUpdateActionPerformed

  private void textBinMsfraggerActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textBinMsfraggerActionPerformed
  }//GEN-LAST:event_textBinMsfraggerActionPerformed

  private void btnOpenInExplorerActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnOpenInExplorerActionPerformed
    String text = txtWorkingDir.getText().trim();
    if (StringUtils.isNullOrWhitespace(text)) {
      JOptionPane.showMessageDialog(this,
          "Empty path", "Not exists",
          JOptionPane.INFORMATION_MESSAGE);
      return;
    }
    Path wd = null;
    try {
      wd = Paths.get(text);
    } catch (InvalidPathException e) {
      JOptionPane.showMessageDialog(this,
          "Path:\n'" + text + "'\nDoes not look to be valid", "Invalid path",
          JOptionPane.WARNING_MESSAGE);
      return;
    }

    if (!Files.exists(wd)) {
      JOptionPane.showMessageDialog(this,
          "Path:\n'" + text + "'\nDoes not exist", "Not exists",
          JOptionPane.INFORMATION_MESSAGE);
      return;
    }
    try {
      Desktop.getDesktop().open(wd.toFile());
    } catch (IOException ex) {
      JOptionPane.showMessageDialog(this,
          "Could not open path in system file browser.", "Error",
          JOptionPane.ERROR_MESSAGE);
      return;
    }
  }//GEN-LAST:event_btnOpenInExplorerActionPerformed

  private void checkEnableDiaumpireStateChanged(
      javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_checkEnableDiaumpireStateChanged
    final String umpireTabName = "DIA-Umpire";

    synchronized (this) {
      final boolean enabled = checkEnableDiaumpire.isSelected();
      if (enabled) {
        //int prevTabIndex = tabPane.indexOfTab("Config");
        final String prevTabName = "Select LC/MS Files";
        int prevTabIndex = tabPane.indexOfTab(prevTabName);
        if (prevTabIndex < 0) {
          throw new IllegalStateException("Could not find tab named " + prevTabName);
        }
        ImageIcon icon = new ImageIcon(
            getClass().getResource("/umich/msfragger/gui/icons/dia-umpire-16x16.png"));
        if (umpirePanel == null || umpireScroll == null) {
          umpirePanel = new UmpirePanel();
          umpireScroll = new JScrollPane();
          umpireScroll.setViewportBorder(BorderFactory.createEmptyBorder());
          umpireScroll.setBorder(BorderFactory.createEmptyBorder());
          umpireScroll.setViewportView(umpirePanel);
        }
        tabPane.insertTab(umpireTabName, icon, umpireScroll, "", prevTabIndex + 1);

      } else {
        int index = tabPane.indexOfTab(umpireTabName);
//                if (index < 0)
//                    throw new IllegalStateException(
//                        "Could not find tab named '" + umpireTabName + "'");
        if (index >= 0) {
          tabPane.removeTabAt(index);
        }
      }
      EventBus.getDefault().post(new MessageIsUmpireRun(isRunUmpireSe()));
    }
  }//GEN-LAST:event_checkEnableDiaumpireStateChanged

  private void btnBrowseBinPythonActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnBrowseBinPythonActionPerformed
    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Select");
    fc.setDialogTitle("Select Python binary");
    fc.setMultiSelectionEnabled(false);
    if (OsUtils.isWindows()) {
      FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Executables",
          "exe");
      fc.setFileFilter(fileNameExtensionFilter);
    }

    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

    Path current = MsfraggerGuiFrameUtils.tryFindStartingPath(textBinPython.getText());
    if (current != null) {
      FileChooserUtils.setPath(fc, current);
    } else {
      List<String> props = Arrays.asList(ThisAppProps.PROP_BIN_PATH_PYTHON, ThisAppProps.PROP_BINARIES_IN);
      String fcPath = ThisAppProps.tryFindPath(props, false);
      FileChooserUtils.setPath(fc, fcPath);
    }

    if (JFileChooser.APPROVE_OPTION == fc
        .showOpenDialog(SwingUtils.findParentFrameForDialog(this))) {
      String path = fc.getSelectedFile().getAbsolutePath();
      EventBus.getDefault().post(new MessagePythonBinSelectedByUser(path));
    }
  }//GEN-LAST:event_btnBrowseBinPythonActionPerformed

  private void btnGroupsConsecutiveActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsConsecutiveActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    final int groupNumMaxLen = (int) Math.ceil(Math.log(m.dataSize()));
    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), "exp", i + 1));
    }
  }//GEN-LAST:event_btnGroupsConsecutiveActionPerformed

  private void btnGroupsByParentDirActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsByParentDirActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      int count = f.getPath().getNameCount();
      String group = count - 2 >= 0
          ? f.getPath().getName(count - 2).toString()
          : f.getPath().getName(count - 1).toString();
      m.dataSet(i, new InputLcmsFile(f.getPath(), group));
    }
  }//GEN-LAST:event_btnGroupsByParentDirActionPerformed

  private void btnGroupsByFilenameActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsByFilenameActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      String group = StringUtils.upToLastDot(f.getPath().getFileName().toString());
      m.dataSet(i, new InputLcmsFile(f.getPath(), group));
    }
  }//GEN-LAST:event_btnGroupsByFilenameActionPerformed

  private void btnGroupsClearActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsClearActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.getPath(), ThisAppProps.DEFAULT_LCMS_EXP_NAME));
    }
  }//GEN-LAST:event_btnGroupsClearActionPerformed

  private void btnGroupsAssignToSelectedActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsAssignToSelectedActionPerformed

    final UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    final ArrayList<InputLcmsFile> data = m.dataCopy();
    List<Integer> selectedRows = Arrays.stream(this.tableRawFiles.getSelectedRows())
        .mapToObj(tableRawFiles::convertRowIndexToModel).collect(Collectors.toList());

    final List<String> paths = selectedRows.stream()
        .map(i -> data.get(i).getPath().toString())
        .collect(Collectors.toList());

    final Set<String> exps = selectedRows.stream()
        .flatMap(i -> data.get(i).getExperiment() == null ? Stream.empty() : Stream.of(data.get(i).getExperiment()))
        .collect(Collectors.toSet());
    final Set<Integer> reps = selectedRows.stream()
        .flatMap(i -> data.get(i).getReplicate() == null ? Stream.empty() : Stream.of(data.get(i).getReplicate()))
        .collect(Collectors.toSet());
    final String defaultExp = exps.size() == 1 ? exps.iterator().next() : null;
    final Integer defaultRep = reps.size() == 1 ? reps.iterator().next() : null;

    ExperimentNameDialog d = new ExperimentNameDialog(this, true, paths, defaultExp, defaultRep);
    d.setVisible(true);
    if (d.isOk()) {
      for (int selectedRow : selectedRows) {
        int i = tableRawFiles.convertRowIndexToModel(selectedRow);
        InputLcmsFile f = m.dataGet(i);
        m.dataSet(i, new InputLcmsFile(f.getPath(), d.getExperimentName(), d.getReplicateNumber()));
      }
    }
  }//GEN-LAST:event_btnGroupsAssignToSelectedActionPerformed

  private void btnPrintCommandsActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPrintCommandsActionPerformed
    EventBus.getDefault().post(new MessageRun(true));
  }//GEN-LAST:event_btnPrintCommandsActionPerformed

  private void txtProteinProphetCmdLineOptsFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_txtProteinProphetCmdLineOptsFocusLost
    String val = txtProteinProphetCmdLineOpts.getText();
    ThisAppProps.save(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, val);
  }//GEN-LAST:event_txtProteinProphetCmdLineOptsFocusLost

  private void checkProcessGroupsSeparatelyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkProcessGroupsSeparatelyActionPerformed
    ThisAppProps
    .save(checkProcessGroupsSeparately, ThisAppProps.PROP_CHECKBOX_PROCESS_GROUPS_SEPARATELY);
  }//GEN-LAST:event_checkProcessGroupsSeparatelyActionPerformed

  private void btnProtProphDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnProtProphDefaultsOpenActionPerformed
    //btnProtProphDefaults(SearchTypeProp.open);
    
    // do NOT allow mass shifted peptides
    ThisAppProps
        .loadFromBundle(txtProteinProphetCmdLineOpts, ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET,
            "tight");
  }//GEN-LAST:event_btnProtProphDefaultsOpenActionPerformed

  private void btnProtProphDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnProtProphDefaultsClosedActionPerformed
    //btnProtProphDefaults(SearchTypeProp.closed);
    
    // allow mass shifted peptides
    ThisAppProps
        .loadFromBundle(txtProteinProphetCmdLineOpts, ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET,
            "open");
  }//GEN-LAST:event_btnProtProphDefaultsClosedActionPerformed

  private void chkRunProteinProphetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunProteinProphetActionPerformed
    final boolean selected = chkRunProteinProphet.isSelected();
    SwingUtils.enableComponents(panelProteinProphet, selected, true,
        Collections.singletonList(chkRunProteinProphet));
  }//GEN-LAST:event_chkRunProteinProphetActionPerformed

  private void textPepProphCmdFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textPepProphCmdFocusLost
    MsfraggerGuiFrameUtils
        .validateAndSavePeptideProphetCmdLineOptions(textPepProphCmd, textPepProphetFocusGained, tipMap);
  }//GEN-LAST:event_textPepProphCmdFocusLost

  private void textPepProphCmdFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textPepProphCmdFocusGained
    textPepProphetFocusGained = textPepProphCmd.getText().trim();
  }//GEN-LAST:event_textPepProphCmdFocusGained

  private void btnPepProphDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPepProphDefaultsClosedActionPerformed
    btnPepProphDefaults(SearchTypeProp.closed);
  }//GEN-LAST:event_btnPepProphDefaultsClosedActionPerformed

  private void btnPepProphDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPepProphDefaultsOpenActionPerformed
    btnPepProphDefaults(SearchTypeProp.open);
  }//GEN-LAST:event_btnPepProphDefaultsOpenActionPerformed

  private void chkRunPeptideProphetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunPeptideProphetActionPerformed
    final boolean selected = chkRunPeptideProphet.isSelected();
    SwingUtils.enableComponents(panelPeptideProphet, selected, true,
        Collections.singletonList(chkRunPeptideProphet));
  }//GEN-LAST:event_chkRunPeptideProphetActionPerformed

  private void checkCombinedPepxmlActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkCombinedPepxmlActionPerformed
    log.debug("Saving checkbox checkCombinedPepxml, value={}", Boolean.toString(checkCombinedPepxml.isSelected()));
    ThisAppProps.save(checkCombinedPepxml, ThisAppProps.PROP_CHECKBOX_COMBINE_PEPXML);
  }//GEN-LAST:event_checkCombinedPepxmlActionPerformed

  private void btnPepProphDefaultsNonspecificActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPepProphDefaultsNonspecificActionPerformed
    btnPepProphDefaults(SearchTypeProp.nonspecific);
  }//GEN-LAST:event_btnPepProphDefaultsNonspecificActionPerformed

  private void txtWorkingDirFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_txtWorkingDirFocusLost
    MsfraggerGuiFrameUtils.saveWorkdirText(txtWorkingDir);
  }//GEN-LAST:event_txtWorkingDirFocusLost

  private void btnDbDownloadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnDbDownloadActionPerformed
    String bin = textBinPhilosopher.getText();
    try {
      FragpipeUtil.downloadDb(this, bin);
    } catch (Exception e) {
      JOptionPane.showMessageDialog(this, "Error when trying to download.\n " + e.getMessage(),
          "Error", JOptionPane.ERROR_MESSAGE);
    }
  }//GEN-LAST:event_btnDbDownloadActionPerformed

  private void btnSaveAllToolsConfigActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSaveAllToolsConfigActionPerformed
    MsfraggerGuiFrameUtils.userSaveForms(this);
  }//GEN-LAST:event_btnSaveAllToolsConfigActionPerformed

  private void btnLoadAllToolsConfigActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnLoadAllToolsConfigActionPerformed
    MsfraggerGuiFrameUtils.userLoadForms(this);
  }//GEN-LAST:event_btnLoadAllToolsConfigActionPerformed

  @Subscribe
  public void databaseUpdate(MessageDbUpdate m) {
    MsfraggerGuiFrameUtils.validateAndSaveFastaPath(this, m.dbPath);
    textSequenceDbPath.setText(m.dbPath);
  }

  //region Load-Last methods
  public void loadLastPeptideProphet() {
    boolean allLoaded = true;
    allLoaded = allLoaded & ThisAppProps.load(textPepProphCmd, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET);
    allLoaded = allLoaded & ThisAppProps.load(checkCombinedPepxml, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET);

    if (!allLoaded) {
      loadDefaultsPeptideProphet(DEFAULT_TYPE);
    }

    removeOldSavedDecoyTagValue(textPepProphCmd, "--decoy");
  }

  private static void removeOldSavedDecoyTagValue(JTextComponent jtc, String tagName) {
    final String text = jtc.getText().trim();
    //Pattern compile = Pattern.compile("--decoy(?:\\s+?[^-]\\S+)?");
    String replaced = text.replaceAll(tagName + "(?!\\S)(?:\\s+?[^-]\\S+)?", "");
    jtc.setText(replaced.trim());
  }

  public void loadDefaultsPeptideProphet(SearchTypeProp type) {
    ThisAppProps.loadFromBundle(textPepProphCmd, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET, type);
    ThisAppProps.loadFromBundle(checkCombinedPepxml, ThisAppProps.PROP_CHECKBOX_COMBINE_PEPXML, type);
  }

  public void loadLastProteinProphet() {
    if (!ThisAppProps
        .load(txtProteinProphetCmdLineOpts, ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET)) {
      loadDefaultsProteinProphet(DEFAULT_TYPE);
    }
  }

  public void loadDefaultsProteinProphet(SearchTypeProp type) {
    ThisAppProps
        .loadFromBundle(txtProteinProphetCmdLineOpts, ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET,
            type);
  }

  private void loadLastDecoyTag() {
    String val = ThisAppProps.load(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);
    if (val != null) {
      textDecoyTagSeqDb.setText(val);
    } else {
      loadDefaultDecoyTag();
    }
  }

  private boolean loadLastCheckboxUseProtxmlInFilter() {
    final String checked = ThisAppProps.load(ThisAppProps.PROP_CHECKBOX_REPORT_FILTER_NO_PROTXML);
    try {
      return Boolean.valueOf(checked);
    } catch (Exception ignored) {
    }
    return false;
  }
  
  private boolean loadLastProcessGroupsSeparately() {
    final String checked = ThisAppProps.load(ThisAppProps.PROP_CHECKBOX_PROCESS_GROUPS_SEPARATELY);
    try {
      return Boolean.parseBoolean(checked);
    } catch (Exception ignored) {
    }
    return false;
  }

  private void loadDefaultDecoyTag() {
    String val = ThisAppProps.getLocalBundle().getString(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);
    textDecoyTagSeqDb.setText(val);
    ThisAppProps.save(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG, val);
  }

  private void loadLastSequenceDb() {
    String val = ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN);
    if (val != null) {
      textSequenceDbPath.setText(val);
    }
  }

  //endregion

  private void updateDecoyTagSeqDb(String newVal, boolean updateOtherTags) {
    textDecoyTagSeqDb.setText(newVal);
    MsfraggerGuiFrameUtils
        .validateAndSaveDecoyTagSeqDb(textDecoyTagFocusGained, textDecoyTagSeqDb, tipMap, null, updateOtherTags);
  }

  private void addChangeListenerTextSequenceDb() {
    SwingUtils.addChangeListener(textSequenceDbPath, e -> {
      if (btnTryDetectDecoyTag != null) {
        btnTryDetectDecoyTag
            .setEnabled(!StringUtils.isNullOrWhitespace(textSequenceDbPath.getText()));
      }
    });

  }

  void resetRunButtons(boolean runEnabled) {
    btnRun.setEnabled(runEnabled);
    btnStop.setEnabled(!runEnabled);
  }

  Map<String, LcmsFileGroup> getLcmsFileGroups() {
    List<InputLcmsFile> lcmsInputs = tableModelRawFiles.dataCopy();
    Map<String, List<InputLcmsFile>> mapGroup2Files = lcmsInputs.stream()
        .collect(Collectors.groupingBy(InputLcmsFile::getGroup));

    Map<String, LcmsFileGroup> result = new TreeMap<>();
    for (Entry<String, List<InputLcmsFile>> e : mapGroup2Files.entrySet()) {
      result.put(e.getKey(), new LcmsFileGroup(e.getKey(), e.getValue()));
    }

    return result;
  }


  /**
   * @param args the command line arguments
   */
  public static void main(String args[]) {
    SwingUtils.setLaf();

    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    Locale.setDefault(Locale.ROOT);

    /* Create and display the form */
    java.awt.EventQueue.invokeLater(() -> {
      final MsfraggerGuiFrame frame = new MsfraggerGuiFrame();

      frame.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          EventBus.getDefault().post(new MessageSaveCache());
          EventBus.getDefault().post(MessageSaveUiState.newForCache());
        }
      });

      Thread.setDefaultUncaughtExceptionHandler((t, e) -> {
        String stacktrace = LogUtils.stacktrace(e);
        log.debug("Something unexpected happened!", e);
        SwingUtils.userShowError(frame, stacktrace);
      });

      LogbackJTextPaneAppender appender = new LogbackJTextPaneAppender();
      appender.start();
      log.debug("Started LogbackJTextPaneAppender logger");
      appender.setTextPane(frame.console);

      frame.setVisible(true);
      Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
      frame.setLocation(dim.width / 2 - frame.getSize().width / 2,
          dim.height / 2 - frame.getSize().height / 2);
    });
  }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnAbout;
    private javax.swing.JButton btnAboutInConfig;
    private javax.swing.JButton btnBrowse;
    private javax.swing.JButton btnBrowseBinPython;
    private javax.swing.JButton btnClearCache;
    private javax.swing.JButton btnClearConsole;
    private javax.swing.JButton btnDbDownload;
    private javax.swing.JButton btnExportLog;
    private javax.swing.JButton btnFindTools;
    private javax.swing.JButton btnGroupsAssignToSelected;
    private javax.swing.JButton btnGroupsByFilename;
    private javax.swing.JButton btnGroupsByParentDir;
    private javax.swing.JButton btnGroupsClear;
    private javax.swing.JButton btnGroupsConsecutive;
    private javax.swing.JButton btnLoadAllToolsConfig;
    private javax.swing.JButton btnMsfraggerBinBrowse;
    private javax.swing.JButton btnMsfraggerBinDownload;
    private javax.swing.JButton btnMsfraggerUpdate;
    private javax.swing.JButton btnOpenInExplorer;
    private javax.swing.JButton btnPepProphDefaultsClosed;
    private javax.swing.JButton btnPepProphDefaultsNonspecific;
    private javax.swing.JButton btnPepProphDefaultsOpen;
    private javax.swing.JButton btnPhilosopherBinBrowse;
    private javax.swing.JButton btnPhilosopherBinDownload;
    private javax.swing.JButton btnPrintCommands;
    private javax.swing.JButton btnProtProphDefaultsClosed;
    private javax.swing.JButton btnProtProphDefaultsOpen;
    private javax.swing.JButton btnRawAddFiles;
    private javax.swing.JButton btnRawAddFolder;
    private javax.swing.JButton btnRawClear;
    private javax.swing.JButton btnRawRemove;
    private javax.swing.JButton btnReportErrors;
    private javax.swing.JButton btnRun;
    private javax.swing.JButton btnSaveAllToolsConfig;
    private javax.swing.JButton btnSelectWrkingDir;
    private javax.swing.JButton btnStop;
    private javax.swing.JButton btnTryDetectDecoyTag;
    private javax.swing.JCheckBox checkCombinedPepxml;
    private javax.swing.JCheckBox checkDryRun;
    private javax.swing.JCheckBox checkEnableDiaumpire;
    private javax.swing.JCheckBox checkProcessGroupsSeparately;
    private javax.swing.JCheckBox chkRunPeptideProphet;
    private javax.swing.JCheckBox chkRunProteinProphet;
    private javax.swing.JScrollPane consoleScrollPane;
    private javax.swing.JEditorPane editorMsfraggerCitation;
    private javax.swing.JEditorPane editorPhilosopherLink;
    private javax.swing.JEditorPane editorSequenceDb;
    private javax.swing.JEditorPane epDbsliceInfo;
    private javax.swing.JEditorPane epSpeclibInfo2;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel34;
    private javax.swing.JLabel jLabel40;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JScrollPane jScrollPane5;
    private javax.swing.JScrollPane jScrollPane6;
    private javax.swing.JLabel lblDbsliceInfo1;
    private javax.swing.JLabel lblFastaCount;
    private javax.swing.JLabel lblFindAutomatically;
    private javax.swing.JLabel lblFraggerJavaVer;
    private javax.swing.JLabel lblOutputDir;
    private javax.swing.JLabel lblPhilosopherInfo;
    private javax.swing.JLabel lblPythonInfo;
    private javax.swing.JLabel lblSpeclibInfo1;
    private javax.swing.JPanel panelBottomHints;
    private javax.swing.JPanel panelConfig;
    private umich.msfragger.params.crystalc.CrystalcPanel panelCrystalc;
    private javax.swing.JPanel panelDbInfo;
    private javax.swing.JPanel panelDownstream;
    private javax.swing.JPanel panelMsfraggerConfig;
    private javax.swing.JPanel panelPeptideProphet;
    private javax.swing.JPanel panelPeptideProphetOptions;
    private javax.swing.JPanel panelPhilosopherConfig;
    private javax.swing.JPanel panelProteinProphet;
    private javax.swing.JPanel panelProteinProphetOptions;
    private umich.msfragger.params.ionquant.QuantJPanel panelQuant;
    private javax.swing.JPanel panelReport;
    private umich.msfragger.params.philosopher.ReportPanel panelReportOptions;
    private javax.swing.JPanel panelRun;
    private javax.swing.JPanel panelSelectFiles;
    private javax.swing.JPanel panelSelectedFiles;
    private javax.swing.JPanel panelSequenceDb;
    private umich.msfragger.params.ptmshepherd.PtmshepherdJPanel ptmshepherdPanel;
    private javax.swing.JScrollPane scrollConfig;
    private javax.swing.JScrollPane scrollDownstream;
    private javax.swing.JScrollPane scrollEpSpeclibInfo2;
    private javax.swing.JScrollPane scrollPaneRawFiles;
    private umich.msfragger.params.speclib.SpeclibPanel speclibPanel1;
    private javax.swing.JTabbedPane tabPane;
    private javax.swing.JTextField textBinMsfragger;
    private javax.swing.JTextField textBinPhilosopher;
    private javax.swing.JTextField textBinPython;
    private javax.swing.JTextField textDecoyTagSeqDb;
    private javax.swing.JTextArea textPepProphCmd;
    private javax.swing.JTextField textSequenceDbPath;
    private TmtiPanel tmtiPanel1;
    private javax.swing.JTextArea txtProteinProphetCmdLineOpts;
    private javax.swing.JTextField txtWorkingDir;
    // End of variables declaration//GEN-END:variables
}
