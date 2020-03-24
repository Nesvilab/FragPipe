package com.dmtavt.fragger.ui;

import com.dmtavt.fragger.ui.messages.MessageExportLog;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Predicate;
import javax.swing.ImageIcon;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.Version;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsInputFileTable;
import umich.msfragger.gui.MsfraggerGuiFrame;
import umich.msfragger.gui.MsfraggerGuiFrameUtils;
import umich.msfragger.gui.api.LogbackJTextPaneAppender;
import umich.msfragger.messages.MessageLcmsFilesAdded;
import umich.msfragger.messages.MessageLcmsFilesList;
import umich.msfragger.messages.MessageLoadAllForms;
import umich.msfragger.messages.MessagePythonBinSelectedByUser;
import umich.msfragger.messages.MessageSaveAllForms;
import umich.msfragger.messages.MessageSaveCache;
import umich.msfragger.messages.MessageType;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.fragger.FraggerMigPanel;
import umich.msfragger.util.FileDrop;
import umich.msfragger.util.LogUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.swing.JPanelWithEnablement;
import umich.swing.console.TextConsole;

public class Fragpipe extends JFrame {
  private static final Logger log = LoggerFactory.getLogger(Fragpipe.class);
  public static final Color COLOR_GREEN = new Color(105, 193, 38);
  public static final Color COLOR_GREEN_DARKER = new Color(104, 184, 55);
  public static final Color COLOR_GREEN_DARKEST = new Color(82, 140, 26);
  public static final Color COLOR_RED = new Color(236, 99, 80);
  public static final Color COLOR_RED_DARKER = new Color(166, 56, 68);
  public static final Color COLOR_RED_DARKEST = new Color(155, 35, 29);
  public static final Color COLOR_BLACK = new Color(0, 0, 0);

  JTabbedPane tabs;
  TextConsole console;
  JLabel defFont;

  public Fragpipe() throws HeadlessException {
    init();
  }

  public static void main(String args[]) {
    /* Set the Nimbus look and feel */
    //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
    /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
     * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html
     */

    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    Locale.setDefault(Locale.ROOT);
    try {
      if (OsUtils.isWindows()) {
        // native look on windows
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } else {
        // nimbus otherwise
        for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager
            .getInstalledLookAndFeels()) {
          if ("Nimbus".equals(info.getName())) {
            javax.swing.UIManager.setLookAndFeel(info.getClassName());
            break;
          }
        }
      }
    } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException e1) {
      java.util.logging.Logger.getLogger(MsfraggerGuiFrame.class.getName())
          .log(java.util.logging.Level.SEVERE, null, e1);
      try {
        for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager
            .getInstalledLookAndFeels()) {
          if ("Nimbus".equals(info.getName())) {
            javax.swing.UIManager.setLookAndFeel(info.getClassName());
            break;
          }
        }
      } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException e2) {
        java.util.logging.Logger.getLogger(MsfraggerGuiFrame.class.getName())
            .log(java.util.logging.Level.SEVERE, null, e2);
      }
    }
    //</editor-fold>

    /* Create and display the form */
    java.awt.EventQueue.invokeLater(() -> {
      final Fragpipe fp = new Fragpipe();

      fp.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          EventBus.getDefault().post(new MessageSaveCache());
          EventBus.getDefault().post(MessageSaveAllForms.forCaching());
        }
      });

      Thread.setDefaultUncaughtExceptionHandler((t, e) -> {
        String stacktrace = LogUtils.stacktrace(e);
        log.error("Something unexpected happened!", e);
        SwingUtils.userShowError(fp, stacktrace);
      });

      LogbackJTextPaneAppender appender = new LogbackJTextPaneAppender();
      appender.start();
      log.debug("Started LogbackJTextPaneAppender logger");
      appender.setTextPane(fp.console);

      fp.pack();
      fp.setVisible(true);
      Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
      fp.setLocation(dim.width / 2 - fp.getSize().width / 2,
          dim.height / 2 - fp.getSize().height / 2);
    });
  }

  private TextConsole createConsole() {
    TextConsole c = new TextConsole();
    final Font currentFont = c.getFont();
    c.setFont(new Font(Font.MONOSPACED, currentFont.getStyle(), currentFont.getSize()));
    c.setContentType("text/plain; charset=UTF-8");
    c.addMouseListener(new MouseAdapter() {

      @Override
      public void mouseReleased(MouseEvent e) {
        if (e.isPopupTrigger()) {
          doPop(e);
        }
      }

      private void doPop(MouseEvent e) {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem menuItem = new JMenuItem("Export to text file");
        menuItem.addActionListener(e1 -> EventBus.getDefault().post(new MessageExportLog()));
        menu.add(menuItem);
        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    });
    return c;
  }

  private JTabbedPane createTabs() {
    final JTabbedPane t = new JTabbedPane(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);

    Consumer<UiTab> addTab = tab -> t.addTab(tab.getTitle(), tab.getIcon(), new JScrollPane(tab.getComponent()), tab.getTooltip());

    TabConfig tabConfig = new TabConfig();
    TabLcmsFiles tabLcmsFiles = new TabLcmsFiles();
    TabDatabase tabDatabase = new TabDatabase();
    TabMsfragger tabMsfragger = new TabMsfragger();
    TabDownstream tabDownstream = new TabDownstream();
    TabQuantitaion tabQuantitaion = new TabQuantitaion();
    TabMisc tabMisc = new TabMisc();
    TabRun tabRun = new TabRun(console);

    addTab.accept(new UiTab("Config", tabConfig, "/umich/msfragger/gui/icons/150-cogs.png", null));
    addTab.accept(new UiTab("LCMS Files", tabLcmsFiles, "/umich/msfragger/gui/icons/186-list-numbered.png", null));
    addTab.accept(new UiTab("Database", tabDatabase, "/umich/msfragger/gui/icons/093-drawer.png", null));
    addTab.accept(new UiTab("MSFragger", tabMsfragger, "/umich/msfragger/gui/icons/bolt-16.png", null));
    addTab.accept(new UiTab("Downstream", tabDownstream, "/umich/msfragger/gui/icons/348-filter.png", null));
    addTab.accept(new UiTab("Quant", tabQuantitaion, "/umich/msfragger/gui/icons/360-sigma.png", null));
    addTab.accept(new UiTab("PTMs + Misc", tabMisc, null, null));
    addTab.accept(new UiTab("Run", tabRun, "/umich/msfragger/gui/icons/video-play-16.png", null));

    return t;
  }

  private void init() {

    setTitle(Version.PROGRAM_TITLE + " (v" + Version.version() + ")");
    setLocale(Locale.ROOT);
    setMinimumSize(new Dimension(640, 480));

//    this.setLayout(new MigLayout(new LC().fillX()));
    this.setLayout(new BorderLayout());

    defFont = new JLabel("dummy label to get default font from");
    console = createConsole();
    tabs = createTabs();

    add(tabs, BorderLayout.CENTER);

//    consoleScrollPane.setViewportView(console);
//
//    defTextColor = UIManager.getColor("TextField.foreground");
//    if (defTextColor == null) {
//      defTextColor = Color.BLACK;
//    }
//
//    // check if fragger jar points to a correct location
//    if (!MsfraggerGuiFrameUtils.validateMsfraggerJarContents(textBinMsfragger.getText())) {
//      log.debug("Msfragger jar is not valid");
//    }
//
//    if (MsfraggerGuiFrameUtils.validatePhilosopherPath(textBinPhilosopher.getText()) == null) {
//      enablePhilosopherPanels(false);
//    }
//
//    tableModelRawFiles = MsfraggerGuiFrameUtils.createTableModelRawFiles();
//    tableModelRawFiles.addTableModelListener(e -> {
//      List<InputLcmsFile> files = tableModelRawFiles.dataCopy();
//      EventBus.getDefault().post(new MessageLcmsFilesList(MessageType.UPDATE, files));
//    });
//    tableRawFiles = new LcmsInputFileTable(tableModelRawFiles);
//    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnRawClear);
//    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsConsecutive);
//    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByParentDir);
//    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsByFilename);
//    tableRawFiles.addComponentsEnabledOnNonEmptyData(btnGroupsClear);
//    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnRawRemove);
//    tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnGroupsAssignToSelected);
//    tableRawFiles.fireInitialization();
//    tableRawFiles.setFillsViewportHeight(true);
//    scrollPaneRawFiles.setViewportView(tableRawFiles);
//
//    // Drag and drop support for files from Explorer to the Application
//    // this works only when tableRawFiles.setFillsViewportHeight(true) is called.
////        tableRawFiles.setDragEnabled(true);
////        tableRawFiles.setDropMode(DropMode.ON);
////        tableRawFiles.setFillsViewportHeight(true);
////        TransferHandler origHandler = tableRawFiles.getTransferHandler();
////        SimpleETableTransferHandler newHandler = new SimpleETableTransferHandler();
////        tableRawFiles.setTransferHandler(newHandler);
//    // dropping onto enclosing JPanel works.
//    tableRawFilesFileDrop = new FileDrop(panelSelectedFiles, true, files -> {
//      Predicate<File> pred = CmdMsfragger.getSupportedFilePredicate(getExtBinSearchPaths());
//      List<Path> accepted = new ArrayList<>(files.length);
//      for (File f : files) {
//        PathUtils.traverseDirectoriesAcceptingFiles(f, pred, accepted, false);
//      }
//      if (!accepted.isEmpty()) {
//        EventBus.getDefault().post(new MessageLcmsFilesAdded(accepted));
//      }
//    });
//
//    textBinPython.addFocusListener(new FocusAdapter() {
//      @Override
//      public void focusLost(FocusEvent e) {
//        EventBus.getDefault().post(new MessagePythonBinSelectedByUser(textBinPython.getText()));
//      }
//    });
//
//    fraggerMigPanel = new FraggerMigPanel();
//    final int fraggerTabIndex = 3;
//    final String fraggerTabName = "MSFragger";
//    tabPane.add(fraggerMigPanel, fraggerTabIndex);
//    tabPane.setTitleAt(fraggerTabIndex, fraggerTabName);
//
//
//    // set icons for tabs
//    Map<String, Integer> mapTabNameToIdx = new HashMap<>();
//    for (int i = 0, tabCount = tabPane.getTabCount(); i < tabCount; i++) {
//      mapTabNameToIdx.put(tabPane.getTitleAt(i), i);
//    }
//    setTabIcon(mapTabNameToIdx, "Config", "/umich/msfragger/gui/icons/146-wrench.png");
//    setTabIcon(mapTabNameToIdx, "Select LC/MS Files", "/umich/msfragger/gui/icons/198-download2.png");
//    setTabIcon(mapTabNameToIdx, "Database", "/umich/msfragger/gui/icons/093-drawer.png");
//    setTabIcon(mapTabNameToIdx, "Downstream", "/umich/msfragger/gui/icons/328-move-down.png");
//    setTabIcon(mapTabNameToIdx, "Report", "/umich/msfragger/gui/icons/185-clipboard.png");
//    setTabIcon(mapTabNameToIdx, fraggerTabName, "/umich/msfragger/gui/icons/bolt-16.png");
//    //setTabIcon(mapTabNameToIdx, "", "");
//
//    exec.submit(() -> MsfraggerGuiFrameUtils.validateAndSaveMsfraggerPath(this, textBinMsfragger.getText()));
//    exec.submit(() -> MsfraggerGuiFrameUtils.validateAndSavePhilosopherPath(this, textBinPhilosopher.getText()));
//    exec.submit(() -> Version.checkUpdates());
//    exec.submit(() -> MsfraggerGuiFrameUtils.checkPreviouslySavedParams(MsfraggerGuiFrame.this));
//
//    // The python check must be run before DbSlice and SpecLibGen.
//    // Don't run these checks asynchronously
//    exec.submit(() -> MsfraggerGuiFrameUtils.checkPython(MsfraggerGuiFrame.this));
//    exec.submit(() -> MsfraggerGuiFrameUtils.validateDbslicing(fraggerVer));
//    exec.submit(MsfraggerGuiFrameUtils::validateSpeclibgen);
//
//
//    exec.submit(() -> MsfraggerGuiFrameUtils.validateMsadjusterEligibility(fraggerVer));
//    exec.submit(() -> MsfraggerGuiFrameUtils.validateMsfraggerMassCalibrationEligibility(fraggerVer));
//
//
//    // submitting all "loadLast" methods for invocation
//    for (Method method : this.getClass().getDeclaredMethods()) {
//      // TODO: Old 'loadLast' mechanism is mostly replaced by auto save/load of components by 'name'
//      if (method.getName().startsWith("loadLast") && method.getParameterCount() == 0) {
//        exec.submit(() -> method.invoke(this));
//      }
//    }
//
//    // Force loading form caches
//    EventBus.getDefault().post(MessageLoadAllForms.forCaching());
//
//    initActions();
  }

  private void initMore() {
    //EventBus.getDefault().register(this);
  }
}
