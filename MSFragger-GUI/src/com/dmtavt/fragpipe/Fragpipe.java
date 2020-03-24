package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.messages.MessageExportLog;
import com.dmtavt.fragpipe.messages.MessageSaveAllForms;
import com.dmtavt.fragpipe.messages.MessageSaveCache;
import com.dmtavt.fragpipe.messages.MessageUmpireEnabled;
import com.github.chhh.utils.LogUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.TextConsole;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.ToolTipManager;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.NoSubscriberEvent;
import org.greenrobot.eventbus.Subscribe;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.Version;
import umich.msfragger.gui.api.LogbackJTextPaneAppender;

public class Fragpipe extends JFrame {
  private static final Logger log = LoggerFactory.getLogger(Fragpipe.class);
  public static final Color COLOR_GREEN = new Color(105, 193, 38);
  public static final Color COLOR_GREEN_DARKER = new Color(104, 184, 55);
  public static final Color COLOR_GREEN_DARKEST = new Color(82, 140, 26);
  public static final Color COLOR_RED = new Color(236, 99, 80);
  public static final Color COLOR_RED_DARKER = new Color(166, 56, 68);
  public static final Color COLOR_RED_DARKEST = new Color(155, 35, 29);
  public static final Color COLOR_BLACK = new Color(0, 0, 0);
  private static final String TAB_NAME_LCMS = "LCMS Files";
  private static final String TAB_NAME_UMPIRE = "DIA-Umpire SE";
  public static final String NAME_PRE_FRAGPIPE = "fragpipe.";
  public static final BiFunction<Component, String, Component> COMP_NAMER = (comp, name) -> {
    String prefixed = name.startsWith(NAME_PRE_FRAGPIPE) ? name : NAME_PRE_FRAGPIPE + name;
    comp.setName(prefixed);
    return comp;
  };

  JTabbedPane tabs;
  TextConsole console;
  JLabel defFont;
  private TabUmpire tabUmpire;

  public Fragpipe() throws HeadlessException {
    init();
    initMore();
  }

  public static void main(String args[]) {
    SwingUtils.setLaf();

    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
    Locale.setDefault(Locale.ROOT);

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

  /**
   * Use to name all the components that need to save state between runs.
   * Will prepend their name with "fragpipe.ui" prefix.
   * @param comp
   * @param name
   * @return
   */
  public static Component name(Component comp, String name) {
    return COMP_NAMER.apply(comp, name);
  }

  private JTabbedPane createTabs(TextConsole console) {
    final JTabbedPane t = new JTabbedPane(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);

    Consumer<UiTab> addTab = tab -> t.addTab(tab.getTitle(), tab.getIcon(), SwingUtils.scroll(tab.getComponent()), tab.getTooltip());

    TabConfig tabConfig = new TabConfig();
    TabLcmsFiles tabLcmsFiles = new TabLcmsFiles();
    TabDatabase tabDatabase = new TabDatabase();
    TabMsfragger tabMsfragger = new TabMsfragger();
    TabDownstream tabDownstream = new TabDownstream();
    TabQuantitaion tabQuantitaion = new TabQuantitaion();
    TabMisc tabMisc = new TabMisc();
    TabRun tabRun = new TabRun(console);
    tabUmpire = new TabUmpire();

    addTab.accept(new UiTab("Config", tabConfig, "/umich/msfragger/gui/icons/150-cogs.png", null));
    addTab.accept(new UiTab(TAB_NAME_LCMS, tabLcmsFiles, "/umich/msfragger/gui/icons/186-list-numbered.png", null));
    addTab.accept(new UiTab("Database", tabDatabase, "/umich/msfragger/gui/icons/093-drawer.png", null));
    addTab.accept(new UiTab("MSFragger", tabMsfragger, "/umich/msfragger/gui/icons/bolt-16.png", null));
    addTab.accept(new UiTab("Downstream", tabDownstream, "/umich/msfragger/gui/icons/348-filter.png", null));
    addTab.accept(new UiTab("Quant", tabQuantitaion, "/umich/msfragger/gui/icons/360-sigma.png", null));
    addTab.accept(new UiTab("PTMs + Misc", tabMisc, null, null));
    addTab.accept(new UiTab("Run", tabRun, "/umich/msfragger/gui/icons/video-play-16.png", null));

    return t;
  }

  private synchronized void init() {
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setTitle(Version.PROGRAM_TITLE + " (v" + Version.version() + ")");
    setLocale(Locale.ROOT);
    setMinimumSize(new Dimension(640, 480));


    this.setLayout(new MigLayout(new LC().fill()));
    //this.setLayout(new BorderLayout());

    defFont = new JLabel("dummy label to get default font from");
    console = createConsole();
    tabs = createTabs(console);

    //add(tabs, BorderLayout.CENTER);
    add(tabs, new CC().grow());

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
    EventBus.getDefault().register(this);
  }

  @Subscribe
  public void onUmpireEnabled(MessageUmpireEnabled m) {
    synchronized (this) {
      if (m.enabled) {
        final String prevTabName = TAB_NAME_LCMS;
        int prevTabIndex = tabs.indexOfTab(prevTabName);
        if (prevTabIndex < 0) {
          throw new IllegalStateException("Could not find tab named " + prevTabName);
        }
        final ImageIcon icon = UiUtils.loadIcon(Fragpipe.class, "/umich/msfragger/gui/icons/dia-umpire-16x16.png");
        tabs.insertTab(TAB_NAME_UMPIRE, icon, SwingUtils.scroll(tabUmpire), "", prevTabIndex + 1);

      } else {
        int index = tabs.indexOfTab(TAB_NAME_UMPIRE);
        if (index >= 0) {
          tabs.removeTabAt(index);
        }
      }
    }
  }

  @Subscribe
  public void onNoSubscriberEvent(NoSubscriberEvent m) {
    String message = String.format("No subscribers for message type [%s]", m.originalEvent.getClass().getSimpleName());
    log.warn(message);
    System.err.println(message);
  }
}
