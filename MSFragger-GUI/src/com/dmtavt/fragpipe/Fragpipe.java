package com.dmtavt.fragpipe;

import static com.dmtavt.fragpipe.Version.PROP_LAST_RELEASE_VER;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.FragpipeCacheUtils;
import com.dmtavt.fragpipe.api.Notifications;
import com.dmtavt.fragpipe.api.PropsFile;
import com.dmtavt.fragpipe.api.UiTab;
import com.dmtavt.fragpipe.api.UpdatePackage;
import com.dmtavt.fragpipe.cmd.ToolingUtils;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.messages.MessageClearCache;
import com.dmtavt.fragpipe.messages.MessageExportLog;
import com.dmtavt.fragpipe.messages.MessageLoadUi;
import com.dmtavt.fragpipe.messages.MessageOpenInExplorer;
import com.dmtavt.fragpipe.messages.MessageSaveCache;
import com.dmtavt.fragpipe.messages.MessageSaveUiState;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.MessageUiRevalidate;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.dmtavt.fragpipe.messages.NoteConfigTips;
import com.dmtavt.fragpipe.messages.NoteFragpipeCache;
import com.dmtavt.fragpipe.messages.NoteFragpipeProperties;
import com.dmtavt.fragpipe.messages.NoteFragpipeUpdate;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.process.ProcessManager;
import com.dmtavt.fragpipe.tabs.TabConfig;
import com.dmtavt.fragpipe.tabs.TabDatabase;
import com.dmtavt.fragpipe.tabs.TabDiann;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tabs.TabPtms;
import com.dmtavt.fragpipe.tabs.TabQuantitaionLabeling;
import com.dmtavt.fragpipe.tabs.TabQuantitaionLfq;
import com.dmtavt.fragpipe.tabs.TabRun;
import com.dmtavt.fragpipe.tabs.TabSpecLib;
import com.dmtavt.fragpipe.tabs.TabUmpire;
import com.dmtavt.fragpipe.tabs.TabValidation;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tools.dbsplit.DbSplit2;
import com.dmtavt.fragpipe.tools.speclibgen.SpecLibGen2;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.ScreenUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.FormEntry.Builder;
import com.github.chhh.utils.swing.HtmlStyledJEditorPane;
import com.github.chhh.utils.swing.LogbackJTextPaneAppender;
import com.github.chhh.utils.swing.TextConsole;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.HeadlessException;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Properties;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;

import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.greenrobot.eventbus.NoSubscriberEvent;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.SubscriberExceptionEvent;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Fragpipe extends JFrameHeadless {
//  static {System.setProperty("java.awt.headless", "true");}
//  public static boolean headless = java.awt.GraphicsEnvironment.isHeadless();
  public static boolean headless = false;
  public static Path manifest_file;
  public static java.util.concurrent.CountDownLatch init_done= new java.util.concurrent.CountDownLatch(1);
  public static java.util.concurrent.CountDownLatch load_manifest_done = new java.util.concurrent.CountDownLatch(1);
  public static java.util.concurrent.CountDownLatch load_workflow_done = new java.util.concurrent.CountDownLatch(1);
  public static boolean dry_run = false;
  public static StringBuilder cmds = new StringBuilder();
  final public static PrintStream out = System.out;

  public static final String UI_STATE_CACHE_FN = "fragpipe-ui.cache";
  private static final Logger log = LoggerFactory.getLogger(Fragpipe.class);
  public static final Color COLOR_GREEN = new Color(105, 193, 38);
  public static final Color COLOR_GREEN_DARKER = new Color(104, 184, 55);
  public static final Color COLOR_GREEN_DARKEST = new Color(82, 140, 26);
  public static final Color COLOR_RED = new Color(236, 99, 80);
  public static final Color COLOR_RED_DARKER = new Color(166, 56, 68);
  public static final Color COLOR_RED_DARKEST = new Color(155, 35, 29);
  public static final Color COLOR_BLACK = new Color(0, 0, 0);

  public static final Color COLOR_TOOL = new Color(140, 3, 89);
  public static final Color COLOR_WORKDIR = new Color(6, 2, 140);
  public static final Color COLOR_CMDLINE = new Color(0, 107, 109);

  public static final String TAB_NAME_LCMS = "Workflow";
  public static final String TAB_NAME_MSFRAGGER = "MSFragger";
  public static final String TAB_NAME_UMPIRE = "DIA-Umpire";
  public static final String PREFIX_FRAGPIPE = "fragpipe.";
  public static final String PROP_NOCACHE = "do-not-cache";

  public static final Function<String, String> PREPEND_FRAGPIPE = (name) -> {
    if (name == null)
      return PREFIX_FRAGPIPE;
    return name.startsWith(PREFIX_FRAGPIPE) ? name : PREFIX_FRAGPIPE + name;
  };
  public static final Function<String, String> APPEND_NO_CACHE = (name) -> {
    if (name == null)
      return PROP_NOCACHE;
    return name.contains(PROP_NOCACHE) ? name : name + "." + PROP_NOCACHE;
  };

  public final Notifications tips = new Notifications();
  private static final FragpipeUpdater updater;

  public JTabbedPane tabs;
  public TextConsole console;
  public JLabel defFont;
  private boolean dontSaveCacheOnExit;

  static {
    updater = new FragpipeUpdater();
    Bus.registerQuietly(updater);
  }

  public Fragpipe() throws HeadlessException {
    super(headless);
    init();
    initUi();
    initMore();
  }

  private void init() {
    log.debug("Start init()");
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        if (!dontSaveCacheOnExit) {
          try {
            saveCache();
          } catch (IllegalStateException ex) {
            log.error("Error while trying to save cache on exit", ex);
          }
        }
      }
    });

    Thread.setDefaultUncaughtExceptionHandler(Fragpipe::uncaughtExceptionHandler);

    log.debug("Done init()");
  }

  public static void uncaughtExceptionHandler(Thread t, Throwable e) {
    final String stacktrace = ExceptionUtils.getStackTrace(e);
    log.error("Something unexpected happened!", e);
    SwingUtils.userShowError(null, stacktrace);
  }

  public static String getBinJava() {
    if (OsUtils.isWindows()) {
      Path dirApp = FragpipeLocations.get().getDirFragpipeRoot();
      Path p = dirApp.resolve("jre/bin/java.exe");
      log.debug("Getting java binary, dirApp [{}], resolved path: {}", dirApp, p);
      Path java = PathUtils.existing(p.toString());
      if (java != null) {
        log.debug("Embedded java binary found: {}", java);
        return java.toString();
      }
      log.debug("Embedded java binary NOT found");
    }
    return "java";
  }

  private void saveCache() {
    log.debug("Saving cache started");
    NoteFragpipeCache cache = Bus.getStickyEvent(NoteFragpipeCache.class);
    if (cache == null)
      throw new IllegalStateException("cache NoteFragpipeCache can't be null");

    Properties tabsAsProps = FragpipeCacheUtils.tabsSave(tabs);
    PropertiesUtils.merge(cache.propsUiState, Collections.singletonList(tabsAsProps));

    log.debug("Saving ui cache: collected {} properties from UI. Size after merging with cached object: {}.",
        tabsAsProps.size(), cache.propsUiState.size());
    try {
      cache.propsUiState.setPath(FragpipeLocations.get().getPathUiCache(false));
      cache.propsUiState.save();
      cache.propsUiState.setPath(FragpipeLocations.get().getPathUiCache(true));
      cache.propsUiState.save();
    } catch (IOException ex) {
      log.error("Error saving ui cache", ex);
    }
    try {
      cache.propsRuntime.setPath(FragpipeLocations.get().getPathRuntimeCache(false));
      cache.propsRuntime.save();
      cache.propsRuntime.setPath(FragpipeLocations.get().getPathRuntimeCache(true));
      cache.propsRuntime.save();
    } catch (IOException ex) {
      log.error("Error saving runtime cache", ex);
    }

    // saving workflows
    Path dirWorkflows = FragpipeLocations.get().getDirWorkflows();
    Path lts = FragpipeLocations.get().getPathLongTermStorage().resolve(dirWorkflows.getFileName());
    log.debug("Trying to save workflows between sessions. From: {}, To: {}", dirWorkflows, lts);
    try {
      FileUtils.copyDirectory(dirWorkflows.toFile(), lts.toFile());
    } catch (IOException e) {
      log.error("Error saving workflows between sessions", e);
      throw new IllegalStateException(e);
    }
  }

  public static FormEntry.Builder fe(JComponent comp, String compName) {
    return FormEntry.builder(comp, StringUtils.prependOnce(compName, PREFIX_FRAGPIPE));
  }

  public static FormEntry.Builder fe(JComponent comp, String compName, String prefix) {
    return FormEntry.builder(comp, StringUtils.prependOnce(compName, prefix));
  }

  public static Builder feNoCache(JComponent comp, String compName) {
    return feNoCache(comp, compName, PREFIX_FRAGPIPE);
  }

  public static FormEntry.Builder feNoCache(JComponent comp, String compName, String prefix) {
    return FormEntry.builder(comp, StringUtils.prependOnce(StringUtils.appendOnce(compName, PROP_NOCACHE), prefix));
  }


  public static void main(String args[]) {
//    args = new String[]{"/home/ci/FragPipe/MSFragger-GUI/resources/workflows/Open.workflow", "/home/ci/tmp/lcms-files.fp-manifest", "--headless", "--dry-run"};
    args = new String[]{"/home/ci/.config/FragPipe/fragpipe/fragpipe-ui.cache", "/home/ci/tmp/lcms-files.fp-manifest", "--headless", "--dry-run"};
    if (args.length > 2 && args[2].equals("--headless")) {
      headless = true;
      manifest_file = Paths.get(args[1]);
    }
    if (args.length > 3 && args[3].equals("--dry-run"))
      dry_run = true;
    SwingUtils.setLaf();
    FragpipeLoader fragpipeLoader = new FragpipeLoader();
    Bus.register(fragpipeLoader);
    if (headless)
      headless(Paths.get(args[0]));
  }

  public static void headless(final Path workflow_file) {
    System.out.println("b4 Bus.post(new MessageManifestLoad()), load workflow file");
    try {
      init_done.await();
    } catch (InterruptedException ex) {
      throw new RuntimeException(ex);
    }
    final FragpipeLocations fpl = FragpipeLocations.get();
    Bus.post(new MessageLoadUi(fpl.tryLoadSilently(workflow_file, "user")));
    Bus.post(new com.dmtavt.fragpipe.messages.MessageManifestLoad());
    System.out.println("b4 Bus.post(new MessageRun(true));");
    try {
      load_workflow_done.await();
      load_manifest_done.await();
      Thread.sleep(500);
    } catch (InterruptedException ex) {
      throw new RuntimeException(ex);
    }
    Bus.post(new com.dmtavt.fragpipe.messages.MessageRun(dry_run));
  }

  static void displayMainWindow() {
    log.debug("Entered displayMainWindow");
    java.awt.EventQueue.invokeLater(() -> {
      log.debug("Creating Fragpipe instance");
      final Fragpipe fp0 = new Fragpipe();
      log.debug("Done creating Fragpipe instance");
      if (headless) {
        init_done.countDown();
        return;
      }
      final JFrame fp = fp0.toJFrame();

      fp.pack();
      decorateFrame(fp);
      log.debug("Showing Fragpipe frame");

      fp.addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent we) {
          int result = JOptionPane.showConfirmDialog(fp, "Do you want to exit now?", "FragPipe", JOptionPane.YES_NO_OPTION);
          if (result == JOptionPane.YES_OPTION) {
            fp.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
          } else if (result == JOptionPane.NO_OPTION) {
            fp.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
          }
        }
      });

      fp.setVisible(true);

      Rectangle screen = ScreenUtils.getScreenTotalArea(fp);
      fp.setSize(fp.getWidth(), Math.min((int)(screen.height * 0.8), fp.getHeight()));
      SwingUtils.centerFrame(fp);
    });
  }

  private TextConsole createConsole() {
    TextConsole c = new TextConsole(false);
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
        menuItem.addActionListener(e1 -> Bus.post(new MessageExportLog()));
        menu.add(menuItem);
        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    });

    LogbackJTextPaneAppender appender = new LogbackJTextPaneAppender();
    appender.start();
    log.debug("Started LogbackJTextPaneAppender logger");
    appender.setTextPane(c);

    return c;
  }

  /**
   * Use to name all the components that need to save state between runs. Will prepend their name
   * with "fragpipe." prefix.
   */
  public static Component rename(Component comp, String name) {
    return rename(comp, name, false);
  }

  /**
   * Use to name all the components that need to save state between runs.
   * Will prepend their name with "fragpipe." prefix.
   */
  public static Component rename(Component comp, String name, boolean isNoCache) {
    String s = PREPEND_FRAGPIPE.apply(name);
    comp.setName(isNoCache ? APPEND_NO_CACHE.apply(s) : s);
    return comp;
  }

  /**
   * Use to name all the components that need to save state between runs.
   * Will prepend their name with provided prefix.
   */
  public static Component rename(Component comp, String name, String prefix) {
    return rename(comp, name, prefix, false);
  }

  /**
   * Use to name all the components that need to save state between runs.
   * Will prepend their name with provided prefix.
   * @param isNoCache Also append name with 'no-cache' tag. Useful if you want to have a unique name
   *                  for an element, but don't want it to be saved to cache file.
   */
  public static Component rename(Component comp, String name, String prefix, boolean isNoCache) {
    String s = StringUtils.prependOnce(name, prefix);
    comp.setName(isNoCache ? APPEND_NO_CACHE.apply(s) : s);
    return comp;
  }

  /** Same as calling {@link #rename(Component, String, boolean)} with True as last arg. */
  public static Component renameNoCache(Component comp, String name) {
    return rename(comp, name, true);
  }

  public static Component renameNoCache(Component comp) {
    String name = comp.getName();
    if (StringUtils.isBlank(name))
      return comp;
    comp.setName(APPEND_NO_CACHE.apply(name));
    return comp;
  }

  /** Same as calling {@link #rename(Component, String, boolean)} with True as last arg. */
  public static Component renameNoCache(Component comp, String name, String prefix) {
    return rename(comp, name, prefix, true);
  }

  private JTabbedPane createTabs(TextConsole console) {
    log.debug("Start createTabs()");
    final JTabbedPane t = new JTabbedPane(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);

    Consumer<UiTab> addTab = tab -> t.addTab(tab.getTitle(), tab.getIcon(), SwingUtils.wrapInScroll(tab.getComponent()), tab.getTooltip());
    Consumer<UiTab> addTabNoScroll = tab -> t.addTab(tab.getTitle(), tab.getIcon(), tab.getComponent(), tab.getTooltip());

    TabConfig tabConfig = new TabConfig();
    TabWorkflow tabWorkflow = new TabWorkflow();
    TabUmpire tabUmpire = new TabUmpire();
    TabDatabase tabDatabase = new TabDatabase();
    TabMsfragger tabMsfragger = new TabMsfragger();
    TabValidation tabValidation = new TabValidation();
    TabQuantitaionLfq tabQuantitaionLfq = new TabQuantitaionLfq();
    TabQuantitaionLabeling tabQuantitaionLabeling = new TabQuantitaionLabeling();
    TabPtms tabPtms = new TabPtms();
    TabSpecLib tabSpecLib = new TabSpecLib();
    TabDiann tabDiann = new TabDiann();
    TabRun tabRun = new TabRun(console);

    addTab.accept(new UiTab("Config", tabConfig, "/com/dmtavt/fragpipe/icons/150-cogs.png", null));
    addTabNoScroll.accept(new UiTab(TAB_NAME_LCMS, tabWorkflow,
        "/com/dmtavt/fragpipe/icons/icon-workflow-16.png", null));
    addTab.accept(new UiTab("Umpire", tabUmpire,
        "/com/dmtavt/fragpipe/icons/dia-umpire-16x16.png", null));
    addTab.accept(new UiTab("Database", tabDatabase,
        "/com/dmtavt/fragpipe/icons/icon-dna-helix-16.png", null));
    addTab.accept(new UiTab(TAB_NAME_MSFRAGGER, tabMsfragger,
        "/com/dmtavt/fragpipe/icons/bolt-outlined-16.png", null));
    addTab.accept(new UiTab("Validation", tabValidation,
        "/com/dmtavt/fragpipe/icons/icon-filtration-16.png", null));
    addTab.accept(new UiTab("PTMs", tabPtms, "/com/dmtavt/fragpipe/icons/icon-edit-16.png", null));
    addTab.accept(new UiTab("Quant (MS1)", tabQuantitaionLfq,
        "/com/dmtavt/fragpipe/icons/icon-scales-balance-16.png", null));
    addTab.accept(new UiTab("Quant (Isobaric)", tabQuantitaionLabeling,
        "/com/dmtavt/fragpipe/icons/icon-scales-balance-color-2-16.png", null));
    addTab.accept(new UiTab("Spec Lib", tabSpecLib,
        "/com/dmtavt/fragpipe/icons/icon-library-16.png", null));
    addTab.accept(new UiTab("Quant (DIA)", tabDiann,
        "/com/dmtavt/fragpipe/icons/icon-diann-16.png", null));
    addTabNoScroll.accept(new UiTab("Run", tabRun, "/com/dmtavt/fragpipe/icons/video-play-16.png", null));

    log.debug("Done createTabs()");
    return t;
  }

  public static void decorateFrame(Window frame) {
    frame.setIconImages(ToolingUtils.loadIcon());
  }

  private synchronized void initUi() {
    log.debug("Start Fragpipe.initUi()");
    final JFrame fp = this.toJFrame();
    if (!headless) {
      fp.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      fp.setTitle(Version.PROGRAM_TITLE + " (v" + Version.version() + ")");
      fp.setLocale(Locale.ROOT);
      fp.setMinimumSize(new Dimension(640, 480));
      fp.setLayout(new MigLayout(new LC().fill()));
    }
    defFont = new JLabel("dummy label to get default font from");
    console = createConsole();
    tabs = createTabs(console);
    if (!headless) {
      fp.add(tabs, new CC().grow());
    }
    if (OsUtils.isMac()) {
      final String notes = "FragPipe is not supported on Mac. Some software used by FragPipe do not work on Mac.";

      JPanel panel = new JPanel();
      panel.setLayout(new BorderLayout());
      panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
      panel.add(new JLabel("Unsupported OS"), BorderLayout.PAGE_START);
      JTextArea notesArea = new JTextArea();
      notesArea.setText(notes);
      JScrollPane notesScroller = new JScrollPane();
      notesScroller.setBorder(BorderFactory.createTitledBorder("Details: "));
      notesScroller.setViewportView(notesArea);
      panel.add(notesScroller, BorderLayout.CENTER);
      if (!headless)
        SwingUtils.showDialog(this.toJFrame(), panel);
    }

    log.debug("Done Fragpipe.initUi()");
  }

  private void initMore() {
    log.debug("Fragpipe.initMore() started, subscribing to bus");
    Bus.register(this);
    Bus.register(tips);
    Bus.postSticky(new NoteConfigTips(tips));
    Bus.postSticky(this);

    // initialize singletons (mainly to subscribe them to the bus)
    ProcessManager.get().init();
    DbSplit2.initClass();
    SpecLibGen2.initClass();
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageSaveCache m) {
    saveCache();
  }

  @Subscribe
  public void on(MessageSaveUiState m) {
    log.debug("Writing ui state cache to: {}", m.path.toString());
    try (OutputStream os = Files.newOutputStream(m.path)) {
      FragpipeCacheUtils.tabsSave(os, tabs);
    } catch (IOException e) {
      log.error("Could not write fragpipe cache to: {}", m.path.toString());
    }
    log.debug("Done writing ui state cache to: {}", m.path.toString());
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageClearCache m) {
    log.debug("Got message MessageClearCache");
    List<Path> paths = FragpipeLocations.get().getCachePaths();

    JEditorPane msg = SwingUtils.createClickableHtml(true,
        "Delete the following files?\n&nbsp;&nbsp;&nbsp;&nbsp;" + paths.stream().map(p -> p.toAbsolutePath().normalize().toString())
            .collect(Collectors.joining("\n&nbsp;&nbsp;&nbsp;&nbsp;")));

    int answer = SwingUtils.showConfirmDialog(this.toJFrame(), msg);
    if (JOptionPane.OK_OPTION != answer) {
      log.debug("User cancelled cache cleaning");
    } else {
      try {
        log.info("Deleting cache files:\n\t" + paths.stream().map(Path::toString)
            .collect(Collectors.joining("\n\t")));
        FragpipeLocations.get().delete(paths);
      } catch (IOException e) {
        log.error("Error deleting cache files", e);
        SwingUtils.showErrorDialogWithStacktrace(e, this.toJFrame());
      }
    }

    if (m.doClose) {
      dontSaveCacheOnExit = true;
      System.exit(0);
    }
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteFragpipeCache m) {
    log.debug("Got NotePreviousUiState, updating UI");
    loadUi(m.propsUiState);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLoadUi m) {
    loadUi(m.props);
  }

  private void loadUi(Properties props) {
    log.debug("loadUi() called");
    @SuppressWarnings("unchecked")
    final Map<String, String> props_workflow_only = (Map)
            props.entrySet().stream().filter(e -> TabWorkflow.filter_props((String) e.getValue()))
                    .collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue()));
    FragpipeCacheUtils.tabsLoad(Fragpipe.headless ? PropertiesUtils.toMap(props) : props_workflow_only, tabs);
    Bus.post(new MessageUiRevalidate());
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN_ORDERED)
  public void on(NoteFragpipeProperties m) {
    if (m.propsFix == null) {
      log.debug("Got NoteFragpipeProperties with null props");
      return;
    }
    String remoteVer = m.propsFix.getProperty(PROP_LAST_RELEASE_VER);
    int cmp = VersionComparator.cmp(Version.version(), remoteVer);
    log.debug("Got NoteFragpipeProperties, property {}={}. Current version: {}, their comparison = {}", PROP_LAST_RELEASE_VER, remoteVer, Version.version(), cmp);
    String announcement = m.propsFix.getProperty(Version.PROP_ANNOUNCE);
    if (cmp < 0) {
      Bus.postSticky(new NoteFragpipeUpdate(remoteVer, m.propsFix.getProperty("fragpipe.download-url"), announcement));
    }

    if (StringUtils.isNotBlank(announcement) && cmp >= 0) {
      Bus.postSticky(new NoteFragpipeUpdate("", "", announcement));
    }

    // check for potential new update packages
    List<UpdatePackage> updates = FragpipeUpdater.checkNewUpdatePackages(m.propsFix);
    FragpipeUpdater.askToDownloadUpdates(this.toJFrame(), updates);
  }



  @Subscribe
  public void on(SubscriberExceptionEvent m) {
    log.error("Error delivering events through the bus", m.throwable);
    SwingUtils.showErrorDialogWithStacktrace(m.throwable, this.toJFrame());
  }

  @Subscribe
  public void on(MessageShowAboutDialog m) {
    showAboutDialog(this.toJFrame());
  }

  private String createAboutBody() {
    final Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
    String linkSite = p.getProperty(ThisAppProps.PROP_LAB_SITE_URL, "http://nesvilab.org");

    return "FragPipe (v" + Version.version() + ")<br>"
        + "Dmitry Avtonomov<br>"
        + "Fengchao Yu<br>"
        + "Guo-Ci Teo<br>"
        + "University of Michigan, 2017-2021<br><br>"
        + "<a href=\"" + linkSite + "\">Alexey Nesvizhskii lab</a><br/>&nbsp;<br/>"
        + "Components:<br>"
        + "<a href='https://diaumpire.nesvilab.org/'>DIA-Umpire</a>: Chih-Chiang Tsou<br>"
        + "<a href='https://msfragger.nesvilab.org/'>MSFragger</a>: Andy Kong, Dmitry Avtonomov, Guo-Ci Teo, Fengchao Yu<br>"
        + "MSFragger Glyco mode: Daniel Polasky, Fengchao Yu, Guo-Ci Teo<br>"
        + "<a href='https://www.nesvilab.org/Crystal-C/'>Crystal-C</a>: Hui-Yin Chang<br>"
        + "MSBooster: Kevin Yang<br>"
        + "<a href='https://philosopher.nesvilab.org/'>Philosopher</a>: Felipe Leprevost<br>"
        + "<a href='http://percolator.ms/'>Percolator</a>: Lukas Käll<br>"
        + "<a href='http://www.tppms.org/tools/ptm/'>PTMProphet</a>: David Shteynberg<br>"
        + "<a href='https://ptmshepherd.nesvilab.org/'>PTM-Shepherd</a>: Andy Kong, Daniel Geiszler<br>"
        + "<a href='https://ionquant.nesvilab.org/'>IonQuant</a>: Fengchao Yu<br>"
        + "<a href='https://tmt-integrator.nesvilab.org/'>TMT-Integrator</a>: Hui-Yin Chang<br>"
        + "Spectral library generation: Guo-Ci Teo<br>"
        + "<a href='https://github.com/grosenberger/easypqp'>EasyPQP</a>: George Rosenberger<br>"
        + "<a href='https://github.com/vdemichev/DiaNN'>DIA-NN</a>: Vadim Demichev<br>"
        + "Websites and tutorials: Sarah Haynes<br>"
        + "Special thanks to our collaborators Lukas Käll (Percolator), George Rosenberger (EasyPQP), and Vadim Demichev (DIA-NN).<br><br>"
        + "RawFileReader reading tool: Copyright © 2016 by Thermo Fisher Scientific, Inc. All rights reserved.<br>"
        + "Bruker SDK library: Copyright by Bruker Daltonics. All rights reserved.<br>";
  }

  public void showAboutDialog(Component parent) {
    log.debug("Showing about dialog");
//    HtmlStyledJEditorPane ep = new HtmlStyledJEditorPane(true, createAboutBody());
    HtmlStyledJEditorPane ep = SwingUtils.createClickableHtml(createAboutBody());
    ep.setPreferredSize(new Dimension(350, 450));
    SwingUtils.showDialog(parent, ep, "About FragPipe", JOptionPane.INFORMATION_MESSAGE);
  }

  @Subscribe
  public void onNoSubscriberEvent(NoSubscriberEvent m) {
    String message = String.format("No subscribers for message type [%s]", m.originalEvent.getClass().getSimpleName());
    log.debug(message);
    //System.err.println(message);
  }

  /** Places to search for ext folder with reader libs. */
  public static List<Path> getExtBinSearchPaths() {
    NoteConfigMsfragger conf = Bus.getStickyEvent(NoteConfigMsfragger.class);
    if (conf == null || !conf.isValid())
      return Collections.emptyList();
    return Collections.singletonList(Paths.get(conf.path));
  }

  public static void getPropsFixAndSetVal(String prop, Component comp) {
    String v = propsFix().getProperty(prop);
    if (v == null) {
      log.warn("No property in bundle: {}", prop);
    } else {
      SwingUtils.valueSet(comp, v);
    }
  }

  public static String getPropFix(String prop) {
    return getPropFix(prop, null);
  }

  public static String getPropFix(String prop, String dotSuffix) {
    String dotted = StringUtils.isBlank(dotSuffix) ? prop : prop + "." + dotSuffix;
    return propsFix().getProperty(dotted);
  }

  public static Properties propsFix() {
    NoteFragpipeProperties p = Bus.getStickyEvent(NoteFragpipeProperties.class);
    if (p != null && p.propsFix != null) {
      return p.propsFix;
    }
    log.error("Message to developers. Fragpipe properties sticky note was empty, but should have been loaded at startup");
    return ThisAppProps.getLocalProperties();
  }

  public static PropsFile propsVar() {
    NoteFragpipeCache p = Bus.getStickyEvent(NoteFragpipeCache.class);
    if (p != null && p.propsRuntime != null) {
      return p.propsRuntime;
    }
    throw new IllegalStateException("Runtime properties should always at least be initialized to empty Properties object");
  }

  public static void propsVarSet(String name, String value) {
    Objects.requireNonNull(name);
    if (value == null) {
      propsVar().remove(name);
    } else {
      propsVar().setProperty(name, value);
    }
  }
  public static String propsVarGet(String name) {
    return propsVar().getProperty(name);
  }
  public static String propsVarGet(String name, String defaultVal) {
    String v = propsVar().getProperty(name);
    return v == null ? defaultVal : v;
  }

  public static PropsFile propsUi() {
    NoteFragpipeCache p = Bus.getStickyEvent(NoteFragpipeCache.class);
    if (p != null && p.propsUiState != null) {
      return p.propsUiState;
    }
    throw new IllegalStateException("UI State properties should always at least be initialized to empty Properties object");
  }

  /**
   * @throws NoSuchElementException in case a sticky of given class is not on the Bus.
   */
  public static <T> T getStickyStrict(Class<T> clazz) {
    T sticky = Bus.getStickyEvent(clazz);
    if (sticky == null) {
      if (clazz.getName().contentEquals("com.dmtavt.fragpipe.messages.NoteConfigPhilosopher")) {
        Bus.postSticky(new NoteConfigPhilosopher(null, "N/A"));
        sticky = Bus.getStickyEvent(clazz);
      } else {
        throw new NoSuchElementException("Sticky note not on the bus: " + clazz.getCanonicalName());
      }
    }
    return sticky;
  }

  public static <T> T getSticky(Class<T> clazz) throws NoStickyException {
    T sticky = Bus.getStickyEvent(clazz);
    if (sticky == null) {
      throw new NoStickyException("Sticky note not on the bus: " + clazz.getCanonicalName());
    }
    return sticky;
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageOpenInExplorer m) {
    if (!headless)
      FragpipeUtils.openInExplorer(this.toJFrame(), m.path.toString());
  }
}
