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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe;

import static org.nesvilab.fragpipe.Version.PROP_LAST_RELEASE_VER;
import static org.nesvilab.fragpipe.Version.version;
import static org.nesvilab.fragpipe.tabs.TabWorkflow.maxProcessors;

import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.FragpipeCacheUtils;
import org.nesvilab.fragpipe.api.Notifications;
import org.nesvilab.fragpipe.api.PropsFile;
import org.nesvilab.fragpipe.api.UiTab;
import org.nesvilab.fragpipe.api.UpdatePackage;
import org.nesvilab.fragpipe.cmd.ToolingUtils;
import org.nesvilab.fragpipe.exceptions.NoStickyException;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.messages.MessageClearCache;
import org.nesvilab.fragpipe.messages.MessageExportLog;
import org.nesvilab.fragpipe.messages.MessageLoadUi;
import org.nesvilab.fragpipe.messages.MessageManifestLoad;
import org.nesvilab.fragpipe.messages.MessageOpenInExplorer;
import org.nesvilab.fragpipe.messages.MessageRun;
import org.nesvilab.fragpipe.messages.MessageSaveCache;
import org.nesvilab.fragpipe.messages.MessageSaveUiState;
import org.nesvilab.fragpipe.messages.MessageShowAboutDialog;
import org.nesvilab.fragpipe.messages.MessageUiRevalidate;
import org.nesvilab.fragpipe.messages.NoteConfigMsfragger;
import org.nesvilab.fragpipe.messages.NoteConfigSpeclibgen;
import org.nesvilab.fragpipe.messages.NoteConfigTips;
import org.nesvilab.fragpipe.messages.NoteFragpipeCache;
import org.nesvilab.fragpipe.messages.NoteFragpipeProperties;
import org.nesvilab.fragpipe.messages.NoteFragpipeUpdate;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.fragpipe.process.ProcessManager;
import org.nesvilab.fragpipe.tabs.TabConfig;
import org.nesvilab.fragpipe.tabs.TabDatabase;
import org.nesvilab.fragpipe.tabs.TabDiaPseudoMs2;
import org.nesvilab.fragpipe.tabs.TabDiann;
import org.nesvilab.fragpipe.tabs.TabDownstream;
import org.nesvilab.fragpipe.tabs.TabGlyco;
import org.nesvilab.fragpipe.tabs.TabMsfragger;
import org.nesvilab.fragpipe.tabs.TabPtms;
import org.nesvilab.fragpipe.tabs.TabQuantificationLabeling;
import org.nesvilab.fragpipe.tabs.TabQuantificationLfq;
import org.nesvilab.fragpipe.tabs.TabRun;
import org.nesvilab.fragpipe.tabs.TabSkyline;
import org.nesvilab.fragpipe.tabs.TabSpecLib;
import org.nesvilab.fragpipe.tabs.TabValidation;
import org.nesvilab.fragpipe.tabs.TabWorkflow;
import org.nesvilab.fragpipe.tools.dbsplit.DbSplit2;
import org.nesvilab.fragpipe.tools.fpop.FpopScript;
import org.nesvilab.fragpipe.tools.speclibgen.SpecLibGen2;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.PropertiesUtils;
import org.nesvilab.utils.ScreenUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.VersionComparator;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.FormEntry.Builder;
import org.nesvilab.utils.swing.HtmlStyledJEditorPane;
import org.nesvilab.utils.swing.LogbackJTextPaneAppender;
import org.nesvilab.utils.swing.TextConsole;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
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
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Properties;
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
import javax.swing.event.HyperlinkEvent;
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

  public static boolean headless = false;
  public static boolean printCommandsInDetail = false;
  public static Path manifestFile = null;
  public static Path workflowFile = null;
  public static java.util.concurrent.CountDownLatch initDone = new java.util.concurrent.CountDownLatch(1);
  public static java.util.concurrent.CountDownLatch loadManifestDone = new java.util.concurrent.CountDownLatch(1);
  public static java.util.concurrent.CountDownLatch loadWorkflowDone = new java.util.concurrent.CountDownLatch(1);
  public static java.util.concurrent.CountDownLatch runDone = new java.util.concurrent.CountDownLatch(1);
  public static boolean dryRun = false;
  public static Integer ram = null;
  static Integer nThreadsHeadlessOnly = null; // Note: this variable is only for headless mode. For the GUI mode, please get the number of threads using TabWorkflow:getThreads().
  public static String workdir = null;
  public static String toolsFolderPath = null;
  public static String philosopherBinPath = null;
  public static String diannBinPath = null;
  public static String pythonBinPath = null;

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

  private UiTab uiTabConfig;
  private UiTab uiTabWorkflow;
  private UiTab uiTabDiaPseudoMS2;
  private UiTab uiTabDb;
  private UiTab uiTabFragger;
  private UiTab uiTabValidation;
  private UiTab uiTabPtms;
  private UiTab uiTabGlyco;
  private UiTab uiTabQuantLfq;
  private UiTab uiTabQuantLabeled;
  private UiTab uiTabSpecLib;
  private UiTab uiTabDiann;
  private UiTab uiTabSkyline;
  private UiTab uiTabRun;
  private UiTab uiTabDownstream;

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

  private static void addTab(JTabbedPane tabPane, UiTab tab, int insertionIndex) {
    final Component comp = tab.isWrapTabInScroll()
            ? SwingUtils.wrapInScroll(tab.getComponent())
            : tab.getComponent();
    if (insertionIndex < 0) {
      tabPane.addTab(tab.getTitle(), tab.getIcon(), comp, tab.getTooltip());
    } else {
      tabPane.insertTab(tab.getTitle(), tab.getIcon(), comp, tab.getTooltip(), insertionIndex);
    }
  }

  private static void addTab(JTabbedPane tabPane, UiTab tab) {
    addTab(tabPane, tab, -1);
  }

  public static void uncaughtExceptionHandler(Thread t, Throwable e) {
    final String stacktrace = ExceptionUtils.getStackTrace(e);
    log.error("Something unexpected happened!", e);
    if (!Fragpipe.headless) {
      SwingUtils.userShowError(null, stacktrace);
    }
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
      cache.propsUiState.setPath(FragpipeLocations.get().getPathUiCache(true));
      cache.propsUiState.save();
    } catch (IOException ex) {
      log.error("Error saving ui cache. It won't affect the results.");
    }
    try {
      cache.propsRuntime.setPath(FragpipeLocations.get().getPathRuntimeCache(true));
      cache.propsRuntime.save();
    } catch (IOException ex) {
      log.error("Error saving runtime cache. It won't affect the results.");
    }

    // saving workflows
    Path dirWorkflows = FragpipeLocations.get().getDirWorkflows();
    Path lts = FragpipeLocations.get().getPathLongTermStorage().resolve(dirWorkflows.getFileName());
    log.debug("Trying to save workflows between sessions. From: {}, To: {}", dirWorkflows, lts);
    try {
      FileUtils.copyDirectory(dirWorkflows.toFile(), lts.toFile());
    } catch (IOException e) {
      log.error("Error saving workflows between sessions.");
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

  public static boolean checkMonoOnLinux() {
    if (OsUtils.isUnix()) {
      final ProcessBuilder pb = new ProcessBuilder("mono");
      try {
        pb.start();
      } catch (IOException e) {
        e.printStackTrace();
        return false;
      }
    }
    return true;
  }

  public static boolean showMonoError(final Component comp){
    JEditorPane ep = new JEditorPane("text/html", "<html>Thermo RAW files were used as input.<br/>"
            + "Mono must be installed to read Thermo .raw files<br/>"
            + "Install intructions from <a href=\"https://www.mono-project.com/download/stable/#download-lin\">Mono</a><br/>"
            + "</html>");
    ep.addHyperlinkListener(e -> {
      if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
        try {
          Desktop.getDesktop().browse(e.getURL().toURI());
        } catch (IOException ex) {
          throw new UncheckedIOException(ex);
        } catch (URISyntaxException ex) {
          throw new RuntimeException(ex);
        }
      }
    });
    ep.setEditable(false);
    JLabel label = new JLabel();
    ep.setBackground(label.getBackground());
    if (!checkMonoOnLinux()) {
      if (Fragpipe.headless) {
        log.error("Thermo RAW files were used as input.\n"+
                "Mono must be installed to read Thermo .raw files\n+" +
                "Install intructions from https://www.mono-project.com/download/stable/#download-lin");
      } else {
        JOptionPane.showMessageDialog(comp, ep, "Mono not installed", JOptionPane.ERROR_MESSAGE);
      }
      return true;
    }
    return false;
  }


  static void main0() {
    if (!headless && (workflowFile != null || manifestFile != null || workdir != null)) {
      System.err.println("It looks like you want to run FragPipe in headless mode, but you did not add --headless flag. Please double check your command.");
      System.exit(1);
    }

    if (!headless) {
      SwingUtils.setLaf();
    }

    FragpipeLoader fragpipeLoader = new FragpipeLoader();
    Bus.register(fragpipeLoader);
    if (headless) {
      if (workflowFile == null || !Files.exists(workflowFile) || !Files.isReadable(workflowFile) || !Files.isRegularFile(workflowFile)) {
        System.err.println("Please provide --workflow <path to workflow file> in the headless mode.");
        System.exit(1);
      } else if (manifestFile == null || !Files.exists(manifestFile) || !Files.isReadable(manifestFile) || !Files.isRegularFile(manifestFile)) {
        System.err.println("Please provide --manifest <path to manifest file> in the headless mode.");
        System.exit(1);
      } else if (ram != null && ram < 0) {
        System.err.println("ram is smaller than 0.");
        System.exit(1);
      } else if (nThreadsHeadlessOnly != null && nThreadsHeadlessOnly < 0) {
        System.err.println("Number of threads is smaller than 0.");
        System.exit(1);
      } else if (workdir == null || workdir.isEmpty()) {
        System.err.println("The path to workdir does not look right.");
        System.exit(1);
      } else if (toolsFolderPath != null && (toolsFolderPath.isEmpty() || !Files.exists(Paths.get(toolsFolderPath)) || !Files.isReadable(Paths.get(toolsFolderPath)))) {
        System.err.println("Tools folder path " + toolsFolderPath + " does not seem right.");
        System.exit(1);
      } else if (philosopherBinPath != null && (philosopherBinPath.isEmpty() || !Files.exists(Paths.get(philosopherBinPath)) || !Files.isReadable(Paths.get(philosopherBinPath)) || !Files.isRegularFile(Paths.get(philosopherBinPath)))) {
        System.err.println("Philosopher binary file path " + philosopherBinPath + " does not seem right.");
        System.exit(1);
      } else if (diannBinPath != null && (diannBinPath.isEmpty() || !Files.exists(Paths.get(diannBinPath)) || !Files.isReadable(Paths.get(diannBinPath)) || !Files.isRegularFile(Paths.get(diannBinPath)))) {
      System.err.println("DIA-NN executable file path " + diannBinPath + " does not seem right.");
      System.exit(1);
      } else if (pythonBinPath != null && (pythonBinPath.isEmpty() || !Files.exists(Paths.get(pythonBinPath)) || !Files.isReadable(Paths.get(pythonBinPath)))) {
        System.err.println("Python path " + pythonBinPath + " does not seem right.");
        System.exit(1);
      } else {
        workdir = Paths.get(workdir).toAbsolutePath().normalize().toString();
        if (toolsFolderPath != null) {
          toolsFolderPath = Paths.get(toolsFolderPath).toAbsolutePath().normalize().toString();
        }
        if (philosopherBinPath != null) {
          philosopherBinPath = Paths.get(philosopherBinPath).toAbsolutePath().normalize().toString();
        }
        if (diannBinPath != null) {
          diannBinPath = Paths.get(diannBinPath).toAbsolutePath().normalize().toString();
        }
        if (pythonBinPath != null) {
          pythonBinPath = Paths.get(pythonBinPath).toAbsolutePath().normalize().toString();
        }
        if (nThreadsHeadlessOnly != null && nThreadsHeadlessOnly == 0) {
          nThreadsHeadlessOnly = Math.max(1, Math.min(Runtime.getRuntime().availableProcessors(), maxProcessors));
        }
        headless(workflowFile);
      }
    }
  }

  public static void headless(final Path workflowFile) {
    try {
      initDone.await();
    } catch (InterruptedException ex) {
      throw new RuntimeException(ex);
    }

    final FragpipeLocations fpl = FragpipeLocations.get();

    PropsFile propsFile = fpl.tryLoadSilently(workflowFile, "user");

    // If there are parameters from command, they have the higher priority than those in the workflow file.
    if (Fragpipe.ram != null) {
      propsFile.setProperty("workflow.ram", Fragpipe.ram + "");
    } else if (propsFile.getProperty("workflow.ram") == null) {
      propsFile.setProperty("workflow.ram", "0");
    }
    if (Fragpipe.nThreadsHeadlessOnly != null) {
      propsFile.setProperty("workflow.threads", Fragpipe.nThreadsHeadlessOnly + "");
    } else if (propsFile.getProperty("workflow.threads") == null) {
      propsFile.setProperty("workflow.threads", Math.max(1, Math.min(Runtime.getRuntime().availableProcessors() - 1, maxProcessors)) + "");
    }
    propsFile.setProperty("workdir", Fragpipe.workdir);
    if (toolsFolderPath != null) {
      propsFile.setProperty(TabConfig.TAB_PREFIX + "tools-folder", toolsFolderPath);
    }
    if (diannBinPath != null) {
      propsFile.setProperty(TabConfig.TAB_PREFIX + "bin-diann", diannBinPath);
    }
    if (pythonBinPath != null) {
      propsFile.setProperty(TabConfig.TAB_PREFIX + "bin-python", pythonBinPath);
    }

    Bus.post(new MessageLoadUi(propsFile, true, true));
    Bus.post(new MessageManifestLoad());

    try {
      loadWorkflowDone.await();
      loadManifestDone.await();
      Thread.sleep(500);
    } catch (InterruptedException ex) {
      throw new RuntimeException(ex);
    }

    Bus.post(new MessageRun(dryRun));

    try {
      runDone.await();
      Thread.sleep(1000);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
  }

  static void displayMainWindow() {
    log.debug("Entered displayMainWindow");
    java.awt.EventQueue.invokeLater(() -> {
      log.debug("Creating Fragpipe instance");
      final Fragpipe fp0 = new Fragpipe();
      log.debug("Done creating Fragpipe instance");
      if (headless) {
        initDone.countDown();
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
          } else {
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
    final JTabbedPane tp = new JTabbedPane(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);

    TabConfig tabConfig = new TabConfig(console);
    TabWorkflow tabWorkflow = new TabWorkflow();
    TabDiaPseudoMs2 tabDiaPseudoMs2 = new TabDiaPseudoMs2();
    TabDatabase tabDatabase = new TabDatabase();
    TabMsfragger tabMsfragger = new TabMsfragger();
    TabValidation tabValidation = new TabValidation();
    TabQuantificationLfq tabQuantificationLfq = new TabQuantificationLfq();
    TabQuantificationLabeling tabQuantificationLabeling = new TabQuantificationLabeling();
    TabPtms tabPtms = new TabPtms();
    TabGlyco tabGlyco = new TabGlyco();
    TabSpecLib tabSpecLib = new TabSpecLib();
    TabDiann tabDiann = new TabDiann();
    TabSkyline tabSkyline = new TabSkyline();
    TabDownstream tabDownstream = new TabDownstream();
    TabRun tabRun = new TabRun(console, tabDownstream);

    uiTabConfig = new UiTab(TabConfig.TAB_NAME, tabConfig, "/org/nesvilab/fragpipe/icons/150-cogs.png", null, true);
    uiTabWorkflow = new UiTab(TAB_NAME_LCMS, tabWorkflow,
      "/org/nesvilab/fragpipe/icons/icon-workflow-16.png", null, false);
    uiTabDiaPseudoMS2 = new UiTab("DIA Pseudo MS2", tabDiaPseudoMs2,
        "/org/nesvilab/fragpipe/icons/dia-umpire-16x16.png", null, true);
    uiTabDb = new UiTab("Database", tabDatabase,
        "/org/nesvilab/fragpipe/icons/icon-dna-helix-16.png", null, true);
    uiTabFragger = new UiTab(TAB_NAME_MSFRAGGER, tabMsfragger,
        "/org/nesvilab/fragpipe/icons/bolt-outlined-16.png", null, true);
    uiTabValidation = new UiTab("Validation", tabValidation,
      "/org/nesvilab/fragpipe/icons/icon-filtration-16.png", null, true);
    uiTabPtms = new UiTab("PTMs", tabPtms, "/org/nesvilab/fragpipe/icons/icon-edit-16.png", null, true);
    uiTabGlyco = new UiTab("Glyco", tabGlyco, "/org/nesvilab/fragpipe/icons/glyco-16.png", null, true);
    uiTabQuantLfq = new UiTab("Quant (MS1)", tabQuantificationLfq,
      "/org/nesvilab/fragpipe/icons/icon-scales-balance-16.png", null, true);
    uiTabQuantLabeled = new UiTab("Quant (Isobaric)", tabQuantificationLabeling,
      "/org/nesvilab/fragpipe/icons/icon-scales-balance-color-2-16.png", null, true);
    uiTabSpecLib = new UiTab("Spec Lib", tabSpecLib,
      "/org/nesvilab/fragpipe/icons/icon-library-16.png", null, true);
    uiTabDiann = new UiTab("Quant (DIA)", tabDiann,
      "/org/nesvilab/fragpipe/icons/icon-diann-16.png", null, true);
    uiTabSkyline = new UiTab("Skyline", tabSkyline, "/org/nesvilab/fragpipe/icons/icon-skyline-16.png", null, true);
    uiTabRun = new UiTab("Run", tabRun, "/org/nesvilab/fragpipe/icons/video-play-16.png", null, false);
    uiTabDownstream = new UiTab("Downstream", tabDownstream, "/org/nesvilab/fragpipe/icons/icon-saint-20.png", null, false);

    addTab(tp, uiTabConfig);
    addTab(tp, uiTabWorkflow);
    addTab(tp, uiTabDiaPseudoMS2);
    addTab(tp, uiTabDb);
    addTab(tp, uiTabFragger);
    addTab(tp, uiTabValidation);
    addTab(tp, uiTabPtms);
    addTab(tp, uiTabGlyco);
    addTab(tp, uiTabQuantLfq);
    addTab(tp, uiTabQuantLabeled);
    addTab(tp, uiTabSpecLib);
    addTab(tp, uiTabDiann);
    addTab(tp, uiTabSkyline);
    addTab(tp, uiTabRun);
    addTab(tp, uiTabDownstream);

    log.debug("Done createTabs()");
    return tp;
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
      fp.setMinimumSize(new Dimension(700, 480));
      fp.setPreferredSize(new Dimension(1300, 1300));
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
      if (!headless) {
        SwingUtils.showDialog(this.toJFrame(), panel);
      }
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
    ProcessManager.get();
    DbSplit2.initClass();
    SpecLibGen2.initClass();
    FpopScript.initClass();

    if (!headless) {
      try {
        TabWorkflow tabWorkflow = Bus.getStickyEvent(TabWorkflow.class);
        if (tabWorkflow != null) {
          PropsFile propsFile = tabWorkflow.workflows.get("Default");
          if (propsFile != null) {
            propsFile.load();
            if (propsFile.containsKey("workflow.workflow-option")) {
              propsFile.setProperty("workflow.workflow-option", "Default");
            }
            Bus.post(new MessageLoadUi(propsFile, true, false));
          }
        }
      } catch (Exception e) {
        log.debug("Error loading default workflow", e);
      }
    }
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageSaveCache m) {
    saveCache();
  }

  @Subscribe(threadMode = ThreadMode.POSTING)
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
    log.debug("Got NoteFragpipeCache, updating UI");
    loadUi(m.propsUiState, true, !headless);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageLoadUi m) {
    loadUi(m.props, m.validateFasta, m.updateBins);
  }

  @SuppressWarnings("unchecked")
  private void loadUi(Properties props, boolean validateFasta, boolean updateBins) {
    log.debug("loadUi() called");
    final Map<String, String> propsWorkflowOnly = (Map) props.entrySet().stream().filter(e -> TabWorkflow.filterPropsForUi((String) e.getKey())).collect(Collectors.toMap(Entry::getKey, Entry::getValue));
    FragpipeCacheUtils.tabsLoad(propsWorkflowOnly, tabs);
    Bus.post(new MessageUiRevalidate(validateFasta, updateBins));
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
    String linkSite = p.getProperty(ThisAppProps.PROP_LAB_SITE_URL, "https://www.nesvilab.org/");

    return "FragPipe Proteomics Platform (v" + Version.version() + ")<br>"
        + "<a href=\"" + linkSite + "\">Alexey Nesvizhskii lab</a><br/>"
        + "University of Michigan<br><br>"
        + "<a href='https://fragpipe.nesvilab.org/'>FragPipe GUI</a>: Fengchao Yu, Dmitry Avtonomov, Daniel Polasky, Guo Ci Teo<br><br>"
        + "Components (software developers):<br>"
        + "<a href='https://diaumpire.nesvilab.org/'>DIA-Umpire</a>: Chih-Chiang Tsou, Guo Ci Teo<br>"
        + "<a href='https://diatracer.nesvilab.org/'>diaTracer</a>: Kai Li, Fengchao Yu<br>"
        + "<a href='https://msfragger.nesvilab.org/'>MSFragger</a>: Andy Kong, Fengchao Yu, Daniel Polasky, Guo Ci Teo, Dmitry Avtonomov<br>"
        + "<a href='https://www.nesvilab.org/Crystal-C/'>Crystal-C</a>: Hui-Yin Chang<br>"
        + "<a href='https://github.com/Nesvilab/MSBooster'>MSBooster</a>: Kevin Yang<br>"
        + "<a href='https://philosopher.nesvilab.org/'>Philosopher</a>: Felipe Leprevost, Yamei Deng<br>"
        + "Philosopher (TPP components): <a href='http://tools.proteomecenter.org/wiki/index.php?title=Software:TPP'>TPP developers with link to the TPP site</a><br>"
        + "<a href='http://percolator.ms/'>Percolator</a>: Lukas Käll<br>"
        + "<a href='http://www.tppms.org/tools/ptm/'>PTMProphet</a>: David Shteynberg<br>"
        + "<a href='https://ptmshepherd.nesvilab.org/'>PTM-Shepherd</a>: Daniel Geiszler, Daniel Polasky, Andy Kong<br>"
        + "<a href='https://github.com/lonelu/PTMLocalization'>O-Pair</a>: Lei Lu, Michael Shortreed, Daniel Polasky<br>"
        + "<a href='https://ionquant.nesvilab.org/'>IonQuant</a>: Fengchao Yu<br>"
        + "<a href='https://tmt-integrator.nesvilab.org/'>TMT-Integrator</a>: Hui-Yin Chang, Ruohong Li<br>"
        + "Spectral library generation: Guo Ci Teo<br>"
        + "<a href='https://github.com/grosenberger/easypqp'>EasyPQP</a>: George Rosenberger, Guo Ci Teo, Fengchao Yu<br>"
        + "<a href='https://github.com/vdemichev/DiaNN'>DIA-NN</a>: Vadim Demichev<br>"
        + "<a href='https://github.com/Nesvilab/FragPipe-PDV'>FragPipe-PDV</a>: Kai Li, Bo Wen<br>"
        + "<a href='https://skyline.ms/project/home/software/Skyline/begin.view'>Skyline</a>: Brendan MacLean, Matthew Chambers<br>"
        + "<a href='https://saint-apms.sourceforge.net/Main.html'>SAINT</a>: Hyungwon Choi, Guo Ci Teo<br>"
        + "Websites and tutorials: Sarah Haynes<br><br>"
        + "Special thanks to Lukas Käll (Percolator), David Shteynberg (TPP, PTMProphet), George Rosenberger (EasyPQP), Vadim Demichev (DIA-NN), Brendan MacLean (Skyline), and Matthew Chambers (Skyline).<br><br>"
        + "RawFileReader reading tool: Copyright (c) 2016 by Thermo Fisher Scientific, Inc. All rights reserved.<br>"
        + "Bruker SDK library: Included software components: Copyright (c) 2022 by Bruker Daltonics GmbH & Co. KG. All rights reserved.”<br>";
  }

  public void showAboutDialog(Component parent) {
    log.debug("Showing about dialog");
//    HtmlStyledJEditorPane ep = new HtmlStyledJEditorPane(true, createAboutBody());
    HtmlStyledJEditorPane ep = SwingUtils.createClickableHtml(createAboutBody());
    ep.setPreferredSize(new Dimension(500, 600));
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
      if (clazz.getName().contentEquals("org.nesvilab.fragpipe.messages.NoteConfigSpeclibgen")) {
        Bus.postSticky(new NoteConfigSpeclibgen(null, new ValidationException("Python binary or EasyPQP not valid")));
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

  static String help() {
    StringBuilder sb = new StringBuilder();
    sb.append("FragPipe v").append(version()).append("\n");
    sb.append("(c) University of Michigan").append("\n");
    sb.append(OsUtils.OsInfo()).append("\n");
    sb.append(OsUtils.JavaInfo()).append("\n");
    sb.append(OsUtils.NetCoreInfo()).append("\n");
    sb.append("Running without GUI. Usage:\n");
    sb.append("\tWindows: fragpipe.bat --headless --workflow <path to workflow file> --manifest <path to manifest file> --workdir <path to result directory>\n");
    sb.append("\tLinux: fragpipe --headless --workflow <path to workflow file> --manifest <path to manifest file> --workdir <path to result directory>\n");
    sb.append("Options:\n");
    sb.append("\t-h\n");
    sb.append("\t--help                          # Print this help message.\n");
    sb.append("\t--headless                      # Running in headless mode.\n");
    sb.append("\t--workflow <string>             # Specify path to workflow file.\n");
    sb.append("\t--manifest <string>             # Specify path to manifest file.\n");
    sb.append("\t--workdir <string>              # Specify the result directory.\n");
    sb.append("\t--dry-run                       # (optional) Dry run, not really run FragPipe.\n");
    sb.append("\t--ram <integer>                 # (optional) Specify the maximum allowed memory size. The unit is GB. Set it to 0 to let FragPipe decide. Default = 0\n");
    sb.append("\t--threads <integer>             # (optional) Specify the number of threads. Default = core number - 1\n");
    sb.append("\t--config-tools-folder <string>  # (optional) specify the folder containing MSFragger, IonQuant, and dirTracer. If not specified, using the one in the cache.\n");
    sb.append("\t--config-diann <string>         # (optional) specify the location of the DIA-NN binary file (the actual executable file `DiaNN.exe`, not the DIA-NN installation file). If not specified, using the one in the cache. It could be from the previously configured or the build-in one.\n");
    sb.append("\t--config-python <string>        # (optional) specify the location of the Python directory. If not specified, using the one in the cache.\n");
    sb.append("To let FragPipe find the TMT annotation file, put the mzML files from the same experiment in the same folder. Then, create the annotation file with the name ending with annotation.txt in the folder.");
    sb.append("Note: There must be only one annotation file in each folder.\n");
    return sb.toString();
  }
}
