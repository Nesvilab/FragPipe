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

import static umich.msfragger.params.fragger.FraggerMigPanel.PROP_FILECHOOSER_LAST_PATH;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.JTextComponent;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.SubscriberExceptionEvent;
import org.greenrobot.eventbus.ThreadMode;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.LoggerFactory;
import umich.msfragger.Version;
import umich.msfragger.cmd.CmdCrystalc;
import umich.msfragger.cmd.CmdIprophet;
import umich.msfragger.cmd.CmdMsAdjuster;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.cmd.CmdPeptideProphet;
import umich.msfragger.cmd.CmdPhilosopherWorkspaceCleanInit;
import umich.msfragger.cmd.CmdProteinProphet;
import umich.msfragger.cmd.CmdReportAbacus;
import umich.msfragger.cmd.CmdReportDbAnnotate;
import umich.msfragger.cmd.CmdReportFilter;
import umich.msfragger.cmd.CmdReportFreequant;
import umich.msfragger.cmd.CmdReportReport;
import umich.msfragger.cmd.CmdSpecLibGen;
import umich.msfragger.cmd.CmdUmpireSe;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.cmd.ProcessBuildersDescriptor;
import umich.msfragger.cmd.ToolingUtils;
import umich.msfragger.gui.ProcessDescription.Builder;
import umich.msfragger.gui.api.SearchTypeProp;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.TableModelColumn;
import umich.msfragger.gui.api.UniqueLcmsFilesTableModel;
import umich.msfragger.gui.api.VersionFetcher;
import umich.msfragger.gui.dialogs.ExperimentNameDialog;
import umich.msfragger.messages.MessageAppendToConsole;
import umich.msfragger.messages.MessageDecoyTag;
import umich.msfragger.messages.MessageExternalProcessOutput;
import umich.msfragger.messages.MessageIsUmpireRun;
import umich.msfragger.messages.MessageKillAll;
import umich.msfragger.messages.MessageLcmsFilesAdded;
import umich.msfragger.messages.MessageRun;
import umich.msfragger.messages.MessageSaveCache;
import umich.msfragger.messages.MessageSearchType;
import umich.msfragger.messages.MessageShowAboutDialog;
import umich.msfragger.messages.MessageStartProcesses;
import umich.msfragger.messages.MessageTipNotification;
import umich.msfragger.messages.MessageToolInit;
import umich.msfragger.messages.MessageValidityFragger;
import umich.msfragger.messages.MessageValidityMsadjuster;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.dbslice.DbSlice;
import umich.msfragger.params.dbslice.DbSlice.MessageInitDone;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.params.fragger.FraggerMigPanel;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.params.fragger.MsfraggerProps;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherGithub;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherLocal;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherServer;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.speclib.SpecLibGen;
import umich.msfragger.params.umpire.UmpirePanel;
import umich.msfragger.util.FileDrop;
import umich.msfragger.util.FileListing;
import umich.msfragger.util.GhostText;
import umich.msfragger.util.HSLColor;
import umich.msfragger.util.IValidateString;
import umich.msfragger.util.LogUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.PrefixCounter;
import umich.msfragger.util.Proc2;
import umich.msfragger.util.PythonInfo;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.Tuple2;
import umich.msfragger.util.UsageTrigger;
import umich.msfragger.util.ValidateTrue;
import umich.msfragger.util.VersionComparator;
import umich.msfragger.util.swing.ISimpleTextComponent;
import umich.swing.console.TextConsole;

/**
 * @author dattam
 */
public class MsfraggerGuiFrame extends javax.swing.JFrame {

  private static final org.slf4j.Logger log = LoggerFactory.getLogger(MsfraggerGuiFrame.class);
  private final Object procRunLock = new Object();

  private FraggerMigPanel fraggerMigPanel;
  private TextConsole console;
  private ExecutorService exec = Executors.newFixedThreadPool(1);;


  //private static final String TEXT_SAME_SEQ_DB = "<Same as in MSFragger>";
  private Color defTextColor;
  private GhostText ghostTextPepProph;
  private GhostText ghostTextProtProp;
  private BalloonTip balloonMsfragger;
  private BalloonTip balloonPhilosopher;

  private HashMap<String, BalloonTip> tipMap = new HashMap<>();
  private static final String TIP_NAME_FRAGGER_JAVA_VER = "msfragger.java.min.ver";

  SimpleETable tableRawFiles;
  UniqueLcmsFilesTableModel tableModelRawFiles;
  FileDrop tableRawFilesFileDrop;

  public static final SearchTypeProp DEFAULT_TYPE = SearchTypeProp.closed;

  private String textPepProphetFocusGained = null;
  private String textReportAnnotateFocusGained = null;
  private String textReportFilterFocusGained = null;
  private String textReportAbacusFocusGained = null;
  private String textDecoyTagFocusGained = null;
  private String textLabelfreeFocusGained = null;

  private Pattern reDecoyTagReportAnnotate = Pattern.compile("--prefix\\s+([^\\s]+)");
  private Pattern reDecoyTagReportFilter = Pattern.compile("--tag\\s+([^\\s]+)");
  private Pattern reDecoyTagReportAbacus = Pattern.compile("--tag\\s+([^\\s]+)");
  private Pattern reDecoyTagPepProphCmd = Pattern.compile("--decoy\\s+([^\\s]+)");
  private Pattern reDecoyTagSequenceDb = Pattern.compile("([^\\s]+)");

  private static final String UNKNOWN_VERSION = "Unknown";
  private String fraggerVer = UNKNOWN_VERSION;
  private String philosopherVer = UNKNOWN_VERSION;

  private UmpirePanel umpirePanel = null;
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

        if (console == null) {
          return;
        }
        String text = console.getText();

        JFileChooser fc = new JFileChooser();
        fc.setApproveButtonText("Save");
        fc.setDialogTitle("Export to");
        fc.setMultiSelectionEnabled(false);
        SwingUtils.setFileChooserPath(fc, ThisAppProps.load(PROP_FILECHOOSER_LAST_PATH));
        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
        Date now = new Date();
        fc.setSelectedFile(new File(String.format("log_%s.txt", df.format(now))));
        Component parent = SwingUtils.findParentFrameForDialog(MsfraggerGuiFrame.this);
        int saveResult = fc.showSaveDialog(parent);
        if (JFileChooser.APPROVE_OPTION == saveResult) {
          File selectedFile = fc.getSelectedFile();
          Path path = Paths.get(selectedFile.getAbsolutePath());
          // if exists, overwrite
          if (Files.exists(path)) {
            int overwrite = JOptionPane
                .showConfirmDialog(parent, "<html>File exists,<br/> overwrtie?", "Overwrite",
                    JOptionPane.OK_CANCEL_OPTION);
            if (JOptionPane.OK_OPTION == overwrite) {
              try {
                Files.delete(path);
              } catch (IOException ex) {
                JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite",
                    JOptionPane.ERROR_MESSAGE);
                return;
              }
            }
          }
          try {
            // save the file
            byte[] bytes = text.getBytes(StandardCharsets.UTF_8);
            Files.write(path, bytes, StandardOpenOption.CREATE_NEW);

          } catch (IOException ex) {
            JOptionPane
                .showMessageDialog(parent, "<html>Could not save file: <br/>" + path.toString()
                    + "<br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            return;
          }
        }
      }
    };

    panelRun.getActionMap().put(exportToTextFile.getValue(Action.NAME), exportToTextFile);
  }

  private void initMore() {

    setTitle(Version.PROGRAM_TITLE + " (v" + Version.version() + ")");
    setLocale(Locale.ROOT);

    console = new TextConsole();
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
    if (!validateMsfraggerJarContents(textBinMsfragger.getText())) {
      log.debug("Msfragger jar is not valid");
    }

    if (validatePhilosopherPath(textBinPhilosopher.getText()) == null) {
      enablePhilosopherPanels(false);
    }

    tableModelRawFiles = createTableModelRawFiles();
    tableRawFiles = new SimpleETable(tableModelRawFiles);
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
      ArrayList<Path> paths = new ArrayList<>(files.length);
      for (File f : files) {
        boolean isDirectory = f.isDirectory();
        if (!isDirectory) {
          if (FraggerMigPanel.fileNameExtensionFilter.accept(f)) {
            paths.add(Paths.get(f.getAbsolutePath()));
          }
        } else {
          PathUtils
              .traverseDirectoriesAcceptingFiles(f, FraggerMigPanel.fileNameExtensionFilter, paths);
        }
      }
      EventBus.getDefault().post(new MessageLcmsFilesAdded(paths));
    });

    textBinPython.addFocusListener(new FocusAdapter() {
      @Override
      public void focusLost(FocusEvent e) {
        final String text = textBinPython.getText();
        validateAndSavePython(text, true);
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
    setTabIcon(mapTabNameToIdx, "Sequence DB", "/umich/msfragger/gui/icons/093-drawer.png");
    setTabIcon(mapTabNameToIdx, "Downstream", "/umich/msfragger/gui/icons/328-move-down.png");
    setTabIcon(mapTabNameToIdx, "Report", "/umich/msfragger/gui/icons/185-clipboard.png");
    setTabIcon(mapTabNameToIdx, fraggerTabName, "/umich/msfragger/gui/icons/bolt-16.png");
    //setTabIcon(mapTabNameToIdx, "", "");

    exec.submit(() -> validateAndSaveMsfraggerPath(textBinMsfragger.getText()));
    exec.submit(() -> validateAndSavePhilosopherPath(textBinPhilosopher.getText()));
    exec.submit(() -> Version.checkUpdates());
    exec.submit(this::checkPreviouslySavedParams);
    exec.submit(this::checkPython);
    exec.submit(this::validateMsadjusterEligibility);
    exec.submit(this::validateDbslicing);
    exec.submit(this::validateSpeclibgen);

    // submitting all "loadLast" methods for invocation
    for (Method method : this.getClass().getDeclaredMethods()) {
      if (method.getName().startsWith("loadLast") && method.getParameterCount() == 0) {
        exec.submit(() -> method.invoke(this));
      }
    }

    initActions();
  }

  private void checkPython() {
    String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_PYTHON);
    PythonInfo pi = PythonInfo.get();
    if (path != null) {
      try {
        if (!pi.setPythonCommand(path)) {
          throw new Exception("Could not set python command to the old value");
        }
      } catch (Exception e) {
        ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PYTHON, "");
        int yesNo = JOptionPane.showConfirmDialog(this,
            "Previously stored Python location is now invalid:\n"
                + "\t" + path + "\n\nDo you want to try automatically find the Python binary?",
            "Previously used Python not available", JOptionPane.YES_NO_OPTION,
            JOptionPane.WARNING_MESSAGE);
        if (JOptionPane.YES_OPTION == yesNo) {
          try {
            pi.findPythonCommand();
            if (!pi.isAvailable()) {
              throw new Exception("Python command not found");
            }
          } catch (Exception e1) {
            JOptionPane.showMessageDialog(this,
                "Python not found.\n\n"
                    + "You can manually select Python binary\n"
                    + "if you know where it is located.",
                "Error", JOptionPane.WARNING_MESSAGE);
          }
        }
      }
      return;
    }
    // No python location was stored. It's only stored when a user manually changes the location.
    // try to auto-detect Python binary
    try {
      PythonInfo.get().findPythonCommand();
    } catch (Exception ignored) {
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
  public void onMessageIsUmpireRun(MessageIsUmpireRun m) {
    if (checkLabelfree.isSelected() && isRunUmpireSe()) {
      checkLabelfree.setSelected(false);
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onMessagePythonInfoChanged(PythonInfo.MessageInfoChanged m) {
    PythonInfo pi = PythonInfo.get();
    if (pi.isAvailable()) {
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

  @Subscribe
  public void onSubscriberException(SubscriberExceptionEvent msg) {
    SwingUtils.showErrorDialog(msg.throwable, this);
  }

  @Subscribe
  public void loadDefaults(MessageSearchType m) {
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

  private void messageToTextComponent(ISimpleTextComponent comp, MessageToolInit m) {
    final String old = comp.getText().trim();
    Pattern reHtml = Pattern.compile("<\\s*/?\\s*html\\s*>", Pattern.CASE_INSENSITIVE);
    String noHtml = reHtml.matcher(old).replaceAll("");
    Document doc = Jsoup.parse(old);
    doc.body().attr("style", SwingUtils.getHtmlBodyStyle());
    if (!m.append) {
      doc.body().html("");
    }
    if (m.isError) {
      doc.body().appendChild(new Element("b").html(m.text));
    } else {
      doc.body().append(m.text);
    }
    String html = doc.html();
    comp.setText(html);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onDbsliceMessage1(DbSlice.Message1 m) {
    messageToTextComponent(ISimpleTextComponent.from(lblDbsliceInfo1), m);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onDbsliceMessage2(DbSlice.Message2 m) {
    messageToTextComponent(ISimpleTextComponent.from(epDbsliceInfo), m);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN)
  public void onDbsliceInitDone(MessageInitDone m) {
    final String text = m.isSuccess ? "Database Splitting enabled." : "Database Splitting disabled.";
    messageToTextComponent(ISimpleTextComponent.from(epDbsliceInfo), new DbSlice.Message2(true, !m.isSuccess, text));

    if (!m.isSuccess) {
      // attach link with instructions
      Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
      String link = p.getProperty(MsfraggerProps.PROP_DBSPLIT_INSTRUCTIONS_URL,
          "https://nesvilab.github.io/MSFragger/");
      String instructions = "<br/>For docs/instructions visit <a href=\"" + link + "\">" + link + "</a>";
      messageToTextComponent(ISimpleTextComponent.from(epDbsliceInfo),
          new DbSlice.Message2(true, false, instructions));
    }
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onSpeclibgenMessage1(SpecLibGen.Message1 m) {
    messageToTextComponent(ISimpleTextComponent.from(lblSpeclibInfo1), m);
  }

  @Subscribe(threadMode = ThreadMode.MAIN)
  public void onSpeclibgenMessage2(SpecLibGen.Message2 m) {
    messageToTextComponent(ISimpleTextComponent.from(epSpeclibInfo2), m);
  }

  @Subscribe(sticky = true, threadMode = ThreadMode.MAIN)
  public void onSpeclibgenInitDone(SpecLibGen.InitDone m) {
    final String text = m.isSuccess ? "Spectral Library Generation enabled. See Report tab."
        : "Spectral Library Generation disabled.";
    messageToTextComponent(ISimpleTextComponent.from(epSpeclibInfo2), new SpecLibGen.Message2(true, !m.isSuccess, text));

    if (!m.isSuccess) {
      // attach link with instructions
      Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
      String link = p.getProperty(MsfraggerProps.PROP_SPECLIBGEN_INSTRUCTIONS_URL,
          "https://nesvilab.github.io/MSFragger/");
      String instructions = "<br/>For docs/instructions visit <a href=\"" + link + "\">" + link + "</a>";
      messageToTextComponent(ISimpleTextComponent.from(epSpeclibInfo2),
          new SpecLibGen.Message2(true, false, instructions));
    }
    enableSpecLibGenPanel(m.isSuccess);
  }

  @Subscribe
  public void onLcmsFilesAdded(MessageLcmsFilesAdded m) {
    // save locations
    ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, m.paths.get(m.paths.size()-1).toString());

    // vet the files
    final HashMap<Path, String> reasons = new HashMap<>();
    m.paths.stream()
        .filter(p -> !com.github.chhh.utils.StringUtils.isPureAscii(p.toString()))
        .forEach(p -> { reasons.merge(p, "Non-ASCII chars", (s1, s2) -> String.join(", ", s1, s2)); });
    m.paths.stream()
        .filter(p -> p.toString().contains(" "))
        .forEach(p -> { reasons.merge(p, "Contains spaces", (s1, s2) -> String.join(", ", s1, s2)); });
    m.paths.stream()
        .filter(p -> !FraggerMigPanel.fileNameExtensionFilter.accept(p.toFile()))
        .forEach(p -> { reasons.merge(p, "Not supported", (s1, s2) -> String.join(", ", s1, s2)); });

    Stream<Path> toAdd = m.paths.stream();
    if (!reasons.isEmpty()) {
      String[] columns = {"Reason", "Path"};
      String[][] data = new String[reasons.size()][2];
      int index = -1;
      for (Entry<Path, String> kv : reasons.entrySet()) {
        data[++index][0] = kv.getValue();
        data[index][1] = kv.getKey().toString();
      }

      DefaultTableModel model = new DefaultTableModel(data, columns);
      JTable table = new JTable(model);
      table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
      JPanel panel = new JPanel(new BorderLayout());
      panel.add(new JLabel("<html>Found problems with some files (" + Integer.toString(reasons.size()) + ").<br/>"
          + "This <b>will likely cause trouble</b> with some of processing tools.<br/><br/>"
          + "Do you want to add these files?<br/>"), BorderLayout.NORTH);
      panel.add(Box.createVerticalStrut(100), BorderLayout.CENTER);
      panel.add(new  JScrollPane(table), BorderLayout.CENTER);

      String[] options = {"Cancel", "Add anyway", "Only add well-behaved paths"};
      int confirmation = JOptionPane
          .showOptionDialog(this, panel, "Add these files?",
              JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);
      switch (confirmation) {
        case 0:
          return;
        case 1:
          break;
        case 2:
          toAdd = toAdd.filter(path -> !reasons.containsKey(path));
          break;
      }
    }

    // add the files
    tableModelRawFiles.dataAddAll(
        toAdd.map(path -> new InputLcmsFile(path, ThisAppProps.DEFAULT_LCMS_GROUP_NAME))
        .collect(Collectors.toList()));
  }

  @Subscribe
  public void onTipNotification(MessageTipNotification m) {
    SwingUtilities.invokeLater(() -> {
      tipMap.computeIfPresent(m.key, (key, tip) -> {
        tip.closeBalloon();
        return null;
      });
      BalloonTip tip = new BalloonTip(btnAboutInConfig, SwingUtils.createClickableHtml(m.text),
          new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
      tip.setVisible(true);
      tipMap.put(m.key, tip);
    });
  }

  @Subscribe
  public void onShowAbout(MessageShowAboutDialog m) {
    // for copying style
    JLabel label = new JLabel();
    Font font = label.getFont();

    // create some css from the label's font
    StringBuilder style = new StringBuilder("font-family:" + font.getFamily() + ";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");


    final Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
    String linkDl = p.getProperty(Version.PROP_DOWNLOAD_URL, "");
    String linkSite = p.getProperty(ThisAppProps.PROP_LAB_SITE_URL, "http://nesvilab.org");
    String linkToPaper = p.getProperty(ThisAppProps.PROP_MANUSCRIPT_URL, "http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html");

    JEditorPane ep = new JEditorPane("text/html", "<html><body style=\"" + style + "\">"
        + "MSFragger - Ultrafast Proteomics Search Engine<br/>"
        + "FragPipe (v" + Version.version() + ")<br/>"
        + "Dmitry Avtonomov<br/>"
        + "University of Michigan, 2017<br/><br/>"
        + "<a href=\"" + linkDl
        + "\">Click here to download</a> the latest version<br/><br/>"
        + "<a href=\"" + linkSite + "\">Alexey Nesvizhskii lab</a><br/>&nbsp;<br/>&nbsp;"
        + "MSFragger authors and contributors:<br/>"
        + "<ul>"
        + "<li>Andy Kong</li>"
        + "<li>Dmitry Avtonomov</li>"
        + "<li>Guo-Ci Teo</li>"
        + "<li>Alexey Nesvizhskii</li>"
        + "</ul>"
        + "<a href=\"" + linkToPaper + "\">Link to the research manuscript</a><br/>"
        + "Reference: <b>doi:10.1038/nmeth.4256</b>"
        + "</body></html>");

    // handle link messages
    ep.addHyperlinkListener(new HyperlinkListener() {
      @Override
      public void hyperlinkUpdate(HyperlinkEvent e) {
        if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
          try {
            Desktop.getDesktop().browse(e.getURL().toURI());
          } catch (URISyntaxException | IOException ex) {
            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
          }
        }
      }
    });
    ep.setEditable(false);
    ep.setBackground(label.getBackground());

    // show
    JOptionPane.showMessageDialog(this, ep, "About", JOptionPane.INFORMATION_MESSAGE);
  }
  //endregion

  public boolean isRunUmpireSe() {
    return checkEnableDiaumpire.isSelected() && umpirePanel != null && umpirePanel.checkRunUmpireSe
        .isSelected();
  }

  private void checkPreviouslySavedParams() {
    ThisAppProps cached = ThisAppProps.loadFromTemp();
    if (cached != null) {
      // if there was a cached version of properties
      VersionComparator vc = new VersionComparator();
      String storedVer = cached.getProperty(Version.PROP_VER, "0.0");
      if (vc.compare(storedVer, "4.0") < 0) {
        // and the version was less than 4.0
        String msg = String.format(Locale.ROOT, "Looks like you've upgraded from an "
            + "older version to 4.0+,\n"
            + "it is HIGHLY recommended to reset the default parameters.\n\n"
            + "Reset the parameters now? \n\n"
            + "This message won't be displayed again.");
        String[] options = {"Cancel", "Load defaults for Closed", "Load defaults of Open"};
        int result = JOptionPane.showOptionDialog(this, msg, "Reset to defautls",
            JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
        switch (result) {
          case 1:
            EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.closed));
            break;
          case 2:
            EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.open));
            break;
        }

        // rewrite the cached params file with a versioned one
        ThisAppProps.save(Version.PROP_VER, Version.version());
      } else if (vc.compare(storedVer, "4.0") >= 0 && vc.compare(storedVer, "5.1") <= 0) {
        // and the version between 4.0 and 5.1
        final String prop = ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET;
        String oldPepProphStr = ThisAppProps.load(prop);
        Pattern re = Pattern.compile("--clevel\\s+2");
        Matcher m = re.matcher(oldPepProphStr);
        if (m.find()) {
          String replaced = oldPepProphStr.replaceAll(re.pattern(), "--clevel -2");
          ThisAppProps.save(prop, replaced);
          ThisAppProps.load(prop, textPepProphCmd);

          String msg = String.format(Locale.ROOT,
              "<html>We've noticed a cached leftover buggy option for PeptideProphet "
                  + "'--clevel 2' and automatically replaced \n"
                  + "it with '--clevel -2'.\n\n"
                  + "If you know what you're doing and intended it to be '--clevel 2' "
                  + "please change it back on PeptideProphet tab.\n\n"
                  + "You also have the option to reload defaults. "
                  + "This message won't be displayed again.");
          String[] options = {"Ok", "Load defaults for Closed Search",
              "Load defaults of Open Search"};
          int result = JOptionPane
              .showOptionDialog(this, msg, "Cached option automatically replaced",
                  JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options,
                  options[0]);
          switch (result) {
            case 1:
              EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.closed));
              break;
            case 2:
              EventBus.getDefault().post(new MessageSearchType(SearchTypeProp.open));
              break;
          }

        }

        // rewrite the cached params file with a versioned one
        ThisAppProps.save(Version.PROP_VER, Version.version());
      }
    }
  }

  private void enablePhilosopherPanels(boolean enabled) {
    SwingUtils.enableComponents(panelPeptideProphet, enabled);
    chkRunPeptideProphet.setSelected(enabled);
    SwingUtils.enableComponents(panelProteinProphet, enabled);
    chkRunProteinProphet.setSelected(enabled);
    SwingUtils.enableComponents(panelReport, enabled);
    checkCreateReport.setSelected(enabled);
  }

  private void enableSpecLibGenPanel(boolean enabled) {
    SwingUtils.enableComponents(panelSpecLibOpts, enabled, true);
    if (!enabled) {
      checkGenerateSpecLib.setSelected(false);
    } else {
      ThisAppProps.load(checkGenerateSpecLib, ThisAppProps.PROP_SPECLIBGEN_RUN);
    }
  }

  public UniqueLcmsFilesTableModel createTableModelRawFiles() {
    if (tableModelRawFiles != null) {
      return tableModelRawFiles;
    }
    List<TableModelColumn<InputLcmsFile, ?>> cols = new ArrayList<>();

    TableModelColumn<InputLcmsFile, String> colPath = new TableModelColumn<>(
        "Path (can drag & drop from Explorer)",
        String.class, false, data -> data.path.toString());
    TableModelColumn<InputLcmsFile, String> colExp = new TableModelColumn<>(
        "Experiment/Group (editable)", String.class, true, data -> data.experiment);
    cols.add(colPath);
    cols.add(colExp);

    tableModelRawFiles = new UniqueLcmsFilesTableModel(cols, 0);
    return tableModelRawFiles;
  }

  private String getDefaultPhilosopherBinName() {
    java.util.ResourceBundle bundle = ThisAppProps.getLocalBundle();
    String winName = bundle.getString("default.philosopher.win"); // NOI18N
    String nixName = bundle.getString("default.philosopher.nix"); // NOI18N
    if (OsUtils.isWindows()) {
      return winName;
    }
    return nixName;
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
    jScrollPane8 = new javax.swing.JScrollPane();
    panelConfig = new javax.swing.JPanel();
    jLabel4 = new javax.swing.JLabel();
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
    panelCrystalc = new javax.swing.JPanel();
    chkRunCrystalc = new javax.swing.JCheckBox();
    jLabel12 = new javax.swing.JLabel();
    btnCrystalcDefaults = new javax.swing.JButton();
    panelCrystalcOptions = new javax.swing.JPanel();
    jLabel6 = new javax.swing.JLabel();
    spinnerCrystalcMaxCharge = new javax.swing.JSpinner();
    jLabel7 = new javax.swing.JLabel();
    spinnerCrystalcNumIsotopes = new javax.swing.JSpinner();
    jLabel8 = new javax.swing.JLabel();
    spinnerCrystalcMassTol = new javax.swing.JSpinner();
    jLabel9 = new javax.swing.JLabel();
    spinnerCrystalcPrecIsoWindow = new javax.swing.JSpinner();
    panelReport = new javax.swing.JPanel();
    panelReportOptions = new javax.swing.JPanel();
    textReportFilter = new javax.swing.JTextField();
    checkReportAbacus = new javax.swing.JCheckBox();
    jLabel1 = new javax.swing.JLabel();
    checkCreateReport = new javax.swing.JCheckBox();
    panelSpecLibOpts = new javax.swing.JPanel();
    checkGenerateSpecLib = new javax.swing.JCheckBox();
    jPanel4 = new javax.swing.JPanel();
    checkLabelfree = new javax.swing.JCheckBox();
    textReportLabelfree = new javax.swing.JTextField();
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

    jScrollPane8.setBorder(null);

    jLabel4.setFont(lblFraggerJavaVer.getFont());
    jLabel4.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
    jLabel4.setText("<html>Tabs on top represent processing steps and will be performed sequentially.<br/>\nTabs will become enabled once the tools on this panel are configured."); // NOI18N

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
    editorMsfraggerCitation.setText(getFraggerCitationHtml());
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
    editorPhilosopherLink.setText(createPhilosopherCitationHtml());
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
          .addComponent(jLabel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
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
    jPanel1.setToolTipText("<html>Requires <b>Python 3</b> with packages <b>Cython, Msproteomicstools</b>.<br/>\nWays to get everything set up:<br>\n<ul>\n<li>Install Python 3 if you don't yet have it.</li>\n<li>Install required python modules using <i>pip</i>, the python package manager, with commands:</li>\n<ul>\n<li>pip install numpy pandas cython</li>\n<li>pip install msproteomicstools</li>\n</ul>\n</ul>\n");

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
              .addComponent(jLabel4, javax.swing.GroupLayout.Alignment.LEADING)
              .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
              .addComponent(jPanel2, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
              .addComponent(jPanel3, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
              .addComponent(panelPhilosopherConfig, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
              .addComponent(panelMsfraggerConfig, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
              .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelConfigLayout.createSequentialGroup()
                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                  .addGroup(panelConfigLayout.createSequentialGroup()
                    .addComponent(btnFindTools)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(lblFindAutomatically))
                  .addGroup(panelConfigLayout.createSequentialGroup()
                    .addGap(67, 67, 67)
                    .addComponent(btnClearCache)
                    .addGap(18, 18, 18)
                    .addComponent(checkEnableDiaumpire)))
                .addGap(0, 188, Short.MAX_VALUE)))
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
        .addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    jPanel2.getAccessibleContext().setAccessibleName("DB Splitting");

    jScrollPane8.setViewportView(panelConfig);

    tabPane.addTab("Config", jScrollPane8);

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
    btnGroupsConsecutive.setToolTipText("<html>Assign each run to its own experiment.<br/>\n<b>Names like \"experiment-01\"</b> will be assgined.");
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

    btnGroupsAssignToSelected.setText("Assign to selected");
    btnGroupsAssignToSelected.setToolTipText("<html>Brings up a dialog to assign selected runs to an<br/>\nexperiment name of your choice.");
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
            .addGap(0, 57, Short.MAX_VALUE)))
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

    panelDbInfo.setBorder(javax.swing.BorderFactory.createTitledBorder("General DB Info (FASTA)"));

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
    initEditorPaneSeqDb();

    javax.swing.GroupLayout panelDbInfoLayout = new javax.swing.GroupLayout(panelDbInfo);
    panelDbInfo.setLayout(panelDbInfoLayout);
    panelDbInfoLayout.setHorizontalGroup(
      panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelDbInfoLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 602, Short.MAX_VALUE)
          .addGroup(panelDbInfoLayout.createSequentialGroup()
            .addGroup(panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
              .addGroup(panelDbInfoLayout.createSequentialGroup()
                .addComponent(jLabel5)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(textDecoyTagSeqDb, javax.swing.GroupLayout.PREFERRED_SIZE, 131, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(btnTryDetectDecoyTag)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(lblFastaCount))
              .addComponent(textSequenceDbPath))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(btnBrowse)))
        .addContainerGap())
    );
    panelDbInfoLayout.setVerticalGroup(
      panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelDbInfoLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(panelDbInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(textSequenceDbPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(btnBrowse))
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

    tabPane.addTab("Sequence DB", panelSequenceDb);

    chkRunPeptideProphet.setSelected(true);
    chkRunPeptideProphet.setText("Run PeptideProphet");
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

    panelPeptideProphetOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Peptide Prophet Options"));

    textPepProphCmd.setColumns(20);
    textPepProphCmd.setLineWrap(true);
    textPepProphCmd.setRows(5);
    textPepProphCmd.setWrapStyleWord(true);
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
            .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 515, Short.MAX_VALUE))
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
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
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

    panelProteinProphetOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Protein Prophet Options"));

    checkProcessGroupsSeparately.setSelected(loadLastProcessGroupsSeparately());
    checkProcessGroupsSeparately.setText("<html>A <b>separate</b> prot-xml file per group/experiment");
    checkProcessGroupsSeparately.setToolTipText("<html><b>Uncheck</b> if you want a report comparing protein abundances across<br/>\nexperiments or just want a single protein identification result from all<br/>\nthe runs.<br/>\n<b>Only check</b> if you want peptide/protein ID results<br/>\nfor each experiment separately. E.g. this might be useful if you have<br/>\n100 files on hand and use the \"assign to experiments\" feature to quickly<br/>\nrun MSFragger + downstream processing on each of those and get a pepxml<br/>\nand/or protxml files.");
    checkProcessGroupsSeparately.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        checkProcessGroupsSeparatelyActionPerformed(evt);
      }
    });

    txtProteinProphetCmdLineOpts.setColumns(20);
    txtProteinProphetCmdLineOpts.setLineWrap(true);
    txtProteinProphetCmdLineOpts.setRows(5);
    txtProteinProphetCmdLineOpts.setWrapStyleWord(true);
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
            .addGap(0, 0, Short.MAX_VALUE))
          .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
            .addComponent(jLabel40)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 515, Short.MAX_VALUE)))
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
        .addContainerGap())
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
        .addComponent(panelProteinProphetOptions, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap())
    );

    chkRunCrystalc.setText("Run Crystal-C");
    chkRunCrystalc.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        chkRunCrystalcActionPerformed(evt);
      }
    });

    jLabel12.setText("<html>Crystal-C performs additional search results cleanup<br/>\n<b>Recommended for Open Searches only</b>");

    btnCrystalcDefaults.setText("Load Defaults");
    btnCrystalcDefaults.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        btnCrystalcDefaultsActionPerformed(evt);
      }
    });

    panelCrystalcOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Crystal-C Options"));

    jLabel6.setText("Max charge");

    spinnerCrystalcMaxCharge.setModel(new javax.swing.SpinnerNumberModel(6, 1, 50, 1));

    jLabel7.setText("Number of isotopes");

    spinnerCrystalcNumIsotopes.setModel(new javax.swing.SpinnerNumberModel(3, 1, 50, 1));

    jLabel8.setText("Mass tolerance (ppm)");

    spinnerCrystalcMassTol.setModel(new javax.swing.SpinnerNumberModel(20.0d, 0.1d, 10000.0d, 0.5d));

    jLabel9.setText("Precursor isolation window");

    spinnerCrystalcPrecIsoWindow.setModel(new javax.swing.SpinnerNumberModel(0.7d, 0.1d, 10.0d, 0.1d));

    javax.swing.GroupLayout panelCrystalcOptionsLayout = new javax.swing.GroupLayout(panelCrystalcOptions);
    panelCrystalcOptions.setLayout(panelCrystalcOptionsLayout);
    panelCrystalcOptionsLayout.setHorizontalGroup(
      panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelCrystalcOptionsLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(jLabel8)
          .addComponent(jLabel6))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
          .addComponent(spinnerCrystalcMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, 60, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(spinnerCrystalcMaxCharge))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
        .addGroup(panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
          .addComponent(jLabel9)
          .addComponent(jLabel7))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
          .addComponent(spinnerCrystalcPrecIsoWindow, javax.swing.GroupLayout.DEFAULT_SIZE, 60, Short.MAX_VALUE)
          .addComponent(spinnerCrystalcNumIsotopes))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    panelCrystalcOptionsLayout.setVerticalGroup(
      panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelCrystalcOptionsLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(jLabel6)
          .addComponent(spinnerCrystalcMaxCharge, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(jLabel7)
          .addComponent(spinnerCrystalcNumIsotopes, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addGap(18, 18, 18)
        .addGroup(panelCrystalcOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(jLabel8)
          .addComponent(spinnerCrystalcMassTol, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(jLabel9)
          .addComponent(spinnerCrystalcPrecIsoWindow, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout panelCrystalcLayout = new javax.swing.GroupLayout(panelCrystalc);
    panelCrystalc.setLayout(panelCrystalcLayout);
    panelCrystalcLayout.setHorizontalGroup(
      panelCrystalcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelCrystalcLayout.createSequentialGroup()
        .addComponent(chkRunCrystalc)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(btnCrystalcDefaults)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
        .addComponent(jLabel12, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addGap(0, 0, Short.MAX_VALUE))
      .addComponent(panelCrystalcOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    panelCrystalcLayout.setVerticalGroup(
      panelCrystalcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelCrystalcLayout.createSequentialGroup()
        .addGroup(panelCrystalcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(panelCrystalcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
            .addComponent(chkRunCrystalc)
            .addComponent(btnCrystalcDefaults))
          .addComponent(jLabel12, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(panelCrystalcOptions, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
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
          .addComponent(panelCrystalc, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        .addContainerGap())
    );
    panelDownstreamLayout.setVerticalGroup(
      panelDownstreamLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelDownstreamLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(panelPeptideProphet, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
        .addComponent(panelProteinProphet, javax.swing.GroupLayout.PREFERRED_SIZE, 145, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
        .addComponent(panelCrystalc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(246, Short.MAX_VALUE))
    );

    loadLastCrystalc();

    tabPane.addTab("Downstream", panelDownstream);
    loadLastCrystalc();

    panelReportOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Report Options"));

    textReportFilter.setToolTipText("<html>--pepxml path-to-pepxml --protxml path-to-combined-protxml<br/>\nwill be added automatically based on previous tabs.<br/>\n\nStatistical filtering, validation and False Discovery Rates assessment<br/>\nphilosopher filter [flags]<br>\nFlags:<br/>\n<ul>\n<li>--ion float        peptide ion FDR level (default 0.01)</li>\n<li>--mapmods          map modifications aquired by an open search</li>\n<li>--models           print model distribution</li>\n<li>--pep float        peptide FDR level (default 0.01)</li>\n<li>--pepProb float    top peptide probability treshold for the FDR filtering (default 0.7)</li>\n<li>--pepxml string    pepXML file or directory containing a set of pepXML files</li>\n<li>--picked           apply the picked FDR algorithm before the protein scoring</li>\n<li>--prot float       protein FDR level (default 0.01)</li>\n<li>--protProb float   protein probability treshold for the FDR filtering (not used with the razor algorithm) (default 0.5)</li>\n<li>--protxml string   protXML file path</li>\n<li>--psm float        psm FDR level (default 0.01)</li>\n<li>--razor            use razor peptides for protein FDR scoring</li>\n<li>--sequential       alternative algorithm that estimates FDR using both filtered PSM and Protein lists</li>\n<li>--tag string       decoy tag (default \"rev_\")</li>\n<li>--weight float     threshold for defining peptide uniqueness (default 1)</li>\n</ul>");
    textReportFilter.addFocusListener(new java.awt.event.FocusAdapter() {
      public void focusGained(java.awt.event.FocusEvent evt) {
        textReportFilterFocusGained(evt);
      }
      public void focusLost(java.awt.event.FocusEvent evt) {
        textReportFilterFocusLost(evt);
      }
    });
    textReportFilter.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        textReportFilterActionPerformed(evt);
      }
    });

    checkReportAbacus.setSelected(loadLastCheckboxAbacus());
    checkReportAbacus.setText("Multi-Experiment Report ");
    checkReportAbacus.setToolTipText("<html>Philosopher abacus command");
    checkReportAbacus.setMargin(new java.awt.Insets(10, 0, 2, 2));
    checkReportAbacus.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        checkReportAbacusActionPerformed(evt);
      }
    });

    jLabel1.setText("Filter");

    javax.swing.GroupLayout panelReportOptionsLayout = new javax.swing.GroupLayout(panelReportOptions);
    panelReportOptions.setLayout(panelReportOptionsLayout);
    panelReportOptionsLayout.setHorizontalGroup(
      panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelReportOptionsLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(panelReportOptionsLayout.createSequentialGroup()
            .addComponent(jLabel1)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(textReportFilter, javax.swing.GroupLayout.DEFAULT_SIZE, 584, Short.MAX_VALUE))
          .addGroup(panelReportOptionsLayout.createSequentialGroup()
            .addComponent(checkReportAbacus)
            .addGap(0, 0, Short.MAX_VALUE))))
    );
    panelReportOptionsLayout.setVerticalGroup(
      panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelReportOptionsLayout.createSequentialGroup()
        .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(textReportFilter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(jLabel1))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(checkReportAbacus)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    loadLastReportFilter();

    checkCreateReport.setSelected(true);
    checkCreateReport.setText("Create report");
    checkCreateReport.setToolTipText("<html>Create tab separated report files with \nsome statistics about search results.");
    checkCreateReport.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        checkCreateReportActionPerformed(evt);
      }
    });

    panelSpecLibOpts.setBorder(javax.swing.BorderFactory.createTitledBorder("Spectral Library"));
    panelSpecLibOpts.setEnabled(false);

    checkGenerateSpecLib.setText("Generate Spectral Library from search results");
    checkGenerateSpecLib.setEnabled(false);
    checkGenerateSpecLib.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        checkGenerateSpecLibActionPerformed(evt);
      }
    });

    javax.swing.GroupLayout panelSpecLibOptsLayout = new javax.swing.GroupLayout(panelSpecLibOpts);
    panelSpecLibOpts.setLayout(panelSpecLibOptsLayout);
    panelSpecLibOptsLayout.setHorizontalGroup(
      panelSpecLibOptsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelSpecLibOptsLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(checkGenerateSpecLib)
        .addContainerGap(373, Short.MAX_VALUE))
    );
    panelSpecLibOptsLayout.setVerticalGroup(
      panelSpecLibOptsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelSpecLibOptsLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(checkGenerateSpecLib)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("Quantitation"));

    checkLabelfree.setText("Label-free Quant");
    checkLabelfree.setToolTipText("<html>Label free quantitation");

    textReportLabelfree.setToolTipText("<html>Label free quantitation<br/>\nFlags:<br/>\n<ul>\n<li>--ptw float    specify the time windows for the peak (minute) (default 0.4)</li>\n<li>--tol float    m/z tolerance in ppm (default 10)</li>\n</ul>");
    textReportLabelfree.addFocusListener(new java.awt.event.FocusAdapter() {
      public void focusGained(java.awt.event.FocusEvent evt) {
        textReportLabelfreeFocusGained(evt);
      }
      public void focusLost(java.awt.event.FocusEvent evt) {
        textReportLabelfreeFocusLost(evt);
      }
    });
    textReportLabelfree.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        textReportLabelfreeActionPerformed(evt);
      }
    });

    javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
    jPanel4.setLayout(jPanel4Layout);
    jPanel4Layout.setHorizontalGroup(
      jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(jPanel4Layout.createSequentialGroup()
        .addContainerGap()
        .addComponent(checkLabelfree)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(textReportLabelfree)
        .addContainerGap())
    );
    jPanel4Layout.setVerticalGroup(
      jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(jPanel4Layout.createSequentialGroup()
        .addContainerGap()
        .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(checkLabelfree)
          .addComponent(textReportLabelfree, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    loadLastFreequant();

    javax.swing.GroupLayout panelReportLayout = new javax.swing.GroupLayout(panelReport);
    panelReport.setLayout(panelReportLayout);
    panelReportLayout.setHorizontalGroup(
      panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelReportLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(panelReportLayout.createSequentialGroup()
            .addComponent(checkCreateReport)
            .addGap(0, 0, Short.MAX_VALUE))
          .addComponent(panelReportOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addComponent(panelSpecLibOpts, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        .addContainerGap())
    );
    panelReportLayout.setVerticalGroup(
      panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelReportLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(checkCreateReport)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(panelReportOptions, javax.swing.GroupLayout.PREFERRED_SIZE, 73, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(panelSpecLibOpts, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(458, Short.MAX_VALUE))
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

    javax.swing.GroupLayout panelRunLayout = new javax.swing.GroupLayout(panelRun);
    panelRun.setLayout(panelRunLayout);
    panelRunLayout.setHorizontalGroup(
      panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(panelRunLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(consoleScrollPane)
          .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelRunLayout.createSequentialGroup()
            .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
              .addGroup(panelRunLayout.createSequentialGroup()
                .addComponent(btnRun, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnStop)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(checkDryRun)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 29, Short.MAX_VALUE)
                .addComponent(btnPrintCommands)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnExportLog)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnReportErrors))
              .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelRunLayout.createSequentialGroup()
                .addComponent(lblOutputDir)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(txtWorkingDir)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnSelectWrkingDir)))
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
        .addComponent(btnAbout)
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
    JFileChooser fileChooser = new JFileChooser();
    //FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("FASTA files", "fa", "fasta");
    //fileChooser.setFileFilter(fileNameExtensionFilter);
    fileChooser.setApproveButtonText("Select directory");
    fileChooser.setApproveButtonToolTipText("Select");
    fileChooser.setDialogTitle("Choose working directory");
    fileChooser.setMultiSelectionEnabled(false);
    fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

    SwingUtils.setFileChooserPath(fileChooser, ThisAppProps.load(ThisAppProps.PROP_FILE_OUT));

    if (!txtWorkingDir.getText().isEmpty()) {
      File toFile = Paths.get(txtWorkingDir.getText()).toFile();
      fileChooser.setCurrentDirectory(toFile);
    }

    int showOpenDialog = fileChooser.showOpenDialog(this);
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File f = fileChooser.getSelectedFile();
        txtWorkingDir.setText(f.getAbsolutePath());
        ThisAppProps.save(ThisAppProps.PROP_FILE_OUT, f.getAbsolutePath());
        break;
    }
  }//GEN-LAST:event_btnSelectWrkingDirActionPerformed

  private void btnClearConsoleActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearConsoleActionPerformed
    console.setText("");
  }//GEN-LAST:event_btnClearConsoleActionPerformed

  private void btnStopActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnStopActionPerformed
    btnRun.setEnabled(true);
    btnStop.setEnabled(false);
    EventBus.getDefault().post(new MessageKillAll());

  }//GEN-LAST:event_btnStopActionPerformed


  private void btnRawClearActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawClearActionPerformed
    tableModelRawFiles.dataClear();
  }//GEN-LAST:event_btnRawClearActionPerformed

  private void btnRawAddFilesActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawAddFilesActionPerformed
    if (btnRawAddFiles == evt.getSource()) {
      String approveText = "Select";
      JFileChooser fc = new JFileChooser();
      fc.setAcceptAllFileFilterUsed(true);
      FileNameExtensionFilter fileNameExtensionFilter = FraggerMigPanel.fileNameExtensionFilter;
      fc.setFileFilter(fileNameExtensionFilter);
      fc.setApproveButtonText(approveText);
      fc.setDialogTitle("Choose raw data files");
      fc.setMultiSelectionEnabled(true);
      fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

      ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN, fc);

      int retVal = fc.showDialog(this, approveText);
      if (retVal != JFileChooser.APPROVE_OPTION)
        return;
      final List<Path> paths = Arrays.stream(fc.getSelectedFiles()).map(File::toPath)
          .collect(Collectors.toList());
      EventBus.getDefault().post(new MessageLcmsFilesAdded(paths));
    }
  }//GEN-LAST:event_btnRawAddFilesActionPerformed

  private void btnRawRemoveActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawRemoveActionPerformed
    int[] sel = tableRawFiles.getSelectedRows();
    if (sel.length == 0) {
      return;
    }
    List<InputLcmsFile> toRemove = new ArrayList<>();
    for (int i = 0; i < sel.length; i++) {
      toRemove.add(tableModelRawFiles.dataGet(sel[i]));
    }
    tableRawFiles.getSelectionModel().clearSelection();
    tableModelRawFiles.dataRemoveAll(toRemove);
  }//GEN-LAST:event_btnRawRemoveActionPerformed

  private void btnRawAddFolderActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawAddFolderActionPerformed
    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Select");
    fc.setApproveButtonToolTipText("Select folder to import");
    fc.setDialogTitle("Select a folder with LC/MS files (searched recursively)");
    fc.setAcceptAllFileFilterUsed(true);
    FileNameExtensionFilter fileNameExtensionFilter = FraggerMigPanel.fileNameExtensionFilter;
    fc.setFileFilter(fileNameExtensionFilter);
    fc.setMultiSelectionEnabled(true);
    fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

    SwingUtils.setFileChooserPath(fc, ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN));

    int confirmation = fc.showOpenDialog(this);

    if (confirmation != JFileChooser.APPROVE_OPTION)
      return;
    List<Path> paths = new ArrayList<>();
    for (File f : fc.getSelectedFiles()) {
      if (f.isDirectory()) {
        ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, f);
        PathUtils.traverseDirectoriesAcceptingFiles(f, FraggerMigPanel.fileNameExtensionFilter, paths);
      } else if (FraggerMigPanel.fileNameExtensionFilter.accept(f)) {
        paths.add(Paths.get(f.getAbsolutePath()));
      }
    }
    EventBus.getDefault().post(new MessageLcmsFilesAdded(paths));
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

    Path curExistingPath = PathUtils.isExisting(textBinMsfragger.getText().trim());
    if (curExistingPath != null) {
      SwingUtils.setFileChooserPath(fileChooser, curExistingPath);
    } else {
      List<String> props = Arrays
          .asList(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, ThisAppProps.PROP_BINARIES_IN);
      String fcPath = ThisAppProps.tryFindPath(props, true);
      SwingUtils.setFileChooserPath(fileChooser, fcPath);
    }

    int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File foundFile = fileChooser.getSelectedFile();
        if (validateAndSaveMsfraggerPath(foundFile.getAbsolutePath())) {
          ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, foundFile.getAbsolutePath());
        }
        break;
    }

    // rerun slicing checks
    validateMsadjusterEligibility();
    validateDbslicing();
  }//GEN-LAST:event_btnMsfraggerBinBrowseActionPerformed

  /**
   * Checks if a file is a JAR file and that it contains MSFragger.class at the top level.
   *
   * @param path file to check.
   * @return True if it's a real JAR file with MSFragger.class at the top level inside.
   */
  private boolean validateAndSaveMsfraggerPath(final String path) {
    boolean isJarValid = validateMsfraggerJarContents(path);
    if (isJarValid) {
      textBinMsfragger.setText(path);
      ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, path);
    }

    if (balloonMsfragger != null) {
      balloonMsfragger.closeBalloon();
      balloonMsfragger = null;
    }

    boolean isPathValid = validateMsfraggerPath(path);
    boolean isVersionValid = isJarValid && validateMsfraggerVersion(path);
    boolean isJavaValid = isVersionValid && validateMsfraggerJavaVersion();

    if (!isPathValid) {
      final String downloadUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_DOWNLOAD_URL, "");
      JEditorPane ep = SwingUtils.createClickableHtml(String.format(
          "<html>Could not find MSFragger jar file at this location.<br/>\n"
              + "Corresponding panel won't be active.<br/><br/>"
              + "<b>If that's the first time you're using %s</b>,<br/>"
              + "you will need to <a href=\"%s\">download MSFragger.jar (click here)</a> first.<br/>"
              + "Use the button on the right to proceed to the download website.",
          Version.PROGRAM_TITLE, downloadUrl));

      balloonMsfragger = new BalloonTip(textBinMsfragger, ep,
          new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
      balloonMsfragger.setVisible(true);
    } else if (!isJarValid) {
      final String downloadUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_DOWNLOAD_URL, "");
      JEditorPane ep = SwingUtils.createClickableHtml(String.format(
          "<html>Looks like you selected an existing jar file, but we<br/>\n"
              + "don't recognize it as a valid MSFragger distribution.<br/><br/>"
              + "<b>If that's the first time you're using %s</b>,<br/>"
              + "you will need to <a href=\"%s\">download MSFragger.jar (click here)</a> first.<br/>"
              + "Use the button on the right to proceed to the download website.",
          Version.PROGRAM_TITLE, downloadUrl));

      balloonMsfragger = new BalloonTip(textBinMsfragger, ep,
          new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
      balloonMsfragger.setVisible(true);
    }

    final boolean msfraggerEnabled = isJarValid && isVersionValid && isJavaValid;
    EventBus.getDefault().postSticky(new MessageValidityFragger(msfraggerEnabled));

    return isJarValid;
  }

  private boolean validateMsfraggerJavaVersion() {
    final boolean javaAtLeast18 = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8);
    final VersionComparator vc = new VersionComparator();
    SwingUtilities.invokeLater(() -> {
      BalloonTip tip = tipMap.remove(TIP_NAME_FRAGGER_JAVA_VER);
      if (tip != null) {
        tip.closeBalloon();
      }
      tip = null;

      if (!javaAtLeast18) {
        tip = new BalloonTip(lblFraggerJavaVer,
            "Msfragger requires Java 1.8. Your version is lower.\n");
      } else {
        // check for Java 9
        final String jver = SystemUtils.JAVA_SPECIFICATION_VERSION;
        final String fver = fraggerVer != null ? fraggerVer
            : MsfraggerProps.testJar(textBinMsfragger.getText()).version;
        if (jver != null && fver != null) {
          if (vc.compare(fver, "20180316") < 0 && vc.compare(jver, "1.9") >= 0) {
            tip = new BalloonTip(lblFraggerJavaVer, "<html>Looks like you're "
                + "running Java 9 or higher with MSFragger v20180316 or lower.<br/>"
                + "That version of MSFragger only supports Java 8.\n");
          }
        }
      }
      if (tip != null) {
        tipMap.put(TIP_NAME_FRAGGER_JAVA_VER, tip);
        tip.setVisible(true);
      }
    });
    return javaAtLeast18;
  }

  private static String tryPythonCommand() throws Exception {
    String[] commands = {"python", "python3"};
    for (String cmd : commands) {
      ProcessBuilder pb = new ProcessBuilder(cmd, "--version");
      pb.redirectErrorStream(true);

      Process pr;
      try {
        pr = pb.start();
      } catch (IOException ex) {
        throw new Exception("Could not start the python/python3 process.");
      }
      try {
        int exitCode = pr.waitFor();
        if (exitCode == 0) {
          return cmd;
        }
      } catch (InterruptedException ex) {
        throw new Exception("Error waiting for python/python3 process to finish.");
      }
    }
    return null;
  }


  public void validateMsadjusterEligibility() {
    new Thread(() -> {
      boolean enableMsadjuster = false;
      String minFraggerVer = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_MIN_VERSION_MSADJUSTER);
      if (minFraggerVer == null) {
        throw new IllegalStateException(MsfraggerProps.PROP_MIN_VERSION_MSADJUSTER +
            " property needs to be in Msfragger properties");
      }

      VersionComparator cmp = new VersionComparator();
      int fraggerVersionCmp = cmp.compare(fraggerVer, minFraggerVer);
      if (fraggerVersionCmp >= 0) {
        enableMsadjuster = true;
      }
      EventBus.getDefault().postSticky(new MessageValidityMsadjuster(enableMsadjuster));
    }).start();
  }

  public void validateSpeclibgen() {
    new Thread(() -> SpecLibGen.get().init()).start();
  }

  public void validateDbslicing() {
    new Thread(() -> DbSlice.get().init(fraggerVer)).start();
  }

  public void validateAndSavePython(final String binPath, boolean showPopupOnError) {
    new Thread(() -> {
      boolean ok;
      PythonInfo pi = PythonInfo.get();
      try {
        ok = PythonInfo.get().setPythonCommand(binPath);
      } catch (Exception e) {
        ok = false;
      }
      if (ok) {
        ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PYTHON, pi.getCommand());
      } else {
        ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PYTHON, "");
      }
      if (!ok && showPopupOnError) {
        JOptionPane.showMessageDialog(MsfraggerGuiFrame.this,
            "Not a valid Python binary path:\n\n" + binPath, "Not a Python binary",
            JOptionPane.WARNING_MESSAGE);
      }
    }).start();

  }

  private void validatePhilosopherVersion(final String binPath) {
    if (balloonPhilosopher != null) {
      balloonPhilosopher.closeBalloon();
    }

    final Pattern regexNewerVerFound = Pattern
        .compile("new version.*available.*?:\\s*(\\S+)", Pattern.CASE_INSENSITIVE);
    final Pattern regexVersion = Pattern
        .compile("build\\s+and\\s+version.*?build.*?=(?<build>\\S+).*version.*?=(?<version>\\S+)",
            Pattern.CASE_INSENSITIVE);
    final VersionComparator vc = new VersionComparator();

    // Check releases on github by running `philosopher version`.
    new Thread(() -> {
      // get the vesrion reported by the current executable
      // if we couldn't download remote properties, try using local ones
      // if we have some philosopher properties (local or better remote)
      // then check if this version is known to be compatible

      ProcessBuilder pb = new ProcessBuilder(binPath, "version");
      pb.redirectErrorStream(true);

      boolean isNewVersionStringFound = false;
      String curVersionAndBuild = null;
      String curVersion = null;

      // get the vesrion reported by the current executable
      String downloadLink = null;
      try {
        Process pr = pb.start();
        BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()));
        String line;
        while ((line = in.readLine()) != null) {
          Matcher m = regexNewerVerFound.matcher(line);
          if (m.find()) {
            isNewVersionStringFound = true;
            downloadLink = m.group(1);
          }
          Matcher mVer = regexVersion.matcher(line);
          if (mVer.find()) {
            curVersionAndBuild = mVer.group("version") + " (build " + mVer.group("build") + ")";
            curVersion = mVer.group("version");
          }
        }

        philosopherVer = StringUtils.isNullOrWhitespace(curVersionAndBuild) ? UNKNOWN_VERSION
            : curVersionAndBuild;
        lblPhilosopherInfo.setText(String.format(
            "Philosopher version: %s. %s", philosopherVer, OsUtils.OsInfo()));

        int returnCode = pr.waitFor();

        JEditorPane ep = null;
        if (isNewVersionStringFound) {
          StringBuilder sb = new StringBuilder();
          sb.append("Newer version of Philosopher available.<br>\n");
          sb.append("<a href=\"").append(downloadLink).append("\">Click here</a> to download.<br>\n");
          Properties props = PhilosopherProps.getProperties();
          if (props != null) {
            // if we have some philosopher properties (local or better remote)
            // then check if this version is known to be compatible
            String latestCompatible = props.getProperty(
                PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + Version.version());
            if (latestCompatible == null) {
              sb.append(
                  "<br>\nHowever, we have not yet checked if it's fully compatible with this version of ")
                  .append(Version.PROGRAM_TITLE).append(".");
            } else if (curVersion != null) {
              int cmp = vc.compare(curVersion, latestCompatible);
              if (cmp == 0) {
                sb.append(
                    "<br>\nHowever, <b>you currently have the latest known tested version</b>.");
              } else if (cmp < 0) {
                sb.append("<br>\nThe latest known tested version is<br>\n"
                    + "<b>Philosopher ").append(latestCompatible).append("</b>.<br/>\n");
                sb.append(
                    "It is not recommended to upgrade to newer versions unless they are tested.");
              }
            }
          }
          ep = SwingUtils.createClickableHtml(sb.toString());

        } else if (returnCode != 0) {
          ep = SwingUtils.createClickableHtml(String.format(Locale.ROOT,
              "Philosopher version too old and is no longer supported.<br>\n"
                  + "Please <a href=\"%s\">click here</a> to download a newer one.",
              PhilosopherProps.getProperties().getProperty(PhilosopherProps.PROP_DOWNLOAD_URL, "")));
        }
        if (ep != null) {
          if (balloonPhilosopher != null) {
            balloonPhilosopher.closeBalloon();
          }
          balloonPhilosopher = new BalloonTip(textBinPhilosopher, ep,
              new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
          balloonPhilosopher.setVisible(true);
        }

      } catch (IOException | InterruptedException e) {
        throw new IllegalStateException(
            "Error while creating a java process for Philosopher test.");
      }
    }).start();
  }

  private boolean validateMsfraggerVersion(final String jarPath) {
    // only validate Fragger version if the current Java version is 1.8 or higher
    if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8)) {
      // we can't test fragger binary verison when java version is less than 1.8
      return true;
    }

    Pattern regex = Pattern
        .compile("MSFragger version (MSFragger-([\\d\\.]{4,}))", Pattern.CASE_INSENSITIVE);

    // get the vesrion reported by the current executable
    final MsfraggerProps.FraggerRunResult jarTest = MsfraggerProps.testJar(jarPath);
    final String localVer = jarTest.isVersionPrintedAtAll ? jarTest.version : "0.0";
    fraggerVer = localVer;

    // update the version label
    fraggerVer = StringUtils.isNullOrWhitespace(localVer) ? UNKNOWN_VERSION : localVer;
    lblFraggerJavaVer.setText(String.format(
        "MSFragger version: %s. %s", fraggerVer, OsUtils.JavaInfo()));

    // The version from cmd line ouput is new enough to pass the local
    // test. Now check the versions on remotes.
    final VersionComparator vc = new VersionComparator();
    Thread t = new Thread(() -> {

      MsfraggerVersionFetcherServer vfServer = new MsfraggerVersionFetcherServer();
      MsfraggerVersionFetcherGithub vfGithub = new MsfraggerVersionFetcherGithub();
      MsfraggerVersionFetcherLocal vfLocal = new MsfraggerVersionFetcherLocal();
      List<VersionFetcher> verFetchers = Arrays.asList(vfServer, vfGithub, vfLocal);
      for (final VersionFetcher vf : verFetchers) {
        if (vf == null) {
          continue;
        }
        try {
          final String updateVer = vf.fetchVersion();
          if (StringUtils.isNullOrWhitespace(updateVer)) {
            continue;
          }
          // we got a non-empty version from some version fetcher
          if (vc.compare(localVer, updateVer) < 0) {
            // local versin is older, than the fetched version
            // show balloon popup, must be done on EDT
            String url = vf.getDownloadUrl();
            final String manualDownloadUrl = StringUtils.isNullOrWhitespace(url)
                ? vfLocal.getDownloadUrl() : url;
            SwingUtilities.invokeLater(() -> {
              if (balloonMsfragger != null) {
                balloonMsfragger.closeBalloon();
              }

              StringBuilder sb = new StringBuilder();
              if (jarTest.isVersionPrintedAtAll) {
                sb.append(String.format("Your version is [%s]<br>\n"
                        + "There is a newer version of MSFragger available [%s].<br>\n",
                    localVer, updateVer));
              } else {
                sb.append(
                    String.format("Your version is <b>too old and not supported anymore</b><br>\n"
                        + "Get a new version of MSFragger [%s].<br>\n", updateVer));
              }
              if (vf.canAutoUpdate()) {
                sb.append("<br>If you choose to auto-update a new version will be downloaded<br>\n"
                    + "and placed in the same folder as the old one. The old one will be kept.");
              }
              JEditorPane ep = SwingUtils.createClickableHtml(sb.toString());

              JPanel panel = new JPanel();
              panel.setBackground(ep.getBackground());
              panel.setLayout(new BorderLayout());

              JPanel panelButtons = new JPanel();
              panelButtons.setBackground(ep.getBackground());
              panelButtons.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

              if (vf.canAutoUpdate()) {
                JButton btnAutoUpdate = new JButton("Auto-update");
                btnAutoUpdate.addActionListener(e -> {
                  if (balloonMsfragger == null) {
                    return;
                  }
                  balloonMsfragger.setVisible(false);
                  balloonMsfragger = null;

                  final JDialog dlg = new JDialog(MsfraggerGuiFrame.this, "Updating MSFragger",
                      true);
                  JProgressBar pb = new JProgressBar(0, 100);
                  pb.setIndeterminate(true);
                  Dimension d = new Dimension(300, 75);
                  pb.setMinimumSize(d);
                  pb.setSize(d);
                  dlg.add(pb, BorderLayout.CENTER);
                  dlg.setSize(d);
                  dlg.setLocationRelativeTo(MsfraggerGuiFrame.this);

                  Thread updateThread = new Thread(() -> {
                    try {

                      Path updated = vf.autoUpdate(Paths.get(jarPath));
                      validateAndSaveMsfraggerPath(updated.toAbsolutePath().toString());

                    } catch (Exception ex) {
                      throw new IllegalStateException(
                          "Something happened during MSFragger auto-update", ex);
                    } finally {
                      dlg.setVisible(false);
                    }
                  });
                  updateThread.start();

                  // show the dialog, this blocks until dlg.setVisible(false) is called
                  // so this call is made in the finally block
                  dlg.setVisible(true);
                });
                panelButtons.add(btnAutoUpdate);
              }

              if (!StringUtils.isNullOrWhitespace(manualDownloadUrl)) {
                JButton btnManualUpdate = new JButton("Manual update");
                btnManualUpdate.addActionListener(e -> {
                  try {
                    SwingUtils.openBrowserOrThrow(new URI(manualDownloadUrl));
                  } catch (URISyntaxException ex) {
                    throw new IllegalStateException("Incorrect url/uri", ex);
                  }
                });
                panelButtons.add(btnManualUpdate);
              }

              JButton btnClose = new JButton("Close");
              btnClose.addActionListener(e -> {
                if (balloonMsfragger == null) {
                  return;
                }
                balloonMsfragger.setVisible(false);
                balloonMsfragger = null;
              });

              panel.add(ep, BorderLayout.CENTER);
              panelButtons.add(btnClose);
              panel.add(panelButtons, BorderLayout.SOUTH);

              balloonMsfragger = new BalloonTip(textBinMsfragger, panel,
                  new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
              balloonMsfragger.setVisible(true);
            });
          }
          return; // stop iterations, we've found that there is no better version than the current

        } catch (Exception ex) {
          // no biggie
          continue;
        }
      }
    });
    t.start();

    return true;
  }

  private boolean validateMsfraggerPath(String path) {
    File f = new File(path);
    if (!f.getName().toLowerCase().endsWith(".jar")) {
      return false;
    }
    Path p = Paths.get(path).toAbsolutePath();
    return Files.exists(p);

  }

  private boolean validateMsfraggerJarContents(String path) {
    if (!validateMsfraggerPath(path)) {
      return false;
    }
    Path p = Paths.get(path).toAbsolutePath();
    final boolean[] found = {false};
    try (FileSystem fs = FileSystems.newFileSystem(p, null)) {
      for (Path root : fs.getRootDirectories()) {
        Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
          Pattern regex = Pattern.compile("msfragger.*\\.jar", Pattern.CASE_INSENSITIVE);

          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            String fileName = file.getFileName().toString();
            if ("MSFragger.class".equalsIgnoreCase(fileName)) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            } else if (regex.matcher(fileName).find()) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            }
            return FileVisitResult.CONTINUE;
          }
        });
      }
    } catch (IOException ex) {
      // doesn't matter
      Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
    }

    return found[0];
  }

  private void btnMsfraggerBinDownloadActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerBinDownloadActionPerformed
    try {
      final String downloadUrl = MsfraggerProps.getProperties().getProperty(MsfraggerProps.PROP_DOWNLOAD_URL, "");
      Desktop.getDesktop().browse(URI.create(downloadUrl));
    } catch (IOException ex) {
      throw new IllegalStateException("Could not open MSFragger download link in browser.", ex);
    }
  }//GEN-LAST:event_btnMsfraggerBinDownloadActionPerformed

  private void urlHandlerViaSystemBrowser(
      javax.swing.event.HyperlinkEvent evt) {//GEN-FIRST:event_urlHandlerViaSystemBrowser
    if (evt.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {

      URI uri;
      try {
        uri = evt.getURL().toURI();
      } catch (URISyntaxException ex) {
        JOptionPane.showMessageDialog(null,
            "Could not convert URL to URI: " + evt.getURL(),
            "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
        return;
      }

      if (Desktop.isDesktopSupported()) {
        Desktop desktop = Desktop.getDesktop();
        try {
          desktop.browse(uri);
        } catch (IOException e) {
          JOptionPane.showMessageDialog(null,
              "Failed to open " + uri + " - your computer is likely misconfigured.\n"
                  + "Error Message: " + e.getMessage(),
              "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
        }
      } else {
        JOptionPane.showMessageDialog(null, "Java is not able to open a browser on your computer.",
            "Cannot Open Link", JOptionPane.WARNING_MESSAGE);
      }
    }
  }//GEN-LAST:event_urlHandlerViaSystemBrowser

  private void btnPhilosopherBinDownloadActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPhilosopherBinDownloadActionPerformed
    downloadPhilosopher();
  }//GEN-LAST:event_btnPhilosopherBinDownloadActionPerformed

  private void btnFindToolsActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnFindToolsActionPerformed

    String fraggerFoundPath = null;
    String philosopherFoundPath = null;

    JFileChooser fileChooser = new JFileChooser();
    fileChooser.setApproveButtonText("Search here");
    fileChooser.setApproveButtonToolTipText("Search this directory recursively");
    fileChooser.setDialogTitle("Select path to search for binaries");
    fileChooser.setMultiSelectionEnabled(false);
    fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

    List<String> props = Arrays
        .asList(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, ThisAppProps.PROP_BINARIES_IN,
            ThisAppProps.PROP_BIN_PATH_PHILOSOPHER);
    String fcPath = ThisAppProps.tryFindPath(props, true);
    SwingUtils.setFileChooserPath(fileChooser, fcPath);

    int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File f = fileChooser.getSelectedFile();

        // Fragger first
        Pattern regexFragger = Pattern
            .compile(".*?MSFragger[^\\/]+?\\.jar", Pattern.CASE_INSENSITIVE);
        FileListing listing = new FileListing(Paths.get(f.getAbsolutePath()), regexFragger);
        List<Path> foundFiles = listing.findFiles();
        for (Path foundFile : foundFiles) {
          if (validateAndSaveMsfraggerPath(foundFile.toString())) {
            fraggerFoundPath = foundFile.toString();
            ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, fraggerFoundPath);
            JOptionPane.showMessageDialog(this, "Found MSFragger jar.\n"
                + fraggerFoundPath, "Info", JOptionPane.INFORMATION_MESSAGE);
            break;
          }
        }
        if (fraggerFoundPath == null) {
          JOptionPane.showMessageDialog(this, "Could not locate MSFragger jar.", "Info",
              JOptionPane.INFORMATION_MESSAGE);
        }

        // now philosopher
        Pattern regexPhilosopher = Pattern
            .compile(".*?philosopher[^\\/]*", Pattern.CASE_INSENSITIVE);
        foundFiles = new FileListing(Paths.get(f.getAbsolutePath()), regexPhilosopher).findFiles();
        for (Path foundFile : foundFiles) {
          if (validateAndSavePhilosopherPath(foundFile.toString())) {
            philosopherFoundPath = foundFile.toString();
            ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, philosopherFoundPath);
            JOptionPane.showMessageDialog(this, "Found Philosopher.\n"
                + philosopherFoundPath, "Info", JOptionPane.INFORMATION_MESSAGE);
            break;
          }
        }
        if (philosopherFoundPath == null) {
          JOptionPane.showMessageDialog(this, "Could not locate Philosopher.", "Info",
              JOptionPane.INFORMATION_MESSAGE);
        }

        break;
    }
  }//GEN-LAST:event_btnFindToolsActionPerformed

  private void btnPhilosopherBinBrowseActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPhilosopherBinBrowseActionPerformed
    JFileChooser fc = new JFileChooser();
    fc.setApproveButtonText("Select");
    fc.setDialogTitle("Select Philosopher binary");
    fc.setMultiSelectionEnabled(false);
//    if (OsUtils.isWindows()) {
//      FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Executables",
//          "exe");
//      fc.setFileFilter(fileNameExtensionFilter);
//    }

    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

    Path curExistingPath = PathUtils.isExisting(textBinPhilosopher.getText().trim());
    if (curExistingPath != null) {
      SwingUtils.setFileChooserPath(fc, curExistingPath);
    } else {
      List<String> props = Arrays
          .asList(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER, ThisAppProps.PROP_BINARIES_IN);
      String fcPath = ThisAppProps.tryFindPath(props, true);
      SwingUtils.setFileChooserPath(fc, fcPath);
    }

    if (JFileChooser.APPROVE_OPTION == fc
        .showOpenDialog(SwingUtils.findParentFrameForDialog(this))) {
      String path = fc.getSelectedFile().getAbsolutePath();
      if (validateAndSavePhilosopherPath(path)) {
        // already saved to PROP_PHILOSOPHER, now save to general PROP_BINARIES
        ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, path);
      }
    }

  }//GEN-LAST:event_btnPhilosopherBinBrowseActionPerformed

  private void textBinMsfraggerFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textBinMsfraggerFocusLost
    validateAndSaveMsfraggerPath(textBinMsfragger.getText());
  }//GEN-LAST:event_textBinMsfraggerFocusLost

  private void textBinPhilosopherFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textBinPhilosopherFocusLost
    validateAndSavePhilosopherPath(textBinPhilosopher.getText());
  }//GEN-LAST:event_textBinPhilosopherFocusLost

  private void textBinPhilosopherActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textBinPhilosopherActionPerformed

  }//GEN-LAST:event_textBinPhilosopherActionPerformed

  private void btnClearCacheActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearCacheActionPerformed
    ThisAppProps.clearCache();
    new MsfraggerParams().clearCache();
    new CrystalcParams().clearCache();
  }//GEN-LAST:event_btnClearCacheActionPerformed

  private boolean validateAndSave(final JTextComponent comp, final String propName,
      final String newText, final IValidateString valid) {

    final String updText = newText != null ? newText : comp.getText().trim();
    final boolean isValid = valid.test(updText);
    comp.setText(updText);
    if (isValid) {
      ThisAppProps.save(propName, updText);
    }

    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        BalloonTip tip = tipMap.get(propName);
        if (tip != null) {
          tip.closeBalloon();
        }

        if (!isValid) {
          tip = new BalloonTip(comp, "Invalid format.");
          tip.setVisible(true);
          tipMap.put(propName, tip);
        }
      }
    });

    return isValid;
  }

  /**
   * Called with null from FocusChange listener. Call it with a new value if you want to update the
   * field programmatically.
   */
  private void validateAndSaveReportFilter(final String newText, boolean updateOtherTags) {
    final JTextComponent comp = textReportFilter;
    final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER,
        newText, ValidateTrue.getInstance());

    if (!isValid) {
      return;
    }

    // check if the filter line has changed since focus was gained
    final String savedText = textReportFilterFocusGained;
    final String oldText = savedText != null ? savedText : comp.getText().trim();
    final String updText = newText != null ? newText : comp.getText().trim();

    // newText == null means it was a programmatic update
    if (!updateOtherTags || oldText.equals(updText)) {
      return;
    }

    // something has changed
  }

  public String getFastaPath() {
    return textSequenceDbPath.getText().trim();
  }

  @Subscribe
  public void onRun(MessageRun m) {
    final boolean isDryRun = m.isDryRun;

    resetRunButtons(false);
    final boolean doRunFragger = fraggerMigPanel.isRun();
    boolean doRunProphetsAndReport = chkRunPeptideProphet.isSelected()
        || chkRunProteinProphet.isSelected()
        || checkCreateReport.isSelected();

    if (!doRunFragger
        && !isRunUmpireSe()
        && !chkRunPeptideProphet.isSelected()
        && !chkRunProteinProphet.isSelected()
        && !checkCreateReport.isSelected()) {
      JOptionPane.showMessageDialog(this, "Nothing to run.\n"
              + "Please mark checkboxes in other tabs to run processing tools.", "Error",
          JOptionPane.WARNING_MESSAGE);
      resetRunButtons(true);
      return;
    }

    // check for TSV output when any other downstream tools are requested
    if (doRunFragger && doRunProphetsAndReport) {
      if (fraggerMigPanel.getOutputType().equals(FraggerOutputType.TSV)) {
        int confirm = JOptionPane.showConfirmDialog(this,
            "You've chosen TSV output for MSFragger while\n"
                + "also requesting to run other downstream processing\n"
                + "tools. Those tools only support PepXML input.\n\n"
                + "Cancel operation and switch before running (manually)?",
            "Switch to pep.xml?", JOptionPane.YES_NO_OPTION);
        if (JOptionPane.YES_OPTION == confirm) {
          resetRunButtons(true);
          return;
        }
      }
    }

    final TextConsole textConsole = console;
    final String workingDir = txtWorkingDir.getText();
    if (workingDir.isEmpty()) {
      JOptionPane.showMessageDialog(this, "Output directory can't be left empty.\n"
              + "Please select an existing directory for the output.", "Error",
          JOptionPane.WARNING_MESSAGE);
      resetRunButtons(true);
      return;
    }
    Path testWdPath;
    try {
      testWdPath = Paths.get(workingDir);
    } catch (InvalidPathException e) {
      JOptionPane.showMessageDialog(this, "Output directory path is not a valid path.\n"
          + "Please select a directory for the output.", "Error", JOptionPane.WARNING_MESSAGE);
      resetRunButtons(true);
      return;
    }
    final Path wdPath = testWdPath;

    if (!isDryRun) {
      if (!Files.exists(wdPath)) {
        int confirmCreation = JOptionPane.showConfirmDialog(this,
            "Output directory doesn't exist. Create?",
            "Create output directory?", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION != confirmCreation) {
          resetRunButtons(true);
          return;
        }
        try {
          Files.createDirectories(wdPath);
        } catch (Exception e) {
          // something went not right during creation of directory structure
          JOptionPane.showMessageDialog(this,
              "Could not create directory structure.\n" + e.getMessage(), "Error",
              JOptionPane.ERROR_MESSAGE);
          resetRunButtons(true);
          return;
        }

      } else {
        try (Stream<Path> inWd = Files.list(wdPath)) {
          if (inWd.findAny().isPresent()) {
            int confirm = JOptionPane.showConfirmDialog(this,
                "The output directory is not empty.\n\n"
                    + "Some files might be overwritten in:\n"
                    + " > " + wdPath.toString() + "\n\n"
                    + "Do you want to proceed?", "Confirmation", JOptionPane.YES_NO_OPTION);
            if (JOptionPane.YES_OPTION != confirm) {
              resetRunButtons(true);
              return;
            }
          }
        } catch (Exception e) {
          JOptionPane.showMessageDialog(this,
              "Could not create directory structure.\n" + e.getMessage(), "Error",
              JOptionPane.ERROR_MESSAGE);
          resetRunButtons(true);
          return;
        }
      }

      // make sure subdirs for experiments are created
      Set<Path> subdirs = getLcmsFileGroups().values().stream().map(g -> g.outputDir(wdPath))
          .collect(Collectors.toSet());
      for (Path subdir : subdirs) {
        if (!Files.exists(subdir)) {
          try {
            Files.createDirectories(subdir);
          } catch (IOException e) {
            JOptionPane.showMessageDialog(this,
                "Could not create directory structure.\n" + e.getMessage(), "Error",
                JOptionPane.ERROR_MESSAGE);
            resetRunButtons(true);
            return;
          }
        }
      }
    }

    final Map<String, LcmsFileGroup> lcmsFileGroups = getLcmsFileGroups();
    final ArrayList<InputLcmsFile> lcmsFilesAll = lcmsFileGroups.values().stream()
        .flatMap(group -> group.lcmsFiles.stream()).collect(Collectors.toCollection(ArrayList::new));

    if (lcmsFilesAll.isEmpty()) {
      JOptionPane.showMessageDialog(this, "No LC/MS data files selected.\n"
          + "Check 'Select Raw Files' tab.", "Error", JOptionPane.WARNING_MESSAGE);
      resetRunButtons(true);
      return;
    } else {
      // check that all input LCMS files have unique filenames
      Map<String, List<Path>> inputFnMap = new HashMap<>();
      for (LcmsFileGroup group : lcmsFileGroups.values()) {
        for (InputLcmsFile inputLcmsFile : group.lcmsFiles) {
          Path path = inputLcmsFile.path;
          String fn = path.getFileName().toString();
          inputFnMap.computeIfAbsent(fn, s -> new LinkedList<>()).add(path);
        }
      }
      List<Entry<String, List<Path>>> collect = inputFnMap.entrySet().stream()
          .filter(kv -> kv.getValue().size() > 1).collect(Collectors.toList());
      if (!collect.isEmpty()) {
        final StringBuilder sb = new StringBuilder();
        sb.append("<html>Some input LCMS files have the same name, "
            + "even though located in different folders:\n");
        collect.forEach(kv -> {
          sb.append("\n<html>File Name - <b>").append(kv.getKey()).append("</b>:");
          kv.getValue().forEach(path -> sb.append("\n    - ").append(path.toString()));
          sb.append("\n");
        });
        sb.append("\nFiles might get overwritten and results might be not what's expected.\n"
            + "Consider renaming input files.");

        JOptionPane.showMessageDialog(this,
            sb.toString(),
            "Input files with same names", JOptionPane.WARNING_MESSAGE);
        resetRunButtons(true);
        return;
      }
    }

    DateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    String dateString = df.format(new Date());

    URI jarFragpipeUri = null;
    Path jarFragpipePath = null;
    try {
      jarFragpipeUri = PathUtils.getCurrentJarUri();
      jarFragpipePath = Paths.get(jarFragpipeUri);
    } catch (Exception ignore) {
      // don't care
    }
    if (jarFragpipeUri == null) {
      JOptionPane.showMessageDialog(this, "Could not get the URI of the currently running jar",
          "Errors", JOptionPane.ERROR_MESSAGE);
      resetRunButtons(true);
      return;
    }

    // check fasta file path
    String fastaPathText = textSequenceDbPath.getText().trim();
    if (StringUtils.isNullOrWhitespace(fastaPathText)) {
      JOptionPane.showMessageDialog(this, "Fasta file path (Sequence DB tab) can't be empty",
          "Warning", JOptionPane.WARNING_MESSAGE);
      resetRunButtons(true);
      return;
    }

    final String fastaPath = PathUtils.testFilePath(fastaPathText, workingDir);
    if (fastaPath == null) {
      JOptionPane.showMessageDialog(this,
          String.format("Could not find fasta file (Sequence DB) at:\n%s", fastaPathText),
          "Errors", JOptionPane.ERROR_MESSAGE);
      resetRunButtons(true);
      return;
    }

    final String binPhilosopher = textBinPhilosopher.getText().trim();
    final List<ProcessBuildersDescriptor> pbDescsToFill = new ArrayList<>();

    // main call to generate all the process builders
    if (!processBuildersNew(wdPath, jarFragpipePath, binPhilosopher, isDryRun, pbDescsToFill)) {
      resetRunButtons(true);
      return;
    }


    String sbSysinfo = OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo() + "\n";
    LogUtils.println(console, String.format(Locale.ROOT, "System info:\n%s",
        sbSysinfo));

    StringBuilder sbVer = new StringBuilder();
    sbVer.append(Version.PROGRAM_TITLE).append(" version ").append(Version.version()).append("\n");
    sbVer.append("MSFragger version ").append(fraggerVer).append("\n");
    sbVer.append("Philosopher version ").append(philosopherVer).append("\n");
    LogUtils.println(console, String.format(Locale.ROOT, "Version info:\n%s", sbVer.toString()));
    LogUtils.println(console, "");

    LogUtils.println(console, String.format(Locale.ROOT, "LCMS files:", sbVer.toString()));
    for (Map.Entry<String, LcmsFileGroup> e : lcmsFileGroups.entrySet()) {
      LogUtils.println(console,
          String.format(Locale.ROOT, "  Experiment/Group: %s", e.getValue().name));
      for (InputLcmsFile f : e.getValue().lcmsFiles) {
        LogUtils.println(console, String.format(Locale.ROOT, "  - %s", f.path.toString()));
      }
    }
    LogUtils.println(console, "");

    final List<ProcessBuilderInfo> pbis = pbDescsToFill.stream()
        .flatMap(pbd -> pbd.pbs.stream().map(pb -> new ProcessBuilderInfo(pb, pbd.name,
            pbd.fileCaptureStdout, pbd.fileCaptureStderr)))
        .collect(Collectors.toList());

    LogUtils.println(console, String.format(Locale.ROOT, "%d commands to execute:", pbis.size()));

    for (final ProcessBuilderInfo pbi : pbis) {
      printProcessDescription(pbi);

    }
    LogUtils.println(console, "~~~~~~~~~~~~~~~~~~~~~~");
    LogUtils.println(console, "");
    LogUtils.println(console, "");

    if (isDryRun) {
      LogUtils.println(console, "It's a dry-run, not running the commands.");
      resetRunButtons(true);
      return;
    }

    // run everything
    List<RunnableDescription> toRun = new ArrayList<>();
    for (final ProcessBuilderInfo pbi : pbis) {
      Runnable runnable = pbiToRunnable(pbi);
      Builder b = new Builder().setName(pbi.name);
      if (pbi.pb.directory() != null) {
        b.setWorkDir(pbi.pb.directory().toString());
      }
      if (pbi.pb.command() != null && !pbi.pb.command().isEmpty()) {
        b.setCommand(String.join(" ", pbi.pb.command()));
      }
      toRun.add(new RunnableDescription(b.create(), runnable));
    }

    // add finalizer process
    final JButton btnStartPtr = btnRun;
    final JButton btnStopPtr = btnStop;
    Runnable finalizerRun = () -> {
      btnStartPtr.setEnabled(true);
      btnStopPtr.setEnabled(false);
      String msg =
          "=========================" +
              "===" +
              "===      Done" +
              "===" +
              "=========================";
      EventBus.getDefault()
          .post(new MessageAppendToConsole(msg, MsfraggerGuiFrame.COLOR_RED_DARKEST));
    };
    String finalizerDesc = "Finalizer task";
    toRun.add(new RunnableDescription(new Builder().setName("Finalizer Task").create(), finalizerRun));
    EventBus.getDefault().post(new MessageStartProcesses(toRun));
  }

  private void printProcessDescription(ProcessBuilderInfo pbi) {
    if (!StringUtils.isNullOrWhitespace(pbi.name)) {
      LogUtils.print(COLOR_TOOL, console, true, pbi.name, false);
    }
    if (pbi.pb.directory() != null) {
      LogUtils.print(COLOR_WORKDIR, console, true, " [Work dir: " + pbi.pb.directory() + "]", false);
    }
    LogUtils.println(console, "");
    final String cmd = org.apache.commons.lang3.StringUtils.join(pbi.pb.command(), " ");
    LogUtils.print(COLOR_CMDLINE, console, true, cmd, true);
  }

  private Runnable pbiToRunnable(final ProcessBuilderInfo pbi) {
    return () -> {

            final ProcessResult pr = new ProcessResult(pbi);
            Process started;
            try {
              log.debug("Starting: {}", pbi.name);
              printProcessDescription(pbi);
              started = pr.start();
              log.debug("Started: {}", pbi.name);
            } catch (IOException e) {
              log.error("Error while starting process: " + pbi.name + ", stopping", e);
              EventBus.getDefault().post(new MessageKillAll());
              return;
            }

            // main loop reading process' output
            try {
              while (true) {

                Thread.sleep(200L);
                final byte[] pollErr = pr.pollStdErr();
                final String errStr = pr.appendErr(pollErr);
                if (errStr != null) {
                  EventBus.getDefault().post(new MessageExternalProcessOutput(true, errStr,
                      pbi.name));
                }
                final byte[] pollOut = pr.pollStdOut();
                final String outStr = pr.appendOut(pollOut);
                if (outStr != null) {
                  EventBus.getDefault().post(new MessageExternalProcessOutput(false, outStr,
                      pbi.name));
                }
                if (started.isAlive()) {
                  continue;
                }

                try {
                  log.debug("Checking exit value: {}", pbi.name);
                  final int exitValue = started.exitValue();
                  log.debug("Exit value '{}': {}", exitValue, pbi.name);
                  Color c = exitValue == 0
                      ? MsfraggerGuiFrame.COLOR_GREEN_DARKER
                      : MsfraggerGuiFrame.COLOR_RED;
                  String msg = String.format(Locale.ROOT,
                      "Process '%s' finished, exit code: %d\n", pbi.name, exitValue);
                  EventBus.getDefault().post(new MessageAppendToConsole(msg, c));
                  if (exitValue != 0) {
                    log.debug("Exit value not zero, killing all processes");
                    EventBus.getDefault().post(new MessageAppendToConsole(
                        "Process returned non-zero exit code, stopping", MsfraggerGuiFrame.COLOR_RED));
                    EventBus.getDefault().post(new MessageKillAll());
                  }

                } catch (IllegalThreadStateException ex) {
                  log.warn("Checking for exit value when subprocess was not alive threw exception.");
                }
                break;
              }

            } catch (IOException e) {
              log.error("Error while starting process " + pbi.name, e);

            } catch (InterruptedException e) {
              // graceful stop request
              String msg = "Processing interrupted, stopping " + pbi.name;
              log.debug(msg, e);
              EventBus.getDefault()
                  .post(new MessageAppendToConsole(msg, MsfraggerGuiFrame.COLOR_RED_DARKEST));
              // all the cleanup is done in the finally block

            } finally {
              // in the end whatever happens always try to kill the process
              if (started != null && started.isAlive()) {
                log.debug("Killing underlying external process");
                started.destroyForcibly();
              }
              try {
                pr.close();
              } catch (Exception e) {
                log.error("Error closing redirected std/err streams from external process", e);
              }
            }
          };
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void onExternalProcessOutput(MessageExternalProcessOutput m) {
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
  public void onAppendToConsole(MessageAppendToConsole m) {
    Color c = m.color == null ? COLOR_BLACK : m.color;
    LogUtils.print(c, console, true, m.text, true);
  }

  private static ExecutorService prepareProcessRunner(ExecutorService runner) {
    if (runner != null && !runner.isTerminated()) {
      runner.shutdownNow();
    }
    return Executors.newFixedThreadPool(1);
  }

  private void btnRunActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRunActionPerformed
    final boolean isDryRun = checkDryRun.isSelected();
    EventBus.getDefault().post(new MessageRun(isDryRun));
  }//GEN-LAST:event_btnRunActionPerformed


  private void teeProcessStream(InputStream is, OutputStream... os) throws IOException {
    int available = is.available();
    if (available > 0) {
      byte[] bytes = new byte[available];
      int read = is.read(bytes);

      for (int i = 0; i < os.length; i++) {
        OutputStream o = os[i];
        if (o == null) {
          continue;
        }
        o.write(bytes);
      }

      String asStr = new String(bytes);
      if (!StringUtils.isNullOrWhitespace(asStr)) {
        LogUtils.println(console, asStr);
      }

    } else {
      // zero bytes available, do nothing I guess
    }
  }

  private String getTimestamp() {
    return "[" + LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_TIME) + "]";
  }

  /**
   * @param wd Global working directory. LCMS file groups' output will be created inside this one.
   */
  private boolean processBuildersNew(Path wd, Path jarFragpipe, String binPhilosopher, boolean isDryRun,
      final List<ProcessBuildersDescriptor> pbDescsToFill) {

    final List<ProcessBuildersDescriptor> pbDescs = new ArrayList<>();

    // Collect input LCMS files
    final Map<String, LcmsFileGroup> lcmsFileGroups = getLcmsFileGroups();
    List<InputLcmsFile> lcmsFiles = lcmsFileGroups.values().stream()
        .flatMap(g -> g.lcmsFiles.stream())
        .collect(Collectors.toList());

    final UsageTrigger usePhi = new UsageTrigger(binPhilosopher, "Philosopher");


    // run DIA-Umpire SE
    final CmdUmpireSe cmdUmpireSe = new CmdUmpireSe(isRunUmpireSe(), wd);
    if (cmdUmpireSe.isRun()) {
      if (!cmdUmpireSe.configure(this, isDryRun, jarFragpipe, usePhi,
          umpirePanel, lcmsFiles))
        return false;
      pbDescs.add(cmdUmpireSe.builders());
      lcmsFiles = cmdUmpireSe.outputs(lcmsFiles);
    }


    final FraggerMigPanel fp = fraggerMigPanel;

    // run MSAdjuster
    final CmdMsAdjuster cmdMsAdjuster = new CmdMsAdjuster(fp.isRun() && fp.isMsadjuster(), wd);
    if (cmdMsAdjuster.isRun()) {
      if (!cmdMsAdjuster.configure(this,
          jarFragpipe, fp, lcmsFiles, false, 49)) {
        return false;
      }
      pbDescs.add(cmdMsAdjuster.builders());
      // MsAdjuster only makes files that are discovered by MsFragger
      // automatically, so no file-list changes are needed
    }


    // run MsFragger
    final String fastaFile = getFastaPath();
    final UsageTrigger binMsfragger = new UsageTrigger(
        textBinMsfragger.getText().trim(), "MsFragger");
    final CmdMsfragger cmdMsfragger = new CmdMsfragger(fp.isRun(), wd);
    if (cmdMsfragger.isRun()) {
      if (!cmdMsfragger.configure(this,
          isDryRun, fp, jarFragpipe, binMsfragger, fastaFile, lcmsFiles)) {
        return false;
      }
      pbDescs.add(cmdMsfragger.builders());

      String warn = ThisAppProps.load(ThisAppProps.PROP_MGF_WARNING, Boolean.TRUE.toString());
      if (warn != null && Boolean.valueOf(warn)) {
        for (InputLcmsFile f : lcmsFiles) {
          if (f.path.toString().toLowerCase().endsWith(".mgf")) {
            JCheckBox checkbox = new JCheckBox("Do not show this message again.");
            String msg = "The list of input files contains MGF entries.\n"
                + "MSFragger has limited MGF support (ProteoWizard output is OK).\n"
                + "The search might fail unexpectedly with errors.\n"
                + "Please consider converting files to mzML/mzXML with ProteoWizard.";
            Object[] params = {msg, checkbox};
            JOptionPane.showMessageDialog(this, params, "Warning",
                JOptionPane.WARNING_MESSAGE);
            if (checkbox.isSelected()) {
              ThisAppProps.save(ThisAppProps.PROP_MGF_WARNING, Boolean.FALSE.toString());
            }
            break;
          }
        }
      }
    }
    Map<InputLcmsFile, Path> pepxmlFiles = cmdMsfragger.outputs(
        lcmsFiles, fp.getOutputFileExt(), wd);


    // run MsAdjuster Cleanup
    if (cmdMsAdjuster.isRun()) {
      if (!cmdMsAdjuster.configure(this,
          jarFragpipe, fp, lcmsFiles, true, 51)) {
        return false;
      }
      pbDescs.add(cmdMsAdjuster.builders());
    }


    // run Crystalc
    final CmdCrystalc cmdCrystalc = new CmdCrystalc(
        chkRunCrystalc.isEnabled() && chkRunCrystalc.isSelected(), wd);
    if (cmdCrystalc.isRun()) {
      CrystalcParams ccParams;
      try {
        ccParams = crystalcFormToParams();
      } catch (IOException e) {
        JOptionPane.showMessageDialog(this,
            "Could not construct Crystal-C parameters from the GUI form input.", "Error",
            JOptionPane.ERROR_MESSAGE);
        return false;
      }
      if (!cmdCrystalc.configure(this,
          fp, isDryRun, ccParams, fastaFile, pepxmlFiles)) {
        return false;
      }
      pbDescs.add(cmdCrystalc.builders());
      pepxmlFiles = cmdCrystalc.outputs(pepxmlFiles, fp.getOutputFileExt());
    }

    // run Peptide Prophet
    final boolean isCombinedPepxml = checkCombinedPepxml.isSelected();
    final String decoyTag = textDecoyTagSeqDb.getText().trim();
    CmdPeptideProphet cmdPeptideProphet = new CmdPeptideProphet(
        chkRunPeptideProphet.isEnabled() && chkRunPeptideProphet.isSelected(), wd);
    if (cmdPeptideProphet.isRun()) {
      final String pepProphCmd = textPepProphCmd.getText().trim();
      if (!cmdPeptideProphet.configure(this,
          usePhi, fastaFile, decoyTag, pepProphCmd, isCombinedPepxml, pepxmlFiles)) {
        return false;
      }
      pbDescs.add(cmdPeptideProphet.builders());
    }
    pepxmlFiles = cmdPeptideProphet.outputs(pepxmlFiles, fp.getOutputFileExt(), isCombinedPepxml);


    // run Protein Prophet
    final boolean isProcessGroupsSeparately = checkProcessGroupsSeparately.isSelected();
    final boolean isRunProteinProphet = SwingUtils.isEnabledAndChecked(chkRunProteinProphet);
    final CmdProteinProphet cmdProteinProphet = new CmdProteinProphet(isRunProteinProphet, wd);
    if (cmdProteinProphet.isRun()) {
      final String protProphCmdStr = txtProteinProphetCmdLineOpts.getText().trim();
      if (!cmdProteinProphet.configure(this,
          usePhi, protProphCmdStr,
          isProcessGroupsSeparately, pepxmlFiles)) {
        return false;
      }
      pbDescs.add(cmdProteinProphet.builders());
    }
    Map<LcmsFileGroup, Path> mapGroupsToProtxml = cmdProteinProphet.outputs(pepxmlFiles, isProcessGroupsSeparately);


    if (cmdPeptideProphet.isRun() || cmdProteinProphet.isRun()) {
      // Check Decoy tags if any of the downstream tools are requested
      if (StringUtils.isNullOrWhitespace(textDecoyTagSeqDb.getText())) {
        int confirm = JOptionPane.showConfirmDialog(this,
            "Downstream analysis tools require decoys in the database,\n"
                + "but the decoy tag was left empty. It's recommended that\n"
                + "you set it.\n\n"
                + "Cancel operation and fix the problem (manually)?",
            "Cancel run and fix parameters?\n", JOptionPane.YES_NO_OPTION);
        if (JOptionPane.YES_OPTION == confirm) {
          return false;
        }
      }
    }

    final boolean isReport = SwingUtils.isEnabledAndChecked(checkCreateReport);
    if (isReport) {
      // run Report - DbAnnotate
      final boolean isDbAnnotate = true;
      final CmdReportDbAnnotate cmdReportDbAnnotate = new CmdReportDbAnnotate(isDbAnnotate, wd);
      if (cmdReportDbAnnotate.isRun()) {
        if (!cmdReportDbAnnotate
            .configure(this, usePhi, fastaFile, decoyTag, pepxmlFiles, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdReportDbAnnotate.builders());
      }

      // run Report - Filter
      final boolean isFilter = isReport;
      final CmdReportFilter cmdReportFilter = new CmdReportFilter(isFilter, wd);
      if (cmdReportFilter.isRun()) {
        if (!cmdReportFilter.configure(this, usePhi,
            decoyTag, textReportFilter.getText(), mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdReportFilter.builders());
      }

      // run Report - Freequant (Labelfree)
      final boolean isFreequant = SwingUtils.isEnabledAndChecked(checkLabelfree);
      final CmdReportFreequant cmdReportFreequant = new CmdReportFreequant(isFreequant, wd);
      if (cmdReportFreequant.isRun()) {
        if (!cmdReportFreequant.configure(this, usePhi,
            textReportLabelfree.getText(), mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdReportFreequant.builders());
      }

      // run Report - Report command itself
      final CmdReportReport cmdReportReport = new CmdReportReport(isReport, wd);
      if (cmdReportReport.isRun()) {
        if (!cmdReportReport.configure(this, usePhi, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdReportReport.builders());
      }

      // run Report - Multi-Experiment report
      final int nThreads = fraggerMigPanel.getThreads();
      final CmdReportAbacus cmdReportAbacus = new CmdReportAbacus(SwingUtils.isEnabledAndChecked(checkReportAbacus), wd);
      if (cmdReportAbacus.isRun()) {
        // run iProphet, will run right after Peptide Prophet because of priority setting
        final CmdIprophet cmdIprophet = new CmdIprophet(cmdReportAbacus.isRun(), wd);
        if (!cmdIprophet.configure(this, usePhi, decoyTag, nThreads, pepxmlFiles)) {
          return false;
        }
        pbDescs.add(cmdIprophet.builders());

        // run Abacus
        if (!cmdReportAbacus.configure(this, usePhi,
            textReportFilter.getText(), decoyTag, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdReportAbacus.builders());
      }
    }

    // run Spectral library generation
    final CmdSpecLibGen cmdSpecLibGen = new CmdSpecLibGen(SwingUtils.isEnabledAndChecked(checkGenerateSpecLib), wd);
    if (cmdSpecLibGen.isRun()) {
      if (!cmdSpecLibGen.configure(this, usePhi, jarFragpipe,
          mapGroupsToProtxml, fastaFile, isRunProteinProphet)) {
        return false;
      }
      pbDescs.add(cmdSpecLibGen.builders());
    }


    // run Philosopher clean/init in all directories where Philosopher will be invoked
    for (Path pathPhiIsRunIn : usePhi.getWorkDirs()) {
      CmdPhilosopherWorkspaceCleanInit cmdPhiCleanInit = new CmdPhilosopherWorkspaceCleanInit(
          true, pathPhiIsRunIn);
      cmdPhiCleanInit.configure(usePhi);
      pbDescs.add(cmdPhiCleanInit.builders());
    }

    // make sure that all subfolders are created for groups/experiments
    if (!isDryRun) {
      List<Path> paths = Stream
          .concat(pepxmlFiles.values().stream(), mapGroupsToProtxml.values().stream())
          .map(Path::getParent).collect(Collectors.toList());
      try {
        for (Path path : paths) {
          if (!Files.exists(path)) {
            Files.createDirectories(path);
          }
        }
      } catch (IOException e) {
        JOptionPane.showMessageDialog(this,
            "Not all directories could be created:\n" + e.getMessage());
        return false;
      }
    }

    pbDescs.sort(Comparator.comparing(pbDesc -> pbDesc.priority, Integer::compare));
    pbDescsToFill.addAll(pbDescs);
    return true;
  }

  private String getCombinedProtFn() {
    return "interact.prot.xml";
  }

  private String getRegexMatch(Pattern re, String text, int groupNum) {
    Matcher m = re.matcher(text);
    return m.find() ? m.group(groupNum) : "";
  }

  private void btnPepProphDefaults(SearchTypeProp t) {
    int confirm1 = JOptionPane.showConfirmDialog(this,
        "<html>Load " + t + " search defaults?");
    if (JOptionPane.YES_OPTION != confirm1) {
      return;
    }

    loadDefaultsPeptideProphet(t);

//    int choice = JOptionPane.showConfirmDialog(this,
//        "<html>Loaded " + t + " search defaults.<br/><br/>"
//            + "Do you want to load defaults <b>for other tools</b> as well?<br/><br/>"
//                + "<b>WARNING:</b><br/>"
//                + "This will reset MSFragger settings!");
//    if (JOptionPane.YES_OPTION == choice) {
//      EventBus.getDefault().post(new MessageSearchType(t));
//    }
  }
  
  private void btnProtProphDefaults(SearchTypeProp t) {

    int confirm1 = JOptionPane.showConfirmDialog(this,
        "<html>Load " + t + " search defaults?");
    if (JOptionPane.YES_OPTION != confirm1) {
      return;
    }

    loadDefaultsProteinProphet(t);

//    int choice = JOptionPane.showConfirmDialog(this,
//        "<html>Loaded " + t + " search defaults.<br/><br/>"
//            + "Do you want to load defaults <b>for other tools</b> as well?<br/><br/>"
//                + "<b>WARNING:</b><br/>"
//                + "This will reset MSFragger settings!");
//    if (JOptionPane.YES_OPTION == choice) {
//      EventBus.getDefault().post(new MessageSearchType(t));
//    }
  }

  private void textReportFilterFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportFilterFocusLost
    validateAndSaveReportFilter(null, true);
  }//GEN-LAST:event_textReportFilterFocusLost

  private void btnAboutInConfigActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAboutInConfigActionPerformed
    EventBus.getDefault().post(new MessageShowAboutDialog());
  }//GEN-LAST:event_btnAboutInConfigActionPerformed

  private void textReportFilterActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textReportFilterActionPerformed

  }//GEN-LAST:event_textReportFilterActionPerformed

  private void textReportFilterFocusGained(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportFilterFocusGained
    textReportFilterFocusGained = textReportFilter.getText();
  }//GEN-LAST:event_textReportFilterFocusGained

  private void btnExportLogActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnExportLogActionPerformed
    Action action = panelRun.getActionMap().get(ACTION_EXPORT_LOG);
    if (action != null) {
      action.actionPerformed(null);
    }
  }//GEN-LAST:event_btnExportLogActionPerformed

  private void textSequenceDbPathFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textSequenceDbPathFocusLost
    validateAndSaveFastaPath(textSequenceDbPath.getText());
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
    String fcPath = ThisAppProps.tryFindPath(Arrays.asList(propName), true);
    SwingUtils.setFileChooserPath(fileChooser, fcPath);

    int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentFrameForDialog(this));
    switch (showOpenDialog) {
      case JFileChooser.APPROVE_OPTION:
        File foundFile = fileChooser.getSelectedFile();
        if (validateAndSaveFastaPath(foundFile.getAbsolutePath())) {
          ThisAppProps.save(propName, foundFile.getAbsolutePath());
        }
        break;
    }
  }//GEN-LAST:event_btnBrowseActionPerformed

  private void textDecoyTagSeqDbFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textDecoyTagSeqDbFocusLost
    validateAndSaveDecoyTagSeqDb(null, true);
  }//GEN-LAST:event_textDecoyTagSeqDbFocusLost

  private void textDecoyTagSeqDbFocusGained(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textDecoyTagSeqDbFocusGained
    textDecoyTagFocusGained = textDecoyTagSeqDb.getText().trim();
  }//GEN-LAST:event_textDecoyTagSeqDbFocusGained

  private void btnTryDetectDecoyTagActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnTryDetectDecoyTagActionPerformed
    Path p = null;
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

    try (BufferedReader br = new BufferedReader(new InputStreamReader(Files.newInputStream(p),
        StandardCharsets.UTF_8))) {
      String line;
      List<String> descriptors = new ArrayList<>();
      List<List<String>> ordered = new ArrayList<>();
      long totalDescriptors = 0;
      long totalLines = 0;
      while ((line = br.readLine()) != null) {
        if (!line.startsWith(">")) {
          continue;
        }
        totalLines++;
        int pos = 1, next;
        int depth = 1;
        while ((next = line.indexOf('|', pos)) >= 0 || pos < line.length() - 1) {
          if (next < 0) {
            next = line.length();
          }
          String desc = line.substring(pos, next).trim();
          descriptors.add(desc);
          if (ordered.size() < depth) {
            ordered.add(new ArrayList<String>());
          }
          ordered.get(depth - 1).add(desc);
          totalDescriptors++;
          pos = next + 1;
          depth++;
        }
      }

      List<List<Tuple2<String, Double>>> prefixesByCol = new ArrayList<>();
      List<List<Tuple2<String, Double>>> suffixesByCol = new ArrayList<>();

      for (int descCol = 0; descCol < ordered.size(); descCol++) {

        List<String> descriptorCol = ordered.get(descCol);
        final int maxDepth = 16;
        PrefixCounter cntFwd = new PrefixCounter(PrefixCounter.Mode.FWD, maxDepth);
        PrefixCounter cntRev = new PrefixCounter(PrefixCounter.Mode.REV, maxDepth);

        for (int i = 0; i < descriptorCol.size(); i++) {
          String descriptor = descriptorCol.get(i);
          cntFwd.add(descriptor);
          cntRev.add(descriptor);
        }
        final long total = descriptorCol.size();
        final StringBuilder sb = new StringBuilder();

        final double pctMin = 0.3;
        final double pctMax = 0.7;

        { // prefixes
          final List<Tuple2<String, Double>> result = new ArrayList<>();
          Proc2<PrefixCounter.Node, PrefixCounter.Mode> action = (n, mode) -> {

            PrefixCounter.Node cur = n;
            //if (cur.getTerminals() > 0)
            //    return; // a prefix or a suffix can never be the whole protein id
            double pct = cur.getHits() / (double) total;
            if (pct < pctMin || pct > pctMax) {
              return;
            }
            sb.setLength(0);
            while (cur != null) {
              if (cur.parent != null) {
                sb.append(cur.ch);
              }
              cur = cur.parent;
            }

            if (sb.length() < 2) {
              return; // no prefixes smaller than 2 characters
            }

            StringBuilder sbPrint = sb
                .reverse();// mode == PrefixCounter.Mode.REV ? sb.reverse() : sb;
            result.add(new Tuple2<>(sbPrint.toString(), pct));
            //System.out.printf("%s : (full string: %s) hits=%.1f%%\n", n, sbPrint.toString(), pct*100d);
          };
//                    System.out.println("Prefixes:");
          cntFwd.iterPrefixCounts(maxDepth, action);
          prefixesByCol.add(cleanUpDecoyTagCandidates(result));
//                    for (Tuple2<String, Double> tuple2 : cleanedResult) {
//                        System.out.printf("% 3.1f%% -> %s\n", tuple2.item2 * 100d, tuple2.item1);
//                    }
//                    System.out.println("Prefixes Done");
        }

        { // suffixes
          final List<Tuple2<String, Double>> result = new ArrayList<>();
          Proc2<PrefixCounter.Node, PrefixCounter.Mode> action = new Proc2<PrefixCounter.Node, PrefixCounter.Mode>() {
            @Override
            public void call(PrefixCounter.Node n, PrefixCounter.Mode mode) {

              PrefixCounter.Node cur = n;
              //if (cur.getTerminals() > 0)
              //    return; // a prefix or a suffix can never be the whole protein id
              double pct = cur.getHits() / (double) total;
              if (pct < pctMin || pct > pctMax) {
                return;
              }
              sb.setLength(0);
              while (cur != null) {
                if (cur.parent != null) {
                  sb.append(cur.ch);
                }
                cur = cur.parent;
              }

              if (sb.length() < 2) {
                return; // no suffixes smaller than 2 chars
              }

              StringBuilder sbPrint = sb;// mode == PrefixCounter.Mode.REV ? sb.reverse() : sb;
              result.add(new Tuple2<>(sbPrint.toString(), pct));
            }
          };
          cntRev.iterPrefixCounts(maxDepth, action);
          suffixesByCol.add(cleanUpDecoyTagCandidates(result));
        }
      }

      int totalCandidates = 0;
      int supportedPrefixes = 0;
      int totalPrefixes = 0;
      int totalSuffixes = 0;
      for (int i = 0; i < prefixesByCol.size(); i++) {
        List<Tuple2<String, Double>> list = prefixesByCol.get(i);
        totalCandidates += list.size();
        totalPrefixes += list.size();
        if (i == 0) {
          supportedPrefixes = list.size();
        }
      }
      for (List<Tuple2<String, Double>> list : suffixesByCol) {
        totalCandidates += list.size();
        totalSuffixes += list.size();
      }

      String selectedPrefix = null;
      if (totalCandidates == 0) {
        String msg = "No candidates for decoy tags found";
        String[] options = {"Ok"};
        int result = JOptionPane.showOptionDialog(this, msg, "Nothing found",
            JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);

      } else if (supportedPrefixes == 1) {
        // good, we've found the one good decoy prefix
        Tuple2<String, Double> prefix = prefixesByCol.get(0).get(0);
        StringBuilder sb = new StringBuilder();
        sb.append(String.format(Locale.ROOT,
            "Found candidate decoy tag: \n\"%s\" in % 3.1f%% entries", prefix.item1,
            prefix.item2 * 100d));
        sb.append("\n\nAll found candidates:");
        appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
        String[] options = {"Set \"" + prefix.item1 + "\" as decoy tag", "Cancel"};
        int result = JOptionPane.showOptionDialog(this, sb.toString(), "Found prefix",
            JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
        if (result == 0) {
          selectedPrefix = prefix.item1;
        }

      } else if (supportedPrefixes > 1) {
        // several possible prefixes found
        StringBuilder sb = new StringBuilder();
        sb.append("Found several possible supported decoy tag prefixes.\n")
            .append("Note: only prefixes in the 1st column are supported by downstream tools.\n");
        appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
        sb.append("\nOnly supported variants are lsited on buttons below.\n");

        List<Tuple2<String, Double>> supported = prefixesByCol.get(0);
        String[] options = new String[supported.size() + 1];
        options[options.length - 1] = "Cancel";
        for (int i = 0; i < supported.size(); i++) {
          options[i] = String.format("Set \"%s\"", supported.get(i).item1);
        }
        int result = JOptionPane
            .showOptionDialog(this, sb.toString(), "Found several possible prefixes",
                JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options,
                options[0]);
        if (result >= 0 && result < options.length - 1) {
          selectedPrefix = supported.get(result).item1;
        }

      } else if (supportedPrefixes == 0) {
        // no prefixes found - this is not supported by downstream tools
        StringBuilder sb = new StringBuilder();
        sb.append("No supported decoy tag prefixes found.\n")
            .append("However found other possible decoy markers, listed below.\n")
            .append("Note: only prefixes in the 1st column are supported by downstream tools.\n");
        appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
        String[] options = {"Ok"};
        int result = JOptionPane.showOptionDialog(this, sb.toString(),
            "Found incompatible decoy marker candidates",
            JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);
      }
      if (selectedPrefix != null) {
        updateDecoyTagSeqDb(selectedPrefix, false);
      }

    } catch (IOException ex) {
      JOptionPane.showMessageDialog(btnTryDetectDecoyTag,
          "<html>Error reading sequence database file", "Error",
          JOptionPane.ERROR_MESSAGE);
    }
  }//GEN-LAST:event_btnTryDetectDecoyTagActionPerformed

  private void appendFoundPrefixes(StringBuilder sb,
      List<List<Tuple2<String, Double>>> prefixesByCol,
      List<List<Tuple2<String, Double>>> suffixesByCol) {

    int totalPrefixes = 0;
    int totalSuffixes = 0;
    for (int i = 0; i < prefixesByCol.size(); i++) {
      List<Tuple2<String, Double>> list = prefixesByCol.get(i);
      totalPrefixes += list.size();
    }
    for (List<Tuple2<String, Double>> list : suffixesByCol) {
      totalSuffixes += list.size();
    }

    final String tab1 = "  ";
    final String tab2 = tab1 + tab1;

    if (totalPrefixes > 0) {
      sb.append(tab1).append("\nPrefixes:\n");
      for (int i = 0; i < prefixesByCol.size(); i++) {
        for (Tuple2<String, Double> tuple2 : prefixesByCol.get(i)) {
          sb.append(tab2).append(String.format("\tColumn #%d: \"%s\" in % 3.1f%% entries\n",
              i + 1, tuple2.item1, tuple2.item2 * 100d));
        }
      }
    }

    if (totalSuffixes > 0) {
      sb.append(tab1).append("\nSuffixes:\n");
      for (int i = 0; i < suffixesByCol.size(); i++) {
        for (Tuple2<String, Double> tuple2 : suffixesByCol.get(i)) {
          sb.append(tab2).append(String.format("\tColumn #%d: \"%s\" in % 3.1f%% entries\n",
              i + 1, tuple2.item1, tuple2.item2 * 100d));
        }
      }
    }

  }

  private List<Tuple2<String, Double>> cleanUpDecoyTagCandidates(
      List<Tuple2<String, Double>> candidates) {
    List<Tuple2<String, Double>> result = new ArrayList<>();

    Collections.sort(candidates, (t1, t2) -> {
      int cmp0 = Double.compare(Math.abs(t1.item2 - 0.5), Math.abs(t2.item2 - 0.5));
      if (cmp0 == 0) {
        cmp0 = t2.item1.compareTo(t1.item1);
      }
      return cmp0;
    });

    for (Tuple2<String, Double> cur : candidates) {
      boolean isBest = true;
      for (Tuple2<String, Double> other : candidates) {
        if (Double.compare(other.item2, cur.item2) == 0) {
          String curStr = cur.item1;
          String othStr = other.item1;
          if (othStr.length() > curStr.length()) {
            if (othStr.startsWith(curStr) || othStr.endsWith(curStr)) {
              isBest = false;
              break;
            }
          }
        }
      }
      if (isBest) {
        result.add(cur);
      }
    }
    return result;
  }

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

  private void textReportLabelfreeActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textReportLabelfreeActionPerformed
    // TODO add your handling code here:
  }//GEN-LAST:event_textReportLabelfreeActionPerformed

  private void textReportLabelfreeFocusGained(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportLabelfreeFocusGained
    textLabelfreeFocusGained = textReportLabelfree.getText();
  }//GEN-LAST:event_textReportLabelfreeFocusGained

  private void textReportLabelfreeFocusLost(
      java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportLabelfreeFocusLost
    validateAndSaveLabelfree(null);
  }//GEN-LAST:event_textReportLabelfreeFocusLost

  private void textBinMsfraggerActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textBinMsfraggerActionPerformed
    // TODO add your handling code here:
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

  private void checkCreateReportActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkCreateReportActionPerformed
    final boolean selected = checkCreateReport.isSelected();
    Container[] comps = new Container[]{
        panelReportOptions
    };
    for (Container c : comps) {
      SwingUtils.enableComponents(c, selected);
    }
  }//GEN-LAST:event_checkCreateReportActionPerformed

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

  private void checkGenerateSpecLibActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkGenerateSpecLibActionPerformed
    ThisAppProps.save(checkGenerateSpecLib, ThisAppProps.PROP_SPECLIBGEN_RUN);
  }//GEN-LAST:event_checkGenerateSpecLibActionPerformed

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

    List<String> props = Arrays
        .asList(ThisAppProps.PROP_BIN_PATH_PYTHON, ThisAppProps.PROP_BINARIES_IN);
    String fcPath = ThisAppProps.tryFindPath(props, false);
    SwingUtils.setFileChooserPath(fc, fcPath);

    if (JFileChooser.APPROVE_OPTION == fc
        .showOpenDialog(SwingUtils.findParentFrameForDialog(this))) {
      String path = fc.getSelectedFile().getAbsolutePath();
      validateAndSavePython(path, true);
    }
  }//GEN-LAST:event_btnBrowseBinPythonActionPerformed

  private void btnGroupsConsecutiveActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsConsecutiveActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    final int groupNumMaxLen = (int) Math.ceil(Math.log(m.dataSize()));
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < groupNumMaxLen; i++) {
      sb.append("0");
    }
    final DecimalFormat fmt = new DecimalFormat(sb.toString());
    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      final String group = "experiment-" + fmt.format(i + 1);
      m.dataSet(i, new InputLcmsFile(f.path, group));
    }

  }//GEN-LAST:event_btnGroupsConsecutiveActionPerformed

  private void btnGroupsByParentDirActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsByParentDirActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      int count = f.path.getNameCount();
      String group = count - 2 >= 0
          ? f.path.getName(count - 2).toString()
          : f.path.getName(count - 1).toString();
      m.dataSet(i, new InputLcmsFile(f.path, group));
    }
  }//GEN-LAST:event_btnGroupsByParentDirActionPerformed

  private void btnGroupsByFilenameActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsByFilenameActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      String group = f.path.getFileName().toString();
      m.dataSet(i, new InputLcmsFile(f.path, group));
    }
  }//GEN-LAST:event_btnGroupsByFilenameActionPerformed

  private void btnGroupsClearActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsClearActionPerformed
    UniqueLcmsFilesTableModel m = this.tableModelRawFiles;

    for (int i = 0, sz = m.dataSize(); i < sz; i++) {
      InputLcmsFile f = m.dataGet(i);
      m.dataSet(i, new InputLcmsFile(f.path, ThisAppProps.DEFAULT_LCMS_GROUP_NAME));
    }
  }//GEN-LAST:event_btnGroupsClearActionPerformed

  private void btnGroupsAssignToSelectedActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnGroupsAssignToSelectedActionPerformed

    final UniqueLcmsFilesTableModel m = this.tableModelRawFiles;
    final List<String> allFiles = m.dataCopy().stream().map(f -> f.path.toString())
        .collect(Collectors.toList());

    final int[] selectedRows = this.tableRawFiles.getSelectedRows();
    final List<String> selected = Arrays.stream(selectedRows)
        .map(tableRawFiles::convertRowIndexToModel)
        .mapToObj(allFiles::get)
        .collect(Collectors.toList());

    ExperimentNameDialog d = new ExperimentNameDialog(this, true, selected);
    d.setVisible(true);
    if (d.isOk()) {
      final String group = d.getExperimentName();
      for (int selectedRow : selectedRows) {
        int i = tableRawFiles.convertRowIndexToModel(selectedRow);
        InputLcmsFile f = m.dataGet(i);
        m.dataSet(i, new InputLcmsFile(f.path, group));
      }
    }
  }//GEN-LAST:event_btnGroupsAssignToSelectedActionPerformed

  private void btnPrintCommandsActionPerformed(
      java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPrintCommandsActionPerformed
    EventBus.getDefault().post(new MessageRun(true));
  }//GEN-LAST:event_btnPrintCommandsActionPerformed

  private void checkReportAbacusActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkReportAbacusActionPerformed
    ThisAppProps.save(checkReportAbacus, ThisAppProps.PROP_CHECKBOX_REPORT_ABACUS);
  }//GEN-LAST:event_checkReportAbacusActionPerformed

  private void btnCrystalcDefaultsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnCrystalcDefaultsActionPerformed
    int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentFrameForDialog(this),
      "Are you sure you want to load defaults for Crystal-C?\n",
      "Confirmation", JOptionPane.OK_CANCEL_OPTION);
    if (JOptionPane.OK_OPTION != confirmation) {
      return;
    }

    CrystalcParams p = new CrystalcParams();
    p.loadDefault();
    try {
      crystalcParamsToForm(p);
      p.save();
    } catch (IOException e) {
      // don't care
    }
  }//GEN-LAST:event_btnCrystalcDefaultsActionPerformed

  private void chkRunCrystalcActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunCrystalcActionPerformed
    final boolean selected = chkRunCrystalc.isSelected();
    SwingUtils.enableComponents(panelCrystalc, selected, true,
        Collections.singletonList(chkRunCrystalc));

    ThisAppProps.save(ThisAppProps.PROP_CRYSTALC_USE, Boolean.toString(selected));
  }//GEN-LAST:event_chkRunCrystalcActionPerformed

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
    validateAndSavePeptideProphetCmdLineOptions(null, true);
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
  
  public void loadDefaultsCrystalC(SearchTypeProp type) {
    ThisAppProps.loadFromBundle(chkRunCrystalc, ThisAppProps.PROP_CRYSTALC_USE, type);
    SwingUtils.enableComponents(panelCrystalc, chkRunCrystalc.isSelected(), true,
        Collections.singletonList(chkRunCrystalc));
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

  private boolean loadLastCheckboxAbacus() {
    final String checked = ThisAppProps.load(ThisAppProps.PROP_CHECKBOX_REPORT_ABACUS);
    try {
      return Boolean.valueOf(checked);
    } catch (Exception ignored) {
    }
    return false;
  }

  private boolean loadLastProcessGroupsSeparately() {
    final String checked = ThisAppProps.load(ThisAppProps.PROP_CHECKBOX_PROCESS_GROUPS_SEPARATELY);
    try {
      return Boolean.valueOf(checked);
    } catch (Exception ignored) {
    }
    return false;
  }

  private void loadDefaultDecoyTag() {
    String val = ThisAppProps.getLocalBundle().getString(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);

    textReportFilter.setText(val);
    ThisAppProps.save(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG, val);
  }

  private void loadDefaultsReport(SearchTypeProp type, boolean askConfirmation) {

    int confirm1 = JOptionPane.showConfirmDialog(this,
        "<html>Load " + type + " search defaults?");
    if (JOptionPane.YES_OPTION != confirm1) {
      return;
    }

    loadDefaultsReportFilter(type);
    loadDefaultsLabelfree(type);

    if (askConfirmation) {
      int confirm2 = JOptionPane.showConfirmDialog(this,
          "<html>Loaded " + type + " search defaults.<br/><br/>"
              + "Do you want to load defaults <b>for other tools</b> as well?<br/><br/>"
              + "<b>WARNING:</b><br/>"
              + "This will reset MSFragger settings!");
      if (JOptionPane.YES_OPTION != confirm2) {
        return;
      }
    }
    EventBus.getDefault().post(new MessageSearchType(type));
  }

  private void loadLastReportFilter() {
    if (!ThisAppProps.load(textReportFilter, ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER)) {
      loadDefaultsReportFilter(DEFAULT_TYPE);
    }
    removeOldSavedDecoyTagValue(textReportFilter, "--tag");

    // temporary plug for old cached data
    final String loaded = textReportFilter.getText().trim();
    if (loaded != null && !loaded.contains("--prot")) {
      textReportFilter.setText(loaded + " --prot 0.01");
    }
  }

  public void loadDefaultsReportFilter(SearchTypeProp type) {
    ThisAppProps.loadFromBundle(textReportFilter, ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER, type);
  }

  public void loadDefaultsLabelfree(SearchTypeProp type) {
    ThisAppProps.loadFromBundle(textReportLabelfree, ThisAppProps.PROP_TEXTFIELD_LABELFREE, type);
  }

  private void loadLastSequenceDb() {
    String val = ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN);
    if (val != null) {
      textSequenceDbPath.setText(val);
    }
  }

  private void loadLastCrystalc() {
    if (!ThisAppProps.load(chkRunCrystalc, ThisAppProps.PROP_CRYSTALC_USE)) {
      loadDefaultsCrystalC(DEFAULT_TYPE);
    }
    chkRunCrystalcActionPerformed(null);

    try {
      CrystalcParams p = new CrystalcParams();
      crystalcParamsToForm(p);
    } catch (Exception e) {
      // doesn't matter
    }
  }

  private void loadLastFreequant() {
    if (!ThisAppProps.load(textReportLabelfree, ThisAppProps.PROP_TEXTFIELD_LABELFREE)) {
      loadDefaultsLabelfree(DEFAULT_TYPE);
    }
  }

  //endregion

  private void validateAndSavePeptideProphetCmdLineOptions(final String newText,
      boolean updateOtherTags) {
    final JTextComponent comp = textPepProphCmd;
    final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET,
        newText, ValidateTrue.getInstance());

    if (!isValid) {
      return;
    }

    // check if the filter line has changed since focus was gained
    final String savedText = textPepProphetFocusGained;
    final String oldText = savedText != null ? savedText : comp.getText().trim();
    final String updText = newText != null ? newText : comp.getText().trim();

    // newText == null means it was a programmatic update
    if (!updateOtherTags || oldText.equals(updText)) {
      return;
    }

    // text in the field has changed
  }

  private boolean validateAndSaveFastaPath(String path) {
    boolean isValid = validateFastaPath(path);
    if (isValid) {
      textSequenceDbPath.setText(path);
      ThisAppProps.save(ThisAppProps.PROP_DB_FILE_IN, path);
      Thread thread;
      thread = new Thread(() -> {
        Path p = Paths.get(textSequenceDbPath.getText());
        if (!Files.exists(p)) {
          return;
        }
        try (BufferedReader br = new BufferedReader(new InputStreamReader(Files.newInputStream(p),
            StandardCharsets.UTF_8))) {
          String line;
          final List<String> descriptors = new ArrayList<>();
          while ((line = br.readLine()) != null) {
            if (!line.startsWith(">")) {
              continue;
            }
            descriptors.add(line);
          }
          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              String format = "###,###";
              DecimalFormatSymbols otherSymbols = new DecimalFormatSymbols(Locale.ROOT);
              otherSymbols.setDecimalSeparator(',');
              otherSymbols.setGroupingSeparator(' ');
              DecimalFormat df = new DecimalFormat(format, otherSymbols);
              lblFastaCount.setText(String.format("%s entries", df.format(descriptors.size())));
            }
          });
        } catch (IOException ex) {
          return;
        }
      });
      thread.start();
    }

    final JComponent anchor = textSequenceDbPath;
    final String name = "textSequenceDbPath";
    BalloonTip tip = tipMap.remove(name);
    if (tip != null) {
      tip.closeBalloon();
    }

    if (!isValid) {
      tip = new BalloonTip(anchor, "<html>Could not find sequence DB file.");
      tip.setVisible(true);
      tipMap.put(name, tip);
    }

    return isValid;
  }

  private boolean validateFastaPath(String path) {
    if (StringUtils.isNullOrWhitespace(path)) {
      return false;
    }
    try {
      Path p = Paths.get(path).toAbsolutePath();
      return Files.exists(p) && !Files.isDirectory(p);
    } catch (Exception e) {
      return false;
    }
  }

  private void validateAndSaveDecoyTagSeqDb(final String newText, boolean updateOtherTags) {

    final JTextComponent comp = textDecoyTagSeqDb;
    final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXTFIELD_DECOY_TAG,
        newText, ValidateTrue.getInstance());

    if (!isValid) {
      return;
    }

    // check if the filter line has changed since focus was gained
    final String savedText = textDecoyTagFocusGained;
    final String oldText = savedText != null ? savedText : comp.getText().trim();
    final String updText = newText != null ? newText : comp.getText().trim();

    // newText == null means it was a programmatic update
    if (!updateOtherTags || oldText.equals(updText)) {
      return;
    }

//    final String message = String.format(Locale.ROOT,
//        "Decoy prefix has changed: from '%s', to '%s'.\n", oldText, updText);
//    int ans = JOptionPane
//        .showConfirmDialog(this, message, "Decoy prefix change", JOptionPane.YES_NO_OPTION);
//    if (ans == JOptionPane.YES_OPTION) {
//      // do something
//    }
  }

  private void updateTextCmdLine(Pattern re, JTextComponent textComp, String newVal,
      String prefix) {
    String line = textComp.getText();
    Matcher m = re.matcher(line);
    String newText;

    if (StringUtils.isNullOrWhitespace(newVal)) {
      // the new value is a zero length string, i.e. it was deleted
      newText = m.replaceAll("");
    } else if (m.find()) {
      // replace all previous instances
      newText = m.replaceAll(String.format(Locale.ROOT, "%s %s", prefix, newVal));
    } else {
      // if it didn't have decoy tag, add it at the end
      newText = String.format(Locale.ROOT, "%s %s %s", line, prefix, newVal);
    }
    textComp.setText(newText);
  }

  private void updateDecoyTagSeqDb(String newVal, boolean updateOtherTags) {
    textDecoyTagSeqDb.setText(newVal);
    validateAndSaveDecoyTagSeqDb(null, updateOtherTags);
  }

  private void addChangeListenerTextSequenceDb() {
    SwingUtils.addChangeListener(textSequenceDbPath, e -> {
      if (btnTryDetectDecoyTag != null) {
        btnTryDetectDecoyTag
            .setEnabled(!StringUtils.isNullOrWhitespace(textSequenceDbPath.getText()));
      }
    });

  }

  private void initEditorPaneSeqDb() {
    // for copying style
    JLabel label = new JLabel();
    Font font = label.getFont();

    // create some css from the label's font
    StringBuilder style = new StringBuilder("font-family:" + font.getFamily() + ";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");

    JEditorPane ep = editorSequenceDb;
    ep.setContentType("text/html");
    ep.setText("<html><body style=\"" + style + "\">"
        + "If you don't have a protein database, you can use Philosopher from the command line to download "
        + "one from UniProt, e.g.:<br/><br/>"
        + "&nbsp;&nbsp;&nbsp;&nbsp;philosopher.exe database --prefix rev_ --contam --id UP000005640<br/>"
        + "<br/>"
        + "To find the proteome ID for the above command or download other databases visit "
        + "<a href=\"http://www.uniprot.org/proteomes/\">UniProt website</a><br/>"
        + "<br/>"
        + "PeptideProphet and other downstream tools require the decoy tag to be a prefix to the whole protein "
        + "string in FASTA file.<br/>"
        + "Examples of compatible formats:"
        + "<ul>"
        + "<li>&gt;<b>rev_</b>tr|J3KNE0|J3KNE0_HUMAN RanBP2-like and GRIP domain-containing protein</li>"
        + "<li>&gt;<b>DECOY_</b>tr|J3KNE0|J3KNE0_HUMAN RanBP2-like and GRIP domain-containing protein</li>"
        + "</ul>"
        + "Examples of <b>incompatible</b> formats:"
        + "<ul>"
        + "<li>&gt;tr<b>_REVERSED</b>|J3KNE0|J3KNE0_HUMAN ...</li>"
        + "<li>&gt;tr|<b>fake_</b>J3KNE0|J3KNE0_HUMAN RanBP2-like ...</li>"
        + "<li>&gt;tr|J3KNE0<b>_DECOY</b>|J3KNE0_HUMAN ...</li>"
        + "</ul>"
        + ""
        + "</body></html>");

    // handle link messages
    ep.addHyperlinkListener(e -> {
      if (e.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
        try {
          Desktop.getDesktop().browse(e.getURL().toURI());
        } catch (URISyntaxException | IOException ex) {
          Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
      }
    });
    ep.setEditable(false);
    ep.setBackground(label.getBackground());
  }


  public CrystalcParams crystalcFormToParams() throws IOException {
    CrystalcParams p = new CrystalcParams();

    p.setFasta(textSequenceDbPath.getText());
    p.setIsoNum((Integer) spinnerCrystalcNumIsotopes.getValue());
    p.setMassTol((Double) spinnerCrystalcMassTol.getValue());
    p.setMaxZ((Integer) spinnerCrystalcMaxCharge.getValue());
    p.setPrecursorIsolationWindow((Double) spinnerCrystalcPrecIsoWindow.getValue());
    int threads = fraggerMigPanel.getThreads();
    threads = threads > 0 ? threads : -1;
    p.setThread(threads);

    p.save();

    return p;
  }

  private void crystalcParamsToForm(CrystalcParams p) throws IOException {

    double massTol = p.getMassTol();
    spinnerCrystalcMassTol.setValue(massTol);
    int maxZ = p.getMaxZ();
    spinnerCrystalcMaxCharge.setValue(maxZ);
    int isoNum = p.getIsoNum();
    spinnerCrystalcNumIsotopes.setValue(isoNum);
    double precursorIsolationWindow = p.getPrecursorIsolationWindow();
    spinnerCrystalcPrecIsoWindow.setValue(precursorIsolationWindow);

    p.save();
  }

  private void validateAndSaveLabelfree(final String newText) {
    final JTextComponent comp = textReportLabelfree;
    IValidateString validator = s -> true;
    final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXTFIELD_LABELFREE,
        newText, validator);
  }

  private boolean validateAndSavePhilosopherPath(final String path) {

    Path p = null;
    try {
      p = Paths.get(path);
    } catch (Exception e) {
      // path not parseable
    }

    if (p == null || !Files.exists(p) || Files.isDirectory(p)) {
      // invalid input
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          if (balloonPhilosopher != null) {
            balloonPhilosopher.closeBalloon();
            balloonPhilosopher = null;
          }

          JEditorPane ep = SwingUtils.createClickableHtml(String.format(
              "<html>Could not find Philosopher binary file at this location.<br/>\n"
                  + "Corresponding panel won't be active.<br/><br/>"
                  + "<b>If that's the first time you're using %s</b>,<br/>"
                  + "you will need to <a href=\"%s\">download Philosopher (click here)</a> first.<br/>"
                  + "Use the button on the right to proceed to the download website.",
              Version.PROGRAM_TITLE, PhilosopherProps.getProperties().getProperty(PhilosopherProps.PROP_DOWNLOAD_URL, "")));

          balloonPhilosopher = new BalloonTip(textBinPhilosopher, ep,
              new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);

          balloonPhilosopher.setVisible(true);
          enablePhilosopherPanels(false);
        }
      });
      return false;
    }

    final boolean isPathAbsolute = p.isAbsolute();
    if (p.isAbsolute()) {
      p = p.normalize().toAbsolutePath();
    }
    final boolean isPathExists = Files.exists(p);
    final boolean isPathRunnable = Files.isExecutable(p);

    final String validatedPath = validatePhilosopherPath(path);
    final boolean isPathValid = validatedPath != null;

    if (isPathValid) {
      textBinPhilosopher.setText(validatedPath);
      ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER, validatedPath);
    }

    Thread t = new Thread(() -> {
      if (balloonPhilosopher != null) {
        balloonPhilosopher.closeBalloon();
        balloonPhilosopher = null;
      }

      final StringBuilder sb = new StringBuilder();
      boolean needsDisplay = false;
      if (isPathAbsolute) {
        sb.append("<html>Absolute path for Philosopher binary provided: <br/>\n")
            .append(path).append("<br/>\n");
        if (!isPathExists) {
          sb.append("\nBut the file does not exist.");
          needsDisplay = true;
        } else if (!isPathRunnable) {
          sb.append("\nBut the file is not runnable.");
          needsDisplay = true;
          if (OsUtils.isWindows()) {
            sb.append("Right click the file, Properties -> Security -> Advanced<br/>\n")
                .append("And change the executable permissions for the file.<br/>\n")
                .append("All the security implications are your responsibility.");
          } else {
            sb.append("Check that the file has execute permission for the JVM.<br/>\n")
                .append("Or you can just try `chmod a+x <philosopher-binary-file>`.<br/>\n")
                .append("All the security implications are your responsibility.");
          }
        } else if (!isPathValid) {
          sb.append("\nBut the file is invalid. It can't be run by the JVM.");
          needsDisplay = true;
        }
      } else {
        // relative path given, i.e. philosopher must be on PATH
        sb.append("<html>Relative path for Philosopher binary provided: <br/>\n")
            .append(path).append("<br/>\n");
        if (!isPathValid) {
          sb.append("But it couldn't be launched properly for some reason.");
          needsDisplay = true;
        }
      }

      if (needsDisplay) {
        SwingUtilities.invokeLater(new Runnable() {
          @Override
          public void run() {
            if (balloonPhilosopher != null) {
              balloonPhilosopher.closeBalloon();
            }
            balloonPhilosopher = new BalloonTip(textBinPhilosopher, sb.toString());
            balloonPhilosopher.setVisible(true);
          }
        });
      } else {
        validatePhilosopherVersion(validatedPath);
      }
      enablePhilosopherPanels(isPathValid);
    });
    t.start();

    return isPathValid;
  }

  private String validatePhilosopherPath(String path) {
    return PathUtils.testBinaryPath(path);
  }

  private Color getLighterColor(Color original, float alpha) {
    HSLColor hslColor = new HSLColor(original);
    Color lighter = HSLColor.toRGB(hslColor.getHSL(), alpha);
    return lighter;
  }

  private void downloadPhilosopher() {

    try {
      Desktop.getDesktop()
          .browse(URI.create("https://github.com/Nesvilab/philosopher/releases/latest"));
    } catch (IOException ex) {
      Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
    }
  }

  private void resetRunButtons(boolean runEnabled) {
    btnRun.setEnabled(runEnabled);
    btnStop.setEnabled(!runEnabled);
  }

  private Map<String, LcmsFileGroup> getLcmsFileGroups() {
    List<InputLcmsFile> lcmsInputs = tableModelRawFiles.dataCopy();
    Map<String, List<InputLcmsFile>> mapGroup2Files = lcmsInputs.stream()
        .collect(Collectors.groupingBy(inputLcmsFile -> inputLcmsFile.experiment));

    Map<String, LcmsFileGroup> result = new TreeMap<>();
    for (Entry<String, List<InputLcmsFile>> e : mapGroup2Files.entrySet()) {
      result.put(e.getKey(), new LcmsFileGroup(e.getKey(), e.getValue()));
    }

    return result;
  }

  /**
   * Get the name of the file less the provided suffix.
   *
   * @param path the filename component will be taken
   * @param suffix lowercase suffix
   * @return filename less suffix
   */
  private String getFileNameLessSuffix(Path path, String suffix) {
    String name = path.getFileName().toString();
    int indexOf = name.toLowerCase().indexOf(suffix);
    return indexOf >= 0 ? name.substring(0, indexOf) : name;
  }

  private String createPhilosopherCitationHtml() {
    // for copying style
    Font font = lblFraggerJavaVer.getFont();

    // create some css from the label's font
    StringBuilder style = new StringBuilder();
    style.append("font-family:").append(font.getFamily()).append(";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");

    StringBuilder sb = new StringBuilder();
    sb.append("<html>");

    sb.append("<head>");
    sb.append("</head>");

    sb.append("<body style=\"").append(style.toString()).append("\"");
    //sb.append("<body>");

    sb.append("<p style=\"margin-top: 0\">");
    sb.append("More info: <a href=\"https://nesvilab.github.io/philosopher/\">Philosopher GitHub page</a>");
    sb.append("<br/>");
    sb.append("</p>");

    sb.append("</body>");
    sb.append("</html>");

    return sb.toString();
  }

  private String getFraggerCitationHtml() {

    // for copying style
    Font font = lblFraggerJavaVer.getFont();

    // create some css from the label's font
    StringBuilder style = new StringBuilder();
    style.append("font-family:").append(font.getFamily()).append(";");
    style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
    style.append("font-size:").append(font.getSize()).append("pt;");

    StringBuilder sb = new StringBuilder();
    sb.append("<html>");

    sb.append("<head>");
    sb.append("</head>");

    sb.append("<body style=\"").append(style.toString()).append("\"");
    //sb.append("<body>");

    final Properties p = ThisAppProps.getRemotePropertiesWithLocalDefaults();
    final String linkMsfragger = p.getProperty(MsfraggerProps.PROP_FRAGGER_SITE_URL, "https://nesvilab.github.io/MSFragger/");
    final String linkFragpipe = p.getProperty(ThisAppProps.PROP_FRAGPIPE_SITE_URL, "https://github.com/Nesvilab/FragPipe");
    final String doi= p.getProperty(ThisAppProps.PROP_MANUSCRIPT_DOI, "10.1038/nmeth.4256");
    final String linkManuscript= p.getProperty(ThisAppProps.PROP_MANUSCRIPT_URL, "http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html");

    sb.append("<p style=\"margin-top: 0\">");
    sb.append("<b>Please cite: </b>");
    sb.append(
        "<a href=\"").append(linkManuscript).append("\">MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry–based proteomics</a>");
    sb.append("<br/>");
    sb.append("<b>DOI: </b>").append(doi);
    sb.append("</p>");

    sb.append("<p style=\"margin-top: 10\">");
    sb.append("More info and docs: <a href=\"").append(linkMsfragger).append("\">MSFragger website</a>")
        .append(", <a href=\"").append(linkFragpipe).append("\">FragPipe GitHub page</a>");

    sb.append("</body>");
    sb.append("</html>");

    return sb.toString();
  }


  /**
   * @param args the command line arguments
   */
  public static void main(String args[]) {
    /* Set the Nimbus look and feel */
    //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
    /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
     * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html
     */

    ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);
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
      final MsfraggerGuiFrame frame = new MsfraggerGuiFrame();

      frame.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          EventBus.getDefault().post(new MessageSaveCache());
        }


      });

      Thread.setDefaultUncaughtExceptionHandler((t, e) -> {
        String stacktrace = LogUtils.stacktrace(e);

        log.debug("Something unexpected happened (1)", e);
        return;

//        JPanel panel = new JPanel();
//        panel.setLayout(new BorderLayout());
//        panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
//        panel.add(new JLabel("Something unexpected happened (1)"), BorderLayout.PAGE_START);
//        JTextArea notesArea = new JTextArea(40, 80);
//        notesArea.setText(stacktrace);
//        JScrollPane notesScroller = new JScrollPane();
//        notesScroller.setBorder(BorderFactory.createTitledBorder("Details: "));
//        notesScroller.setViewportView(notesArea);
//        panel.add(notesScroller, BorderLayout.CENTER);
//        //JOptionPane.showMessageDialog(frame, "Some error details:\n\n" + notes, "Error", JOptionPane.ERROR_MESSAGE);
//        //JOptionPane.showMessageDialog(frame, panel, "Error", JOptionPane.ERROR_MESSAGE);
//        showDialog(frame, panel);
      });

      frame.setVisible(true);
      Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
      frame.setLocation(dim.width / 2 - frame.getSize().width / 2,
          dim.height / 2 - frame.getSize().height / 2);
    });
  }

  private static void showDialog(Component frame, final Component component) {
    // wrap a scrollpane around the component
    JScrollPane scrollPane = new JScrollPane(component);
    // make the dialog resizable
    component.addHierarchyListener(e -> {
      Window window = SwingUtilities.getWindowAncestor(component);
      if (window instanceof Dialog) {
        Dialog dialog = (Dialog) window;
        if (!dialog.isResizable()) {
          dialog.setResizable(true);
        }
      }
    });
    // display them in a message dialog
    JOptionPane.showMessageDialog(frame, scrollPane);
  }

  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JButton btnAbout;
  private javax.swing.JButton btnAboutInConfig;
  private javax.swing.JButton btnBrowse;
  private javax.swing.JButton btnBrowseBinPython;
  private javax.swing.JButton btnClearCache;
  private javax.swing.JButton btnClearConsole;
  private javax.swing.JButton btnCrystalcDefaults;
  private javax.swing.JButton btnExportLog;
  private javax.swing.JButton btnFindTools;
  private javax.swing.JButton btnGroupsAssignToSelected;
  private javax.swing.JButton btnGroupsByFilename;
  private javax.swing.JButton btnGroupsByParentDir;
  private javax.swing.JButton btnGroupsClear;
  private javax.swing.JButton btnGroupsConsecutive;
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
  private javax.swing.JButton btnSelectWrkingDir;
  private javax.swing.JButton btnStop;
  private javax.swing.JButton btnTryDetectDecoyTag;
  private javax.swing.JCheckBox checkCombinedPepxml;
  private javax.swing.JCheckBox checkCreateReport;
  private javax.swing.JCheckBox checkDryRun;
  private javax.swing.JCheckBox checkEnableDiaumpire;
  private javax.swing.JCheckBox checkGenerateSpecLib;
  private javax.swing.JCheckBox checkLabelfree;
  private javax.swing.JCheckBox checkProcessGroupsSeparately;
  private javax.swing.JCheckBox checkReportAbacus;
  private javax.swing.JCheckBox chkRunCrystalc;
  private javax.swing.JCheckBox chkRunPeptideProphet;
  private javax.swing.JCheckBox chkRunProteinProphet;
  private javax.swing.JScrollPane consoleScrollPane;
  private javax.swing.JEditorPane editorMsfraggerCitation;
  private javax.swing.JEditorPane editorPhilosopherLink;
  private javax.swing.JEditorPane editorSequenceDb;
  private javax.swing.JEditorPane epDbsliceInfo;
  private javax.swing.JEditorPane epSpeclibInfo2;
  private javax.swing.JLabel jLabel1;
  private javax.swing.JLabel jLabel10;
  private javax.swing.JLabel jLabel11;
  private javax.swing.JLabel jLabel12;
  private javax.swing.JLabel jLabel2;
  private javax.swing.JLabel jLabel3;
  private javax.swing.JLabel jLabel34;
  private javax.swing.JLabel jLabel4;
  private javax.swing.JLabel jLabel40;
  private javax.swing.JLabel jLabel5;
  private javax.swing.JLabel jLabel6;
  private javax.swing.JLabel jLabel7;
  private javax.swing.JLabel jLabel8;
  private javax.swing.JLabel jLabel9;
  private javax.swing.JPanel jPanel1;
  private javax.swing.JPanel jPanel2;
  private javax.swing.JPanel jPanel3;
  private javax.swing.JPanel jPanel4;
  private javax.swing.JScrollPane jScrollPane1;
  private javax.swing.JScrollPane jScrollPane2;
  private javax.swing.JScrollPane jScrollPane3;
  private javax.swing.JScrollPane jScrollPane4;
  private javax.swing.JScrollPane jScrollPane5;
  private javax.swing.JScrollPane jScrollPane6;
  private javax.swing.JScrollPane jScrollPane8;
  private javax.swing.JLabel lblDbsliceInfo1;
  private javax.swing.JLabel lblFastaCount;
  private javax.swing.JLabel lblFindAutomatically;
  private javax.swing.JLabel lblFraggerJavaVer;
  private javax.swing.JLabel lblOutputDir;
  private javax.swing.JLabel lblPhilosopherInfo;
  private javax.swing.JLabel lblPythonInfo;
  private javax.swing.JLabel lblSpeclibInfo1;
  private javax.swing.JPanel panelConfig;
  private javax.swing.JPanel panelCrystalc;
  private javax.swing.JPanel panelCrystalcOptions;
  private javax.swing.JPanel panelDbInfo;
  private javax.swing.JPanel panelDownstream;
  private javax.swing.JPanel panelMsfraggerConfig;
  private javax.swing.JPanel panelPeptideProphet;
  private javax.swing.JPanel panelPeptideProphetOptions;
  private javax.swing.JPanel panelPhilosopherConfig;
  private javax.swing.JPanel panelProteinProphet;
  private javax.swing.JPanel panelProteinProphetOptions;
  private javax.swing.JPanel panelReport;
  private javax.swing.JPanel panelReportOptions;
  private javax.swing.JPanel panelRun;
  private javax.swing.JPanel panelSelectFiles;
  private javax.swing.JPanel panelSelectedFiles;
  private javax.swing.JPanel panelSequenceDb;
  private javax.swing.JPanel panelSpecLibOpts;
  private javax.swing.JScrollPane scrollEpSpeclibInfo2;
  private javax.swing.JScrollPane scrollPaneRawFiles;
  private javax.swing.JSpinner spinnerCrystalcMassTol;
  private javax.swing.JSpinner spinnerCrystalcMaxCharge;
  private javax.swing.JSpinner spinnerCrystalcNumIsotopes;
  private javax.swing.JSpinner spinnerCrystalcPrecIsoWindow;
  private javax.swing.JTabbedPane tabPane;
  private javax.swing.JTextField textBinMsfragger;
  private javax.swing.JTextField textBinPhilosopher;
  private javax.swing.JTextField textBinPython;
  private javax.swing.JTextField textDecoyTagSeqDb;
  private javax.swing.JTextArea textPepProphCmd;
  private javax.swing.JTextField textReportFilter;
  private javax.swing.JTextField textReportLabelfree;
  private javax.swing.JTextField textSequenceDbPath;
  private javax.swing.JTextArea txtProteinProphetCmdLineOpts;
  private javax.swing.JTextField txtWorkingDir;
  // End of variables declaration//GEN-END:variables
}
