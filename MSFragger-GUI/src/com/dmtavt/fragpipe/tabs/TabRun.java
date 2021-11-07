package com.dmtavt.fragpipe.tabs;

import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeRun;
import com.dmtavt.fragpipe.Version;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.cmd.CmdMsfragger;
import com.dmtavt.fragpipe.messages.MessageAppendToConsole;
import com.dmtavt.fragpipe.messages.MessageClearConsole;
import com.dmtavt.fragpipe.messages.MessageExportLog;
import com.dmtavt.fragpipe.messages.MessageExternalProcessOutput;
import com.dmtavt.fragpipe.messages.MessageKillAll;
import com.dmtavt.fragpipe.messages.MessageKillAll.REASON;
import com.dmtavt.fragpipe.messages.MessagePrintToConsole;
import com.dmtavt.fragpipe.messages.MessageRun;
import com.dmtavt.fragpipe.messages.MessageRunButtonEnabled;
import com.dmtavt.fragpipe.messages.MessageSaveAsWorkflow;
import com.dmtavt.fragpipe.messages.MessageSaveLog;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import com.github.chhh.utils.swing.FormEntry;
import com.github.chhh.utils.swing.JPanelWithEnablement;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.TextConsole;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TabRun extends JPanelWithEnablement {
  private static final Logger log = LoggerFactory.getLogger(TabRun.class);
  public static final MigUtils mu = MigUtils.get();
  public static final String TAB_PREFIX = "tab-run.";
  private static final String LAST_WORK_DIR = "workdir.last-path";
  private static final String PROP_FILECHOOSER_LAST_PATH = TAB_PREFIX + "filechooser.last-path";
  final TextConsole console;
  Color defTextColor;
  private UiText uiTextWorkdir;
  private UiCheck uiCheckDryRun;
  private JButton btnRun;
  private JPanel pTop;
  private JPanel pConsole;
  private UiCheck uiCheckWordWrap;

  public TabRun(TextConsole console) {
    this.console = console;
    init();
    initMore();
  }

  private void initMore() {
    //SwingUtils.renameDeep(this, false, TAB_PREFIX, null);
    Bus.registerQuietly(this);
    Bus.postSticky(this);
  }

  private void clearConsole() {
    console.setText("");
  }

  public String getWorkdirText() {
    return uiTextWorkdir.getNonGhostText();
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void on(MessageClearConsole m) {
    clearConsole();
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void on(MessageRunButtonEnabled m) {
    btnRun.setEnabled(m.isEnabled);
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void on(MessagePrintToConsole m) {
    if(Fragpipe.headless) {
      if (false && m.color.equals(Fragpipe.COLOR_CMDLINE) && !m.text.startsWith("("))
        Fragpipe.cmds.append(m.text).append('\n');
      Fragpipe.out.println(m.text);
    }
    if (Fragpipe.headless) return;
    console.append(m.color, m.text);
    if (m.addNewline) {
      console.append("\n");
    }
    console.getParent().getParent().revalidate();
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void on(MessageExternalProcessOutput m) {
    if (m.output == null) {
      log.warn("MessageExternalProcessOutput with null text, this is a bug, report to devs");
      return;
    }

    // special case, colorize output from MSFragger
    if (CmdMsfragger.NAME.equals(m.procName)) {
      if (m.isError) {
        toConsole(Fragpipe.COLOR_RED_DARKEST, m.output, false);
      } else {
        toConsole(m.output, false);
      }
      return;
    }

    toConsole(null, m.output, false); // print with ANSI colors
  }

  /**
   * @deprecated All console communication needs to be done through the new
   * {@link MessagePrintToConsole} type instead.
   */
  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  @Deprecated
  public void on(MessageAppendToConsole m) {
    console.append(m.color, m.text);
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageRun m) {
    FragpipeRun.run(m);
    if (Fragpipe.headless) {
      System.out.println(Fragpipe.cmds.toString());
      System.exit(0);
    }
  }

  private JPanel createPanelTop(TextConsole console) {
    JButton btnSaveAsWorkflow = UiUtils.createButton("Save current settings as a workflow",
            e -> Bus.post(new MessageSaveAsWorkflow(true)));
    JButton btnAbout = UiUtils.createButton("About", e -> Bus.post(new MessageShowAboutDialog()));
    uiTextWorkdir = UiUtils.uiTextBuilder().cols(30).create();
    FormEntry feWorkdir = mu.feb("workdir", uiTextWorkdir).label("Output dir:")
        .tooltip("Processing results will be stored in this directory").create();
    JButton btnBrowse = feWorkdir
        .browseButton(() -> FileChooserUtils.builder("Select output directory")
                .mode(FcMode.DIRS_ONLY).multi(false)
                .paths(Stream.of(uiTextWorkdir.getNonGhostText(),
                    Fragpipe.propsVar().getProperty(LAST_WORK_DIR))).create(),
            "Select output directory",
            selected -> {
              uiTextWorkdir.setText(selected.get(0).toString());
            }
        );
    JButton btnOpenInFileManager = UiUtils.createButton("Open in File Manager", e -> {
      String text = uiTextWorkdir.getNonGhostText();
      if (StringUtils.isBlank(text)) {
        SwingUtils.showInfoDialog(TabRun.this, "Empty path", "Does not exist");
        return;
      }
      Path existing = PathUtils.existing(text);
      if (existing == null) {
        SwingUtils
            .showInfoDialog(TabRun.this, "Path:\n'" + text + "'\nDoes not exist", "Does not exist");
        return;
      }
      try {
        Desktop.getDesktop().open(existing.toFile());
      } catch (IOException ex) {
        SwingUtils
            .showErrorDialog(TabRun.this, "Could not open path in system file browser.", "Error");
        return;
      }
    });

    uiCheckDryRun = UiUtils.createUiCheck("Dry Run", false);
    btnRun = UiUtils
        .createButton("<html><b>RUN", e -> Bus.post(new MessageRun(isDryRun())));

    JButton btnStop = UiUtils
        .createButton("Stop", e -> {
          Bus.post(new MessageKillAll(REASON.USER_ACTION));
          Path existing = PathUtils.existing(getWorkdirText());
          if (existing != null) {
            Bus.post(MessageSaveLog.saveInDir(existing));
          }
        });
    JButton btnPrintCommands = UiUtils
        .createButton("Print Commands", e -> Bus.post(new MessageRun(true)));
    JButton btnExport = UiUtils.createButton("Export Log", e -> Bus.post(new MessageExportLog()));
    JButton btnReportErrors = UiUtils.createButton("Report Errors", e -> {
      final String prop = Version.isDevBuild() ? Version.PROP_ISSUE_TRACKER_URL_DEV : Version.PROP_ISSUE_TRACKER_URL;
      final String issueTrackerAddress = Fragpipe.getPropFix(prop);
      try {
        Desktop.getDesktop().browse(URI.create(issueTrackerAddress));
      } catch (IOException ex) {
        log.error("Exception while trying to open default browser: {}", ex.getMessage());
        SwingUtils.showErrorDialogWithStacktrace(ex, TabRun.this);
      }
    });
    JButton btnClearConsole = UiUtils.createButton("Clear Console", e -> clearConsole() );
    uiCheckWordWrap = UiUtils
        .createUiCheck("Word wrap", console.getScrollableTracksViewportWidth(), e -> {
          console.setScrollableTracksViewportWidth(uiCheckWordWrap.isSelected());
          console.setVisible(false);
          console.setVisible(true);
        });

    JPanel p = mu.newPanel(null, true);
    mu.add(p, btnSaveAsWorkflow).split().spanX();
    mu.add(p, btnAbout).wrap();
    mu.add(p, feWorkdir.label(), false).split().spanX();
    mu.add(p, feWorkdir.comp).growX();
    mu.add(p, btnBrowse);
    mu.add(p, btnOpenInFileManager).wrap();
    mu.add(p, btnRun).split().spanX();
    mu.add(p, btnStop);
    mu.add(p, uiCheckDryRun).pushX();
    mu.add(p, btnPrintCommands);
    mu.add(p, btnExport);
    mu.add(p, btnReportErrors);
    mu.add(p, btnClearConsole);
    mu.add(p, uiCheckWordWrap).wrap();

    return p;
  }

  public boolean isDryRun() {
    return SwingUtils.isEnabledAndChecked(uiCheckDryRun);
  }

  protected void init() {
    defTextColor = UIManager.getColor("TextField.foreground");
    if (defTextColor == null) {
      defTextColor = Color.BLACK;
    }

    pTop = createPanelTop(console);
    pTop.setPreferredSize(new Dimension(400, 50));
    initConsole(console);
    pConsole = createPanelConsole(console);

    mu.layout(this, mu.lcNoInsetsTopBottom().fillX());
    mu.add(this, pTop).growX().alignY("top").wrap();
    mu.add(this, pConsole).grow().push().alignY("top").wrap();
  }

  private JPanel createPanelConsole(TextConsole tc) {
    JPanel p = mu.newPanel("Console", mu.lcNoInsetsTopBottom());

    JScrollPane scroll = SwingUtils.wrapInScroll(tc);
    scroll.setMinimumSize(new Dimension(400, 50));
    // the editor does not originally occupy the whole width of the viewport
    // so we mask it off with the same color as the console
    scroll.getViewport().setBackground(tc.getBackground());

    mu.add(p, scroll).grow().push().wrap();
    return p;
  }

  private void initConsole(TextConsole console) {
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
        JMenuItem ctxItemExport = new JMenuItem("Export log to text file");
        ctxItemExport.addActionListener(e1 -> {
          Bus.post(new MessageExportLog());
        });
        menu.add(ctxItemExport);
        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    });
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void on(MessageSaveLog m) {
    log.debug("Got MessageSaveLog, trying to save log");
    saveLogToFile(console, m.workDir);
  }

  @Subscribe(threadMode = ThreadMode.MAIN_ORDERED)
  public void on(MessageExportLog m) {
    log.debug("Got MessageExportLog, trying to save log");
    exportLogToFile(console, uiTextWorkdir.getNonGhostText());
  }

  private void exportLogToFile(TextConsole console, String savePathHint) {
    JFileChooser fc = FileChooserUtils.builder("Export log to").approveButton("Save")
        .acceptAll(true).mode(FcMode.FILES_ONLY).multi(false)
        .paths(Seq.of(savePathHint, Fragpipe.propsVarGet(TabRun.PROP_FILECHOOSER_LAST_PATH))).create();
    SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    Date now = new Date();
    fc.setSelectedFile(new File(String.format("log_%s.txt", df.format(now))));

    final Component parent = SwingUtils.findParentFrameForDialog(this);
    if (JFileChooser.APPROVE_OPTION == fc.showSaveDialog(parent)) {
      File selectedFile = fc.getSelectedFile();
      Path path = Paths.get(selectedFile.getAbsolutePath());
      Fragpipe.propsVarSet(TabRun.PROP_FILECHOOSER_LAST_PATH, path.toString());

      // if exists, overwrite
      if (Files.exists(path)) {
        int overwrite = JOptionPane
            .showConfirmDialog(parent, "<html>File exists, overwrtie?<br/><br/>" + path.toString(), "Overwrite",
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
      saveLogToFile(console, path);
    }
  }

  @Deprecated
  public static void saveLogToFileOld(TextConsole console, Path path) {
    final String text = console.getText().replaceAll("[^\n]+\u200B" + System.lineSeparator(), "");
    try (BufferedWriter bufferedWriter = Files.newBufferedWriter(path)) {
      bufferedWriter.write(text);
    } catch (IOException e) {
      log.error("Error writing log to file", e);
    }
  }

  private static void saveLogToFileCreateNew(final String text, final Path path) throws FileAlreadyExistsException {
    try (BufferedWriter bufferedWriter = Files.newBufferedWriter(path, StandardOpenOption.CREATE_NEW)) {
      bufferedWriter.write(text);
    } catch (FileAlreadyExistsException e) {
      throw e;
    } catch (IOException e) {
      log.error("Error writing log to file", e);
    }
  }

  public static void saveLogToFile(final TextConsole console, final Path path) {
    final String text = console.getText().replaceAll("[^\n]+\u200B" + System.lineSeparator(), "");
    Path pathNew = path;
    for (int i = 0; ; ++i) {
      try {
        saveLogToFileCreateNew(text, pathNew);
      } catch (FileAlreadyExistsException e) {
        pathNew = Paths.get(path.toString() + "__" + i);
        continue;
      }
      break;
    }
  }
}
