package com.dmtavt.fragger.ui;

import com.dmtavt.fragger.ui.messages.MessageExportLog;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
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
import java.util.function.Predicate;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTabbedPane;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
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

  TextConsole console;
  JLabel defFont;


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
        log.debug("Something unexpected happened!", e);
        SwingUtils.userShowError(fp, stacktrace);
      });

      LogbackJTextPaneAppender appender = new LogbackJTextPaneAppender();
      appender.start();
      log.debug("Started LogbackJTextPaneAppender logger");
      appender.setTextPane(fp.console);

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
    JTabbedPane tabs = new JTabbedPane(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);
    
  }

  private void initMore() {

    setTitle(Version.PROGRAM_TITLE + " (v" + Version.version() + ")");
    setLocale(Locale.ROOT);
    defFont = new JLabel("dummy label to get default font from");
    console = createConsole();

    {
      JLabel c = new JLabel();
      c.setFont(defFont.getFont());
      c.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      c.setAlignmentX(Component.CENTER_ALIGNMENT);
      c.setText(
          "<html>Tabs on top represent processing steps and will be performed sequentially.<br/>\n"
              + "Tabs will become enabled once the tools on this panel are configured.");
      panelBottomHints.add(c);
    }

    {
      Properties props = ThisAppProps.getRemotePropertiesWithLocalDefaults();
//      Properties props = ThisAppProps.getLocalProperties(); // for testing
      String linkUrl = props.getProperty(ThisAppProps.PROP_SETUP_TUTORIAL_URL,
          "https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html");

      JEditorPane c = SwingUtils.createClickableHtml(
          "<a href='" + linkUrl + "'>Configuration Help</a>");
      c.setFont(defFont.getFont());
      c.setAlignmentX(Component.CENTER_ALIGNMENT);
      JPanel p = new JPanel();
      p.setAlignmentX(Component.CENTER_ALIGNMENT);
      p.add(c);
      panelBottomHints.add(p);
    }



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
    EventBus.getDefault().post(MessageLoadAllForms.forCaching());

    initActions();
  }
}
