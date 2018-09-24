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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.AbstractAction;
import javax.swing.Action;
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
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.JTextComponent;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import umich.msfragger.Version;
import static umich.msfragger.gui.FraggerPanel.PROP_FILECHOOSER_LAST_PATH;
import umich.msfragger.gui.api.DataConverter;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.SimpleUniqueTableModel;
import umich.msfragger.gui.api.TableModelColumn;
import umich.msfragger.gui.api.VersionFetcher;
import umich.msfragger.params.pepproph.PeptideProphetParams;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.protproph.ProteinProphetParams;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.params.fragger.MsfraggerProps;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherGithub;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherLocal;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherServer;
import umich.msfragger.util.FileDelete;
import umich.msfragger.util.FileDrop;
import umich.msfragger.util.FileListing;
import umich.msfragger.util.FileMove;
import umich.msfragger.util.GhostText;
import umich.msfragger.util.HSLColor;
import umich.msfragger.util.LogUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.ValidateTrue;
import umich.msfragger.util.VersionComparator;
import umich.swing.console.TextConsole;
import umich.msfragger.util.IValidateString;
import umich.msfragger.util.PrefixCounter;
import umich.msfragger.util.Proc2;
import umich.msfragger.util.Tuple2;

/**
 *
 * @author dattam
 */
public class MsfraggerGuiFrame extends javax.swing.JFrame {

    protected FraggerPanel fraggerPanel;
    protected TextConsole console;
    protected ExecutorService exec;
    private final List<Process> submittedProcesses = new ArrayList<>(100);
    //private static final String TEXT_SAME_SEQ_DB = "<Same as in MSFragger>";
    private Color defTextColor;
    private GhostText ghostTextPepProph;
    private GhostText ghostTextProtProp;
    private BalloonTip balloonMsfragger;
    private BalloonTip balloonPhilosopher;

    private HashMap<String, BalloonTip> tipMap = new HashMap<>();
    private static final String TIP_NAME_FRAGGER_JAVA_VER = "msfragger.java.min.ver";

    SimpleETable tableRawFiles;
    SimpleUniqueTableModel<Path> tableModelRawFiles;
    FileDrop tableRawFilesFileDrop;

    public static final SearchTypeProp DEFAULT_TYPE = SearchTypeProp.closed;

    private String textPepProphetFocusGained = null;
    private String textReportAnnotateFocusGained = null;
    private String textReportFilterFocusGained = null;
    private String textDecoyTagFocusGained = null;
    private String textLabelfreeFocusGained = null;
    
    private Path slicingScriptPath = null;
    private boolean slicingEnabled = false;
    private String pythonCommand = null;
    
    private Pattern reDecoyTagReportAnnotate = Pattern.compile("--prefix\\s+([^\\s]+)");
    private Pattern reDecoyTagReportFilter = Pattern.compile("--tag\\s+([^\\s]+)");
    private Pattern reDecoyTagPepProphCmd = Pattern.compile("--decoy\\s+([^\\s]+)");
    private Pattern reDecoyTagSequenceDb = Pattern.compile("([^\\s]+)");

    private static final String UNKNOWN_VERSION = "Unknown";
    private String fraggerVer = UNKNOWN_VERSION;
    private String philosopherVer = UNKNOWN_VERSION;

    private final String ACTION_EXPORT_LOG = "Export-Log";    

    public MsfraggerGuiFrame() {
        initComponents();
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
                Component parent = SwingUtils.findParentComponentForDialog(MsfraggerGuiFrame.this);
                int saveResult = fc.showSaveDialog(parent);
                if (JFileChooser.APPROVE_OPTION == saveResult) {
                    File selectedFile = fc.getSelectedFile();
                    Path path = Paths.get(selectedFile.getAbsolutePath());
                    // if exists, overwrite
                    if (Files.exists(path)) {
                        int overwrite = JOptionPane.showConfirmDialog(parent, "<html>File exists,<br/> overwrtie?", "Overwrite", JOptionPane.OK_CANCEL_OPTION);
                        if (JOptionPane.OK_OPTION == overwrite) {
                            try {
                                Files.delete(path);
                            } catch (IOException ex) {
                                JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite", JOptionPane.ERROR_MESSAGE);
                                return;
                            }
                        }
                    }
                    try {
                        // save the file
                        byte[] bytes = text.getBytes(StandardCharsets.UTF_8);
                        Files.write(path, bytes, StandardOpenOption.CREATE_NEW);

                    } catch (IOException ex) {
                        JOptionPane.showMessageDialog(parent, "<html>Could not save file: <br/>" + path.toString()
                                + "<br/>" + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                }
            }
        };

        panelRun.getActionMap().put(exportToTextFile.getValue(Action.NAME), exportToTextFile);
    }

    private void initMore() {

        setTitle(Version.PROGRAM_TITLE + " (v" + Version.getVersion() + ")");
        setLocale(Locale.ROOT);

        console = new TextConsole();
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

        exec = Executors.newFixedThreadPool(1);
        fraggerPanel = new FraggerPanel(this);
        scrollPaneMsFragger.setViewportView(fraggerPanel);
        scrollPaneMsFragger.getVerticalScrollBar().setUnitIncrement(16);

        // check if fragger jar points to a correct location
        if (!validateMsfraggerJarContents(textBinMsfragger.getText())) {
            enableMsfraggerPanels(false);
        }

        if (validatePhilosopherPath(textBinPhilosopher.getText()) == null) {
            enablePhilosopherPanels(false);
        }

        tableModelRawFiles = createTableModelRawFiles();
        tableRawFiles = new SimpleETable(tableModelRawFiles);
        tableRawFiles.addComponentsEnabledOnNonEmptyData(btnRawClear);
        tableRawFiles.addComponentsEnabledOnNonEmptySelection(btnRawRemove);
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
        tableRawFilesFileDrop = new FileDrop(panelSelectedFiles, true, new FileDrop.Listener() {
            @Override
            public void filesDropped(File[] files) {
                ArrayList<Path> paths = new ArrayList<>(files.length);
                for (File f : files) {
                    boolean isDirectory = f.isDirectory();
                    if (!isDirectory) {
                        if (FraggerPanel.fileNameExtensionFilter.accept(f)) {
                            paths.add(Paths.get(f.getAbsolutePath()));
                        }
                    } else {
                        PathUtils.traverseDirectoriesAcceptingFiles(f, FraggerPanel.fileNameExtensionFilter, paths);
                    }
                }
                tableModelRawFiles.dataAddAll(paths);
            }
        });

        // check binary paths (can only be done after manual MSFragger panel creation)
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                validateAndSaveMsfraggerPath(textBinMsfragger.getText());
                validateAndSavePhilosopherPath(textBinPhilosopher.getText());
                checkPreviouslySavedParams();
                validatePythonAndSlicingVersion();
            }
        });

        initActions();
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
                        btnLoadDefaultsClosedActionPerformed(null);
                        break;
                    case 2:
                        btnLoadDefaultsOpenActionPerformed(null);
                        break;
                }

                // rewrite the cached params file with a versioned one
                ThisAppProps.save(Version.PROP_VER, Version.VERSION);
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
                    String[] options = {"Ok", "Load defaults for Closed Search", "Load defaults of Open Search"};
                    int result = JOptionPane.showOptionDialog(this, msg, "Cached option automatically replaced",
                            JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
                    switch (result) {
                        case 1:
                            btnLoadDefaultsClosedActionPerformed(null);
                            break;
                        case 2:
                            btnLoadDefaultsOpenActionPerformed(null);
                            break;
                    }
                
                }

                // rewrite the cached params file with a versioned one
                ThisAppProps.save(Version.PROP_VER, Version.VERSION);
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

    private void enableMsfraggerPanels(boolean enabled) {
        SwingUtils.enableComponents(scrollPaneMsFragger, enabled);
        fraggerPanel.getCheckboxIsRunFragger().setSelected(enabled);
    }

    public SimpleUniqueTableModel<Path> createTableModelRawFiles() {
        if (tableModelRawFiles != null) {
            return tableModelRawFiles;
        }
        List<TableModelColumn<Path, ?>> cols = new ArrayList<>();

        TableModelColumn<Path, String> colPath = new TableModelColumn<>(
                "Path (supports Drag & Drop from Explorer)", 
                String.class, false, new DataConverter<Path, String>() {
            @Override
            public String convert(Path data) {
                return data.toString();
            }
        });
        cols.add(colPath);

        tableModelRawFiles = new SimpleUniqueTableModel<>(cols, 0);
        return tableModelRawFiles;
    }

    private String getDefaultPhilosopherBinName() {
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle(Version.PATH_BUNDLE);
        String winName = bundle.getString("default.philosopher.win"); // NOI18N
        String nixName = bundle.getString("default.philosopher.nix"); // NOI18N
        if (OsUtils.isWindows()) {
            return winName;
        }
        return nixName;
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel2 = new javax.swing.JLabel();
        tabPane = new javax.swing.JTabbedPane();
        panelConfig = new javax.swing.JPanel();
        jLabel4 = new javax.swing.JLabel();
        panelMsfraggerConfig = new javax.swing.JPanel();
        btnMsfraggerBinDownload = new javax.swing.JButton();
        btnMsfraggerBinBrowse = new javax.swing.JButton();
        textBinMsfragger = new javax.swing.JTextField();
        lblMsfraggerCitation = new javax.swing.JLabel();
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
        btnLoadDefaultsOpen = new javax.swing.JButton();
        btnLoadDefaultsClosed = new javax.swing.JButton();
        btnAboutInConfig = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        lblPythonInfo = new javax.swing.JLabel();
        lblPythonMore = new javax.swing.JLabel();
        panelSelectFiles = new javax.swing.JPanel();
        panelSelectedFiles = new javax.swing.JPanel();
        btnRawAddFiles = new javax.swing.JButton();
        btnRawClear = new javax.swing.JButton();
        scrollPaneRawFiles = new javax.swing.JScrollPane();
        btnRawAddFolder = new javax.swing.JButton();
        btnRawRemove = new javax.swing.JButton();
        chkProcessEachFiileSeparately = new javax.swing.JCheckBox();
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
        scrollPaneMsFragger = new javax.swing.JScrollPane();
        panelPeptideProphet = new javax.swing.JPanel();
        chkRunPeptideProphet = new javax.swing.JCheckBox();
        panelPeptideProphetOptions = new javax.swing.JPanel();
        jLabel34 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        textPepProphCmd = new javax.swing.JTextArea();
        btnPepProphDefaultsClosed = new javax.swing.JButton();
        btnPepProphDefaultsOpen = new javax.swing.JButton();
        panelProteinProphet = new javax.swing.JPanel();
        chkRunProteinProphet = new javax.swing.JCheckBox();
        panelProteinProphetOptions = new javax.swing.JPanel();
        jScrollPane4 = new javax.swing.JScrollPane();
        txtProteinProphetCmdLineOpts = new javax.swing.JTextArea();
        jLabel40 = new javax.swing.JLabel();
        chkProteinProphetInteractStar = new javax.swing.JCheckBox();
        txtCombinedProtFile = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        btnProtProphDefaultsClosed = new javax.swing.JButton();
        btnProtProphDefaultsOpen = new javax.swing.JButton();
        panelReport = new javax.swing.JPanel();
        panelReportOptions = new javax.swing.JPanel();
        checkReportDbAnnotate = new javax.swing.JCheckBox();
        textReportAnnotate = new javax.swing.JTextField();
        checkReportFilter = new javax.swing.JCheckBox();
        textReportFilter = new javax.swing.JTextField();
        checkReportProteinLevelFdr = new javax.swing.JCheckBox();
        textReportLabelfree = new javax.swing.JTextField();
        checkLabelfree = new javax.swing.JCheckBox();
        checkCreateReport = new javax.swing.JCheckBox();
        btnReportDefaultsClosed = new javax.swing.JButton();
        btnReportDefaultsOpen = new javax.swing.JButton();
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

        jLabel2.setText("jLabel2");

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("MSFragger");
        setIconImages(loadIcon());
        setName("frameMain"); // NOI18N
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowOpened(java.awt.event.WindowEvent evt) {
                formWindowOpened(evt);
            }
        });

        tabPane.setName(""); // NOI18N

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

        textBinMsfragger.setText(getDefaultBinMsfragger());
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

        lblMsfraggerCitation.setText("If you are using MSFragger search engine for publications, please cite the following paper:");

        jScrollPane1.setBorder(null);

        editorMsfraggerCitation.setEditable(false);
        editorMsfraggerCitation.setBackground(lblMsfraggerCitation.getBackground());
        editorMsfraggerCitation.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        editorMsfraggerCitation.setContentType("text/html"); // NOI18N
        editorMsfraggerCitation.setFont(lblMsfraggerCitation.getFont());
        editorMsfraggerCitation.setText(getFraggerCitationHtml());
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
                        .addComponent(lblMsfraggerCitation)
                        .addGap(0, 0, Short.MAX_VALUE))
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
                .addContainerGap()
                .addGroup(panelMsfraggerConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(textBinMsfragger, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnMsfraggerBinDownload)
                    .addComponent(btnMsfraggerBinBrowse)
                    .addComponent(btnMsfraggerUpdate))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(lblFraggerJavaVer)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(lblMsfraggerCitation)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 58, Short.MAX_VALUE)
                .addContainerGap())
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

        textBinPhilosopher.setText(getDefaultBinPhilosopher());
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
        editorPhilosopherLink.setBackground(lblMsfraggerCitation.getBackground());
        editorPhilosopherLink.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        editorPhilosopherLink.setContentType("text/html"); // NOI18N
        editorPhilosopherLink.setFont(lblMsfraggerCitation.getFont());
        editorPhilosopherLink.setText(getPhilosopherCitationHtml());
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
                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, 46, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        btnFindTools.setText("Search tools");
        btnFindTools.setToolTipText(lblFindAutomatically.getToolTipText());
        btnFindTools.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnFindToolsActionPerformed(evt);
            }
        });

        lblFindAutomatically.setLabelFor(lblFindAutomatically);
        lblFindAutomatically.setText("Recursively search for tools in a directory (e.g. Downloads)");
        lblFindAutomatically.setToolTipText("<html>If you have the tools downloaded somewhere already, you can<br/>\nuse this button to automatically look for them.");

        btnClearCache.setText("Clear Cache");
        btnClearCache.setToolTipText("<html>Forget all the stored text-field information.<br/>\nAfter you relaunch the application everything will reset<br/>\nto default values."); // NOI18N
        btnClearCache.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnClearCacheActionPerformed(evt);
            }
        });

        btnLoadDefaultsOpen.setText("Load Defaults for Open Search");
        btnLoadDefaultsOpen.setToolTipText("<html>Load default parameters for MSFragger and Prophets<br/>\nfor \"Open\" search - large mass tolerances, good for unknown PTM identification");
        btnLoadDefaultsOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnLoadDefaultsOpenActionPerformed(evt);
            }
        });

        btnLoadDefaultsClosed.setText("Load Defaults for Closed Search");
        btnLoadDefaultsClosed.setToolTipText("<html>Load default parameters for MSFragger and Prophets<br/>\nfor \"Closed\" search - small mass tolerances, super-fast conventional search");
        btnLoadDefaultsClosed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnLoadDefaultsClosedActionPerformed(evt);
            }
        });

        btnAboutInConfig.setText("About");
        btnAboutInConfig.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnAboutInConfigActionPerformed(evt);
            }
        });

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("DB Slicing"));

        lblPythonInfo.setText(OsUtils.PythonInfo());

        lblPythonMore.setText("   Python 3 with NumPy and Pandas is only needed for DB slicing feature");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblPythonInfo)
                    .addComponent(lblPythonMore))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(lblPythonInfo)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(lblPythonMore)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout panelConfigLayout = new javax.swing.GroupLayout(panelConfig);
        panelConfig.setLayout(panelConfigLayout);
        panelConfigLayout.setHorizontalGroup(
            panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelConfigLayout.createSequentialGroup()
                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelConfigLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(panelMsfraggerConfig, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addGroup(panelConfigLayout.createSequentialGroup()
                        .addGap(18, 18, 18)
                        .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(panelConfigLayout.createSequentialGroup()
                                .addComponent(btnLoadDefaultsOpen)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(btnLoadDefaultsClosed)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(btnAboutInConfig))
                            .addGroup(panelConfigLayout.createSequentialGroup()
                                .addComponent(btnFindTools)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(lblFindAutomatically)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 168, Short.MAX_VALUE)
                                .addComponent(btnClearCache))))
                    .addGroup(panelConfigLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(panelPhilosopherConfig, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addComponent(jLabel4, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(panelConfigLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelConfigLayout.setVerticalGroup(
            panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnFindTools)
                    .addComponent(lblFindAutomatically)
                    .addComponent(btnClearCache))
                .addGap(18, 18, 18)
                .addGroup(panelConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnLoadDefaultsOpen)
                    .addComponent(btnLoadDefaultsClosed)
                    .addComponent(btnAboutInConfig))
                .addGap(18, 18, 18)
                .addComponent(panelMsfraggerConfig, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(panelPhilosopherConfig, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 116, Short.MAX_VALUE)
                .addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        validateGuiVersion();

        tabPane.addTab("Config", null, panelConfig, "Set up paths to tools");

        panelSelectedFiles.setBorder(javax.swing.BorderFactory.createTitledBorder("Selected files (Drag & Drop files or folders here, it's OK if they contain non LC/MS files)"));

        btnRawAddFiles.setText("Add files");
        btnRawAddFiles.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawAddFilesActionPerformed(evt);
            }
        });

        btnRawClear.setText("Clear");
        btnRawClear.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawClearActionPerformed(evt);
            }
        });

        btnRawAddFolder.setText("Add Folder");
        btnRawAddFolder.setToolTipText("<html>Recursively search a directory, importing<br/>\nall compatible LC/MS files.");
        btnRawAddFolder.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawAddFolderActionPerformed(evt);
            }
        });

        btnRawRemove.setText("Remove");
        btnRawRemove.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRawRemoveActionPerformed(evt);
            }
        });

        chkProcessEachFiileSeparately.setText("Process each file separately");
        chkProcessEachFiileSeparately.setToolTipText("<html><b>Not yet implemented</b><br/>\n\nProcess each file separately isntead of combining the results.");
        chkProcessEachFiileSeparately.setEnabled(false);

        javax.swing.GroupLayout panelSelectedFilesLayout = new javax.swing.GroupLayout(panelSelectedFiles);
        panelSelectedFiles.setLayout(panelSelectedFilesLayout);
        panelSelectedFilesLayout.setHorizontalGroup(
            panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(scrollPaneRawFiles)
                    .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                        .addComponent(btnRawClear)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnRawRemove)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnRawAddFiles)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnRawAddFolder)
                        .addGap(18, 18, 18)
                        .addComponent(chkProcessEachFiileSeparately)
                        .addGap(0, 134, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelSelectedFilesLayout.setVerticalGroup(
            panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelSelectedFilesLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelSelectedFilesLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnRawClear)
                    .addComponent(btnRawRemove)
                    .addComponent(btnRawAddFiles)
                    .addComponent(btnRawAddFolder)
                    .addComponent(chkProcessEachFiileSeparately))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(scrollPaneRawFiles, javax.swing.GroupLayout.DEFAULT_SIZE, 585, Short.MAX_VALUE)
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
                    .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 615, Short.MAX_VALUE)
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
                .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 551, Short.MAX_VALUE)
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
        tabPane.addTab("MSFragger", null, scrollPaneMsFragger, "MSFragger search engine");

        chkRunPeptideProphet.setSelected(true);
        chkRunPeptideProphet.setText("Run PeptideProphet");
        chkRunPeptideProphet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkRunPeptideProphetActionPerformed(evt);
            }
        });

        panelPeptideProphetOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Options"));

        jLabel34.setText("Cmd Line Options");

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

        javax.swing.GroupLayout panelPeptideProphetOptionsLayout = new javax.swing.GroupLayout(panelPeptideProphetOptions);
        panelPeptideProphetOptions.setLayout(panelPeptideProphetOptionsLayout);
        panelPeptideProphetOptionsLayout.setHorizontalGroup(
            panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                .addGap(29, 29, 29)
                .addComponent(jLabel34)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 509, Short.MAX_VALUE)
                .addContainerGap())
        );
        panelPeptideProphetOptionsLayout.setVerticalGroup(
            panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                        .addComponent(jLabel34)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 146, Short.MAX_VALUE)))
        );

        btnPepProphDefaultsClosed.setText("Defaults Closed Search");
        btnPepProphDefaultsClosed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPepProphDefaultsClosedActionPerformed(evt);
            }
        });

        btnPepProphDefaultsOpen.setText("Defaults Open Search");
        btnPepProphDefaultsOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnPepProphDefaultsOpenActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelPeptideProphetLayout = new javax.swing.GroupLayout(panelPeptideProphet);
        panelPeptideProphet.setLayout(panelPeptideProphetLayout);
        panelPeptideProphetLayout.setHorizontalGroup(
            panelPeptideProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelPeptideProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(panelPeptideProphetOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(panelPeptideProphetLayout.createSequentialGroup()
                        .addComponent(chkRunPeptideProphet)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(btnPepProphDefaultsOpen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnPepProphDefaultsClosed)))
                .addContainerGap())
        );
        panelPeptideProphetLayout.setVerticalGroup(
            panelPeptideProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelPeptideProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(chkRunPeptideProphet)
                    .addComponent(btnPepProphDefaultsClosed)
                    .addComponent(btnPepProphDefaultsOpen))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(panelPeptideProphetOptions, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(456, Short.MAX_VALUE))
        );

        tabPane.addTab("PeptideProphet", panelPeptideProphet);

        chkRunProteinProphet.setSelected(true);
        chkRunProteinProphet.setText("Run ProteinProphet");
        chkRunProteinProphet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkRunProteinProphetActionPerformed(evt);
            }
        });

        panelProteinProphetOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Options"));

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

        chkProteinProphetInteractStar.setText("Use 'interact-*pep.xml' as file filter for ProteinProphet (Philosopher only)");
        chkProteinProphetInteractStar.setToolTipText("<html>If checked will use 'interact-*pep.xml' to match pep.xml files to be passed to ProteinProphet.<br/> Otherwise will add files as separate entries, \nwhich might cause problems on Windows<br/> when there are many pepxml files, as the length of command line parameter string is limited to 8192 chars."); // NOI18N

        txtCombinedProtFile.setText("interact.prot.xml");
        txtCombinedProtFile.setToolTipText("<html>The .pep.xml extension will be added to this name.<br/>\nIf left empty will default to \"interact.pep.xml\"");

        jLabel1.setText("Output File");

        javax.swing.GroupLayout panelProteinProphetOptionsLayout = new javax.swing.GroupLayout(panelProteinProphetOptions);
        panelProteinProphetOptions.setLayout(panelProteinProphetOptionsLayout);
        panelProteinProphetOptionsLayout.setHorizontalGroup(
            panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelProteinProphetOptionsLayout.createSequentialGroup()
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addGap(29, 29, 29)
                        .addComponent(jLabel40)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jScrollPane4))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addContainerGap(252, Short.MAX_VALUE)
                        .addComponent(chkProteinProphetInteractStar))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addGap(59, 59, 59)
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(txtCombinedProtFile)))
                .addContainerGap())
        );
        panelProteinProphetOptionsLayout.setVerticalGroup(
            panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(txtCombinedProtFile, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel40)
                    .addComponent(jScrollPane4, javax.swing.GroupLayout.PREFERRED_SIZE, 128, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(chkProteinProphetInteractStar)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        btnProtProphDefaultsClosed.setText("Defaults Closed Search");
        btnProtProphDefaultsClosed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnProtProphDefaultsClosedActionPerformed(evt);
            }
        });

        btnProtProphDefaultsOpen.setText("Defaults Open Search");
        btnProtProphDefaultsOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnProtProphDefaultsOpenActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelProteinProphetLayout = new javax.swing.GroupLayout(panelProteinProphet);
        panelProteinProphet.setLayout(panelProteinProphetLayout);
        panelProteinProphetLayout.setHorizontalGroup(
            panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(panelProteinProphetOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(panelProteinProphetLayout.createSequentialGroup()
                        .addComponent(chkRunProteinProphet)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(btnProtProphDefaultsOpen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnProtProphDefaultsClosed)))
                .addContainerGap())
        );
        panelProteinProphetLayout.setVerticalGroup(
            panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(chkRunProteinProphet)
                    .addGroup(panelProteinProphetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(btnProtProphDefaultsClosed)
                        .addComponent(btnProtProphDefaultsOpen)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(panelProteinProphetOptions, javax.swing.GroupLayout.PREFERRED_SIZE, 206, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(430, Short.MAX_VALUE))
        );

        tabPane.addTab("ProteinProphet", panelProteinProphet);

        panelReportOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Options"));

        checkReportDbAnnotate.setSelected(true);
        checkReportDbAnnotate.setText("Database Annotation");

        textReportAnnotate.setToolTipText("<html>philosopher database --annotate<br/>\nFlags:<br/>\n<ul>\n<li>--prefix string     define a decoy prefix (default \"rev_\")</li>\n</ul>");
        textReportAnnotate.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                textReportAnnotateFocusGained(evt);
            }
            public void focusLost(java.awt.event.FocusEvent evt) {
                textReportAnnotateFocusLost(evt);
            }
        });

        checkReportFilter.setSelected(true);
        checkReportFilter.setText("Filter");
        checkReportFilter.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkReportFilterActionPerformed(evt);
            }
        });

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

        checkReportProteinLevelFdr.setSelected(true);
        checkReportProteinLevelFdr.setText("Report Protein level FDR");
        checkReportProteinLevelFdr.setToolTipText("<html>Which FDR (False Discovery Rate) level to use:\n<ul>\n  <li>Checked - Protein level FDR</li>\n  <li>Unchecked - Peptide level FDR</li>\n</ul>");
        checkReportProteinLevelFdr.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                checkReportProteinLevelFdrStateChanged(evt);
            }
        });

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

        checkLabelfree.setText("Label-free Quant");
        checkLabelfree.setToolTipText("<html>Label free quantitation");

        javax.swing.GroupLayout panelReportOptionsLayout = new javax.swing.GroupLayout(panelReportOptions);
        panelReportOptions.setLayout(panelReportOptionsLayout);
        panelReportOptionsLayout.setHorizontalGroup(
            panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelReportOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelReportOptionsLayout.createSequentialGroup()
                        .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                .addComponent(checkReportFilter, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(checkReportDbAnnotate, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addComponent(checkLabelfree))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(textReportAnnotate, javax.swing.GroupLayout.DEFAULT_SIZE, 486, Short.MAX_VALUE)
                            .addComponent(textReportFilter)
                            .addComponent(textReportLabelfree)))
                    .addGroup(panelReportOptionsLayout.createSequentialGroup()
                        .addComponent(checkReportProteinLevelFdr)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelReportOptionsLayout.setVerticalGroup(
            panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelReportOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(checkReportDbAnnotate)
                    .addComponent(textReportAnnotate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(checkReportFilter)
                    .addGroup(panelReportOptionsLayout.createSequentialGroup()
                        .addComponent(textReportFilter, javax.swing.GroupLayout.DEFAULT_SIZE, 25, Short.MAX_VALUE)
                        .addGap(2, 2, 2)))
                .addGroup(panelReportOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(textReportLabelfree, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(checkLabelfree))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(checkReportProteinLevelFdr)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        loadLastReportAnnotate();
        loadLastReportFilter();
        loadLastReportProteinLevelFdr();
        loadLastLabelfree();

        checkCreateReport.setSelected(true);
        checkCreateReport.setText("Create report");
        checkCreateReport.setToolTipText("<html>Create tab separated report files with \nsome statistics about search results.");
        checkCreateReport.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkCreateReportActionPerformed(evt);
            }
        });

        btnReportDefaultsClosed.setText("Defaults Closed Search");
        btnReportDefaultsClosed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnReportDefaultsClosedActionPerformed(evt);
            }
        });

        btnReportDefaultsOpen.setText("Defaults Open Search");
        btnReportDefaultsOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnReportDefaultsOpenActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout panelReportLayout = new javax.swing.GroupLayout(panelReport);
        panelReport.setLayout(panelReportLayout);
        panelReportLayout.setHorizontalGroup(
            panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelReportLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelReportLayout.createSequentialGroup()
                        .addComponent(checkCreateReport)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(btnReportDefaultsOpen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnReportDefaultsClosed))
                    .addComponent(panelReportOptions, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        panelReportLayout.setVerticalGroup(
            panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelReportLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(checkCreateReport)
                    .addGroup(panelReportLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(btnReportDefaultsClosed)
                        .addComponent(btnReportDefaultsOpen)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panelReportOptions, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(496, Short.MAX_VALUE))
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

        btnRun.setText("RUN");
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
        btnOpenInExplorer.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnOpenInExplorerActionPerformed(evt);
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
                                .addComponent(lblOutputDir)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(txtWorkingDir))
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelRunLayout.createSequentialGroup()
                                .addComponent(btnRun)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnStop)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(checkDryRun)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 82, Short.MAX_VALUE)
                                .addComponent(btnExportLog)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnReportErrors)))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(btnAbout)
                            .addComponent(btnSelectWrkingDir))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(btnClearConsole, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(btnOpenInExplorer, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
                .addContainerGap())
        );
        panelRunLayout.setVerticalGroup(
            panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelRunLayout.createSequentialGroup()
                .addGap(40, 40, 40)
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblOutputDir)
                    .addComponent(btnSelectWrkingDir)
                    .addComponent(txtWorkingDir, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnOpenInExplorer))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnStop)
                    .addComponent(btnClearConsole)
                    .addComponent(btnAbout)
                    .addComponent(checkDryRun)
                    .addComponent(btnReportErrors)
                    .addComponent(btnRun)
                    .addComponent(btnExportLog))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(consoleScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 567, Short.MAX_VALUE)
                .addContainerGap())
        );

        tabPane.addTab("Run", panelRun);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(tabPane))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(tabPane)
        );

        tabPane.getAccessibleContext().setAccessibleName("MSFragger");
        tabPane.getAccessibleContext().setAccessibleDescription("Run MSFragger pipeline");
        addChangeListenerTextSequenceDb();

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void btnAboutActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAboutActionPerformed

        // for copying style
        JLabel label = new JLabel();
        Font font = label.getFont();

        // create some css from the label's font
        StringBuilder style = new StringBuilder("font-family:" + font.getFamily() + ";");
        style.append("font-weight:").append(font.isBold() ? "bold" : "normal").append(";");
        style.append("font-size:").append(font.getSize()).append("pt;");

        JEditorPane ep = new JEditorPane("text/html", "<html><body style=\"" + style + "\">"
                + "MSFragger - Ultrafast Proteomics Search Engine<br/>"
                + "GUI Wrapper (v" + Version.getVersion() + ")<br/>"
                + "Dmitry Avtonomov<br/>"
                + "University of Michigan, 2017<br/><br/>"
                + "<a href=\"" + getGuiDownloadLink() + "\">Click here to download</a> the latest version<br/><br/>"
                + "<a href=\"http://nesvilab.org/\">Alexey Nesvizhskii lab</a><br/>&nbsp;<br/>&nbsp;"
                + "MSFragger authors and contributors:<br/>"
                + "<ul>"
                + "<li>Andy Kong</li>"
                + "<li>Dmitry Avtonomov</li>"
                + "<li>Alexey Nesvizhskii</li>"
                + "</ul>"
                + "<a href=\"http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html\">Original MSFragger paper link</a><br/>"
                + "Reference: <b>doi:10.1038/nmeth.4256</b>"
                + "</body></html>");

        // handle link events
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
    }//GEN-LAST:event_btnAboutActionPerformed

    private void btnSelectWrkingDirActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSelectWrkingDirActionPerformed
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

    private void btnClearConsoleActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearConsoleActionPerformed
        console.setText("");
    }//GEN-LAST:event_btnClearConsoleActionPerformed

    private void btnStopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnStopActionPerformed
        btnRun.setEnabled(true);
        btnStop.setEnabled(false);

        if (exec != null) {
            exec.shutdownNow();
        }
        for (Process p : submittedProcesses) {
            p.destroy();
        }
        submittedProcesses.clear();
    }//GEN-LAST:event_btnStopActionPerformed

    private void chkRunProteinProphetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunProteinProphetActionPerformed
        boolean selected = chkRunProteinProphet.isSelected();
        Container[] comps = new Container[]{
            panelProteinProphetOptions
        };
        for (Container c : comps) {
            SwingUtils.enableComponents(c, selected);
        }
    }//GEN-LAST:event_chkRunProteinProphetActionPerformed

    private void chkRunPeptideProphetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunPeptideProphetActionPerformed
        boolean selected = chkRunPeptideProphet.isSelected();
        Container[] comps = new Container[]{
            panelPeptideProphetOptions
        };
        for (Container c : comps) {
            SwingUtils.enableComponents(c, selected);
        }
    }//GEN-LAST:event_chkRunPeptideProphetActionPerformed


    private void btnRawClearActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawClearActionPerformed
        tableModelRawFiles.dataClear();
    }//GEN-LAST:event_btnRawClearActionPerformed

    private void btnRawAddFilesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawAddFilesActionPerformed
        if (btnRawAddFiles == evt.getSource()) {
            String approveText = "Select";
            JFileChooser fc = new JFileChooser();
            fc.setAcceptAllFileFilterUsed(true);
            FileNameExtensionFilter fileNameExtensionFilter = FraggerPanel.fileNameExtensionFilter;
            fc.setFileFilter(fileNameExtensionFilter);
            fc.setApproveButtonText(approveText);
            fc.setDialogTitle("Choose raw data files");
            fc.setMultiSelectionEnabled(true);
            fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

            ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN, fc);

            int retVal = fc.showDialog(this, approveText);
            if (retVal == JFileChooser.APPROVE_OPTION) {
                File[] files = fc.getSelectedFiles();
                if (files.length > 0) {
                    ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, files[0]);
                    List<Path> paths = new ArrayList<>(files.length);
                    for (File f : files) {
                        paths.add(Paths.get(f.getAbsolutePath()));
                    }
                    tableModelRawFiles.dataAddAll(paths);
                }

            } else {

            }
        }
    }//GEN-LAST:event_btnRawAddFilesActionPerformed

    private void btnRawRemoveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawRemoveActionPerformed
        int[] sel = tableRawFiles.getSelectedRows();
        if (sel.length == 0) {
            return;
        }
        List<Path> toRemove = new ArrayList<>();
        for (int i = 0; i < sel.length; i++) {
            toRemove.add(tableModelRawFiles.dataGet(sel[i]));
        }
        tableRawFiles.getSelectionModel().clearSelection();
        tableModelRawFiles.dataRemoveAll(toRemove);
    }//GEN-LAST:event_btnRawRemoveActionPerformed

    private void btnRawAddFolderActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRawAddFolderActionPerformed
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setApproveButtonText("Select");
        fileChooser.setApproveButtonToolTipText("Select folder to import");
        fileChooser.setDialogTitle("Select a folder with LC/MS files (searched recursively)");
        fileChooser.setAcceptAllFileFilterUsed(true);
        FileNameExtensionFilter fileNameExtensionFilter = FraggerPanel.fileNameExtensionFilter;
        fileChooser.setFileFilter(fileNameExtensionFilter);
        fileChooser.setMultiSelectionEnabled(true);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

        SwingUtils.setFileChooserPath(fileChooser, ThisAppProps.load(ThisAppProps.PROP_LCMS_FILES_IN));

        int showOpenDialog = fileChooser.showOpenDialog(this);
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File[] files = fileChooser.getSelectedFiles();
                ArrayList<Path> paths = new ArrayList<>(files.length);
                for (File f : files) {
                    boolean isDirectory = f.isDirectory();
                    if (isDirectory) {
                        ThisAppProps.save(ThisAppProps.PROP_LCMS_FILES_IN, f);
                        PathUtils.traverseDirectoriesAcceptingFiles(f, FraggerPanel.fileNameExtensionFilter, paths);
                    } else if (FraggerPanel.fileNameExtensionFilter.accept(f)) {
                        paths.add(Paths.get(f.getAbsolutePath()));
                    }
                }
                tableModelRawFiles.dataAddAll(paths);

                break;
        }
    }//GEN-LAST:event_btnRawAddFolderActionPerformed

    private void btnReportErrorsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReportErrorsActionPerformed
        final String issueTrackerAddress = Version.bundle().getString(Version.PROP_ISSUE_TRACKER_URL);
        try {
            Desktop.getDesktop().browse(URI.create(issueTrackerAddress));
        } catch (IOException ex) {
            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    }//GEN-LAST:event_btnReportErrorsActionPerformed

    private void btnMsfraggerBinBrowseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerBinBrowseActionPerformed
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setApproveButtonText("Select");
        fileChooser.setDialogTitle("Select MSFragger jar");
        fileChooser.setMultiSelectionEnabled(false);
        FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("JAR files", "jar");
        fileChooser.setFileFilter(fileNameExtensionFilter);

        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        List<String> props = Arrays.asList(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, ThisAppProps.PROP_BINARIES_IN);
        String fcPath = ThisAppProps.tryFindPath(props, true);
        SwingUtils.setFileChooserPath(fileChooser, fcPath);

        int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentComponentForDialog(this));
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File foundFile = fileChooser.getSelectedFile();
                if (validateAndSaveMsfraggerPath(foundFile.getAbsolutePath())) {
                    ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, foundFile.getAbsolutePath());
                }
                break;
        }
        
        // rerun slicing checks
        validatePythonAndSlicingVersion();
    }//GEN-LAST:event_btnMsfraggerBinBrowseActionPerformed

    /**
     * Checks if a file is a JAR file and that it contains MSFragger.class at
     * the top level.
     *
     * @param f file to check.
     * @return True if it's a real JAR file with MSFragger.class at the top
     * level inside.
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
        boolean isVersionValid = isJarValid ? validateMsfraggerVersion(path) : false;
        boolean isJavaValid = isVersionValid ? validateMsfraggerJavaVersion() : false;
        
        if (!isPathValid) {
            JEditorPane ep = SwingUtils.createClickableHtml(String.format(
                    "<html>Could not find MSFragger jar file at this location.<br/>\n"
                    + "Corresponding panel won't be active.<br/><br/>"
                    + "<b>If that's the first time you're using %s</b>,<br/>"
                    + "you will need to <a href=\"%s\">download MSFragger.jar (click here)</a> first.<br/>"
                    + "Use the button on the right to proceed to the download website.", 
                    Version.PROGRAM_TITLE, MsfraggerProps.DOWNLOAD_URL));

            balloonMsfragger = new BalloonTip(textBinMsfragger, ep,
                    new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
            balloonMsfragger.setVisible(true);
        } else if (!isJarValid) {
            JEditorPane ep = SwingUtils.createClickableHtml(String.format(
                    "<html>Looks like you selected an existing jar file, but we.<br/>\n"
                    + "don't recognize it as a valid MSFragger distribution.<br/><br/>"
                    + "<b>If that's the first time you're using %s</b>,<br/>"
                    + "you will need to <a href=\"%s\">download MSFragger.jar (click here)</a> first.<br/>"
                    + "Use the button on the right to proceed to the download website.", 
                    Version.PROGRAM_TITLE, MsfraggerProps.DOWNLOAD_URL));

            balloonMsfragger = new BalloonTip(textBinMsfragger, ep,
                    new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
            balloonMsfragger.setVisible(true);
        }
        
        enableMsfraggerPanels(isJarValid && isVersionValid && isJavaValid);

        return isJarValid;
    }

    private boolean validateMsfraggerJavaVersion() {
        final boolean javaAtLeast18 = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8);
        final VersionComparator vc = new VersionComparator();
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                BalloonTip tip = tipMap.remove(TIP_NAME_FRAGGER_JAVA_VER);
                if (tip != null)
                    tip.closeBalloon();
                tip = null;
                
                if (!javaAtLeast18) {
                    tip = new BalloonTip(lblFraggerJavaVer, "Msfragger requires Java 1.8. Your version is lower.\n");
                } else {
                    // check for Java 9
                    final String jver = SystemUtils.JAVA_SPECIFICATION_VERSION;
                    final String fver = fraggerVer != null ? fraggerVer : MsfraggerProps.testJar(textBinMsfragger.getText()).version;
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
            }
        });
        return javaAtLeast18;
    }

    private String getGuiDownloadLink() {
        String locallyKnownDownloadUrl = null;
        try (InputStream is = MsfraggerGuiFrame.class.getResourceAsStream("Bundle.properties")) {
            
            if (is == null) {
                throw new IllegalStateException("Could not read Bundle.properties from the classpath");
            }
            Properties prop = new Properties();
            prop.load(is);
            locallyKnownDownloadUrl = prop.getProperty(Version.PROP_DOWNLOAD_URL);
            if (locallyKnownDownloadUrl == null) {
                throw new IllegalStateException("Property "
                        + Version.PROP_DOWNLOAD_URL
                        + " was not found in Bundle.properties");
            }
        } catch (IOException e) {
            throw new IllegalStateException("Error reading Bundle.properties from the classpath");
        }

        return locallyKnownDownloadUrl;
        //final String downloadUrl = props.getProperty(Version.PROP_DOWNLOAD_URL, locallyKnownDownloadUrl);
    }

    public static Properties loadPropertiesFromBundle() {
        try (InputStream is = MsfraggerGuiFrame.class.getResourceAsStream("Bundle.properties")) {
            if (is == null) {
                throw new IllegalStateException("Could not read Bundle.properties from the classpath");
            }
            Properties props = new Properties();
            props.load(is);
            return props;
        } catch (IOException e) {
            throw new IllegalStateException("Error reading Bundle.properties from the classpath");
        }
    }
    
    public static String loadPropFromBundle(String propName) {
        Properties props = loadPropertiesFromBundle();
        String value = props.getProperty(propName);
        if (value == null) {
            throw new IllegalStateException("Property " + propName
                    + " was not found in Bundle.properties");
        }
        return value;
    }

    private void validateGuiVersion() {
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    String githubProps = IOUtils.toString(Version.PROPERTIES_REMOTE_URI.toURL(), Charset.forName("UTF-8"));
                    
                    //Properties propsGh = new Properties();
                    //propsGh.load(new StringReader(githubProps));
                    Properties propsGh = PropertiesUtils.loadPropertiesRemote(Version.PROPERTIES_REMOTE_URI);
                    //Properties propsGh = PropertiesUtils.loadPropertiesRemoteOrLocal(
                    //        Arrays.asList(Version.PROPERTIES_REMOTE_URI), MsfraggerGuiFrame.class, "Bundle.properties");
                    
                    if (propsGh == null)
                        propsGh = new Properties();
                    
                    // this is used to test functionality without pushing changes to github
//                        propsGh.put("msfragger.gui.version", "5.7");
//                        propsGh.put("msfragger.gui.important-updates", "3.1,3.5,4.9,5.2");
//                        propsGh.put("msfragger.gui.critical-updates", "2.0,3.0,4.6,5.0, 4.7");
//                        propsGh.put("msfragger.gui.download-message", "Happy new year!");
//                        propsGh.put("msfragger.gui.download-message.4.7", "Crit 4.7");
//                        propsGh.put("msfragger.gui.download-message.2.0", "Crit 2.0");
//                        propsGh.put("msfragger.gui.download-message.5.0", "Crit 4.7");
//                        propsGh.put("msfragger.gui.download-message.5.0", "Crit 5.0");
//                        propsGh.put("msfragger.gui.download-message.3.1", "Important 3.1");
//                        propsGh.put("msfragger.gui.download-message.4.9", "Important 4.9");
                    final StringBuilder sb = new StringBuilder();
                    final VersionComparator vc = new VersionComparator();

                    // add new versions notification
                    final String githubVersion = propsGh.getProperty(Version.PROP_VER);
                    final String localVersion = Version.VERSION;
                    if (githubVersion != null && vc.compare(localVersion, githubVersion) < 0) {
                        if (sb.length() > 0) {
                            sb.append("<br><br>");
                        }
                        String locallyKnownDownloadUrl = loadPropFromBundle(Version.PROP_DOWNLOAD_URL);
                        final String downloadUrl = propsGh.getProperty(Version.PROP_DOWNLOAD_URL, locallyKnownDownloadUrl);
                        sb.append(String.format(Locale.ROOT,
                                "Your %s version is [%s]<br>\n"
                                + "There is a newer version of %s available [%s]).<br/>\n"
                                + "Please <a href=\"%s\">click here</a> to download a newer one.<br/>",
                                Version.PROGRAM_TITLE, localVersion, Version.PROGRAM_TITLE, githubVersion, downloadUrl));

                        // check for critical or important updates since the current version
                        List<String> updatesImportant = Version.updatesSinceCurrentVersion(
                                propsGh.getProperty(Version.PROP_IMPORTANT_UPDATES, ""));
                        List<String> updatesCritical = Version.updatesSinceCurrentVersion(
                                propsGh.getProperty(Version.PROP_CRITICAL_UPDATES, ""));

                        if (!updatesCritical.isEmpty()) {
                            TreeSet<String> newerVersions = new TreeSet<>(updatesCritical);
                            List<String> messages = createGuiUpdateMessages(newerVersions, propsGh);
                            if (!messages.isEmpty()) {
                                sb.append("<br/><br/><b>Critical updates:</b><br><ul>");
                                for (String message : messages) {
                                    sb.append("<li>").append(message).append("</li>");
                                }
                                sb.append("</ul>");
                            } else {
                                sb.append("<br/><b>There have been critical updates.</b><br>");
                            }
                        }
                        
                        if (!updatesImportant.isEmpty()) {
                            TreeSet<String> newerVersions = new TreeSet<>(updatesImportant);
                            List<String> messages = createGuiUpdateMessages(newerVersions, propsGh);
                            if (!messages.isEmpty()) {
                                sb.append("<br/>Important updates:<br><ul>");
                                for (String message : messages) {
                                    sb.append("<li>").append(message).append("</li>");
                                }
                                sb.append("</ul>");
                            } else {
                                sb.append("<br/><br/>There have been important updates.<br>");
                            }
                        }
                    }

                    final String downloadMessage = propsGh.getProperty(Version.PROP_DOWNLOAD_MESSAGE, "");
                    if (!StringUtils.isNullOrWhitespace(downloadMessage)) {
                        if (sb.length() > 0) {
                            sb.append("<br><br><b>");
                        }
                        sb.append(downloadMessage).append("</b>");
                    }

                    if (sb.length() > 0) {
                        // show balloon popup, must be done on EDT
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                BalloonTip tip = tipMap.get(Version.PROP_VER);
                                if (tip != null) {
                                    tip.closeBalloon();
                                    tipMap.remove(Version.PROP_VER);
                                }

                                JEditorPane ep = SwingUtils.createClickableHtml(sb.toString());

                                BalloonTip t = new BalloonTip(btnAboutInConfig, ep,
                                        new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
                                t.setVisible(true);
                                tipMap.put(Version.PROP_VER, t);
                            }
                        });
                    }
                } catch (IOException ex) {
                    // it doesn't matter, it's fine if we can't fetch the file from github
                    System.err.println("Could not download Bundle.properties file from github");
                }
            }
        });
        t.start();

    }

    private List<String> createGuiUpdateMessages(TreeSet<String> newerVersionStrings, Properties propsRemote) {
        List<String> messages = new ArrayList<>();
        for (String newerVersion : newerVersionStrings) {
            String verMsg = propsRemote.getProperty(Version.PROP_DOWNLOAD_MESSAGE + "." + newerVersion, "");
            if (StringUtils.isNullOrWhitespace(verMsg))
                continue;
            messages.add(verMsg);
        }
        return messages;
    }
    
    private static String tryPythonCommand() throws Exception {
        String[] commands = {"python", "python3"};
        for(String cmd : commands) {
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
    
    
    public enum Installed {
        YES, NO, UNKNOWN
    }
    
    private Installed checkPythonPackageAvailability(String pythonCmd, String pkgName) {
        boolean isInstalled = false;
        ProcessBuilder pb = new ProcessBuilder(pythonCmd,
                "-c", "import pkgutil; print(1 if pkgutil.find_loader('" + pkgName + "') else 0)");
        Process pr = null;
        boolean isError = false;
        try {
            pr = pb.start();
        } catch (IOException ex) {
            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, 
                    "Could not start python " + pkgName + " check process", ex);
            isError = true;
        }
        if (pr != null) {
            try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
                String line;
                while ((line = in.readLine()) != null) {
                    if ("1".equals(line))
                        isInstalled = true;
                }
            } catch (IOException ex) {
                Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, 
                        "Could not read python " + pkgName + " check output", ex);
                isError = true;
            }
            try {
                int exitCode = pr.waitFor();
            } catch (InterruptedException ex) {
                Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, 
                        "Error while waiting for python " + pkgName + " check process to finish", ex);
            }
        }

        if (isInstalled) {
            return Installed.YES;
        } else if (!isError) {
            return Installed.NO;
        } else {
            return Installed.UNKNOWN;
        }
    }
    
    private void validatePythonAndSlicingVersion() {
        Thread t;
        t = new Thread(new Runnable() {
            @Override
            public void run() {
                String version = "";
                boolean isPython3 = false;
                final String[] packages = {"numpy", "pandas"};
                Map<String, Boolean> isPkgInstalled = new LinkedHashMap<>();
                for (String pkg : packages)
                    isPkgInstalled.put(pkg, false);
                boolean isFraggerVerCompatible = false;
                boolean isSlicingScriptUnpacked = false;
                slicingEnabled = false;
                fraggerPanel.enableDbSlicing(false);
                
                try {
                    InputStream in = getClass().getResourceAsStream("/" + MsfraggerProps.PYTHON_SPLITTER_NAME); 
                    Path tempFile = Files.createTempFile("fragpipe-", "-" + MsfraggerProps.PYTHON_SPLITTER_NAME);
                    tempFile.toFile().deleteOnExit();
                    Files.copy(in, tempFile, StandardCopyOption.REPLACE_EXISTING);
                    slicingScriptPath = tempFile;
                    isSlicingScriptUnpacked = true;
                } catch (IOException | NullPointerException ex) {
                    JOptionPane.showMessageDialog(MsfraggerGuiFrame.this, 
                            "Could not unpack assets to temporary directory.\n"
                                    + "DB slicing won't be enabled.",
                                "Can't unpack", JOptionPane.WARNING_MESSAGE);
                }
                
                StringBuilder sbPythonInfo = new StringBuilder("Python Info:");
                String pythonCmd = null;
                try {
                    pythonCmd = tryPythonCommand();
                } catch (Exception ex) {
                    sbPythonInfo.append(" " + ex.getMessage());
                }
                
                if (StringUtils.isNullOrWhitespace(pythonCmd)) {
                    sbPythonInfo.append(" Python command not found on PATH.");
                } else {
                    pythonCommand = pythonCmd;
                    // checking python version
                    ProcessBuilder pb = new ProcessBuilder(pythonCmd, "--version");
                    pb.redirectErrorStream(true);

                    Process prPythonVer = null;
                    try {
                        prPythonVer = pb.start();
                    } catch (IOException ex) {
                        sbPythonInfo.append(" Could not start 'python --version' process.");
                    }
                    
                    if (prPythonVer != null) {
                        
                        try (BufferedReader in = new BufferedReader(new InputStreamReader(prPythonVer.getInputStream()))) {
                            Pattern pythonVersionRe = Pattern.compile("(python\\s+[0-9\\.]+)", Pattern.CASE_INSENSITIVE);
                            boolean isPythonVerFound = false;
                            String line;
                            while ((line = in.readLine()) != null) {
                                Matcher m = pythonVersionRe.matcher(line);
                                if (m.find()) {
                                    isPythonVerFound = true;
                                    version = m.group(1);
                                    sbPythonInfo.append(" ").append(version).append(".");
                                    Pattern verRe = Pattern.compile("python\\s+([0-9]+)", Pattern.CASE_INSENSITIVE);
                                    Matcher m1 = verRe.matcher(version);
                                    if (m1.find()) {
                                        String pythonMajorVer = m1.group(1);
                                        if ("3".equals(pythonMajorVer))
                                            isPython3 = true;
                                    }
                                }
                            }
                            
                            if (!isPythonVerFound) {
                                sbPythonInfo.append(" Python version was not found in 'python --vesrion' output.");
                            }
                            
                        } catch (IOException ex) {
                            sbPythonInfo.append(" Could not read python version from the python process.");
                        }
                        
                        try {
                            int exitCode = prPythonVer.waitFor();
                        } catch (InterruptedException ex) {
                            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, 
                                    "Error waiting for python --version process to finish", ex);
                        }
                    }
                }
                
                for (String pkg : packages) {
                    if (pythonCmd == null) {
                        sbPythonInfo.append(" ").append(pkg).append(" - N/A.");
                    } else {
                        Installed inst = checkPythonPackageAvailability(pythonCmd, pkg);
                        switch (inst) {
                            case YES:
                                isPkgInstalled.put(pkg, true);
                                sbPythonInfo.append(" ").append(pkg).append(" - Yes.");
                                break;
                            case NO:
                                sbPythonInfo.append(" ").append(pkg).append(" - No.");
                                break;
                            case UNKNOWN:
                                sbPythonInfo.append(" ").append(pkg).append(" - N/A.");
                                break;
                            default:
                                throw new AssertionError(inst.name());
                        }
                    }
                }
                    
                VersionComparator cmp = new VersionComparator();
                String minFraggerVer = "20180924";
                Properties props = PropertiesUtils.loadPropertiesLocal(MsfraggerProps.class, MsfraggerProps.PROPERTIES_FILE_NAME);
                if (props != null)
                    minFraggerVer = props.getProperty(MsfraggerProps.PROP_MIN_VERSION_SLICING, "20180924");
                int fraggerVersionCmp = cmp.compare(fraggerVer, minFraggerVer);
                if (fraggerVersionCmp >= 0) {
                    isFraggerVerCompatible = true;
                }

                lblPythonInfo.setText(sbPythonInfo.toString());
                boolean areAllPkgsInstalled = true;
                for (String pkg : packages) {
                    Boolean isInstalled = isPkgInstalled.get(pkg);
                    if (isInstalled == null || !isInstalled) {
                        areAllPkgsInstalled = false;
                        break;
                    }
                }
                if (isPython3 && areAllPkgsInstalled 
                        && isFraggerVerCompatible && isSlicingScriptUnpacked) {
                    fraggerPanel.enableDbSlicing(true);
                    slicingEnabled = true;
                    lblPythonMore.setText("Slicing enabled");
                } else {
                    slicingEnabled = false;
                    StringBuilder err = new StringBuilder("<html><b>Slicing disabled</b>.");
                    err.append(" Python install: ");
                    if (isPython3 && areAllPkgsInstalled) {
                        err.append("OK.");
                    } else {
                        err.append("<b>Need Python 3 with [")
                                .append(StringUtils.join(packages, ", "))
                                .append("]</b>.");
                    }
                    if (isFraggerVerCompatible) {
                        err.append(" MSFragger: OK.");
                    } else {
                        err.append(" MSFragger: <b>Update to version: ").append(minFraggerVer).append("+</b>.");
                    }
                    if (isSlicingScriptUnpacked) {
                        err.append(" Scripts: Unpacked OK.");
                    } else {
                        err.append(" Scripts: <b>NOT Unpacked</b>.");
                    }

                    lblPythonMore.setText(err.toString());
                }
            }
        });
        t.start();
    }
    
    private void validatePhilosopherVersion(final String binPath) {
        if (balloonPhilosopher != null) {
            balloonPhilosopher.closeBalloon();
        }

        final Pattern regexNewerVerFound = Pattern.compile("new version.*available.*?\\:\\s*(\\S+)", Pattern.CASE_INSENSITIVE);
        final Pattern regexVersion = Pattern.compile("build\\s+and\\s+version.*?build.*?=(?<build>\\S+).*version.*?=(?<version>\\S+)", Pattern.CASE_INSENSITIVE);
        final VersionComparator vc = new VersionComparator();
        
        
        // Check releases on github by running `philosopher version`.
        Thread t;
        t = new Thread(new Runnable() {
            @Override
            public void run() {
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

                    Properties props = PropertiesUtils.loadPropertiesRemote(PhilosopherProps.PROPERTIES_URI);
                    if (props == null) // if we couldn't download remote properties, try using local ones
                        props = PropertiesUtils.loadPropertiesLocal(PhilosopherProps.class, PhilosopherProps.PROPERTY_FILE_NAME);
                    
                    philosopherVer = StringUtils.isNullOrWhitespace(curVersionAndBuild) ? UNKNOWN_VERSION : curVersionAndBuild;
                    lblPhilosopherInfo.setText(String.format(
                            "Philosopher version: %s. %s", philosopherVer, OsUtils.OsInfo()));

                    int returnCode = pr.waitFor();

                    JEditorPane ep = null;
                    if (isNewVersionStringFound) {
                        StringBuilder sb = new StringBuilder();
                        sb.append("Newer version of Philosopher available.<br>\n");
                        sb.append("<a href=\"").append(downloadLink).append("\">Click here</a> to download.<br>\n");
                        if (props != null) {
                            // if we have some philosopher properties (local or better remote)
                            // then check if this version is known to be compatible
                            String latestCompatible = props.getProperty(PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + Version.VERSION);
                            if (latestCompatible == null) {
                                sb.append("<br>\nHowever, we have not yet checked if it's fully compatible with this version of ")
                                        .append(Version.PROGRAM_TITLE).append(".");
                            } else if (curVersion != null) {
                                int cmp = vc.compare(curVersion, latestCompatible);
                                if (cmp == 0) {
                                    sb.append("<br>\nHowever, <b>you currently have the latest known tested version</b>.");
                                } else if (cmp < 0) {
                                    sb.append("<br>\nThe latest known tested version is<br>\n"
                                            + "<b>Philosopher ").append(latestCompatible).append("</b>.<br/>\n");
                                    sb.append("It is not recommended to upgrade to newer versions unless they are tested.");
                                }
                            }
                        }
                        ep = SwingUtils.createClickableHtml(sb.toString());

                    } else if (returnCode != 0) {
                        ep = SwingUtils.createClickableHtml(String.format(Locale.ROOT,
                                "Philosopher version too old and is no longer supported.<br>\n"
                                        + "Please <a href=\"%s\">click here</a> to download a newer one.",
                                PhilosopherProps.DOWNLOAD_URL));
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
                    throw new IllegalStateException("Error while creating a java process for Philosopher test.");
                }
            }
        });
        t.start();
    }

    private boolean validateMsfraggerVersion(final String jarPath) {
        Path p = Paths.get(jarPath);
        
        // only validate Fragger version if the current Java version is 1.8 or higher
        if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8)) {
            // we can't test fragger binary verison when java version is less than 1.8
            return true;
        }

        Pattern regex = Pattern.compile("MSFragger version (MSFragger-([\\d\\.]{4,}))", Pattern.CASE_INSENSITIVE);

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
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {

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
                            SwingUtilities.invokeLater(new Runnable() {
                                @Override
                                public void run() {
                                    if (balloonMsfragger != null) {
                                        balloonMsfragger.closeBalloon();
                                    }

                                    StringBuilder sb = new StringBuilder();
                                    if (jarTest.isVersionPrintedAtAll) {
                                        sb.append(String.format("Your version is [%s]<br>\n"
                                                + "There is a newer version of MSFragger available [%s].<br>\n",
                                                localVer, updateVer));
                                    } else {
                                        sb.append(String.format("Your version is <b>too old and not supported anymore</b><br>\n"
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
                                        btnAutoUpdate.addActionListener(new ActionListener() {
                                            @Override
                                            public void actionPerformed(ActionEvent e) {
                                                if (balloonMsfragger == null) {
                                                    return;
                                                }
                                                balloonMsfragger.setVisible(false);
                                                balloonMsfragger = null;

                                                final JDialog dlg = new JDialog(MsfraggerGuiFrame.this, "Updating MSFragger", true);
                                                JProgressBar pb = new JProgressBar(0, 100);
                                                pb.setIndeterminate(true);
                                                Dimension d = new Dimension(300, 75);
                                                pb.setMinimumSize(d);
                                                pb.setSize(d);
                                                dlg.add(pb, BorderLayout.CENTER);
                                                dlg.setSize(d);
                                                dlg.setLocationRelativeTo(MsfraggerGuiFrame.this);

                                                Thread updateThread = new Thread(new Runnable() {
                                                    @Override
                                                    public void run() {
                                                        try {

                                                            Path updated = vf.autoUpdate(Paths.get(jarPath));
                                                            validateAndSaveMsfraggerPath(updated.toAbsolutePath().toString());

                                                        } catch (Exception ex) {
                                                            throw new IllegalStateException("Something happened during MSFragger auto-update", ex);
                                                        } finally {
                                                            dlg.setVisible(false);
                                                        }
                                                    }
                                                });
                                                updateThread.start();

                                                // show the dialog, this blocks until dlg.setVisible(false) is called
                                                // so this call is made in the finally block
                                                dlg.setVisible(true);
                                            }
                                        });
                                        panelButtons.add(btnAutoUpdate);
                                    }

                                    if (!StringUtils.isNullOrWhitespace(manualDownloadUrl)) {
                                        JButton btnManualUpdate = new JButton("Manual update");
                                        btnManualUpdate.addActionListener(new ActionListener() {
                                            @Override
                                            public void actionPerformed(ActionEvent e) {
                                                try {
                                                    SwingUtils.openBrowserOrThrow(new URI(manualDownloadUrl));
                                                } catch (URISyntaxException ex) {
                                                    throw new IllegalStateException("Incorrect url/uri", ex);
                                                }
                                            }
                                        });
                                        panelButtons.add(btnManualUpdate);
                                    }

                                    JButton btnClose = new JButton("Close");
                                    btnClose.addActionListener(new ActionListener() {
                                        @Override
                                        public void actionPerformed(ActionEvent e) {
                                            if (balloonMsfragger == null) {
                                                return;
                                            }
                                            balloonMsfragger.setVisible(false);
                                            balloonMsfragger = null;
                                        }
                                    });

                                    panel.add(ep, BorderLayout.CENTER);
                                    panelButtons.add(btnClose);
                                    panel.add(panelButtons, BorderLayout.SOUTH);

                                    balloonMsfragger = new BalloonTip(textBinMsfragger, panel,
                                            new RoundedBalloonStyle(5, 5, Color.WHITE, Color.BLACK), true);
                                    balloonMsfragger.setVisible(true);
                                }
                            });
                        }
                        return; // stop iterations, we've found that there is no better version than the current

                    } catch (Exception ex) {
                        // no biggie
                        continue;
                    }
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
        if (!Files.exists(p)) {
            return false;
        }
        return true;
        
    }
    
    private boolean validateMsfraggerJarContents(String path) {
        if (!validateMsfraggerPath(path))
            return false;
        Path p = Paths.get(path).toAbsolutePath();
        final boolean[] found = {false};
        try (FileSystem fs = FileSystems.newFileSystem(p, null)) {
            for (Path root : fs.getRootDirectories()) {
                Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
                    Pattern regex = Pattern.compile("msfragger.*\\.jar", Pattern.CASE_INSENSITIVE);

                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                            throws IOException {
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

    private void btnMsfraggerBinDownloadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerBinDownloadActionPerformed
        try {
            Desktop.getDesktop().browse(MsfraggerProps.DOWNLOAD_URI);
        } catch (IOException ex) {
            throw new IllegalStateException("Could not open MSFragger download link in browser.", ex);
        }
    }//GEN-LAST:event_btnMsfraggerBinDownloadActionPerformed

    private void urlHandlerViaSystemBrowser(javax.swing.event.HyperlinkEvent evt) {//GEN-FIRST:event_urlHandlerViaSystemBrowser
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

    private void btnPhilosopherBinDownloadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPhilosopherBinDownloadActionPerformed
        downloadPhilosopher();
    }//GEN-LAST:event_btnPhilosopherBinDownloadActionPerformed

    private void btnFindToolsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnFindToolsActionPerformed

        String fraggerFoundPath = null;
        String philosopherFoundPath = null;

        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setApproveButtonText("Search here");
        fileChooser.setApproveButtonToolTipText("Search this directory recursively");
        fileChooser.setDialogTitle("Select path to search for binaries");
        fileChooser.setMultiSelectionEnabled(false);
        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        List<String> props = Arrays.asList(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, ThisAppProps.PROP_BINARIES_IN, ThisAppProps.PROP_BIN_PATH_PHILOSOPHER);
        String fcPath = ThisAppProps.tryFindPath(props, true);
        SwingUtils.setFileChooserPath(fileChooser, fcPath);

        int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentComponentForDialog(this));
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File f = fileChooser.getSelectedFile();

                // Fragger first
                Pattern regexFragger = Pattern.compile(".*?MSFragger[^\\/]+?\\.jar", Pattern.CASE_INSENSITIVE);
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
                    JOptionPane.showMessageDialog(this, "Could not locate MSFragger jar.", "Info", JOptionPane.INFORMATION_MESSAGE);
                }

                // now philosopher
                Pattern regexPhilosopher = Pattern.compile(".*?philosopher[^\\/]*", Pattern.CASE_INSENSITIVE);
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
                    JOptionPane.showMessageDialog(this, "Could not locate Philosopher.", "Info", JOptionPane.INFORMATION_MESSAGE);
                }

                break;
        }
    }//GEN-LAST:event_btnFindToolsActionPerformed

    private void btnPhilosopherBinBrowseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPhilosopherBinBrowseActionPerformed
        JFileChooser fc = new JFileChooser();
        fc.setApproveButtonText("Select");
        fc.setDialogTitle("Select Philosopher binary");
        fc.setMultiSelectionEnabled(false);
        
        fc.setAcceptAllFileFilterUsed(true);
        
        
        if (OsUtils.isWindows()) {
            FileNameExtensionFilter ff = new FileNameExtensionFilter("Executables", "exe");
            fc.addChoosableFileFilter(ff);
        }

        fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

        List<String> props = Arrays.asList(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER, ThisAppProps.PROP_BINARIES_IN);
        String fcPath = ThisAppProps.tryFindPath(props, true);
        SwingUtils.setFileChooserPath(fc, fcPath);

        int showOpenDialog = fc.showOpenDialog(SwingUtils.findParentComponentForDialog(this));
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File f = fc.getSelectedFile();
                if (validateAndSavePhilosopherPath(f.getAbsolutePath())) {
                    ThisAppProps.save(ThisAppProps.PROP_BINARIES_IN, f.getAbsolutePath());
                }
                break;
        }
    }//GEN-LAST:event_btnPhilosopherBinBrowseActionPerformed

    private void textBinMsfraggerFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textBinMsfraggerFocusLost
        validateAndSaveMsfraggerPath(textBinMsfragger.getText());
    }//GEN-LAST:event_textBinMsfraggerFocusLost

    private void textBinPhilosopherFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textBinPhilosopherFocusLost
        validateAndSavePhilosopherPath(textBinPhilosopher.getText());
    }//GEN-LAST:event_textBinPhilosopherFocusLost

    private void textBinPhilosopherActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textBinPhilosopherActionPerformed
        
    }//GEN-LAST:event_textBinPhilosopherActionPerformed

    private void btnClearCacheActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearCacheActionPerformed
        ThisAppProps.clearCache();
        MsfraggerParams.clearCache();
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
                if (tip != null)
                    tip.closeBalloon();

                if (!isValid) {
                    tip = new BalloonTip(comp, "Invalid format.");
                    tip.setVisible(true);
                    tipMap.put(propName, tip);
                }
            }
        });
        
        return isValid;
    }
    
    private void validateAndSaveReportAnnotate(final String newText, boolean updateOtherTags) {
        final JTextComponent comp = textReportAnnotate;
        final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXTFIELD_REPORT_ANNOTATE, 
                newText, ValidateTrue.getInstance());    
        
        if (!isValid)
            return;
        
        // check if the filter line has changed since focus was gained
        final String savedText = textReportAnnotateFocusGained;
        final String oldText = savedText != null ? savedText : comp.getText().trim();
        final String updText = newText != null ? newText : comp.getText().trim();
        
        if (!updateOtherTags || oldText.equals(updText)) // newText == null means it was a programmatic update
            return;
        
        // check if the reverse tag has changed
        Pattern re = reDecoyTagReportAnnotate; 
        String oldVal = "", newVal = "";
        Matcher m = re.matcher(updText);
        if (m.find()) {
            newVal = m.group(1);
        }
        m = re.matcher(oldText);
        if (m.find()) {
            oldVal = m.group(1);
        }
        if (!oldVal.equals(newVal)) {
            final String message = String.format(Locale.ROOT,
                    "Decoy prefix in Philosopher DB Annotate options has changed "
                    + "from '%s' to '%s'.\n"
                    + "Do you want to also change it in other commands as well?", oldVal, newVal);

            // does the user want to chnage the Report tag automatically?
            int ans = JOptionPane.showConfirmDialog(this, message, "Decoy prefix change", JOptionPane.YES_NO_OPTION);
            if (ans == JOptionPane.YES_OPTION) {
                updateDecoyTagSeqDb(newVal, false);
                updateDecoyTagPepProphCmd(newVal, false);
                updateDecoyTagReportFilter(newVal, false);
            }
        }
    }
    
    /**
     * Called with null from FocusChange listener. Call it with a new value
     * if you want to update the field programmatically.
     */
    private void validateAndSaveReportFilter(final String newText, boolean updateOtherTags) {
        final JTextComponent comp = textReportFilter;
        final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER, 
                newText, ValidateTrue.getInstance());        
        
        if (!isValid)
            return;

        // check if the filter line has changed since focus was gained
        final String savedText = textReportFilterFocusGained;
        final String oldText = savedText != null ? savedText : comp.getText().trim();
        final String updText = newText != null ? newText : comp.getText().trim();
        
        if (!updateOtherTags || oldText.equals(updText)) // newText == null means it was a programmatic update
            return;
        
        // check if the reverse tag has changed
        Pattern re = reDecoyTagReportFilter;
        String oldVal = "", newVal = "";
        Matcher m = re.matcher(updText);
        if (m.find()) {
            newVal = m.group(1);
        }
        m = re.matcher(oldText);
        if (m.find()) {
            oldVal = m.group(1);
        }
        if (!oldVal.equals(newVal)) {
            final String message = String.format(Locale.ROOT,
                    "Decoy prefix in Philosopher Report options has changed "
                    + "from '%s' to '%s'.\n"
                    + "Do you want to also change it in other commands as well?", oldVal, newVal);

            // does the user want to chnage the Report tag automatically?
            int ans = JOptionPane.showConfirmDialog(this, message, "Decoy prefix change", JOptionPane.YES_NO_OPTION);
            if (ans == JOptionPane.YES_OPTION) {
                updateDecoyTagSeqDb(newVal, false);
                updateDecoyTagPepProphCmd(newVal, false);
                updateDecoyTagReportAnnotate(newVal, false);
            }
        }
    }
    
    public String getFastaPath() {
        return textSequenceDbPath.getText().trim();
    }
    
    public void setFastaPath(String path) {
        textSequenceDbPath.setText(path);
    }

    private void btnRunActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRunActionPerformed

        resetRunButtons(false);

        boolean doRunFragger = fraggerPanel.isRunMsfragger();
        boolean doRunAnyOtherTools = chkRunPeptideProphet.isSelected() 
                                  || chkRunProteinProphet.isSelected() 
                                  || checkCreateReport.isSelected();

        if (!fraggerPanel.isRunMsfragger()
                && !chkRunPeptideProphet.isSelected()
                && !chkRunProteinProphet.isSelected()
                && !checkCreateReport.isSelected()) {
            JOptionPane.showMessageDialog(this, "Nothing to run.\n"
                    + "Please mark checkboxes in other tabs to run processing tools.", "Error", JOptionPane.WARNING_MESSAGE);
            resetRunButtons(true);
            return;
        }

        // check for TSV output when any other downstream tools are requested
        if (doRunFragger && doRunAnyOtherTools) {
            if (fraggerPanel.getOutputType().equals(FraggerOutputType.TSV)) {
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
                    + "Please select an existing directory for the output.", "Error", JOptionPane.WARNING_MESSAGE);
            resetRunButtons(true);
            return;
        }
        final Path workingDirPath = Paths.get(workingDir);

        if (!Files.exists(workingDirPath)) {
            int confirmCreation = JOptionPane.showConfirmDialog(this, "Output directory doesn't exist. Create?",
                    "Create output directory?", JOptionPane.OK_CANCEL_OPTION);
            switch (confirmCreation) {
                case JOptionPane.OK_OPTION:
                    try {
                        Files.createDirectories(workingDirPath);
                    } catch (Exception e) {
                        // something went not right during creation of directory structure
                        JOptionPane.showMessageDialog(this, "Could not create directory structure", "Error", JOptionPane.ERROR_MESSAGE);
                        resetRunButtons(true);
                        return;
                    }
                    break;
                default:
                    resetRunButtons(true);
                    return;
            }
        }

        List<String> lcmsFilePaths = getLcmsFilePaths();
        if (lcmsFilePaths.isEmpty()) {
            JOptionPane.showMessageDialog(this, "No LC/MS data files selected.\n"
                    + "Check 'Select Raw Files' tab.", "Error", JOptionPane.WARNING_MESSAGE);
            resetRunButtons(true);
            return;
        }

        List<ProcessBuilder> processBuilders = new ArrayList();
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
        String dateString = df.format(new Date());

        if (!OsUtils.isWindows()) {
            // On Linux create symlinks to mzXML files
            try {
                createLcmsFileSymlinks(Paths.get(workingDir));
            } catch (IOException ex) {
                String msg = String.format(Locale.ROOT, "Something went wronng when creating symlinks to LCMS files.\n%s", ex.getMessage());
                JOptionPane.showMessageDialog(this, msg, "Error", JOptionPane.ERROR_MESSAGE);

                resetRunButtons(true);
                return;
            }
        } else {
            // On windows copy the files over to the working directory
//            List<ProcessBuilder> processBuildersCopyFiles = processBuildersCopyFiles(programsDir, workingDir, lcmsFilePaths);
//            processBuilders.addAll(processBuildersCopyFiles);
        }

        // check fasta file path
        String fastaPath = textSequenceDbPath.getText().trim();
        if (StringUtils.isNullOrWhitespace(fastaPath)) {
            JOptionPane.showMessageDialog(this, "Fasta file path (Sequence DB tab) can't be empty",
                        "Warning", JOptionPane.WARNING_MESSAGE);
            resetRunButtons(true);
            return;
        }
        String fastaPathOrig = fastaPath;
        fastaPath = PathUtils.testFilePath(fastaPath, workingDir);
        if (fastaPath == null) {
            JOptionPane.showMessageDialog(this, String.format("Could not find fasta file (Sequence DB) at:\n%s", fastaPathOrig),
                    "Errors", JOptionPane.ERROR_MESSAGE);
            resetRunButtons(true);
            return;
        }
        
        
        // check that all input files are in the same folder for Labelfree quant
        if (checkLabelfree.isSelected() && !lcmsFilePaths.isEmpty()) {
            try {
                String firstRaw = lcmsFilePaths.get(0);
                Path firstDir = Paths.get(firstRaw).getParent();
                for (String f : lcmsFilePaths) {
                    if (!firstDir.equals(Paths.get(f).getParent())) {
                        JOptionPane.showMessageDialog(this, "Not all input raw files are in the same folder.\n"
                                + "Label free quant requires all files to be in the same directory :/",
                                "Errors", JOptionPane.ERROR_MESSAGE);
                        resetRunButtons(true);
                        return;
                    }
                }
            } catch (Exception e) {
                JOptionPane.showMessageDialog(this, String.format("Error validating raw file paths for quant"),
                    "Errors", JOptionPane.ERROR_MESSAGE);
                resetRunButtons(true);
                return;
            }
        }
        
        
        // we will now compose parameter objects for running processes.
        // at first we will try to load the base parameter files, if the file paths
        // in the GUI are not empty. If empty, we will load the defaults and
        // add params from the GUI to it.
        List<ProcessBuilder> processBuildersFragger = processBuildersFragger("", workingDir, lcmsFilePaths, dateString);
        if (processBuildersFragger == null) {
            resetRunButtons(true);
            return;
        }
        processBuilders.addAll(processBuildersFragger);
        // if we have at least one MSFragger task, check for MGF file presence
        if (!processBuildersFragger.isEmpty()) {
            // check for MGF files and warn
            String warn = ThisAppProps.load(ThisAppProps.PROP_MGF_WARNING, Boolean.TRUE.toString());
            if (warn != null && Boolean.valueOf(warn)) {
                for (String f : lcmsFilePaths) {
                    if (f.toLowerCase().endsWith(".mgf")) {
                        JCheckBox checkbox = new JCheckBox("Do not show this message again.");
                        String msg = String.format(Locale.ROOT, "The list of input files contains MGF entries.\n"
                                + "MSFragger has limited MGF support (ProteoWizard output is OK).\n"
                                + "The search might fail unexpectedly with errors.\n"
                                + "Please consider converting files to mzML/mzXML with ProteoWizard.");
                        Object[] params = {msg, checkbox};
                        JOptionPane.showMessageDialog(this, params, "Warning", JOptionPane.WARNING_MESSAGE);
                        if (checkbox.isSelected()) {
                            ThisAppProps.save(ThisAppProps.PROP_MGF_WARNING, Boolean.FALSE.toString());
                        }
                        break;
                    }
                }
            }
        }

        List<ProcessBuilder> processBuildersPeptideProphet = processBuildersPeptideProphet("", workingDir, lcmsFilePaths);
        if (processBuildersPeptideProphet == null) {
            resetRunButtons(true);
            return;
        }

        List<ProcessBuilder> processBuildersProteinProphet = processBuildersProteinProphet("", workingDir, lcmsFilePaths);
        if (processBuildersProteinProphet == null) {
            resetRunButtons(true);
            return;
        }

        List<ProcessBuilder> processBuildersReport = processBuildersReport("", workingDir, lcmsFilePaths);
        if (processBuildersReport == null) {
            resetRunButtons(true);
            return;
        }

        // if any of Philosopher stuff needs to be run, then clean/init the "workspace"
        if (!processBuildersPeptideProphet.isEmpty()
                || !processBuildersProteinProphet.isEmpty()
                || !processBuildersReport.isEmpty()) {
            String bin = textBinPhilosopher.getText().trim();
            bin = PathUtils.testBinaryPath(bin, "");
            boolean isPhilosopher = isPhilosopherBin(bin);

            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--clean");
                ProcessBuilder pb = new ProcessBuilder(cmd);
                processBuilders.add(pb);
            }

            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--init");
                ProcessBuilder pb = new ProcessBuilder(cmd);
                processBuilders.add(pb);
            }
        }
        
        
        // Check Decoy tags if any of the downstream tools are requested
        if (doRunAnyOtherTools) { // downstream tools
            if (StringUtils.isNullOrWhitespace(textDecoyTagSeqDb.getText())) {
                int confirm = JOptionPane.showConfirmDialog(this,
                        "Downstream analysis tools require decoys in the database,\n"
                        + "but the decoy tag was left empty. It's recommended that\n"
                        + "you set it.\n\n"
                        + "Cancel operation and fix the problem (manually)?",
                        "Cancel and fix parameters before run?\n", JOptionPane.YES_NO_OPTION);
                if (JOptionPane.YES_OPTION == confirm) {
                    resetRunButtons(true);
                    return;
                }
            } else if (!checkDecoyTagsEqual()) {
                int confirm = JOptionPane.showConfirmDialog(this,
                        "Decoy sequence database tags differ between various tools\n"
                        + "to be run.\n\n"
                        + "This will most likely result in errors or incorrect results.\n\n"
                        + "It's recommended that you change decoy tags to the same value.\n"
                        + "You can switch to 'Sequence DB' tab and change it there,\n"
                        + "you'll be offered to automatically change the values in other places.\n\n"
                        + "Cancel operation and fix the problem (manually)?",
                        "Cancel and fix parameters before run?\n", JOptionPane.YES_NO_OPTION);
                if (JOptionPane.YES_OPTION == confirm) {
                    resetRunButtons(true);
                    return;
                }
            }
                
        }
        

        processBuilders.addAll(processBuildersPeptideProphet);
        processBuilders.addAll(processBuildersProteinProphet);
        processBuilders.addAll(processBuildersReport);

        if (!OsUtils.isWindows()) {
            // On Linux we created symlinks to mzXML files, leave them there
        } else {
            // On windows we copied the files over to the working directory
            // so will delete them now
//            List<ProcessBuilder> processBuildersDeleteFiles = processBuildersDeleteFiles(workingDir, lcmsFilePaths);
//            processBuilders.addAll(processBuildersDeleteFiles);
        }

        StringBuilder sbSysinfo = new StringBuilder();
        sbSysinfo.append(OsUtils.OsInfo()).append("\n");
        sbSysinfo.append(OsUtils.JavaInfo()).append("\n");
        LogUtils.println(console, String.format(Locale.ROOT, "System info:\n%s", sbSysinfo.toString()));

        StringBuilder sbVer = new StringBuilder();
        sbVer.append(Version.PROGRAM_TITLE).append(" version ").append(Version.VERSION).append("\n");
        sbVer.append("MSFragger version ").append(fraggerVer).append("\n");
        sbVer.append("Philosopher version ").append(philosopherVer).append("\n");
        LogUtils.println(console, String.format(Locale.ROOT, "Version info:\n%s", sbVer.toString()));

        LogUtils.println(console, String.format(Locale.ROOT, "Will execute %d commands:", processBuilders.size()));
        for (final ProcessBuilder pb : processBuilders) {
            StringBuilder sb = new StringBuilder();
            List<String> command = pb.command();
            for (String commandPart : command) {
                sb.append(commandPart).append(" ");
            }
            LogUtils.println(console, sb.toString());
            LogUtils.println(console, "");
        }
        LogUtils.println(console, "~~~~~~~~~~~~~~~~~~~~~~");
        LogUtils.println(console, "");
        LogUtils.println(console, "");

        if (checkDryRun.isSelected()) {
            LogUtils.println(console, "Dry Run selected, not running the commands.");
            resetRunButtons(true);
            return;
        }

        if (exec != null && !exec.isTerminated()) {
            exec.shutdownNow();
        }

        exec = Executors.newFixedThreadPool(1);
        try // run everything
        {
            final ProcessResult[] processResults = new ProcessResult[processBuilders.size()];

            for (int i = 0; i < processBuilders.size(); i++) {

                final int index = i;
                final ProcessBuilder pb = processBuilders.get(index);
                final ProcessResult pr = new ProcessResult(pb);
                processResults[index] = pr;

                Path wd = Paths.get(workingDir);
                pb.directory(wd.toFile());
                pr.setWorkingDir(wd);

                final Color green = new Color(122, 211, 38);
                final Color greenDarker = new Color(122, 211, 38);
                final Color greenDarkest = new Color(82, 140, 26);
                final Color red = new Color(236, 52, 42);
                final Color redDarker = new Color(198, 10, 0);
                final Color redDarkest = new Color(155, 35, 29);
                final Color black = new Color(0, 0, 0);
                REHandler reHandler;
                reHandler = new REHandler(new Runnable() {
                    @Override
                    public void run() {

                        StringBuilder command = new StringBuilder();
                        for (String part : pb.command()) {
                            command.append(part).append(" ");
                        }

                        // if it's not the first process, check that the previous
                        // one returned zero exit code
                        if (index > 0) {
                            Integer exitCode = processResults[index - 1].getExitCode();
                            if (exitCode == null) {
                                LogUtils.print(redDarker, console, true, "Cancelled execution of: ", false);
                                LogUtils.print(black, console, true, command.toString(), true);
                                return;
                            } else if (exitCode != 0) {
                                LogUtils.print(red, console, true, 
                                        String.format("Previous process returned exit code [%d], cancelling further processing..", exitCode), true);
                                LogUtils.print(redDarker, console, true, "Cancelled execution of: ", false);
                                LogUtils.print(black, console, true, command.toString(), true);
                                return;
                            }
                        }

                        Process process = null;
                        try {

                            LogUtils.println(console, "Executing command:\n$> " + command.toString());
                            process = pb.start();
                            pr.setStarted(true);
                            String toAppend = "Process started";
                            LogUtils.println(console, toAppend);

                            InputStream err = process.getErrorStream();
                            InputStream out = process.getInputStream();
                            while (true) {
                                Thread.sleep(200L);
                                int errAvailable = err.available();
                                if (errAvailable > 0) {
                                    byte[] bytes = new byte[errAvailable];
                                    int read = err.read(bytes);
                                    toAppend = new String(bytes);
                                    LogUtils.println(console, toAppend);
                                    pr.getOutput().append(toAppend);
                                }
                                int outAvailable = out.available();
                                if (outAvailable > 0) {
                                    byte[] bytes = new byte[outAvailable];
                                    int read = out.read(bytes);
                                    toAppend = new String(bytes);
                                    LogUtils.println(console, toAppend);
                                    pr.getOutput().append(toAppend);
                                }
                                try {
                                    final int exitValue = process.exitValue();
                                    pr.setExitCode(exitValue);
                                    //toAppend = String.format(Locale.ROOT, "Process finished, exit value: %d\n", exitValue);
                                    //LogUtils.println(console, toAppend); // changing this to manual call, because I want to print with color
                                    SwingUtilities.invokeLater(new Runnable() {
                                        @Override
                                        public void run() {
                                            Color c = exitValue == 0 ? greenDarker : red;
                                            console.append(c, String.format(
                                                    Locale.ROOT, "Process finished, exit value: %d\n", exitValue));
                                        }
                                    });

                                    break;
                                } catch (IllegalThreadStateException ignore) {
                                    // this error is thrown by process.exitValue() if the underlying process has not yet finished
                                }
                            }

                        } catch (IOException ex) {
                            String toAppend = String.format(Locale.ROOT, "IOException: Error in process,\n%s", ex.getMessage());
                            LogUtils.println(console, toAppend);
                        } catch (InterruptedException ex) {
                            if (process != null) {
                                process.destroy();
                            }
                            String toAppend = String.format(Locale.ROOT, "InterruptedException: Error in process,\n%s", ex.getMessage());
                            LogUtils.println(console, toAppend);
                        }
                    }
                }, console, System.err);
                exec.submit(reHandler);

                // On windows try to schedule copied mzXML file deletion
                if (OsUtils.isWindows()) {
                    REHandler deleteTask = new REHandler(new Runnable() {
                        @Override
                        public void run() {
                            List<String> lcmsFiles = getLcmsFilePaths();
                            List<Path> copiedFiles = getLcmsFilePathsInWorkdir(Paths.get(workingDir));
                            if (lcmsFiles.size() != copiedFiles.size()) {
                                throw new IllegalStateException("LCMS file list sizes should be equal.");
                            }
                            for (int i = 0; i < lcmsFiles.size(); i++) {
                                Path origPath = Paths.get(lcmsFiles.get(i));
                                Path linkPath = copiedFiles.get(i);
                                if (!linkPath.getParent().equals(origPath.getParent())) {
                                    linkPath.toFile().deleteOnExit();
                                }
                            }

                        }
                    }, console, System.err);
                    exec.submit(deleteTask);
                }
            }
        } finally {

        }

        final JButton btnStartPtr = btnRun;
        final JButton btnStopPtr = btnStop;
        REHandler finalizerTask = new REHandler(new Runnable() {
            @Override
            public void run() {
                submittedProcesses.clear();
                btnStartPtr.setEnabled(true);
                btnStopPtr.setEnabled(false);
                LogUtils.println(console, String.format("========================="));
                LogUtils.println(console, String.format("==="));
                LogUtils.println(console, String.format("===        Done"));
                LogUtils.println(console, String.format("==="));
                LogUtils.println(console, String.format("========================="));
            }
        }, console, System.err);

        exec.submit(finalizerTask);

        exec.shutdown();

    }//GEN-LAST:event_btnRunActionPerformed

    /**
     * Check that decoy tags are the same in:<br/>
     * <ul>
     * <li>Sequence DB tab</li>
     * <li>Peptide Prophet</li>
     * <li>Report Annotate</li>
     * <li>Report Filter</li>
     * </ul>
     * @return 
     */
    private boolean checkDecoyTagsEqual() {
        List<String> tags = Arrays.asList(
                getRegexMatch(reDecoyTagSequenceDb, textDecoyTagSeqDb.getText(), 1),
                getRegexMatch(reDecoyTagPepProphCmd, textPepProphCmd.getText(), 1),
                getRegexMatch(reDecoyTagReportAnnotate, textReportAnnotate.getText(), 1),
                getRegexMatch(reDecoyTagReportFilter, textReportFilter.getText(), 1)
                );
        HashSet<String> set = new HashSet<>(tags);
        return set.size() == 1;
    }
    
    private String getRegexMatch(Pattern re, String text, int groupNum) {
        Matcher m = re.matcher(text);
        return m.find() ? m.group(groupNum) : "";
    }
    
    private void btnLoadDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnLoadDefaultsOpenActionPerformed
        int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentComponentForDialog(this),
                "Are you sure you want to load defaults for open search?\n"
                + "It's a search with large precursor mass tolerance\n"
                + "usually used to identify PTMs.\n"
                + "Will update parameters for MSFragger, both Prophets\n"
                + "and Report Filter.", "Confirmation", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION == confirmation) {
            fraggerPanel.loadDefaultsOpen();
            MsfraggerGuiFrame.SearchTypeProp type = MsfraggerGuiFrame.SearchTypeProp.open;
            loadDefaultsSequenceDb(type);
            loadDefaultsPeptideProphet(type);
            loadDefaultsProteinProphet(type);
            loadDefaultsReportFilter(type);
            loadDefaultsReportAnnotate(type);
        }
    }//GEN-LAST:event_btnLoadDefaultsOpenActionPerformed

    private void btnLoadDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnLoadDefaultsClosedActionPerformed
        int confirmation = JOptionPane.showConfirmDialog(SwingUtils.findParentComponentForDialog(this),
                "Are you sure you want to load defaults for open search?\n"
                + "It's a search with large precursor mass tolerance\n"
                + "usually used to identify PTMs.\n"
                + "Will update parameters for MSFragger, both Prophets\n"
                + "and Report Filter.", "Confirmation", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION == confirmation) {
            fraggerPanel.loadDefaultsClosed();
            MsfraggerGuiFrame.SearchTypeProp type = MsfraggerGuiFrame.SearchTypeProp.closed;
            loadDefaultsSequenceDb(type);
            loadDefaultsPeptideProphet(type);
            loadDefaultsProteinProphet(type);
            loadDefaultsReportFilter(type);
            loadDefaultsReportAnnotate(type);
        }
    }//GEN-LAST:event_btnLoadDefaultsClosedActionPerformed

    private void textPepProphCmdFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textPepProphCmdFocusLost
        validateAndSavePeptideProphetCmdLineOptions(null, true);
    }//GEN-LAST:event_textPepProphCmdFocusLost

    private void txtProteinProphetCmdLineOptsFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_txtProteinProphetCmdLineOptsFocusLost
        String val = txtProteinProphetCmdLineOpts.getText();
        ThisAppProps.save(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, val);
    }//GEN-LAST:event_txtProteinProphetCmdLineOptsFocusLost

    private void btnPepProphDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPepProphDefaultsOpenActionPerformed
        loadDefaultsPeptideProphet(SearchTypeProp.open);
    }//GEN-LAST:event_btnPepProphDefaultsOpenActionPerformed

    private void btnPepProphDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnPepProphDefaultsClosedActionPerformed
        loadDefaultsPeptideProphet(SearchTypeProp.closed);
    }//GEN-LAST:event_btnPepProphDefaultsClosedActionPerformed

    private void btnProtProphDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnProtProphDefaultsOpenActionPerformed
        loadDefaultsProteinProphet(SearchTypeProp.open);
    }//GEN-LAST:event_btnProtProphDefaultsOpenActionPerformed

    private void btnProtProphDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnProtProphDefaultsClosedActionPerformed
        loadDefaultsProteinProphet(SearchTypeProp.closed);
    }//GEN-LAST:event_btnProtProphDefaultsClosedActionPerformed

    private void textReportFilterFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportFilterFocusLost
        validateAndSaveReportFilter(null, true);
    }//GEN-LAST:event_textReportFilterFocusLost

    private void btnAboutInConfigActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAboutInConfigActionPerformed
        btnAboutActionPerformed(null);
    }//GEN-LAST:event_btnAboutInConfigActionPerformed

    private void btnReportDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReportDefaultsClosedActionPerformed
        SearchTypeProp type = SearchTypeProp.closed;
        loadDefaultsReportFilter(type);
        loadDefaultsReportAnnotate(type);
    }//GEN-LAST:event_btnReportDefaultsClosedActionPerformed

    private void btnReportDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReportDefaultsOpenActionPerformed
        SearchTypeProp type = SearchTypeProp.open;
        loadDefaultsReportFilter(type);
        loadDefaultsReportAnnotate(type);
    }//GEN-LAST:event_btnReportDefaultsOpenActionPerformed

    private void checkReportProteinLevelFdrStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_checkReportProteinLevelFdrStateChanged
        boolean selected = checkReportProteinLevelFdr.isSelected();
        ThisAppProps.save(ThisAppProps.PROP_CHECKBOX_REPORT_PROTEIN_LEVEL_FDR, Boolean.toString(selected));
    }//GEN-LAST:event_checkReportProteinLevelFdrStateChanged

    private void textPepProphCmdFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textPepProphCmdFocusGained
        textPepProphetFocusGained = textPepProphCmd.getText().trim();
    }//GEN-LAST:event_textPepProphCmdFocusGained

    private void textReportFilterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textReportFilterActionPerformed
        
    }//GEN-LAST:event_textReportFilterActionPerformed

    private void textReportFilterFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportFilterFocusGained
        textReportFilterFocusGained = textReportFilter.getText();
    }//GEN-LAST:event_textReportFilterFocusGained

    private void btnExportLogActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnExportLogActionPerformed
        Action action = panelRun.getActionMap().get(ACTION_EXPORT_LOG);
        if (action != null) {
            action.actionPerformed(null);
        }
    }//GEN-LAST:event_btnExportLogActionPerformed

    private void textSequenceDbPathFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textSequenceDbPathFocusLost
        validateAndSaveFastaPath(textSequenceDbPath.getText());
    }//GEN-LAST:event_textSequenceDbPathFocusLost

    private void btnBrowseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnBrowseActionPerformed
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setApproveButtonText("Select");
        fileChooser.setDialogTitle("Select FASTA file");
        fileChooser.setMultiSelectionEnabled(false);
        FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("FASTA DB", "fasta", "fa", "fas", "fast");
        fileChooser.setFileFilter(fileNameExtensionFilter);

        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        final String propName = ThisAppProps.PROP_DB_FILE_IN;
        String fcPath = ThisAppProps.tryFindPath(Arrays.asList(propName), true);
        SwingUtils.setFileChooserPath(fileChooser, fcPath);

        int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentComponentForDialog(this));
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File foundFile = fileChooser.getSelectedFile();
                if (validateAndSaveFastaPath(foundFile.getAbsolutePath())) {
                    ThisAppProps.save(propName, foundFile.getAbsolutePath());
                }
                break;
        }
    }//GEN-LAST:event_btnBrowseActionPerformed

    private void textDecoyTagSeqDbFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textDecoyTagSeqDbFocusLost
        validateAndSaveDecoyTagSeqDb(null, true);
    }//GEN-LAST:event_textDecoyTagSeqDbFocusLost

    private void textDecoyTagSeqDbFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textDecoyTagSeqDbFocusGained
        textDecoyTagFocusGained = textDecoyTagSeqDb.getText().trim();
    }//GEN-LAST:event_textDecoyTagSeqDbFocusGained

    private void textReportAnnotateFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportAnnotateFocusLost
        validateAndSaveReportAnnotate(null, true);
    }//GEN-LAST:event_textReportAnnotateFocusLost

    private void textReportAnnotateFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportAnnotateFocusGained
        textReportAnnotateFocusGained = textReportAnnotate.getText().trim();
    }//GEN-LAST:event_textReportAnnotateFocusGained

    private void btnTryDetectDecoyTagActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnTryDetectDecoyTagActionPerformed
        Path p = null;
        try {
            p = Paths.get(textSequenceDbPath.getText());
            if (!Files.exists(p))
                throw new FileNotFoundException("File doesn't exist: " + p.toAbsolutePath().toString());
            
        } catch (Exception e)  {
            JOptionPane.showConfirmDialog(btnTryDetectDecoyTag, 
                    "<html>Could not open sequence database file", "File not found", 
                    JOptionPane.OK_OPTION, JOptionPane.ERROR_MESSAGE);
            return;
        }
        
        try (BufferedReader br = new BufferedReader(new InputStreamReader(Files.newInputStream(p), "UTF-8"))) {
            String line;
            List<String> descriptors = new ArrayList<>();
            List<List<String>> ordered = new ArrayList<>();
            long totalDescriptors = 0;
            long totalLines = 0;
            while ((line = br.readLine()) != null) {
                if (!line.startsWith(">"))
                    continue;
                totalLines++;
                int pos = 1, next;
                int depth = 1;
                while ((next = line.indexOf('|', pos)) >= 0 || pos < line.length() - 1 ) {
                    if (next < 0) {
                        next = line.length();
                    }
                    String desc = line.substring(pos, next).trim();
                    descriptors.add(desc);
                    if (ordered.size() < depth)
                        ordered.add(new ArrayList<String>());
                    ordered.get(depth-1).add(desc);
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
                    Proc2<PrefixCounter.Node, PrefixCounter.Mode> action = new Proc2<PrefixCounter.Node, PrefixCounter.Mode>() {
                        @Override
                        public void call(PrefixCounter.Node n, PrefixCounter.Mode mode) {

                            PrefixCounter.Node cur = n;
                            //if (cur.getTerminals() > 0)
                            //    return; // a prefix or a suffix can never be the whole protein id
                            double pct = cur.getHits() / (double) total;
                            if (pct < pctMin || pct > pctMax)
                                    return;
                            sb.setLength(0);
                            while (cur != null) {
                                if (cur.parent != null)
                                    sb.append(cur.ch);
                                cur = cur.parent;
                            }

                            if (sb.length() < 2)
                                return; // no prefixes smaller than 2 characters
                            
                            StringBuilder sbPrint = sb.reverse();// mode == PrefixCounter.Mode.REV ? sb.reverse() : sb;
                            result.add(new Tuple2<>(sbPrint.toString(), pct));
                            //System.out.printf("%s : (full string: %s) hits=%.1f%%\n", n, sbPrint.toString(), pct*100d);
                        }
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
                            if (pct < pctMin || pct > pctMax)
                                    return;
                            sb.setLength(0);
                            while (cur != null) {
                                if (cur.parent != null)
                                    sb.append(cur.ch);
                                cur = cur.parent;
                            }

                            if (sb.length() < 2)
                                return; // no suffixes smaller than 2 chars
                            
                            StringBuilder sbPrint = sb;// mode == PrefixCounter.Mode.REV ? sb.reverse() : sb;
                            result.add(new Tuple2<>(sbPrint.toString(), pct));
                            //System.out.printf("%s : (full string: %s) hits=%.1f%%\n", n, sbPrint.toString(), pct*100d);
                        }
                    };
//                    System.out.println("Suffixes:");
                    cntRev.iterPrefixCounts(maxDepth, action);
                    suffixesByCol.add(cleanUpDecoyTagCandidates(result));
//                    for (Tuple2<String, Double> tuple2 : cleanedResult) {
//                        System.out.printf("% 3.1f%% -> %s\n", tuple2.item2 * 100d, tuple2.item1);
//                    }
//                    System.out.println("Suffixes Done");
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
                if (i == 0)
                    supportedPrefixes = list.size();
            }
            for (List<Tuple2<String, Double>> list : suffixesByCol) {
                totalCandidates += list.size();
                totalSuffixes += list.size();
            }
            
            String selectedPrefix = null;
            if (totalCandidates == 0) {
                String msg = String.format(Locale.ROOT, "No candidates for decoy tags found");
                String[] options = {"Ok"};
                int result = JOptionPane.showOptionDialog(this, msg, "Nothing found",
                        JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);
            
            } else if (supportedPrefixes == 1) {
                // good, we've found the one good decoy prefix
                Tuple2<String, Double> prefix = prefixesByCol.get(0).get(0);
                StringBuilder sb = new StringBuilder();
                sb.append(String.format(Locale.ROOT, 
                        "Found candidate decoy tag: \n\"%s\" in % 3.1f%% entries", prefix.item1, prefix.item2*100d));
                sb.append("\n\nAll found candidates:");
                appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
                String[] options = {"Set \"" + prefix.item1 + "\" as decoy tag", "Cancel"};
                int result = JOptionPane.showOptionDialog(this, sb.toString(), "Found prefix",
                        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
                if (result == 0)
                    selectedPrefix = prefix.item1;
            
            } else if (supportedPrefixes > 1) {
                // several possible prefixes found
                StringBuilder sb = new StringBuilder();
                sb.append("Found several possible supported decoy tag prefixes.\n")
                        .append("Note: only prefixes in the 1st column are supported by downstream tools.\n");
                appendFoundPrefixes(sb, prefixesByCol, suffixesByCol);
                sb.append("\nOnly supported variants are lsited on buttons below.\n");
                
                List<Tuple2<String, Double>> supported = prefixesByCol.get(0);
                String[] options = new String[supported.size() + 1];
                options[options.length-1] = "Cancel";
                for (int i = 0; i < supported.size(); i++) {
                    options[i] = String.format("Set \"%s\"", supported.get(i).item1);
                }
                int result = JOptionPane.showOptionDialog(this, sb.toString(), "Found several possible prefixes",
                        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0]);
                if (result >=0 && result < options.length - 1)
                    selectedPrefix = supported.get(result).item1;
                
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
                updateDecoyTagPepProphCmd(selectedPrefix, false);
                updateDecoyTagReportAnnotate(selectedPrefix, false);
                updateDecoyTagReportFilter(selectedPrefix, false);
            }
            
        } catch (IOException ex) {
            JOptionPane.showConfirmDialog(btnTryDetectDecoyTag, 
                    "<html>Error reading sequence database file", "Error", 
                    JOptionPane.OK_OPTION, JOptionPane.ERROR_MESSAGE);
        }
    }//GEN-LAST:event_btnTryDetectDecoyTagActionPerformed

    private void appendFoundPrefixes(StringBuilder sb,
            List<List<Tuple2<String, Double>>> prefixesByCol, List<List<Tuple2<String, Double>>> suffixesByCol) {
        
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
                            i+1, tuple2.item1, tuple2.item2*100d));
                }
            }
        }
        
        if (totalSuffixes > 0) {
            sb.append(tab1).append("\nSuffixes:\n");
            for (int i = 0; i < suffixesByCol.size(); i++) {
                for (Tuple2<String, Double> tuple2 : suffixesByCol.get(i)) {
                    sb.append(tab2).append(String.format("\tColumn #%d: \"%s\" in % 3.1f%% entries\n", 
                            i+1, tuple2.item1, tuple2.item2*100d));
                }
            }
        }
        
    }
    
    private List<Tuple2<String, Double>> cleanUpDecoyTagCandidates(List<Tuple2<String, Double>> candidates) {
        List<Tuple2<String, Double>> result = new ArrayList<>();
        
        Collections.sort(candidates, new Comparator<Tuple2<String, Double>>() {
            @Override
            public int compare(Tuple2<String, Double> t1, Tuple2<String, Double> t2) {
                int cmp0 = Double.compare(Math.abs(t1.item2 - 0.5), Math.abs(t2.item2 - 0.5));
                if (cmp0 == 0) {
                    cmp0 = t2.item1.compareTo(t1.item1);
                }
                return cmp0;
            }
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
            if (isBest)
                result.add(cur);
        }
        return result;
    }
    
    private void formWindowOpened(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowOpened
        
    }//GEN-LAST:event_formWindowOpened

    private void btnMsfraggerUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerUpdateActionPerformed
        try {
            String url = MsfraggerProps.loadProperties().getProperty(MsfraggerProps.PROP_UPDATESERVER_WEBSITE_URL);
            Desktop.getDesktop().browse(URI.create(url));
        } catch (IOException ex) {
            throw new IllegalStateException("Could not open MSFragger update link in browser.", ex);
        }
    }//GEN-LAST:event_btnMsfraggerUpdateActionPerformed

    private void textReportLabelfreeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textReportLabelfreeActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_textReportLabelfreeActionPerformed

    private void textReportLabelfreeFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportLabelfreeFocusGained
        textLabelfreeFocusGained = textReportLabelfree.getText();
    }//GEN-LAST:event_textReportLabelfreeFocusGained

    private void textReportLabelfreeFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportLabelfreeFocusLost
        validateAndSaveLabelfree(null);
    }//GEN-LAST:event_textReportLabelfreeFocusLost

    private void textBinMsfraggerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textBinMsfraggerActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_textBinMsfraggerActionPerformed

    private void btnOpenInExplorerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnOpenInExplorerActionPerformed
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

    private void checkCreateReportActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkCreateReportActionPerformed
        final boolean selected = checkCreateReport.isSelected();
        Container[] comps = new Container[]{
            panelReportOptions
        };
        for (Container c : comps) {
            SwingUtils.enableComponents(c, selected);
        }
    }//GEN-LAST:event_checkCreateReportActionPerformed

    private void checkReportFilterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkReportFilterActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_checkReportFilterActionPerformed

    public void loadLastPeptideProphet() {
        if (!load(textPepProphCmd, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET)) {
            loadDefaultsPeptideProphet(DEFAULT_TYPE);
        }
    }

    public void loadDefaultsPeptideProphet(SearchTypeProp type) {
        loadDefaults(textPepProphCmd, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET, type);
    }

    public void loadLastProteinProphet() {
        if (!load(txtProteinProphetCmdLineOpts, ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET)) {
            loadDefaultsProteinProphet(DEFAULT_TYPE);
        }
    }

    public void loadDefaultsProteinProphet(SearchTypeProp type) {
        loadDefaults(txtProteinProphetCmdLineOpts, ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, type);
    }
    
    private void loadLastDecoyTag() {
        String val = ThisAppProps.load(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);
        if (val != null) {
            textDecoyTagSeqDb.setText(val);
        } else {
            loadDefaultDecoyTag();
        }
    }
    
    private void loadDefaultDecoyTag() {
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle(Version.PATH_BUNDLE);
        String val = bundle.getString(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);
        
        textReportFilter.setText(val);
        ThisAppProps.save(ThisAppProps.PROP_TEXTFIELD_DECOY_TAG, val);
    }

    private void loadLastReportFilter() {
        if (!load(textReportFilter, ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER)) {
            loadDefaultsReportFilter(DEFAULT_TYPE);
        }
    }

    private void loadDefaultsReportFilter(SearchTypeProp type) {
        loadDefaults(textReportFilter, ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER, type);
    }

    private void loadLastReportProteinLevelFdr() {
        String v = ThisAppProps.load(ThisAppProps.PROP_CHECKBOX_REPORT_PROTEIN_LEVEL_FDR);
        if (v == null) {
            checkReportProteinLevelFdr.setSelected(true);
        } else {
            Boolean wasSelected = Boolean.valueOf(v);
            checkReportProteinLevelFdr.setSelected(wasSelected);
        }
    }
    
    private void loadLastSequenceDb() {
        String val = ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN);
        if (val != null) {
            textSequenceDbPath.setText(val);
        }
    }

    private void validateAndSavePeptideProphetCmdLineOptions(final String newText, boolean updateOtherTags) {
        final JTextComponent comp = textPepProphCmd;
        final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET, 
                newText, ValidateTrue.getInstance());    
        
        if (!isValid)
            return;
        
        // check if the filter line has changed since focus was gained
        final String savedText = textPepProphetFocusGained;
        final String oldText = savedText != null ? savedText : comp.getText().trim();
        final String updText = newText != null ? newText : comp.getText().trim();
        
        if (!updateOtherTags || oldText.equals(updText)) // newText == null means it was a programmatic update
            return;
        
        // text in the field has changed
        Pattern re = reDecoyTagPepProphCmd;
        String newDecoyPrefix = "", oldDecoyPrefix = "";
        Matcher m = re.matcher(updText);
        if (m.find()) {
            newDecoyPrefix = m.group(1);
        }
        m = re.matcher(oldText);
        if (m.find()) {
            oldDecoyPrefix = m.group(1);
        }

        // if the new prefix differs from the old one
        if (!oldDecoyPrefix.equals(newDecoyPrefix)) {
            final String message = String.format(
                    "Decoy prefix in PepetideProphet options has changed from '%s' to '%s'.\n"
                    + "Do you want to also change it in other commands?", oldDecoyPrefix, newDecoyPrefix);

            // does the user want to chnage the Report tag automatically?
            int ans = JOptionPane.showConfirmDialog(this, message, "Decoy prefix change", JOptionPane.YES_NO_OPTION);
            if (ans == JOptionPane.YES_OPTION) {
                updateDecoyTagSeqDb(newDecoyPrefix, false);
                updateDecoyTagReportAnnotate(newDecoyPrefix, false);
                updateDecoyTagReportFilter(newDecoyPrefix, false);
            }
        }
    }

    private boolean validateAndSaveFastaPath(String path) {
        boolean isValid = validateFastaPath(path);
        if (isValid) {
            textSequenceDbPath.setText(path);
            ThisAppProps.save(ThisAppProps.PROP_DB_FILE_IN, path);
            Thread thread;
            thread = new Thread(new Runnable() {
                @Override
                public void run() {
                    Path p = Paths.get(textSequenceDbPath.getText());
                    if (!Files.exists(p))
                        return;
                    try (BufferedReader br = new BufferedReader(new InputStreamReader(Files.newInputStream(p), "UTF-8"))) {
                        String line;
                        final List<String> descriptors = new ArrayList<>();
                        while ((line = br.readLine()) != null) {
                            if (!line.startsWith(">"))
                                continue;
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
                }
            });
            thread.start();
        }
        
        final JComponent anchor = textSequenceDbPath;
        final String name = "textSequenceDbPath";
        BalloonTip tip = tipMap.remove(name);
        if (tip != null)
            tip.closeBalloon();
        
        if (!isValid) {
            tip = new BalloonTip(anchor, "<html>Could not find sequence DB file.");
            tip.setVisible(true);
            tipMap.put(name, tip);
        }

        return isValid;
    }

    private boolean validateFastaPath(String path) {
        if (StringUtils.isNullOrWhitespace(path))
            return false;
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
        
        if (!isValid)
            return;
        
        // check if the filter line has changed since focus was gained
        final String savedText = textDecoyTagFocusGained;
        final String oldText = savedText != null ? savedText : comp.getText().trim();
        final String updText = newText != null ? newText : comp.getText().trim();
        
        if (!updateOtherTags || oldText.equals(updText)) // newText == null means it was a programmatic update
            return;
        
        final String message = String.format(Locale.ROOT,
                "Decoy prefix has changed: from '%s', to '%s'.\n"
                + "Do you want to also change it in PeptideProphet, Report commands?", oldText, updText);
        int ans = JOptionPane.showConfirmDialog(this, message, "Decoy prefix change", JOptionPane.YES_NO_OPTION);
        if (ans == JOptionPane.YES_OPTION) {
            updateDecoyTagPepProphCmd(updText, false);
            updateDecoyTagReportAnnotate(updText, false);
            updateDecoyTagReportFilter(updText, false);
            
        }
    }
        
    private void updateTextCmdLine(Pattern re, JTextComponent textComp, String newVal, String prefix) {
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
    
    private void updateDecoyTagPepProphCmd(String newVal, boolean updateOtherTags) {
        updateTextCmdLine(reDecoyTagPepProphCmd, textPepProphCmd, newVal, "--decoy");
        validateAndSavePeptideProphetCmdLineOptions(null, updateOtherTags);
    }
    
    private void updateDecoyTagReportFilter(String newVal, boolean updateOtherTags) {
        updateTextCmdLine(reDecoyTagReportFilter, textReportFilter, newVal, "--tag");
        validateAndSaveReportFilter(null, updateOtherTags);
    }
    
    private void updateDecoyTagReportAnnotate(String newVal, boolean updateOtherTags) {;
        updateTextCmdLine(reDecoyTagReportAnnotate, textReportAnnotate, newVal, "--prefix");
        validateAndSaveReportAnnotate(null, updateOtherTags);
    }

    private boolean load(JTextComponent text, String propName) {
        String val = ThisAppProps.load(propName);
        if (val != null) {
            text.setText(val);
            return true;
        }
        return false;
    }
    
    private void save(JTextComponent text, String propName) {
        ThisAppProps.save(propName, text.getText().trim());
    }
    
    private void loadLastReportAnnotate() {
        if (!load(textReportAnnotate, ThisAppProps.PROP_TEXTFIELD_REPORT_ANNOTATE)) {
            loadDefaultsReportAnnotate(DEFAULT_TYPE);
        }
    }
    
    private void loadDefaultsReportAnnotate(SearchTypeProp type) {
        loadDefaults(textReportAnnotate, ThisAppProps.PROP_TEXTFIELD_REPORT_ANNOTATE, type);
    }
    
    private void loadDefaults(JTextComponent text, String propName, SearchTypeProp type) {
        final String prop = propName + "." + type.name();
        loadDefaults(text, prop);
    }
    
    private void loadDefaults(JTextComponent text, String propName) {
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle(Version.PATH_BUNDLE);
        String val = bundle.getString(propName);
        text.setText(val);
        ThisAppProps.save(propName, val);
    }

    private void loadDefaultsSequenceDb(SearchTypeProp type) {
        loadDefaults(textDecoyTagSeqDb, ThisAppProps.PROP_TEXTFIELD_DECOY_TAG);
    }

    private void addChangeListenerTextSequenceDb() {
        SwingUtils.addChangeListener(textSequenceDbPath, new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (btnTryDetectDecoyTag != null)
                    btnTryDetectDecoyTag.setEnabled(!StringUtils.isNullOrWhitespace(textSequenceDbPath.getText()));
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

        // handle link events
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
    }

    private void loadLastLabelfree() {
        if (!load(textReportLabelfree, ThisAppProps.PROP_TEXTFIELD_LABELFREE)) {
            loadDefaultsLabelfree(DEFAULT_TYPE);
        }
    }
    
    private void loadDefaultsLabelfree(SearchTypeProp type) {
        loadDefaults(textReportLabelfree, ThisAppProps.PROP_TEXTFIELD_LABELFREE, type);
    }

    private void validateAndSaveLabelfree(final String newText) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        final JTextComponent comp = textReportLabelfree;
        final boolean isValid = validateAndSave(comp, ThisAppProps.PROP_TEXTFIELD_LABELFREE, 
                newText, new IValidateString() {
            @Override
            public boolean test(String s) {
                Pattern re = Pattern.compile("--([^\\s]+)");
                Matcher m = re.matcher(s);
                List<String> allowed = new ArrayList<>();
                allowed.add("ptw");
                allowed.add("tol");
                while (m.find()) {
                    if (!allowed.contains(m.group(1)))
                        return false;
                }
                
                for (String paramName : allowed) {
                    Pattern reFullParam = Pattern.compile(String.format("--%s\\s+(\\d+(?:\\.\\d+)?)", paramName));
                    if (!reFullParam.matcher(s).find())
                        return false;
                }
                
                return true;
            }
        });
        
        if (!isValid)
            return;

        // check if the filter line has changed since focus was gained
        final String savedText = textReportFilterFocusGained;
        final String oldText = savedText != null ? savedText : comp.getText().trim();
        final String updText = newText != null ? newText : comp.getText().trim();
    }

    public enum SearchTypeProp {
        open, closed
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
                        Version.PROGRAM_TITLE, PhilosopherProps.DOWNLOAD_URL));

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

        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
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
            }
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
            Desktop.getDesktop().browse(URI.create("https://github.com/prvst/philosopher/releases/latest"));
        } catch (IOException ex) {
            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void resetRunButtons(boolean runEnabled) {
        btnRun.setEnabled(runEnabled);
        btnStop.setEnabled(!runEnabled);
    }

    private List<String> getLcmsFilePaths() {
        ArrayList<Path> paths = tableModelRawFiles.dataCopy();
        ArrayList<String> result = new ArrayList<>(paths.size());
        for (Path p : paths) {
            result.add(p.toString());
        }
        return result;
    }

    /**
     * This returns the paths to files to be created. Might be symlinks or
     * actual file copies. It does not create the files!
     *
     * @param workDir
     * @return
     */
    private List<Path> getLcmsFilePathsInWorkdir(Path workDir) {
        List<String> lcmsFilePaths = getLcmsFilePaths();
        ArrayList<Path> result = new ArrayList<>();
        for (String lcmsFilePath : lcmsFilePaths) {
            result.add(workDir.resolve(Paths.get(lcmsFilePath).getFileName()));
        }
        return result;
    }

    private void createLcmsFileSymlinks(Path workDir) throws IOException {
        List<String> lcmsFilePaths = getLcmsFilePaths();
        List<Path> paths = new ArrayList<>();
        for (String s : lcmsFilePaths) {
            paths.add(Paths.get(s));
        }

        List<Path> links = getLcmsFilePathsInWorkdir(workDir);
        for (int i = 0; i < paths.size(); i++) {
            Path lcmsPath = paths.get(i);
            Path link = links.get(i);
            if (link.equals(lcmsPath)) {
                return;
            }
            if (Files.exists(link)) {
                // if that link already exists we need to make sure it points to
                // the same file
                if (!Files.isSymbolicLink(link)) {
                    throw new FileAlreadyExistsException(link.toString(), null, "A file already exists and is not a symbolic link");
                }
                Path linkTarget = Files.readSymbolicLink(link);
                if (!linkTarget.equals(lcmsPath)) {
                    String msg = String.format("A symblic link to mzXML file already exists, but points to a different file: %s", link);
                    throw new FileAlreadyExistsException(link.toString(), null, msg);
                }
                return;
            }
            Files.createSymbolicLink(link, lcmsPath);
        }
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

    private String getPhilosopherCitationHtml() {
        // for copying style
        Font font = lblMsfraggerCitation.getFont();

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
        sb.append("<a href=\"https://prvst.github.io/philosopher\">Philosopher GitHub page</a>");
        sb.append("<br/>");
        sb.append("</p>");

        sb.append("</body>");
        sb.append("</html>");

        return sb.toString();
    }

    private String getFraggerCitationHtml() {

        // for copying style
        Font font = lblMsfraggerCitation.getFont();

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
        sb.append("<a href=\"http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html\">MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry–based proteomics</a>");
        sb.append("<br/>");
        sb.append("<b>DOI:10.1038/nmeth.4256</b>");
        sb.append("</p>");

        sb.append("</body>");
        sb.append("</html>");

        return sb.toString();
    }

    private static class UmpireGarbageFiles {

        static List<String> filesToMove = Arrays.asList("diaumpire_se.log");
        static List<String> fileNameSuffixesToMove = Arrays.asList(
                "_Peak", ".DIAWindowsFS", ".RTidxFS",
                ".ScanClusterMapping_Q1", ".ScanClusterMapping_Q2", ".ScanClusterMapping_Q3",
                ".ScanidxFS", ".ScanPosFS", ".ScanRTFS", "_diasetting.ser", "_params.ser",
                "_Q1.mgf", "_Q2.mgf", "_Q3.mgf");
        List<String> toMove = new ArrayList<>();
    }

    private List<ProcessBuilder> processBuildersCopyFiles(String programsDir, String workingDir, List<String> lcmsFilePaths) {
        List<ProcessBuilder> processBuilders = new LinkedList<>();

        URI currentJarUri = PathUtils.getCurrentJarPath();
        String currentJarPath = Paths.get(currentJarUri).toAbsolutePath().toString();
        Path wd = Paths.get(workingDir);

        for (String lcmsFilePath : lcmsFilePaths) {

            Path fileIn = Paths.get(lcmsFilePath);
            if (fileIn.getParent().equals(wd)) {
                continue;
            }

            List<String> commands = new ArrayList<>();
            commands.add("java");
            commands.add("-cp");
            commands.add(currentJarPath);
            commands.add("dia.umpire.util.FileCopy");
            commands.add(lcmsFilePath);
            Path copyTo = Paths.get(workingDir, Paths.get(lcmsFilePath).getFileName().toString());
            commands.add(copyTo.toString());
            ProcessBuilder pb = new ProcessBuilder(commands);
            processBuilders.add(pb);
        }
        return processBuilders;
    }

    private boolean checkLcmsFileForDeletion(String workingDir, String lcmsFilePath) {
        Path wd = Paths.get(workingDir);
        Path file = Paths.get(lcmsFilePath);
        return !wd.equals(file.getParent());
    }

    private List<ProcessBuilder> processBuildersDeleteFiles(String workingDir, List<String> lcmsFilePaths) {
        List<ProcessBuilder> processBuilders = new LinkedList<>();

        URI currentJarUri = PathUtils.getCurrentJarPath();
        String currentJarPath = Paths.get(currentJarUri).toAbsolutePath().toString();

        Path wd = Paths.get(workingDir);

        for (String lcmsFilePath : lcmsFilePaths) {

            if (wd.equals(Paths.get(lcmsFilePath).getParent())) {
                continue;
            }

            List<String> commands = new ArrayList<>();
            commands.add("java");
            commands.add("-cp");
            commands.add(currentJarPath);
            commands.add("dia.umpire.util.FileDelete");
            Path copyTo = Paths.get(workingDir, Paths.get(lcmsFilePath).getFileName().toString());
            commands.add(copyTo.toString());
            ProcessBuilder pb = new ProcessBuilder(commands);
            processBuilders.add(pb);
        }
        return processBuilders;
    }

    private String getBinJava(String programsDir) {
        String binJava = "java";
        binJava = PathUtils.testBinaryPath(binJava, programsDir);
        if (binJava != null) {
            return binJava;
        }
        JOptionPane.showMessageDialog(this, "Java could not be found.\n"
                + "please make sure you have it installed \n"
                + "and that java.exe can be found on PATH", "Error", JOptionPane.ERROR_MESSAGE);
        return null;
    }

    /**
     * @return Combined protein file name without extension.
     */
    private String getCombinedProtOpt() {
        String combined = txtCombinedProtFile.getText().trim();
        String combinedOpt = null;
        if (StringUtils.isNullOrWhitespace(combined)) {
            combinedOpt = "interact";
        } else {
            final String protExt = ".prot.xml";
            if (combined.toLowerCase().endsWith(protExt)) {
                combinedOpt = combined.substring(0, combined.toLowerCase().indexOf(protExt));
            } else {
                combinedOpt = combined;
            }
        }

        return combinedOpt;
    }

    /**
     * @return Combined protein file name with extension.
     */
    private String getCombinedProtFileName() {
        return getCombinedProtOpt() + ".prot.xml";
    }

    private Path getCombinedProtFilePath(String workingDir) {
        String combinedProtFile = getCombinedProtFileName();
        if (StringUtils.isNullOrWhitespace(combinedProtFile)) {
            JOptionPane.showMessageDialog(this, String.format("Please specify ProteinProphet output path on ProteinProphet tab.\n"
                    + "This is needed even if you're not running ProteinProphet right now.\n"
                    + "In which case check the box to run it, add the filename and uncheck the filebox.\n"
                    + "Sorry for the inconvenience."),
                    "Errors", JOptionPane.ERROR_MESSAGE);
            return null;
        } else {
            Path combinedProtFileFullPath = Paths.get(workingDir, combinedProtFile).toAbsolutePath().normalize();
            return combinedProtFileFullPath;
        }
    }

    private boolean isPhilosopherBin(String binPathToCheck) {
        Pattern isPhilosopherRegex = Pattern.compile("philosopher", Pattern.CASE_INSENSITIVE);
        Matcher matcher = isPhilosopherRegex.matcher(binPathToCheck);
        return matcher.find();
    }

    private List<ProcessBuilder> processBuildersFragger(String programsDir, String workingDir, List<String> lcmsFilePaths, String dateStr) {
        List<ProcessBuilder> builders = new LinkedList<>();
        if (fraggerPanel.isRunMsfragger()) {

            final int numSlices = fraggerPanel.getNumSlices();
            final boolean isSlicing = numSlices > 1;
            if (isSlicing) {
                // slicing requested
                if (!slicingEnabled || slicingScriptPath == null) {
                    JOptionPane.showMessageDialog(this, 
                            "MSFragger number of DB slices requested was more than 1.\n"
                            + "However not all preconditions for enabling slicing were met.\n"
                            + "Check the bottom of \"Config\" tab for details.",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
                }
            }
            
            String bin = textBinMsfragger.getText().trim();
            if (StringUtils.isNullOrWhitespace(bin)) {
                JOptionPane.showMessageDialog(this, "Binary for running Fragger can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            bin = PathUtils.testFilePath(bin, programsDir);
            if (bin == null) {
                JOptionPane.showMessageDialog(this, "Binary for running Fragger not found or could not be run.\n"
                        + "Neither on PATH, nor in the working directory",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            String fastaPath = fraggerPanel.getFastaPath();
            if (StringUtils.isNullOrWhitespace(fastaPath)) {
                JOptionPane.showMessageDialog(this, "Fasta file path (Fragger) can't be empty",
                        "Warning", JOptionPane.WARNING_MESSAGE);
                return null;
            }

            // create a params file in the output directory
            MsfraggerParams params = null;
            try {
                params = fraggerPanel.collectParams();
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(this, "Could not collect MSFragger params from GUI.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            Path savedParamsPath = Paths.get(workingDir, MsfraggerParams.DEFAULT_FILE);
            try {
                params.save(new FileOutputStream(savedParamsPath.toFile()));
                // cache the params
                params.save();
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(this, "Could not save fragger.params file to working dir.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            int ramGb = fraggerPanel.getRamGb();

            Map<String, String> mapRawToPep = createPepxmlFilePathsDirty(lcmsFilePaths, params.getOutputFileExtension());

            StringBuilder sb = new StringBuilder();
            // 32k symbols splitting for regular command. 
            // But for slicing it's all up to the python script.
            //final int commandLenLimit = isSlicing ? Integer.MAX_VALUE : 1 << 15;
            final int commandLenLimit = 1 << 15;

            
            final URI currentJarUri = PathUtils.getCurrentJarPath();
            final String currentJarPath = Paths.get(currentJarUri).toAbsolutePath().toString();
            final Path wdPath = Paths.get(workingDir);
            
            
            // delete temp slicing index dir before run
            final String tempDirName = "split_peptide_index_tempdir";
            
            int fileIndex = 0;
            
            while (fileIndex < lcmsFilePaths.size()) {
                int fileIndexLo = fileIndex;
                ArrayList<String> cmd = new ArrayList<>();
                if (isSlicing) {
                    cmd.add(pythonCommand);
                    cmd.add(slicingScriptPath.toAbsolutePath().normalize().toString());
                    cmd.add(Integer.toString(numSlices));
                    cmd.add("\"");
                }
                cmd.add("java");
                cmd.add("-jar");
                if (ramGb > 0) {
                    cmd.add(new StringBuilder().append("-Xmx").append(ramGb).append("G").toString());
                }
                if (isSlicing) {
                    cmd.add("\"");
                }
                cmd.add(bin);
                cmd.add(savedParamsPath.toString());

                for (String s : cmd) {
                    sb.append(s).append(" ");
                }
                if (sb.length() > commandLenLimit) {
                    JOptionPane.showMessageDialog(this, "MSFragger command line length too large even for a single file.",
                            "Error", JOptionPane.ERROR_MESSAGE);
                    return null;
                }

                while (fileIndex < lcmsFilePaths.size()) {
                    String nextFilePath = lcmsFilePaths.get(fileIndex);
                    if (sb.length() + nextFilePath.length() + 1 > commandLenLimit) {
                        break;
                    }
                    sb.append(nextFilePath).append(" ");
                    cmd.add(nextFilePath);
                    fileIndex++;
                }

                ProcessBuilder pbFragger = new ProcessBuilder(cmd);
                builders.add(pbFragger);
                sb.setLength(0);

                // move the files if the output directory is not the same as where
                // the lcms files were
                
                for (int i = fileIndexLo; i < fileIndex; i++) {
                    String pepFile = mapRawToPep.get(lcmsFilePaths.get(i));
                    Path pepPath = Paths.get(pepFile);

                    if (!wdPath.equals(pepPath.getParent())) {
                        ArrayList<String> cmdMove = new ArrayList<>();
                        cmdMove.add("java");
                        cmdMove.add("-cp");
                        cmdMove.add(currentJarPath);
                        //cmdMove.add("umich.msfragger.util.FileMove");
                        cmdMove.add(FileMove.class.getCanonicalName());
                        String origin = pepPath.toAbsolutePath().toString();
                        String destination = Paths.get(wdPath.toString(), pepPath.getFileName().toString()).toString();
                        cmdMove.add(origin);
                        cmdMove.add(destination);
                        ProcessBuilder pbFileMove = new ProcessBuilder(cmdMove);
                        builders.add(pbFileMove);
                    }
                }
            }
        }
        

        return builders;
    }

    private Map<String, String> createPepxmlFilePathsDirty(List<String> lcmsFilePaths, String ext) {
        HashMap<String, String> pepxmls = new HashMap<>();
        for (String s : lcmsFilePaths) {
            String baseName = s.substring(0, s.lastIndexOf(".") + 1);
            pepxmls.put(s, baseName + ext);
        }
        return pepxmls;
    }

    private Map<String, String> createPepxmlFilePathsAfterMove(Map<String, String> dirtyPepXmls, String workingDir) {
        HashMap<String, String> pepxmls = new HashMap<>();
        Path wd = Paths.get(workingDir);
        for (Map.Entry<String, String> entry : dirtyPepXmls.entrySet()) {
            String raw = entry.getKey();
            String pepxmlDirty = entry.getValue();
            Path pepxmlClean = wd.resolve(Paths.get(pepxmlDirty).getFileName()).toAbsolutePath();
            pepxmls.put(raw, pepxmlClean.toString());
        }
        return pepxmls;
    }

    private Map<String, String> createInteractFilePaths(Map<String, String> cleanPepXmls, String workingDir, String pepxmlExt) {
        HashMap<String, String> interacts = new HashMap<>();
        Path wd = Paths.get(workingDir);
        for (Map.Entry<String, String> entry : cleanPepXmls.entrySet()) {
            String raw = entry.getKey();
            String pepxmlClean = entry.getValue();
            String pepxmlCleanFilename = Paths.get(pepxmlClean).getFileName().toString();

            // hardcode typical params
            String[] typicalExts = {pepxmlExt, "pep.xml", "pepxml"};
            String lowerCase = pepxmlCleanFilename.toLowerCase();
            String nameWithoutExt = null;
            for (String ext : typicalExts) {
                if (pepxmlCleanFilename.toLowerCase().endsWith(ext)) {
                    int lastIndex = lowerCase.lastIndexOf(ext);
                    nameWithoutExt = pepxmlCleanFilename.substring(0, lastIndex);
                    break;
                }
            }
            if (nameWithoutExt == null) {
                throw new IllegalStateException(String.format("Could not identify the extension for file: %s", pepxmlClean));
            }

            Path interactXml = wd.resolve("interact-" + nameWithoutExt + "pep.xml").toAbsolutePath();
            interacts.put(raw, interactXml.toString());
        }
        return interacts;
    }

//    private List<ProcessBuilder> processBuildersComet(String programsDir, String workingDir, List<String> lcmsFilePaths, String dateStr) {
//        List<ProcessBuilder> processBuilders = new LinkedList<>();
//        if (chkRunCometSearch.isSelected()) {
//            try {
//                CometParams collectedCometParams = collectCometParams();
//                
//                String bin = txtBinComet.getText().trim();
//                if (bin.isEmpty()) {
//                    JOptionPane.showMessageDialog(this, "Binary for running Comet can not be an empty string.\n",
//                        "Error", JOptionPane.ERROR_MESSAGE);
//                    return null;
//                }
//                bin = testBinaryPath(bin, programsDir);
//                if (bin == null) {
//                    JOptionPane.showMessageDialog(this, "Binary for running Comet not found or could not be run.\n"
//                            + "Neither on PATH, nor in the working directory",
//                        "Error", JOptionPane.ERROR_MESSAGE);
//                    return null;
//                }
//                
//                String fastaPath = txtCometSeqDb.getText().trim();
//                if (fastaPath.isEmpty()) {
//                    JOptionPane.showMessageDialog(this, "Fasta file (Comet) path can't be empty",
//                        "Warning", JOptionPane.WARNING_MESSAGE);
//                    return null;
//                }
//                String fastaPathOrig = new String(fastaPath);
//                fastaPath = testFilePath(fastaPath, workingDir);
//                if (fastaPath == null) {
//                    JOptionPane.showMessageDialog(this, String.format("Could not find fasta file (Comet) at:\n%s", fastaPathOrig),
//                            "Errors", JOptionPane.ERROR_MESSAGE);
//                    return null;
//                }
//                
//                    
//
//                // writing Comet params file
//                String cometParamsFileName = CometParams.FILE_BASE_NAME + "_" + dateStr + "." + CometParams.FILE_BASE_EXT;
//                Path cometParamsFilePath = Paths.get(workingDir, cometParamsFileName);
//                FileOutputStream fos = new FileOutputStream(cometParamsFilePath.toFile());
//                PropertiesUtils.writePropertiesContent(collectedCometParams, fos);
//                
//                // run comet for each file
//                Object value = spinnerRam.getModel().getValue();
//                int ram = (Integer)spinnerRam.getModel().getValue();
//                if (ram < 1)
//                    ram = 1;
//                
//                List<String> createdMzXmlFiles = new ArrayList<>();
//                boolean isPhilosopher = isPhilosopherBin(bin);
//                for (String filePath : lcmsFilePaths) {
//                    // Comet
//                    for (int i = 1; i <= 3; i++) {
//                        List<String> commands = new ArrayList<>();
//                        commands.add(bin);
//                        if (isPhilosopher)
//                            commands.add(Philosopher.CMD_COMET);
//                        
//                        String cmdOpts = txtCometCmdLineOpts.getText().trim();
//                        if (!cmdOpts.isEmpty()) {
//                            String[] opts = cmdOpts.split("\\s+");
//                            for (String opt : opts) {
//                                if (!opt.isEmpty()) {
//                                    if (!isPhilosopher && opt.equals(Philosopher.CMD_COMET)) // for non-philosopher skip this option
//                                        continue;
//                                    commands.add(opt);
//                                }
//                            }
//                        }
//                        
//                        commands.add("--param");
//                        commands.add(cometParamsFilePath.toString());
//
//                        Path curMzXMl = Paths.get(filePath);
//                        Path mzXmlFileName = curMzXMl.getFileName();
//                    
//                        String s = mzXmlFileName.toString();
//                        int indexOf = s.toLowerCase().indexOf(".mzxml");
//                        String baseName = mzXmlFileName.toString().substring(0, indexOf);
//                        Path createdMzXml = Paths.get(workingDir, baseName+"_Q"+i+".mzXML");
//                        commands.add(createdMzXml.toString());
//                        ProcessBuilder pb = new ProcessBuilder(commands);
//                        Map<String, String> env = pb.environment();
//                        // set environment 
//                        String ENV_WEBSERVER_ROOT = "WEBSERVER_ROOT";
//                        String webroot = env.get(ENV_WEBSERVER_ROOT);
//                        if (webroot == null) {
//                            env.put(ENV_WEBSERVER_ROOT, "fake-WEBSERVER_ROOT-value");
//                        }
//                        processBuilders.add(pb);
//                        createdMzXmlFiles.add(createdMzXml.toString());
//                    }
//                }
//                
//            } catch (ParsingException ex) {
//                JOptionPane.showMessageDialog(this, "Error collecting user variables for Comet Search.\n",
//                        "Error", JOptionPane.ERROR_MESSAGE);
//                return null;
//            } catch (FileNotFoundException | FileWritingException ex) {
//                JOptionPane.showMessageDialog(this, "Error collecting user variables for Comet Search.\n",
//                        "Error", JOptionPane.ERROR_MESSAGE);
//                return null;
//            }
//        }
//        return processBuilders;
//    }
    
    
    /**
     * Creates the ProcessBuilders for running PeptideProphet.
     *
     * @param workingDir
     * @param lcmsFilePaths
     * @return null in case of errors, or a list of process builders.
     */
    private List<ProcessBuilder> processBuildersPeptideProphet(String programsDir, String workingDir, List<String> lcmsFilePaths) {
        List<ProcessBuilder> builders = new LinkedList<>();
        if (chkRunPeptideProphet.isSelected()) {
            String bin = textBinPhilosopher.getText().trim();
            if (StringUtils.isNullOrWhitespace(bin)) {
                JOptionPane.showMessageDialog(this, "Philosopher (PeptideProphet) binary can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            bin = PathUtils.testBinaryPath(bin, programsDir);
            if (bin == null) {
                JOptionPane.showMessageDialog(this, "Philosopher (PeptideProphet) binary not found.\n"
                        + "Neither on PATH, nor in the working directory",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            String fastaPath = textSequenceDbPath.getText().trim();
            
            if (StringUtils.isNullOrWhitespace(fastaPath)) {
                JOptionPane.showMessageDialog(this, "Fasta file (PeptideProphet) path can't be empty",
                            "Warning", JOptionPane.WARNING_MESSAGE);
                    return null;
            }
            String fastaPathOrig = fastaPath;
            fastaPath = PathUtils.testFilePath(fastaPath, workingDir);
            if (fastaPath == null) {
                JOptionPane.showMessageDialog(this, String.format("Could not find fasta file (PeptideProphet) at:\n%s", fastaPathOrig),
                        "Errors", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
            peptideProphetParams.setCmdLineParams(textPepProphCmd.getText().trim());

            String philosopherPeptideprophetCmd = "peptideprophet";
            boolean isPhilosopher = isPhilosopherBin(bin);

            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir);
            for (String rawFilePath : lcmsFilePaths) {
                // Comet
                List<String> commands = new ArrayList<>();
                commands.add(bin);
                if (isPhilosopher) // for philosopher we always add the correct command
                {
                    commands.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);
                }

                if (!peptideProphetParams.getCmdLineParams().isEmpty()) {
                    String cmdOpts = peptideProphetParams.getCmdLineParams();
                    List<String> opts = StringUtils.splitCommandLine(cmdOpts);
                    for (String opt : opts) {
                        if (!opt.isEmpty()) {
                            if (opt.equals(PhilosopherProps.CMD_PEPTIDE_PROPHET)) {
                                continue;
                            }
                            commands.add(opt);
                        }
                    }
                }
                commands.add("--database");
                commands.add(fastaPath);

                String pepxmlInWd = pepxmlClean.get(rawFilePath);
                if (pepxmlInWd == null) {
                    JOptionPane.showMessageDialog(this, "PeptideProphet process could not figure where a pepxml was.\n"
                            + "RAW: " + rawFilePath + "\n",
                            "Error", JOptionPane.ERROR_MESSAGE);
                    return null;
                }

                commands.add(pepxmlInWd);
                ProcessBuilder pb = new ProcessBuilder(commands);
                Map<String, String> env = pb.environment();
                // set environment 
                String ENV_WEBSERVER_ROOT = "WEBSERVER_ROOT";
                String webroot = env.get(ENV_WEBSERVER_ROOT);
                if (webroot == null) {
                    env.put(ENV_WEBSERVER_ROOT, "fake-WEBSERVER_ROOT-value");
                }
                builders.add(pb);
            }

        }
        return builders;
    }

    /**
     * Creates the processBuilders for running ProteinProphet.
     *
     * @return null in case of error, empty list if nothing needs to be added.
     */
    private List<ProcessBuilder> processBuildersProteinProphet(String programsDir, String workingDir, List<String> lcmsFilePaths) {
        if (chkRunProteinProphet.isSelected()) {
            String bin = textBinPhilosopher.getText().trim();
            if (StringUtils.isNullOrWhitespace(bin)) {
                JOptionPane.showMessageDialog(this, "ProteinProphet binary can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            bin = PathUtils.testBinaryPath(bin, programsDir);
            if (bin == null) {
                JOptionPane.showMessageDialog(this, "ProteinProphet binary not found or could not be launched.\n"
                        + "Neither on PATH, nor in the working directory",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            String outputFileName = getCombinedProtFilePath(workingDir).toString();
            if (outputFileName.isEmpty()) {
                JOptionPane.showMessageDialog(this, "ProteinProphet output file name can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
//            else if (!outputFileName.toLowerCase().endsWith(".prot.xml")) {
//                JOptionPane.showMessageDialog(this, "ProteinProphet output file name must end with '.prot.xml'.\n",
//                    "Error", JOptionPane.ERROR_MESSAGE);
//                return null;
//            } else {
//                int index = outputFileName.trim().toLowerCase().indexOf(".prot.xml");
//                if (index <= 0) {
//                    JOptionPane.showMessageDialog(this, "ProteinProphet output file name must have content before '.prot.xml'.\n",
//                        "Error", JOptionPane.ERROR_MESSAGE);
//                    return null;
//                }
//            }

            ProteinProphetParams proteinProphetParams = new ProteinProphetParams();
            proteinProphetParams.setCmdLineParams(txtProteinProphetCmdLineOpts.getText());
            List<ProcessBuilder> builders = new ArrayList<>();

            List<String> createdInteractFiles = new ArrayList<>();
            List<String> commands = new ArrayList<>();
            commands.add(bin);
            boolean isPhilosopher = isPhilosopherBin(bin);

            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir);
            Map<String, String> interacts = createInteractFilePaths(pepxmlClean, workingDir, fraggerPanel.getOutputFileExt());

            if (isPhilosopher) {
                commands.add(PhilosopherProps.CMD_PROTEIN_PROPHET);

                // --output flag should be available in the latest philosopher
                String combined = txtCombinedProtFile.getText().trim();
                String combinedOpt = null;
                if (StringUtils.isNullOrWhitespace(combined)) {
                    combinedOpt = "interact";
                } else {
                    final String pepxmlExt = ".prot.xml";
                    if (combined.toLowerCase().endsWith(pepxmlExt)) {
                        combinedOpt = combined.substring(0, combined.toLowerCase().indexOf(pepxmlExt));
                    } else {
                        combinedOpt = combined;
                    }
                }
                if (combinedOpt != null) {
                    commands.add("--output");
                    commands.add(combinedOpt);
                }

                // for Philosopher command line flags go before files
                String cmdLineOpts = proteinProphetParams.getCmdLineParams().trim();
                if (!StringUtils.isNullOrWhitespace(cmdLineOpts)) {
                    List<String> opts = StringUtils.splitCommandLine(cmdLineOpts);
                    commands.addAll(opts);
                }

                if (chkProteinProphetInteractStar.isSelected()) {
                    String sep = FileSystems.getDefault().getSeparator();
                    String interactsGlob = workingDir + sep + "interact-*.pep.xml";
                    commands.add(interactsGlob);
                    //commands.add(getCombinedProtFilePath(workingDir).getFileName().toString());
                    ProcessBuilder pb = new ProcessBuilder(commands);
                    builders.add(pb);
                } else {
                    for (String filePath : lcmsFilePaths) {
                        String interact = interacts.get(filePath);
                        if (!StringUtils.isNullOrWhitespace(interact)) {
                            createdInteractFiles.add(interact);
                        }
                    }
                    for (String f : createdInteractFiles) {
                        Path interactFullPath = Paths.get(f);
                        String interactFileName = interactFullPath.getFileName().toString();
                        commands.add(interactFileName);
                    }
                    ProcessBuilder pb = new ProcessBuilder(commands);
                    builders.add(pb);
                }
            } else {
                for (String filePath : lcmsFilePaths) {
                    String interact = interacts.get(filePath);
                    if (!StringUtils.isNullOrWhitespace(interact)) {
                        createdInteractFiles.add(interact);
                    }
                }

                // output file
                Path combinedProtFilePath = getCombinedProtFilePath(workingDir);
                commands.add(combinedProtFilePath.toString());

                // for native ProteinProphet command line flags go in the end
                String cmdLineOpts = proteinProphetParams.getCmdLineParams().trim();
                if (!cmdLineOpts.isEmpty()) {
                    List<String> opts = StringUtils.splitCommandLine(cmdLineOpts);
                    commands.addAll(opts);
                }
                ProcessBuilder pb = new ProcessBuilder(commands);
                builders.add(pb);
            }

            for (ProcessBuilder pb : builders) {
                pb.directory(Paths.get(workingDir).toFile());
                Map<String, String> env = pb.environment();

                // add this variable so that TPP didn't try to use webserver stuff
                String ENV_XML_ONLY = "XML_ONLY";
                env.put(ENV_XML_ONLY, "1");

                // collect variables from system
                StringBuilder pathEnv = new StringBuilder();
                Set<String> mergedKeys = new HashSet<>();
                Set<String> envKeys = env.keySet();
                for (String key : envKeys) {
                    if (key.toLowerCase().equals("path")) {
                        String pathVal = env.get(key);
                        pathVal = pathVal.trim();
                        pathEnv.append(pathVal);
                        if (!pathVal.endsWith(";")) {
                            pathEnv.append(";");
                        }
                        mergedKeys.add(key);
                    }
                }
                for (String key : mergedKeys) {
                    env.remove(key);
                }

                String ENV_PATH = "PATH";
                Path binPath = Paths.get(bin);
                String binFolder = null;
                if (binPath.isAbsolute()) {
                    // the path to the executable was specified as absolute, other needed files must be there as well
                    binFolder = binPath.toAbsolutePath().getParent().toString();
                } else if (Files.exists(binPath)) {
                    binFolder = binPath.toAbsolutePath().getParent().toString();
                } else {
                    binPath = Paths.get(workingDir, bin);
                    if (Files.exists(binPath)) {
                        binFolder = binPath.toAbsolutePath().getParent().toString();
                    }
                }
                if (binFolder != null) {
                    pathEnv.append(";").append(binFolder);
                }
                String pathEnvValue = pathEnv.toString();
                env.put(ENV_PATH, pathEnvValue);
            }

            // for native TPP we will add some magic variables
//            if (!isPhilosopher) {
//                String ENV_XML_ONLY = "XML_ONLY";
//                env.put(ENV_XML_ONLY, "1");
//
//                String ENV_PATH = "PATH";
//                String envPath = env.get(ENV_PATH);
//                if (envPath == null) {
//                    envPath = "";
//                } else {
//                    envPath = envPath.trim();
//                }
//                StringBuilder sbEnvPath = new StringBuilder(envPath);
//                if (sbEnvPath.length() != 0)
//                    sbEnvPath.append(";");
//                // the ProteinProphet can be either in working directory, or in some directory
//                // that we can get from the executable absolute path
//                String binFolder = null;
//                try {
//                    Path binPath = Paths.get(bin);
//                    if (binPath.isAbsolute()) {
//                        // the path to the executable was specified as absolute, other needed files must be there as well
//                        binFolder = binPath.toAbsolutePath().getParent().toString();
//                    } else if (Files.exists(binPath)) {
//                        binFolder = binPath.toAbsolutePath().getParent().toString();
//                    } else {
//                        binPath = Paths.get(workingDir, bin);
//                        if (Files.exists(binPath)) {
//                            binFolder = binPath.toAbsolutePath().getParent().toString();
//                        }
//                    }                  
//                } catch (Exception ignore) {
//                    // let's hope that everything ProteinProphet needs can be found on system PATH
//                }
//                if (binFolder != null) {
//                    sbEnvPath.append(binFolder);
//                    env.put(ENV_PATH, sbEnvPath.toString());
//                }
//            }
            return builders;
        }
        return Collections.emptyList();
    }

    /**
     * Creates the processBuilders for running ProteinProphet.
     *
     * @return null in case of error, empty list if nothing needs to be added.
     */
    private List<ProcessBuilder> processBuildersReport(String programsDir, String workingDir, List<String> lcmsFilePaths) {
        if (checkCreateReport.isSelected()) {
            String bin = textBinPhilosopher.getText().trim();
            if (StringUtils.isNullOrWhitespace(bin)) {
                JOptionPane.showMessageDialog(this, "Philosopher binary can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            bin = PathUtils.testBinaryPath(bin, programsDir);
            if (bin == null) {
                JOptionPane.showMessageDialog(this, "Philosopher binary not found or could not be launched.\n"
                        + "Neither on PATH, nor in the working directory",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            String outputFileName = getCombinedProtFilePath(workingDir).toString();
            if (outputFileName.isEmpty()) {
                JOptionPane.showMessageDialog(this, "ProteinProphet output file name can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            } else if (!outputFileName.toLowerCase().endsWith(".prot.xml")) {
                JOptionPane.showMessageDialog(this, "ProteinProphet output file name must end with '.prot.xml'.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            } else {
                int index = outputFileName.trim().toLowerCase().indexOf(".prot.xml");
                if (index <= 0) {
                    JOptionPane.showMessageDialog(this, "ProteinProphet output file name must have content before '.prot.xml'.\n",
                            "Error", JOptionPane.ERROR_MESSAGE);
                    return null;
                }
            }

            List<ProcessBuilder> builders = new ArrayList<>();
            boolean isPhilosopher = isPhilosopherBin(bin);

            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir);
            Map<String, String> interacts = createInteractFilePaths(pepxmlClean, workingDir, fraggerPanel.getOutputFileExt());
            Path combinedProtFilePath = getCombinedProtFilePath(workingDir);

            if (checkReportDbAnnotate.isSelected()) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add(PhilosopherProps.CMD_DATABASE);
                cmd.add("--annotate");
                String fastaPath = getFastaPath();
                if (fastaPath == null) {
                    JOptionPane.showMessageDialog(this, "Fasta file path can't be empty (Report)",
                            "Warning", JOptionPane.WARNING_MESSAGE);
                    return null;
                }
                cmd.add(fastaPath);
                String annotateParams = textReportAnnotate.getText().trim();
                if (!StringUtils.isNullOrWhitespace(annotateParams)) {
                    String[] params = annotateParams.split("[\\s]+");
                    for (String p : params) {
                        cmd.add(p);
                    }
                }
                builders.add(new ProcessBuilder(cmd));
            }

            // philosopher filter
            if (checkReportFilter.isSelected()) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add(PhilosopherProps.CMD_FILTER);
                String filterParams = textReportFilter.getText().trim();
                if (!StringUtils.isNullOrWhitespace(filterParams)) {
                    String[] params = filterParams.split("[\\s]+");
                    for (String p : params) {
                        cmd.add(p);
                    }
                }
                cmd.add("--pepxml");
                cmd.add(workingDir);
                if (checkReportProteinLevelFdr.isSelected()) {
                    cmd.add("--protxml");
                    cmd.add(combinedProtFilePath.toString());
                }
                builders.add(new ProcessBuilder(cmd));
            }

            // philosopher freequant (labelfree)
            if (checkLabelfree.isSelected()) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add(PhilosopherProps.CMD_LABELFREE);
                
                List<String> allowed = new ArrayList<>();
                allowed.add("ptw");
                allowed.add("tol");
                String labelfreeParams = textReportLabelfree.getText().trim();
                for (String paramName : allowed) {
                    Pattern reFullParam = Pattern.compile(String.format("--%s\\s+(\\d+(?:\\.\\d+)?)", paramName));
                    Matcher m = reFullParam.matcher(labelfreeParams);
                    if (m.find()) {
                        cmd.add("--" + paramName);
                        cmd.add(m.group(1));
                    }
                }
                // we have checked that all lcms files are in the same folder, so
                Path lcmsDir = Paths.get(lcmsFilePaths.get(0)).getParent();
                cmd.add("--dir");
                cmd.add(lcmsDir.toAbsolutePath().toString());
                
                builders.add(new ProcessBuilder(cmd));
            }
            
            // philosopher report
            if (true) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add(PhilosopherProps.CMD_REPORT);
                builders.add(new ProcessBuilder(cmd));
            }

            // set working dir for all processes
            final File wd = new File(workingDir);
            for (ProcessBuilder pb : builders) {
                pb.directory(wd);
            }

            return builders;
        }
        return Collections.emptyList();
    }

    private Path getWorkingDir() {
        String wdStr = txtWorkingDir.getText().trim();
        Path path = Paths.get(wdStr).toAbsolutePath();
        return path;
    }

    private String getDefaultTextMsfragger() {
        String value = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSFRAGGER);
        if (value != null) {
            return value;
        }
        ResourceBundle bundle = ResourceBundle.getBundle(Version.PATH_BUNDLE);
        return bundle.getString("default.msfragger.jar");
    }

    private String getDefaultTextProgramsDir() {
        String value = ThisAppProps.load(ThisAppProps.PROP_BINARIES_IN);
        return value == null ? "" : value;
    }

    private String getDefaultTextPeptideProphetCmdOpen() {
        return "";
    }

    private String getDefaultTextPeptideProphetCmdClosed() {
        return "";
    }

    private String getDefaultTextProteinProphetCmdOpen() {
        return "";
    }

    private String getDefaultTextPeptideProteinCmdClosed() {
        return "";
    }

    private String getDefaultTextMsconvert() {
        String value = ThisAppProps.load(ThisAppProps.PROP_TEXTFIELD_PATH_MSCONVERT);
        if (value != null) {
            return value;
        }

        String binaryName;
        ResourceBundle bundle = ResourceBundle.getBundle("dia/umpire/gui/Bundle"); // NOI18N
        if (OsUtils.isWindows()) {
            binaryName = bundle.getString("default.msconvert.win");
        } else {
            binaryName = bundle.getString("default.msconvert.nix");
        }
        String testedBinaryPath = PathUtils.testBinaryPath(binaryName);
        if (testedBinaryPath != null && !testedBinaryPath.isEmpty()) {
            return testedBinaryPath;
        }

        if (OsUtils.isWindows()) {
            try {
                // on Windows try to find MSConvert in a few predefined locations
                List<String> paths = Arrays.asList(
                        "program files (x64)",
                        "program files"
                );
                String folder = "proteowizard";
                String folder2 = "pwiz";
                final String toSearch = "msconvert.exe";

                final Holder<Path> foundPathHolder = new Holder<>();

                FileVisitor<Path> fileVisitor = new FileVisitor<Path>() {
                    @Override
                    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                        if (file.getFileName().toString().toLowerCase().equals(toSearch)) {
                            foundPathHolder.obj = file;
                            return FileVisitResult.TERMINATE;
                        }
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                        return FileVisitResult.CONTINUE;
                    }
                };

                Iterable<Path> rootDirs = FileSystems.getDefault().getRootDirectories();
                for (Path rootDir : rootDirs) {
                    try {
                        DirectoryStream<Path> dirStream = Files.newDirectoryStream(rootDir);
                        for (Path file : dirStream) {
                            for (String path : paths) {
                                if (file.getFileName().toString().toLowerCase().startsWith(path)) {
                                    // search for proteowizard
                                    DirectoryStream<Path> dirStream2 = Files.newDirectoryStream(file);
                                    for (Path file2 : dirStream2) {
                                        String toLowerCase = file2.getFileName().toString().toLowerCase();
                                        if (toLowerCase.startsWith(folder) || toLowerCase.startsWith(folder2)) {
                                            // this might be a proteo wizard folder, recursively search it
                                            Files.walkFileTree(file2, fileVisitor);
                                            if (foundPathHolder.obj != null) {
                                                return foundPathHolder.obj.toAbsolutePath().toString();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } catch (IOException ex) {
                        // doesn't matter
                    }
                }
            } catch (Exception e) {
                // we don't care if anything within this block breaks
            }
        }
        return "";
    }

    private class Holder<T> {

        T obj;
    }

    private static List<Image> loadIcon() {
        // Icon attribution string:
        // <div>Icons made by <a href="http://www.freepik.com" title="Freepik">Freepik</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
        List<Image> images = new ArrayList<>();
        int[] sizes = {16, 24, 32, 64, 128, 256};
        final String path = "icons/";
        final String baseName = "fragpipe-";
        final String ext = ".png";
        for (int size : sizes) {
            String location = path + baseName + Integer.toString(size) + ext;
            Image icon = Toolkit.getDefaultToolkit().getImage(MsfraggerGuiFrame.class.getResource(location));
            images.add(icon);
        }
        return images;
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

        try {
            if (OsUtils.isWindows()) {
                // native look on windows
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } else {
                // nimbus otherwise
                for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                    if ("Nimbus".equals(info.getName())) {
                        javax.swing.UIManager.setLookAndFeel(info.getClassName());
                        break;
                    }
                }
            }
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e1) {
            java.util.logging.Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, e1);
            try {
                for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                    if ("Nimbus".equals(info.getName())) {
                        javax.swing.UIManager.setLookAndFeel(info.getClassName());
                        break;
                    }
                }
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException e2) {
                java.util.logging.Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, e2);
            }
        }
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                final MsfraggerGuiFrame frame = new MsfraggerGuiFrame();
                
                Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
                    @Override
                    public void uncaughtException(Thread t, Throwable e) {
                        StringWriter sw = new StringWriter();
                        e.printStackTrace(new PrintWriter(sw, true));
                        JOptionPane.showMessageDialog(frame, "Some error details:\n\n" + sw.toString(),
                                "Error", JOptionPane.ERROR_MESSAGE);
                    }
                });
                
                frame.setVisible(true);
                Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
                frame.setLocation(dim.width / 2 - frame.getSize().width / 2, dim.height / 2 - frame.getSize().height / 2);
            }
        });
    }

    private String getDefaultBinMsfragger() {
        String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSFRAGGER);
        return path == null ? "MSFragger.jar" : path;
    }

    private String getDefaultBinPhilosopher() {
        String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER);
        if (path != null) {
            return path;
        }
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle(Version.PATH_BUNDLE);
        String winName = bundle.getString("default.philosopher.win"); // NOI18N
        String nixName = bundle.getString("default.philosopher.nix"); // NOI18N
        if (OsUtils.isWindows()) {
            return winName;
        }
        return nixName;
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnAbout;
    private javax.swing.JButton btnAboutInConfig;
    private javax.swing.JButton btnBrowse;
    private javax.swing.JButton btnClearCache;
    private javax.swing.JButton btnClearConsole;
    private javax.swing.JButton btnExportLog;
    private javax.swing.JButton btnFindTools;
    private javax.swing.JButton btnLoadDefaultsClosed;
    private javax.swing.JButton btnLoadDefaultsOpen;
    private javax.swing.JButton btnMsfraggerBinBrowse;
    private javax.swing.JButton btnMsfraggerBinDownload;
    private javax.swing.JButton btnMsfraggerUpdate;
    private javax.swing.JButton btnOpenInExplorer;
    private javax.swing.JButton btnPepProphDefaultsClosed;
    private javax.swing.JButton btnPepProphDefaultsOpen;
    private javax.swing.JButton btnPhilosopherBinBrowse;
    private javax.swing.JButton btnPhilosopherBinDownload;
    private javax.swing.JButton btnProtProphDefaultsClosed;
    private javax.swing.JButton btnProtProphDefaultsOpen;
    private javax.swing.JButton btnRawAddFiles;
    private javax.swing.JButton btnRawAddFolder;
    private javax.swing.JButton btnRawClear;
    private javax.swing.JButton btnRawRemove;
    private javax.swing.JButton btnReportDefaultsClosed;
    private javax.swing.JButton btnReportDefaultsOpen;
    private javax.swing.JButton btnReportErrors;
    private javax.swing.JButton btnRun;
    private javax.swing.JButton btnSelectWrkingDir;
    private javax.swing.JButton btnStop;
    private javax.swing.JButton btnTryDetectDecoyTag;
    private javax.swing.JCheckBox checkCreateReport;
    private javax.swing.JCheckBox checkDryRun;
    private javax.swing.JCheckBox checkLabelfree;
    private javax.swing.JCheckBox checkReportDbAnnotate;
    private javax.swing.JCheckBox checkReportFilter;
    private javax.swing.JCheckBox checkReportProteinLevelFdr;
    private javax.swing.JCheckBox chkProcessEachFiileSeparately;
    private javax.swing.JCheckBox chkProteinProphetInteractStar;
    private javax.swing.JCheckBox chkRunPeptideProphet;
    private javax.swing.JCheckBox chkRunProteinProphet;
    private javax.swing.JScrollPane consoleScrollPane;
    private javax.swing.JEditorPane editorMsfraggerCitation;
    private javax.swing.JEditorPane editorPhilosopherLink;
    private javax.swing.JEditorPane editorSequenceDb;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel34;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel40;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JScrollPane jScrollPane5;
    private javax.swing.JLabel lblFastaCount;
    private javax.swing.JLabel lblFindAutomatically;
    private javax.swing.JLabel lblFraggerJavaVer;
    private javax.swing.JLabel lblMsfraggerCitation;
    private javax.swing.JLabel lblOutputDir;
    private javax.swing.JLabel lblPhilosopherInfo;
    private javax.swing.JLabel lblPythonInfo;
    private javax.swing.JLabel lblPythonMore;
    private javax.swing.JPanel panelConfig;
    private javax.swing.JPanel panelDbInfo;
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
    private javax.swing.JScrollPane scrollPaneMsFragger;
    private javax.swing.JScrollPane scrollPaneRawFiles;
    private javax.swing.JTabbedPane tabPane;
    private javax.swing.JTextField textBinMsfragger;
    private javax.swing.JTextField textBinPhilosopher;
    private javax.swing.JTextField textDecoyTagSeqDb;
    private javax.swing.JTextArea textPepProphCmd;
    private javax.swing.JTextField textReportAnnotate;
    private javax.swing.JTextField textReportFilter;
    private javax.swing.JTextField textReportLabelfree;
    private javax.swing.JTextField textSequenceDbPath;
    private javax.swing.JTextField txtCombinedProtFile;
    private javax.swing.JTextArea txtProteinProphetCmdLineOpts;
    private javax.swing.JTextField txtWorkingDir;
    // End of variables declaration//GEN-END:variables

}
