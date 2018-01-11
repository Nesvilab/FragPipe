/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.gui;

import java.awt.Color;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.file.DirectoryStream;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import umich.msfragger.Version;
import umich.msfragger.gui.api.DataConverter;
import umich.msfragger.gui.api.SimpleETable;
import umich.msfragger.gui.api.SimpleUniqueTableModel;
import umich.msfragger.gui.api.TableModelColumn;
import umich.msfragger.params.PeptideProphetParams;
import umich.msfragger.params.Philosopher;
import umich.msfragger.params.ProteinProphetParams;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.params.fragger.MsfraggerProperties;
import umich.msfragger.util.FileDrop;
import umich.msfragger.util.FileListing;
import umich.msfragger.util.GhostText;
import umich.msfragger.util.HSLColor;
import umich.msfragger.util.LogUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.VersionComparator;
import umich.swing.console.TextConsole;

/**
 *
 * @author dattam
 */
public class MsfraggerGuiFrame extends javax.swing.JFrame {
    
    protected FraggerPanel fraggerPanel;
    protected TextConsole console;
    protected ExecutorService exec;
    private final List<Process> submittedProcesses = new ArrayList<>(100);
    private static final String TEXT_SAME_SEQ_DB = "<Same as in MSFragger>";
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
    
    public static final SearchTypeProp DEFAULT_TYPE = SearchTypeProp.open;
    
    private String textPepProphetFocusGained = "";
    private String textReportFilterFocusGained= "";

    public MsfraggerGuiFrame() {
        initComponents();
        initMore();
    }

    private void initMore() {
        
        setTitle("MSFragger GUI (v" + Version.getVersion() + ")");
    
        console = new TextConsole();
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
        if (!validateMsfraggerPath(textBinMsfragger.getText())) {
            enableMsfraggerPanels(false);
        }
        
        validateMsfraggerJavaVersion();
        
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
                        if (FraggerPanel.fileNameExtensionFilter.accept(f))
                            paths.add(Paths.get(f.getAbsolutePath()));
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
            }
        });
    }
    
    private void checkPreviouslySavedParams() {
        ThisAppProps cached = ThisAppProps.loadFromTemp();
        if (cached != null) {
            // if there was a cached version of properties
            VersionComparator vc = new VersionComparator();
            String storedVer = cached.getProperty(Version.PROP_VER, "0.0");
            final String minVer = "4.0";
            if (vc.compare(storedVer, minVer) < 0) {
                // and the version was less than 4.0
                String msg = String.format("Looks like you've upgraded from an "
                        + "older version to 4.0+,\n"
                        + "it is recommended to reset the default parameters.\n\n"
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
        SwingUtils.enableComponents(panelMsFragger, enabled);
        fraggerPanel.getCheckboxIsRunFragger().setSelected(enabled);
    }
    
    public SimpleUniqueTableModel<Path> createTableModelRawFiles() {
        if (tableModelRawFiles != null)
            return tableModelRawFiles; 
        List<TableModelColumn<Path, ?>> cols = new ArrayList<>();
       
        TableModelColumn<Path, String> colPath = new TableModelColumn<>("Path", String.class, false, new DataConverter<Path, String>() {
            @Override public String convert(Path data) {
                return data.toString();
            }
        });
        cols.add(colPath);
        
        tableModelRawFiles = new SimpleUniqueTableModel<>(cols, 0);
        return tableModelRawFiles;
    }
    
    private String getDefaultPhilosopherBinName() {
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N
        String winName = bundle.getString("default.philosopher.win"); // NOI18N
        String nixName = bundle.getString("default.philosopher.nix"); // NOI18N
        if (OsUtils.isWindows())
            return winName;
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
        panelPhilosopherConfig = new javax.swing.JPanel();
        btnPhilosopherBinDownload = new javax.swing.JButton();
        btnPhilosopherBinBrowse = new javax.swing.JButton();
        textBinPhilosopher = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jScrollPane3 = new javax.swing.JScrollPane();
        editorPhilosopherLink = new javax.swing.JEditorPane();
        btnFindTools = new javax.swing.JButton();
        lblFindAutomatically = new javax.swing.JLabel();
        btnClearCache = new javax.swing.JButton();
        btnLoadDefaultsOpen = new javax.swing.JButton();
        btnLoadDefaultsClosed = new javax.swing.JButton();
        btnAboutInConfig = new javax.swing.JButton();
        panelSelectFiles = new javax.swing.JPanel();
        panelSelectedFiles = new javax.swing.JPanel();
        btnRawAddFiles = new javax.swing.JButton();
        btnRawClear = new javax.swing.JButton();
        scrollPaneRawFiles = new javax.swing.JScrollPane();
        btnRawAddFolder = new javax.swing.JButton();
        btnRawRemove = new javax.swing.JButton();
        panelMsFragger = new javax.swing.JPanel();
        scrollPaneMsFragger = new javax.swing.JScrollPane();
        panelPeptideProphet = new javax.swing.JPanel();
        chkRunPeptideProphet = new javax.swing.JCheckBox();
        panelPeptideProphetOptions = new javax.swing.JPanel();
        jLabel34 = new javax.swing.JLabel();
        jLabel35 = new javax.swing.JLabel();
        txtPeptideProphetSeqDb = new javax.swing.JTextField();
        ghostTextPepProph = new GhostText(txtPeptideProphetSeqDb, TEXT_SAME_SEQ_DB, defTextColor);
        btnSelectPeptideProphetSeqDbPath = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        txtPeptideProphetCmdLineOptions = new javax.swing.JTextArea();
        btnPepProphDefaultsClosed = new javax.swing.JButton();
        btnPepProphDefaultsOpen = new javax.swing.JButton();
        panelProteinProphet = new javax.swing.JPanel();
        chkRunProteinProphet = new javax.swing.JCheckBox();
        panelProteinProphetOptions = new javax.swing.JPanel();
        btnProteinProphetSeqDb = new javax.swing.JButton();
        txtProteinProphetSeqDb = new javax.swing.JTextField();
        ghostTextProtProp = new GhostText(txtProteinProphetSeqDb, TEXT_SAME_SEQ_DB, defTextColor);
        jLabel39 = new javax.swing.JLabel();
        jScrollPane4 = new javax.swing.JScrollPane();
        txtProteinProphetCmdLineOpts = new javax.swing.JTextArea();
        jLabel40 = new javax.swing.JLabel();
        chkProteinProphetInteractStar = new javax.swing.JCheckBox();
        txtCombinedProtFile = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        btnProtProphDefaultsClosed = new javax.swing.JButton();
        btnProtProphDefaultsOpen = new javax.swing.JButton();
        panelReport = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        checkReportDbAnnotate = new javax.swing.JCheckBox();
        textReportDbAnnotate = new javax.swing.JTextField();
        ghostTextPepProph = new GhostText(textReportDbAnnotate, TEXT_SAME_SEQ_DB, defTextColor);
        checkReportFilter = new javax.swing.JCheckBox();
        textReportFilter = new javax.swing.JTextField();
        checkReportProteinLevelFdr = new javax.swing.JCheckBox();
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
        btnCheckJavaVersion = new javax.swing.JButton();
        btnRun = new javax.swing.JButton();

        jLabel2.setText("jLabel2");

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("MSFragger");
        setIconImages(loadIcon());
        setName("frameMain"); // NOI18N

        tabPane.setName(""); // NOI18N

        jLabel4.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel4.setText("<html>Tabs on top represent processing steps and will be performed sequentially.<br/>\nTabs will become enabled once the tools on this panel are configured."); // NOI18N

        panelMsfraggerConfig.setBorder(javax.swing.BorderFactory.createTitledBorder("MSFragger"));

        btnMsfraggerBinDownload.setText("Download");
        btnMsfraggerBinDownload.setToolTipText("<html>Open the download web-page for MSFragger<br/>\nYou need to agree to the license terms."); // NOI18N
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

        lblFraggerJavaVer.setText(getFraggerLableJavaVer());

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
                        .addComponent(btnMsfraggerBinDownload))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, panelMsfraggerConfigLayout.createSequentialGroup()
                        .addComponent(lblMsfraggerCitation)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        panelMsfraggerConfigLayout.setVerticalGroup(
            panelMsfraggerConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelMsfraggerConfigLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelMsfraggerConfigLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(textBinMsfragger, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(btnMsfraggerBinDownload)
                    .addComponent(btnMsfraggerBinBrowse))
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

        jLabel5.setText(createSysInfoPhilosopherText());

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
                    .addComponent(jLabel5, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
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
                .addComponent(jLabel5)
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
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 143, Short.MAX_VALUE)
                                .addComponent(btnClearCache))))
                    .addGroup(panelConfigLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(panelPhilosopherConfig, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addComponent(jLabel4, javax.swing.GroupLayout.Alignment.TRAILING))
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
                .addGap(17, 17, 17)
                .addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(91, Short.MAX_VALUE))
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
                        .addGap(0, 286, Short.MAX_VALUE)))
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
                    .addComponent(btnRawAddFolder))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(scrollPaneRawFiles, javax.swing.GroupLayout.DEFAULT_SIZE, 476, Short.MAX_VALUE)
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

        tabPane.addTab("Select LC/MS Files", panelSelectFiles);

        javax.swing.GroupLayout panelMsFraggerLayout = new javax.swing.GroupLayout(panelMsFragger);
        panelMsFragger.setLayout(panelMsFraggerLayout);
        panelMsFraggerLayout.setHorizontalGroup(
            panelMsFraggerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(scrollPaneMsFragger, javax.swing.GroupLayout.DEFAULT_SIZE, 642, Short.MAX_VALUE)
        );
        panelMsFraggerLayout.setVerticalGroup(
            panelMsFraggerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(scrollPaneMsFragger, javax.swing.GroupLayout.DEFAULT_SIZE, 572, Short.MAX_VALUE)
        );

        tabPane.addTab("MSFragger", panelMsFragger);

        chkRunPeptideProphet.setSelected(true);
        chkRunPeptideProphet.setText("Run PeptideProphet");
        chkRunPeptideProphet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                chkRunPeptideProphetActionPerformed(evt);
            }
        });

        panelPeptideProphetOptions.setBorder(javax.swing.BorderFactory.createTitledBorder("Options"));

        jLabel34.setText("Cmd Line Options");

        jLabel35.setText("Sequence Database");
        jLabel35.setToolTipText("If left empty, will try to get value from Comet search sequence database");

        txtPeptideProphetSeqDb.setToolTipText("If left empty, will try to get value from MSFragger search sequence database");

        btnSelectPeptideProphetSeqDbPath.setText("Browse");
        btnSelectPeptideProphetSeqDbPath.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnSelectPeptideProphetSeqDbPathActionPerformed(evt);
            }
        });

        txtPeptideProphetCmdLineOptions.setColumns(20);
        txtPeptideProphetCmdLineOptions.setLineWrap(true);
        txtPeptideProphetCmdLineOptions.setRows(5);
        txtPeptideProphetCmdLineOptions.setWrapStyleWord(true);
        txtPeptideProphetCmdLineOptions.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                txtPeptideProphetCmdLineOptionsFocusGained(evt);
            }
            public void focusLost(java.awt.event.FocusEvent evt) {
                txtPeptideProphetCmdLineOptionsFocusLost(evt);
            }
        });
        jScrollPane2.setViewportView(txtPeptideProphetCmdLineOptions);
        loadLastPeptideProphet();

        javax.swing.GroupLayout panelPeptideProphetOptionsLayout = new javax.swing.GroupLayout(panelPeptideProphetOptions);
        panelPeptideProphetOptions.setLayout(panelPeptideProphetOptionsLayout);
        panelPeptideProphetOptionsLayout.setHorizontalGroup(
            panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel34, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel35, javax.swing.GroupLayout.Alignment.TRAILING))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                        .addComponent(txtPeptideProphetSeqDb)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnSelectPeptideProphetSeqDbPath))
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 484, Short.MAX_VALUE))
                .addContainerGap())
        );
        panelPeptideProphetOptionsLayout.setVerticalGroup(
            panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                .addGroup(panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnSelectPeptideProphetSeqDbPath)
                    .addComponent(txtPeptideProphetSeqDb, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel35))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelPeptideProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelPeptideProphetOptionsLayout.createSequentialGroup()
                        .addComponent(jLabel34)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 117, Short.MAX_VALUE))
                .addContainerGap())
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
                .addContainerGap(347, Short.MAX_VALUE))
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

        btnProteinProphetSeqDb.setText("Browse");
        btnProteinProphetSeqDb.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnProteinProphetSeqDbActionPerformed(evt);
            }
        });

        txtProteinProphetSeqDb.setToolTipText("If not specified, the value will be taken from PeptideProphet or MSFragger tabs");

        jLabel39.setText("Sequence Database");
        jLabel39.setToolTipText("Not Used Now. If not specified, the value will be taken from PeptideProphet or Comet tabs");

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
            .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addGap(0, 217, Short.MAX_VALUE)
                        .addComponent(chkProteinProphetInteractStar))
                    .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                        .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                    .addComponent(jLabel39)
                                    .addComponent(jLabel1))
                                .addGap(10, 10, 10))
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelProteinProphetOptionsLayout.createSequentialGroup()
                                .addComponent(jLabel40)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                        .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(txtCombinedProtFile)
                            .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                                .addComponent(txtProteinProphetSeqDb)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(btnProteinProphetSeqDb))
                            .addComponent(jScrollPane4))))
                .addContainerGap())
        );
        panelProteinProphetOptionsLayout.setVerticalGroup(
            panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelProteinProphetOptionsLayout.createSequentialGroup()
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnProteinProphetSeqDb)
                    .addComponent(txtProteinProphetSeqDb, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel39))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(txtCombinedProtFile, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelProteinProphetOptionsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel40))
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
                .addContainerGap(321, Short.MAX_VALUE))
        );

        tabPane.addTab("ProteinProphet", panelProteinProphet);

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Options"));

        checkReportDbAnnotate.setSelected(true);
        checkReportDbAnnotate.setText("Database Annotation");

        textReportDbAnnotate.setToolTipText("Path to database fasta file. Preferrably leave it as is.");

        checkReportFilter.setSelected(true);
        checkReportFilter.setText("Filter");

        textReportFilter.setToolTipText("<html>Additional flags for Philosopher<br/>\n--pepxml path-to-pepxml --protxml path-to-combined-protxml<br/>\nwill be added automatically based on previous tabs.");
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

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addComponent(checkReportFilter, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(checkReportDbAnnotate, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(textReportDbAnnotate, javax.swing.GroupLayout.DEFAULT_SIZE, 461, Short.MAX_VALUE)
                            .addComponent(textReportFilter)))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(checkReportProteinLevelFdr)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(checkReportDbAnnotate)
                    .addComponent(textReportDbAnnotate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(checkReportFilter)
                    .addComponent(textReportFilter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(checkReportProteinLevelFdr)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        loadLastReportFilter();
        loadLastReportProteinLevelFdr();

        checkCreateReport.setSelected(true);
        checkCreateReport.setText("Create report");
        checkCreateReport.setToolTipText("<html>Create tab separated report files with \nsome statistics about search results.");

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
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
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
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(415, Short.MAX_VALUE))
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

        btnCheckJavaVersion.setText("Check Java Version");
        btnCheckJavaVersion.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnCheckJavaVersionActionPerformed(evt);
            }
        });

        btnRun.setText("RUN");
        btnRun.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                btnRunActionPerformed(evt);
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
                    .addGroup(panelRunLayout.createSequentialGroup()
                        .addComponent(btnRun)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnStop)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(checkDryRun)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 142, Short.MAX_VALUE)
                        .addComponent(btnCheckJavaVersion)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnAbout)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnClearConsole))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panelRunLayout.createSequentialGroup()
                        .addComponent(lblOutputDir)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(txtWorkingDir)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnSelectWrkingDir)))
                .addContainerGap())
        );
        panelRunLayout.setVerticalGroup(
            panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panelRunLayout.createSequentialGroup()
                .addGap(40, 40, 40)
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblOutputDir)
                    .addComponent(btnSelectWrkingDir)
                    .addComponent(txtWorkingDir, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panelRunLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnStop)
                    .addComponent(btnClearConsole)
                    .addComponent(btnAbout)
                    .addComponent(checkDryRun)
                    .addComponent(btnCheckJavaVersion)
                    .addComponent(btnRun))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(consoleScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 458, Short.MAX_VALUE)
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
        ep.addHyperlinkListener(new HyperlinkListener()
        {
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

    private void btnProteinProphetSeqDbActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnProteinProphetSeqDbActionPerformed
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("FASTA files", "fa", "fasta");
        fileChooser.setFileFilter(fileNameExtensionFilter);
        fileChooser.setApproveButtonText("Select file");
        fileChooser.setApproveButtonToolTipText("Select");
        fileChooser.setDialogTitle("Choose FASTA file");
        fileChooser.setMultiSelectionEnabled(false);

        if (!StringUtils.isNullOrWhitespace(txtProteinProphetSeqDb.getText())
                && !TEXT_SAME_SEQ_DB.contains(txtProteinProphetSeqDb.getText())) {
            try {
                File toFile = Paths.get(txtProteinProphetSeqDb.getText()).toFile();
                fileChooser.setCurrentDirectory(toFile);
            } catch (Exception e) {
                SwingUtils.setFileChooserPath(fileChooser, ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN));
            }
        } else {
            SwingUtils.setFileChooserPath(fileChooser, ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN));
        }

        int showOpenDialog = fileChooser.showOpenDialog(this);
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File f = fileChooser.getSelectedFile();
                txtProteinProphetSeqDb.setText(f.getAbsolutePath());
                ThisAppProps.save(ThisAppProps.PROP_DB_FILE_IN, f.getAbsolutePath());
                break;
        }
    }//GEN-LAST:event_btnProteinProphetSeqDbActionPerformed

    private void chkRunProteinProphetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunProteinProphetActionPerformed
        boolean selected = chkRunProteinProphet.isSelected();
        Container[] comps = new Container[] {
            panelProteinProphetOptions
        };
        for (Container c : comps) {
            SwingUtils.enableComponents(c, selected);
        }
    }//GEN-LAST:event_chkRunProteinProphetActionPerformed

    private void btnSelectPeptideProphetSeqDbPathActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnSelectPeptideProphetSeqDbPathActionPerformed
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("FASTA files", "fa", "fasta");
        fileChooser.setFileFilter(fileNameExtensionFilter);
        fileChooser.setApproveButtonText("Select file");
        fileChooser.setApproveButtonToolTipText("Select");
        fileChooser.setDialogTitle("Choose FASTA file");
        fileChooser.setMultiSelectionEnabled(false);

        if (!StringUtils.isNullOrWhitespace(txtPeptideProphetSeqDb.getText())
                && !TEXT_SAME_SEQ_DB.contains(txtPeptideProphetSeqDb.getText())) {
            try {
                File toFile = Paths.get(txtPeptideProphetSeqDb.getText()).toFile();
                fileChooser.setCurrentDirectory(toFile);
            } catch (Exception e) {
                SwingUtils.setFileChooserPath(fileChooser, ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN));
            }
        } else {
            SwingUtils.setFileChooserPath(fileChooser, ThisAppProps.load(ThisAppProps.PROP_DB_FILE_IN));
        }

        int showOpenDialog = fileChooser.showOpenDialog(this);
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File f = fileChooser.getSelectedFile();
                txtPeptideProphetSeqDb.setText(f.getAbsolutePath());
                ThisAppProps.save(ThisAppProps.PROP_DB_FILE_IN, f.getAbsolutePath());
                break;
        }
    }//GEN-LAST:event_btnSelectPeptideProphetSeqDbPathActionPerformed

    private void chkRunPeptideProphetActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_chkRunPeptideProphetActionPerformed
        boolean selected = chkRunPeptideProphet.isSelected();
        Container[] comps = new Container[] {
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
        if (sel.length == 0)
            return;
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
                    } else {
                        if (FraggerPanel.fileNameExtensionFilter.accept(f)) {
                            paths.add(Paths.get(f.getAbsolutePath()));
                        }
                    }
                }
                tableModelRawFiles.dataAddAll(paths);
                
                
                break;
        }
    }//GEN-LAST:event_btnRawAddFolderActionPerformed

    private void btnCheckJavaVersionActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnCheckJavaVersionActionPerformed
//        LinkedHashMap<String, String> map = new LinkedHashMap<>();
//        map.put("java.version", "")
        List<String> propNames = Arrays.asList(
                "java.version",
                "java.vm.specification.version",
                "java.vm.specification.vendor",
                "java.vm.specification.name",
                "java.vm.version",
                "java.vm.vendor",
                "java.vm.name",
                "java.specification.version",
                "java.specification.vendor",
                "java.specification.name"
                );
        StringBuilder sb = new StringBuilder("Java Info:\n");
        for (String propName : propNames) {
            String val = System.getProperty(propName);
            if (!StringUtils.isNullOrWhitespace(val)) {
                sb.append(propName).append(": ").append(val).append("\n");
            }
        }
        
        LogUtils.println(console, sb.toString());
    }//GEN-LAST:event_btnCheckJavaVersionActionPerformed

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
    }//GEN-LAST:event_btnMsfraggerBinBrowseActionPerformed

    /**
     * Checks if a file is a JAR file and that it contains MSFragger.class at the top level.
     * @param f  file to check.
     * @return  True if it's a real JAR file with MSFragger.class at the top level inside.
     */
    private boolean validateAndSaveMsfraggerPath(String path) {
        boolean isValid = validateMsfraggerPath(path);
        if (isValid) {
            textBinMsfragger.setText(path);
            ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_MSFRAGGER, path);
        }
        if (balloonMsfragger != null) {
            balloonMsfragger.closeBalloon();
            balloonMsfragger = null;
        }
        if (!isValid) {
            balloonMsfragger = new BalloonTip(textBinMsfragger, 
                    "<html>Could not find MSFragger jar file at this location\n<br/>."
                    + "Corresponding panel won't be active");
            balloonMsfragger.setVisible(true);
            
        }
        
        if (isValid) {
            boolean valVer = validateMsfraggerVersion(path);
            isValid = isValid && valVer;
        }
        
        enableMsfraggerPanels(isValid);
        
        return isValid;
    }
    
    private boolean validateMsfraggerJavaVersion() {
        final boolean javaAtLeast18 = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8);
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                BalloonTip tip = tipMap.get(TIP_NAME_FRAGGER_JAVA_VER);
                if (tip != null) {
                    tip.closeBalloon();
                    tipMap.remove(TIP_NAME_FRAGGER_JAVA_VER);
                }
                if (!javaAtLeast18) {
                    tip = new BalloonTip(lblFraggerJavaVer, "Msfragger requires Java 1.8.\n");
                    tipMap.put(TIP_NAME_FRAGGER_JAVA_VER, tip);
                    tip.setVisible(true);
                }
            }
        });
        return javaAtLeast18;
    }

    private String getGuiDownloadLink() {
        String locallyKnownDownloadUrl = null;
        try {
            InputStream is = MsfraggerGuiFrame.class.getResourceAsStream("Bundle.properties");
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
    
    private void validateGuiVersion() {
        // The version from cmd line ouput is new enough to pass the local
            // test. Now check the file on github.
            Thread t = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        String githubProps = IOUtils.toString(Version.PROPERTIES_URI.toURL(), Charset.forName("UTF-8"));
                        Properties props = new Properties();
                        props.load(new StringReader(githubProps));
                        final String githubVersion = props.getProperty(Version.PROP_VER);
                        if (githubVersion == null) {
                            throw new IllegalStateException("Property "
                                    + Version.PROP_VER 
                                    + " was not found in Bundle.properties from github");
                        }
                        
                        
                        String locallyKnownDownloadUrl = null;
                        try {
                            InputStream is = MsfraggerGuiFrame.class.getResourceAsStream("Bundle.properties");
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
                        
                        final String downloadUrl = props.getProperty(Version.PROP_DOWNLOAD_URL, locallyKnownDownloadUrl);
                        VersionComparator vc = new VersionComparator();
                        if (vc.compare(Version.VERSION, githubVersion) < 0) {
                            // show balloon popup, must be done on EDT
                            SwingUtilities.invokeLater(new Runnable() {
                                @Override
                                public void run() {
                                    BalloonTip tip = tipMap.get(Version.PROP_VER);
                                    if (tip != null) {
                                        tip.closeBalloon();
                                        tipMap.remove(Version.PROP_VER);
                                    }

                                    JEditorPane ep = SwingUtils.createClickableHtml(String.format(
                                            "Your MSFragger-GUI version is [%s]<br>\n"
                                            + "There is a newer version of MSFragger-GUI available [%s]).<br>\n"
                                            + "Please <a href=\"%s\">click here</a> to download a newer one.", 
                                            Version.VERSION, githubVersion, downloadUrl));

                                    BalloonTip t = new BalloonTip(btnAboutInConfig, ep, 
                                            new RoundedBalloonStyle(5,5,Color.WHITE, Color.BLACK), true);
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
    
    private boolean validateMsfraggerVersion(String jarPath) {
        ProcessBuilder pb = new ProcessBuilder("java", "-jar", jarPath);
        pb.redirectErrorStream(true);
        Pattern regex = Pattern.compile("MSFragger version (MSFragger-([\\d\\.]{4,}))", Pattern.CASE_INSENSITIVE);
        
        boolean isVersionPrintedAtAll = false;
        
        // get the vesrion reported by the current executable
        String verStr = null;
        try {
            Process pr = pb.start();
            BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()));
            String line;
            while ((line = in.readLine()) != null) {
                Matcher m = regex.matcher(line);
                if (m.matches()) {
                    isVersionPrintedAtAll = true;
                    verStr = m.group(2);
                }
            }
            pr.waitFor();
        } catch (IOException | InterruptedException e) {
            
        }
        final String matchedVersion = verStr;
        
        // get the latest known version stored in the text file
        String latestVersion = null;
        try {
            InputStream is = MsfraggerParams.class.getResourceAsStream("msfragger.properties");
            if (is == null) {
                throw new IllegalStateException("Could not read msfragger.properties from the classpath");
            }
            Properties prop = new Properties();
            prop.load(is);
            latestVersion = prop.getProperty(MsfraggerProperties.PROP_LATEST_VERSION);
            if (latestVersion == null) {
                throw new IllegalStateException("Property "
                        + MsfraggerProperties.PROP_LATEST_VERSION 
                        + " was not found in msfragger.properties");
            }
        } catch (IOException e) {
            throw new IllegalStateException("Error reading msfragger.properties from the classpath");
        }
        
        if (!isVersionPrintedAtAll) {
            // a very old fragger version, need to download a new one
            if (balloonMsfragger != null) {
                balloonMsfragger.closeBalloon();
            }
            
            
            JEditorPane ep = SwingUtils.createClickableHtml(String.format("Your version of MSFragger "
                    + "is not supported anumore.<br>\n"
                    + "Please <a href=\"%s\">click here</a> to download a newer one.", 
                    MsfraggerProperties.DOWNLOAD_URL));
            balloonMsfragger = new BalloonTip(textBinMsfragger, ep, 
                    new RoundedBalloonStyle(5,5,Color.WHITE, Color.BLACK), true);
            
            balloonMsfragger.setVisible(true);
            return false;
        }
        
        // compare the versions
        final VersionComparator vc = new VersionComparator();
        if (vc.compare(matchedVersion, latestVersion) < 0) {
            if (balloonMsfragger != null) {
                balloonMsfragger.closeBalloon();
            }
            
            JEditorPane ep = SwingUtils.createClickableHtml(String.format(
                    "There is a newer version of MSFragger available.<br>\n"
                    + "Your version is [%s]<br>\n"
                    + "Please <a href=\"%s\">click here</a> to download a newer one.", 
                    matchedVersion, MsfraggerProperties.DOWNLOAD_URL));
            
            balloonMsfragger = new BalloonTip(textBinMsfragger, ep, 
                    new RoundedBalloonStyle(5,5,Color.WHITE, Color.BLACK), true);
            balloonMsfragger.setVisible(true);
            return true;
        } else {
            // The version from cmd line ouput is new enough to pass the local
            // test. Now check the file on github.
            Thread t = new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        String githubProps = IOUtils.toString(MsfraggerProperties.PROPERTIES_URI.toURL(), Charset.forName("UTF-8"));
                        Properties props = new Properties();
                        props.load(new StringReader(githubProps));
                        final String githubVersion = props.getProperty(MsfraggerProperties.PROP_LATEST_VERSION);
                        if (githubVersion == null) {
                            throw new IllegalStateException("Property "
                                    + MsfraggerProperties.PROP_LATEST_VERSION 
                                    + " was not found in msfragger.properties from github");
                        }
                        final String downloadUrl = props.getProperty(MsfraggerProperties.PROP_DOWNLOAD_URL, MsfraggerProperties.DOWNLOAD_URL);
                        if (vc.compare(matchedVersion, githubVersion) < 0) {
                            // show balloon popup, must be done on EDT
                            SwingUtilities.invokeLater(new Runnable() {
                                @Override
                                public void run() {
                                    if (balloonMsfragger != null) {
                                        balloonMsfragger.closeBalloon();
                                    }

                                    JEditorPane ep = SwingUtils.createClickableHtml(String.format(
                                            "Your version is [%s]<br>\n"
                                            + "There is a newer version of MSFragger available [%s]).<br>\n"
                                            + "Please <a href=\"%s\">click here</a> to download a newer one.", 
                                            matchedVersion, githubVersion, downloadUrl));

                                    balloonMsfragger = new BalloonTip(textBinMsfragger, ep, 
                                            new RoundedBalloonStyle(5,5,Color.WHITE, Color.BLACK), true);
                                    balloonMsfragger.setVisible(true);
                                }
                            });
                        }
                    } catch (IOException ex) {
                        // it doesn't matter, it's fine if we can't fetch the file from github
                        System.err.println("Could not download msfragger.properties file from github");
                    }
                }
            });
            t.start();
        }
        
        return true;
    }
    
    private boolean validateMsfraggerPath(String path) {
        File f = new File(path);
        if (!f.getName().toLowerCase().endsWith(".jar"))
            return false;
        if (!Files.exists(Paths.get(f.getAbsolutePath()))) {
            return false;
        }
        
        try {
            ZipFile zf = new ZipFile(f);
            Enumeration<? extends ZipEntry> entries = zf.entries();
            while(entries.hasMoreElements()) {
                ZipEntry ze = entries.nextElement();
                if ("MSFragger.class".equals(ze.getName())) {
                    return true;
                }
            }
        } catch (IOException ex) {
            // doesn't matter
            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return false;
    }
    
    private void btnMsfraggerBinDownloadActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnMsfraggerBinDownloadActionPerformed
        try {
            Desktop.getDesktop().browse(URI.create("http://inventions.umich.edu/technologies/7143_msfrager-ultrafast-and-comprehensive-identification-of-peptides-from-tandem-mass-spectra"));
        } catch (IOException ex) {
            Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE, null, ex);
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
                for(Path foundFile : foundFiles) {
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
                for(Path foundFile : foundFiles) {
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
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setApproveButtonText("Select");
        fileChooser.setDialogTitle("Select Philosopher binary");
        fileChooser.setMultiSelectionEnabled(false);
        if (OsUtils.isWindows()) {
            FileNameExtensionFilter fileNameExtensionFilter = new FileNameExtensionFilter("Executables", "exe");
            fileChooser.setFileFilter(fileNameExtensionFilter);
        }
        

        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

        List<String> props = Arrays.asList(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER, ThisAppProps.PROP_BINARIES_IN);
        String fcPath = ThisAppProps.tryFindPath(props, true);
        SwingUtils.setFileChooserPath(fileChooser, fcPath);

        int showOpenDialog = fileChooser.showOpenDialog(SwingUtils.findParentComponentForDialog(this));
        switch (showOpenDialog) {
            case JFileChooser.APPROVE_OPTION:
                File f = fileChooser.getSelectedFile();
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
        // TODO add your handling code here:
    }//GEN-LAST:event_textBinPhilosopherActionPerformed

    private void btnClearCacheActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnClearCacheActionPerformed
        ThisAppProps.clearCache();
    }//GEN-LAST:event_btnClearCacheActionPerformed

    private void validateAndSaveReportFilter() {
        String val = textReportFilter.getText().trim();
        
        final boolean isValid = true;
        if (isValid) {
            textReportFilter.setText(val);
            ThisAppProps.save(ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER, val);
        }
        
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                BalloonTip tip = tipMap.get(ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER);
                if (tip != null) {
                    tip.closeBalloon();
                }

                if (!isValid) {
                    tip = new BalloonTip(textReportFilter, "Invalid format.");
                    tip.setVisible(true);
                    tipMap.put(ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER, tip);
                }
            }
        });
        
        // check if the filter line has changed since focus was gained
        String newText = textReportFilter.getText();
        String oldText = textReportFilterFocusGained;
        if (!oldText.equals(newText)) {
            // check if the reverse tag has changed
            Pattern p1 = Pattern.compile("--tag\\s+([^\\s]+)");
            String newVal = "", oldVal = "";
            Matcher m = p1.matcher(newText);
            if (m.find()) {
                newVal = m.group(1);
            }
            m = p1.matcher(oldText);
            if (m.find()) {
                oldVal = m.group(1);
            }
            if (!oldVal.equals(newVal)) {
                final String message = String.format(
                        "Decoy prefix in Philosopher Report options has changed "
                        + "from '%s' to '%s'.\n"
                        + "Do you want to also change it in PeptideProphet command?", oldVal, newVal);
                
                // does the user want to chnage the Report tag automatically?
                int ans = JOptionPane.showConfirmDialog(this, message, "Decoy prefix change", JOptionPane.YES_NO_OPTION);
                if (ans == JOptionPane.YES_OPTION) {
                    Pattern p2 = Pattern.compile("--decoy\\s+([^\\s]+)");
                    String pepProphCmd = txtPeptideProphetCmdLineOptions.getText();
                    m = p2.matcher(pepProphCmd);
                    String newPepProphText;
                    if (newVal.endsWith("_")) {
                        newVal = newVal.substring(0, newVal.length() - 1);
                    }
                    if (newVal.isEmpty()) {
                        // the new value is a zero length string, i.e. it was deleted
                        newPepProphText = m.replaceAll("");
                    } else {
                        if (m.find()) {
                            newPepProphText = m.replaceAll(String.format("--decoy %s", newVal));
                        } else {
                            // if Reprot didn't have prefix, add it at the end
                            newPepProphText = String.format("%s --decoy %s", pepProphCmd, newVal);
                        }
                    }
                    txtPeptideProphetCmdLineOptions.setText(newPepProphText);
                }
            }
        }
    }

    private void btnRunActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnRunActionPerformed
                                               
        resetRunButtons(false);

        if (
            !fraggerPanel.isRunMsfragger() &&
            !chkRunPeptideProphet.isSelected() &&
            !chkRunProteinProphet.isSelected() &&
            !checkCreateReport.isSelected()) {
            JOptionPane.showMessageDialog(this, "Nothing to run.\n"
                + "Please mark checkboxes in other tabs to run processing tools.", "Error", JOptionPane.WARNING_MESSAGE);
            resetRunButtons(true);
            return;
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
                String msg = String.format("Something went wronng when creating symlinks to LCMS files.\n%s", ex.getMessage());
                JOptionPane.showMessageDialog(this, msg, "Error", JOptionPane.ERROR_MESSAGE);

                resetRunButtons(true);
                return;
            }
        } else {
            // On windows copy the files over to the working directory
//            List<ProcessBuilder> processBuildersCopyFiles = processBuildersCopyFiles(programsDir, workingDir, lcmsFilePaths);
//            processBuilders.addAll(processBuildersCopyFiles);
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
                        String msg = String.format("The list of input files contains MGF entries.\n"
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
        processBuilders.addAll(processBuildersPeptideProphet);

        List<ProcessBuilder> processBuildersProteinProphet = processBuildersProteinProphet("", workingDir, lcmsFilePaths);
        if (processBuildersProteinProphet == null) {
            resetRunButtons(true);
            return;
        }
        processBuilders.addAll(processBuildersProteinProphet);
        
        List<ProcessBuilder> processBuildersReport = processBuildersReport("", workingDir, lcmsFilePaths);
        if (processBuildersReport == null) {
            resetRunButtons(true);
            return;
        }
        processBuilders.addAll(processBuildersReport);
        
        if (!OsUtils.isWindows()) {
            // On Linux we created symlinks to mzXML files, leave them there
        } else {
            // On windows we copied the files over to the working directory
            // so will delete them now
//            List<ProcessBuilder> processBuildersDeleteFiles = processBuildersDeleteFiles(workingDir, lcmsFilePaths);
//            processBuilders.addAll(processBuildersDeleteFiles);
        }

        LogUtils.println(console, String.format("Will execute %d commands:", processBuilders.size()));
        for (final ProcessBuilder pb : processBuilders) {
            StringBuilder sb = new StringBuilder();
            List<String> command = pb.command();
            for (String commandPart : command)
            sb.append(commandPart).append(" ");
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
        
        try // run everything
        {
            exec = Executors.newFixedThreadPool(1);
            for (final ProcessBuilder pb : processBuilders) {

                pb.directory(Paths.get(workingDir).toFile());

                REHandler reHandler;
                reHandler = new REHandler(new Runnable() {
                    @Override
                    public void run() {
                        Process process = null;
                        try {
                            List<String> command = pb.command();
                            StringBuilder sb = new StringBuilder("Executing command:\n$> ");
                            for (String commandPart : command)
                            sb.append(commandPart).append(" ");
                            String toAppend = sb.toString();
                            LogUtils.println(console, toAppend);
                            process = pb.start();
                            toAppend = "Process started";
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
                                }
                                int outAvailable = out.available();
                                if (outAvailable > 0) {
                                    byte[] bytes = new byte[outAvailable];
                                    int read = out.read(bytes);
                                    toAppend = new String(bytes);
                                    LogUtils.println(console, toAppend);
                                }
                                try {
                                    int exitValue = process.exitValue();
                                    toAppend = String.format("Process finished, exit value: %d\n", exitValue);
                                    LogUtils.println(console, toAppend);
                                    break;
                                } catch (IllegalThreadStateException ignore) {
                                    // this error is thrown by process.exitValue() if the underlying process has not yet finished
                                }
                            }

                        } catch (IOException ex) {
                            String toAppend = String.format("IOException: Error in process,\n%s", ex.getMessage());
                            LogUtils.println(console, toAppend);
                        } catch (InterruptedException ex) {
                            if (process != null) {
                                process.destroy();
                            }
                            String toAppend = String.format("InterruptedException: Error in process,\n%s", ex.getMessage());
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
                                if (!linkPath.getParent().equals(origPath.getParent()))
                                linkPath.toFile().deleteOnExit();
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
                btnRun.setEnabled(true);
                btnStop.setEnabled(false);
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
            loadDefaultsPeptideProphet(type);
            loadDefaultsProteinProphet(type);
            loadDefaultsReportFilter(type);
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
            loadDefaultsPeptideProphet(type);
            loadDefaultsProteinProphet(type);
            loadDefaultsReportFilter(type);
        }
    }//GEN-LAST:event_btnLoadDefaultsClosedActionPerformed

    private void txtPeptideProphetCmdLineOptionsFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_txtPeptideProphetCmdLineOptionsFocusLost
        validateAndSavePeptideProphetCmdLineOptions();
    }//GEN-LAST:event_txtPeptideProphetCmdLineOptionsFocusLost

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
        validateAndSaveReportFilter();
    }//GEN-LAST:event_textReportFilterFocusLost

    private void btnAboutInConfigActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnAboutInConfigActionPerformed
        btnAboutActionPerformed(null);
    }//GEN-LAST:event_btnAboutInConfigActionPerformed

    private void btnReportDefaultsClosedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReportDefaultsClosedActionPerformed
        loadDefaultsReportFilter(SearchTypeProp.closed);
    }//GEN-LAST:event_btnReportDefaultsClosedActionPerformed

    private void btnReportDefaultsOpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_btnReportDefaultsOpenActionPerformed
        loadDefaultsReportFilter(SearchTypeProp.open);
    }//GEN-LAST:event_btnReportDefaultsOpenActionPerformed

    private void checkReportProteinLevelFdrStateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_checkReportProteinLevelFdrStateChanged
        boolean selected = checkReportProteinLevelFdr.isSelected();
        ThisAppProps.save(ThisAppProps.PROP_CHECKBOX_REPORT_PROTEIN_LEVEL_FDR, Boolean.toString(selected));
    }//GEN-LAST:event_checkReportProteinLevelFdrStateChanged

    private void txtPeptideProphetCmdLineOptionsFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_txtPeptideProphetCmdLineOptionsFocusGained
        String t = txtPeptideProphetCmdLineOptions.getText();
        if (!t.equals(textPepProphetFocusGained)) {
            textPepProphetFocusGained = t;
        }
    }//GEN-LAST:event_txtPeptideProphetCmdLineOptionsFocusGained

    private void textReportFilterActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_textReportFilterActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_textReportFilterActionPerformed

    private void textReportFilterFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_textReportFilterFocusGained
        textReportFilterFocusGained = textReportFilter.getText();
    }//GEN-LAST:event_textReportFilterFocusGained

    public void loadLastPeptideProphet() {
        String val = ThisAppProps.load(ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET);
        if (val != null) {
            txtPeptideProphetCmdLineOptions.setText(val);
        } else {
            loadDefaultsPeptideProphet(SearchTypeProp.open);
        }
    }
    
    public void loadDefaultsPeptideProphet(SearchTypeProp type) {
        final String prop = ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET + "." + type.name();
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N
        String val = bundle.getString(prop);
        
        txtPeptideProphetCmdLineOptions.setText(val);
        ThisAppProps.save(ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET, val);
    }
    
    public void loadLastProteinProphet() {
        String val = ThisAppProps.load(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET);
        if (val != null) {
            txtProteinProphetCmdLineOpts.setText(val);
        } else {
            loadDefaultsProteinProphet(SearchTypeProp.open);
        }
    }
    
    public void loadDefaultsProteinProphet(SearchTypeProp type) {
        final String prop = ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET + "." + type.name();
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N        
        String val = bundle.getString(prop);
        
        txtProteinProphetCmdLineOpts.setText(val);
        ThisAppProps.save(ThisAppProps.PROP_TEXT_CMD_PROTEIN_PROPHET, val);
    }
    
    private void loadLastReportFilter() {
        String val = ThisAppProps.load(ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER);
        if (val != null) {
            textReportFilter.setText(val);
        } else {
            loadDefaultsReportFilter(SearchTypeProp.open);
        }
    }
    
    private void loadDefaultsReportFilter(SearchTypeProp type) {
        final String prop = ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER + "." + type.name();
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N        
        String val = bundle.getString(prop);
        
        textReportFilter.setText(val);
        ThisAppProps.save(ThisAppProps.PROP_TEXTFIELD_REPORT_FILTER, val);
    }

    private String getFraggerLableJavaVer() {
        List<String> propNames = Arrays.asList(
                "java.version",
                "java.vm.name",
                "java.vm.vendor"
        );
        StringBuilder sb = new StringBuilder("Java Info: ");
        for (int i = 0; i < propNames.size(); i++) {
            String p = propNames.get(i);
            String val = System.getProperty(p);
            if (!StringUtils.isNullOrWhitespace(val)) {
                sb.append(val);
            }
            if (i < propNames.size() - 1) {
                sb.append(", ");
            }
        }
        
        return sb.toString();
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

    private void validateAndSavePeptideProphetCmdLineOptions() {
        String curText = txtPeptideProphetCmdLineOptions.getText();
        String oldText = textPepProphetFocusGained;
        ThisAppProps.save(ThisAppProps.PROP_TEXT_CMD_PEPTIDE_PROPHET, curText);
        if (!oldText.equals(curText)) {
            // text in the field has changed
            Pattern p1 = Pattern.compile("--decoy\\s+([^\\s]+)");
            String newDecoyPrefix = "", oldDecoyPrefix = "";
            Matcher m = p1.matcher(curText);
            if (m.find()) {
                newDecoyPrefix = m.group(1);
            }
            m = p1.matcher(oldText);
            if (m.find()) {
                oldDecoyPrefix = m.group(1);
            }
            
            // if the new prefix differs from the old one
            if (!oldDecoyPrefix.equals(newDecoyPrefix)) {
                final String message = String.format(
                        "Decoy prefix in PepetideProphet options has changed from '%s' to '%s'.\n"
                        + "Do you want to also change it in Philosopher Report "
                        + "command?", oldDecoyPrefix, newDecoyPrefix);
                
                // does the user want to chnage the Report tag automatically?
                int ans = JOptionPane.showConfirmDialog(this, message, "Decoy prefix change", JOptionPane.YES_NO_OPTION);
                if (ans == JOptionPane.YES_OPTION) {
                    // check if Report tab had a decoy prefix at all (--tag XXX_)
                    Pattern p2 = Pattern.compile("--tag\\s+([^\\s]+)");
                    String report = textReportFilter.getText();
                    m = p2.matcher(report);
                    String newReportText;
                    if (newDecoyPrefix.isEmpty()) {
                        newReportText = m.replaceAll("");
                    } else {
                        if (m.find()) {
                            newReportText = m.replaceAll(String.format("--tag %s_", newDecoyPrefix));
                        } else {
                            // if Reprot didn't have prefix, add it at the end
                            newReportText = String.format("%s --tag %s_", report, newDecoyPrefix);
                        }
                    }
                    textReportFilter.setText(newReportText);
                }
            }
        }
    }

    public enum SearchTypeProp {open, closed}
    
    private boolean validateAndSavePhilosopherPath(final String path) {
        
        Path p;
        try {
            p = Paths.get(path);
        } catch (Exception e) {
            // invalid input
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    if (balloonPhilosopher != null) {
                        balloonPhilosopher.closeBalloon();
                        balloonPhilosopher = null;
                    }
                    balloonPhilosopher = new BalloonTip(textBinPhilosopher, 
                                "Invalid input for Philosopher path.");
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
        
        
        String validatedPath = validatePhilosopherPath(path);
        final boolean isPathValid = validatedPath != null;
        
        if (isPathValid) {
            textBinPhilosopher.setText(validatedPath);
            ThisAppProps.save(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER, validatedPath);
        }
        
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (balloonPhilosopher != null) {
                    balloonPhilosopher.closeBalloon();
                    balloonPhilosopher = null;
                }
                
                StringBuilder sb = new StringBuilder();
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
                    balloonPhilosopher = new BalloonTip(textBinPhilosopher, sb.toString());
                    balloonPhilosopher.setVisible(true);
                }
                enablePhilosopherPanels(isPathValid);
            }
        });
        
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
     * This returns the paths to files to be created. Might be symlinks or actual file copies.
     * It does not create the files!
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
            if (link.equals(lcmsPath))
                return;
            if (Files.exists(link)) {
                // if that link already exists we need to make sure it points to
                // the same file
                if (!Files.isSymbolicLink(link)) {
                    throw new FileAlreadyExistsException(link.toString(), null, "A file already exists and is not a symbolic link");
                }
                Path linkTarget = Files.readSymbolicLink(link);
                if(!linkTarget.equals(lcmsPath)) {
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

    private String createSysInfoPhilosopherText() {
        String os = OsUtils.getOsName();
        os = os != null ? os : "?";
        OsUtils.ARCH arch = OsUtils.getSystemArch();
        String archStr = arch != null ? arch.toString().toLowerCase() : "?";
        String sysInfo = String.format("System OS: %s, Architecture: %s", os, archStr);
        return sysInfo;
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
            if (fileIn.getParent().equals(wd))
                continue;
            
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
            
            if (wd.equals(Paths.get(lcmsFilePath).getParent()))
                continue;
            
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
        if (binJava != null)
            return binJava;
        JOptionPane.showMessageDialog(this, "Java could not be found.\n"
                + "please make sure you have it installed \n"
                + "and that java.exe can be found on PATH", "Error", JOptionPane.ERROR_MESSAGE);
        return null;
    }
    
    /**
     * @return  Combined protein file name without extension.
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
     * @return  Combined protein file name with extension.
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
            final int commandLenLimit = 8192;

            int fileIndex = 0;
            while (fileIndex < lcmsFilePaths.size()) {
                int fileIndexLo = fileIndex;
                ArrayList<String> cmd = new ArrayList<>();
                cmd.add("java");
                cmd.add("-jar");
                if (ramGb > 0) {
                    cmd.add(new StringBuilder().append("-Xmx").append(ramGb).append("G").toString());
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
                URI currentJarUri = PathUtils.getCurrentJarPath();
                String currentJarPath = Paths.get(currentJarUri).toAbsolutePath().toString();
                Path wdPath = Paths.get(workingDir);

                for (int i = fileIndexLo; i < fileIndex; i++) {
                    String pepFile = mapRawToPep.get(lcmsFilePaths.get(i));
                    Path pepPath = Paths.get(pepFile);

                    if (!wdPath.equals(pepPath.getParent())) {
                        ArrayList<String> cmdMove = new ArrayList<>();
                        cmdMove.add("java");
                        cmdMove.add("-cp");
                        cmdMove.add(currentJarPath);
                        cmdMove.add("umich.msfragger.util.FileMove");
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
            if (nameWithoutExt == null)
                throw new IllegalStateException(String.format("Could not identify the extension for file: %s", pepxmlClean));
            
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
     * Compares the value of the textfield to the "ghost text" value and returns
     * the one from MSFragger panel, if it matches or is empty.
     * @return 
     */
    private String getActualDbPath(String fastaPath) {
        fastaPath = fastaPath.trim();
        if (TEXT_SAME_SEQ_DB.equals(fastaPath)) {
            fastaPath = "";
        }
        if (StringUtils.isNullOrWhitespace(fastaPath)) {
            fastaPath = fraggerPanel.getTxtMsfraggerDb().getText().trim();
            if (StringUtils.isNullOrWhitespace(fastaPath)) {
                return null;
            }
        }
        return fastaPath;
    }
    
    /**
     * Creates the ProcessBuilders for running PeptideProphet.
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


            String fastaPath = txtPeptideProphetSeqDb.getText().trim();
            if (TEXT_SAME_SEQ_DB.equals(fastaPath)) {
                fastaPath = "";
            }
            if (StringUtils.isNullOrWhitespace(fastaPath)) {
                fastaPath = fraggerPanel.getTxtMsfraggerDb().getText().trim();
                if (StringUtils.isNullOrWhitespace(fastaPath)) {
                    JOptionPane.showMessageDialog(this, "Fasta file (PeptideProphet) path can't be empty",
                        "Warning", JOptionPane.WARNING_MESSAGE);
                    return null;
                }
            }
            String fastaPathOrig = fastaPath;
            fastaPath = PathUtils.testFilePath(fastaPath, workingDir);
            if (fastaPath == null) {
                JOptionPane.showMessageDialog(this, String.format("Could not find fasta file (PeptideProphet) at:\n%s", fastaPathOrig),
                        "Errors", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
            peptideProphetParams.setCmdLineParams(txtPeptideProphetCmdLineOptions.getText().trim());
            
            String philosopherPeptideprophetCmd = "peptideprophet";
            boolean isPhilosopher = isPhilosopherBin(bin);
            
            // for new philoshopher we need to run 'init' command first
            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--init");
                ProcessBuilder pb = new ProcessBuilder(cmd);
                builders.add(pb);
            }
            
            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir);
            for (String rawFilePath : lcmsFilePaths) {
                // Comet
                List<String> commands = new ArrayList<>();
                commands.add(bin);
                if (isPhilosopher) // for philosopher we always add the correct command
                    commands.add(Philosopher.CMD_PEPTIDE_PROPHET);

                if (!peptideProphetParams.getCmdLineParams().isEmpty()) {
                    String cmdOpts = peptideProphetParams.getCmdLineParams();
                    List<String> opts = StringUtils.splitCommandLine(cmdOpts);
                    for (String opt : opts) {
                        if (!opt.isEmpty()) {
                            if (opt.equals(Philosopher.CMD_PEPTIDE_PROPHET))
                                continue;
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
            
            // for new philoshopher 'clean' after ourselves
            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--clean");
                ProcessBuilder pb = new ProcessBuilder(cmd);
                builders.add(pb);
            }
        }
        return builders;
    }
    
    /**
     * Creates the processBuilders for running ProteinProphet.
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
            
            // for new philoshopher we need to run 'init' command first
            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--init");
                builders.add(new ProcessBuilder(cmd));
            }
            
            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir);
            Map<String, String> interacts = createInteractFilePaths(pepxmlClean, workingDir, fraggerPanel.getOutputFileExt());
            
            if (isPhilosopher) {
                commands.add(Philosopher.CMD_PROTEIN_PROPHET);
                
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
                    if (!StringUtils.isNullOrWhitespace(interact))
                        createdInteractFiles.add(interact);
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
            
            // for new philoshopher 'clean' after ourselves
            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--clean");
                ProcessBuilder pb = new ProcessBuilder(cmd);
                builders.add(pb);
            }

            return builders;
        }
        return Collections.emptyList();
    }
    
    /**
     * Creates the processBuilders for running ProteinProphet.
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
            
            // for new philoshopher we need to run 'init' command first
            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--init");
                builders.add(new ProcessBuilder(cmd));
            }
            
            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir);
            Map<String, String> interacts = createInteractFilePaths(pepxmlClean, workingDir, fraggerPanel.getOutputFileExt());
            Path combinedProtFilePath = getCombinedProtFilePath(workingDir);
            
            if (checkReportDbAnnotate.isSelected()) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add(Philosopher.CMD_DATABASE);
                cmd.add("--annotate");
                String fastaPath = getActualDbPath(textReportDbAnnotate.getText());
                if (fastaPath == null) {
                    JOptionPane.showMessageDialog(this, "Fasta file path can't be empty (Report)",
                            "Warning", JOptionPane.WARNING_MESSAGE);
                    return null;
                }
                cmd.add(fastaPath);
                builders.add(new ProcessBuilder(cmd));
            }
            
            // philosopher filter
            if (checkReportFilter.isSelected()) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add(Philosopher.CMD_FILTER);
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
            
            // philosopher report
            if (true) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add(Philosopher.CMD_REPORT);
                builders.add(new ProcessBuilder(cmd));
            }
                    
            
            // for new philoshopher 'clean' after ourselves
            if (isPhilosopher) {
                List<String> cmd = new ArrayList<>();
                cmd.add(bin);
                cmd.add("workspace");
                cmd.add("--clean");
                ProcessBuilder pb = new ProcessBuilder(cmd);
                builders.add(pb);
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
        if (value != null)
            return value;
        ResourceBundle bundle = ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N
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
        if (value != null)
            return value;

        String binaryName;
        ResourceBundle bundle = ResourceBundle.getBundle("dia/umpire/gui/Bundle"); // NOI18N
        if (OsUtils.isWindows()) {
            binaryName = bundle.getString("default.msconvert.win");
        } else {
            binaryName = bundle.getString("default.msconvert.nix");
        }
        String testedBinaryPath = PathUtils.testBinaryPath(binaryName);
        if (testedBinaryPath != null && !testedBinaryPath.isEmpty())
            return testedBinaryPath;
        
        
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
                for (Path rootDir: rootDirs) {
                    try {
                        DirectoryStream<Path> dirStream = Files.newDirectoryStream(rootDir);
                        for (Path file: dirStream) {
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
        int[] sizes = {16, 24, 32, 64, 128, 256, 512};
        final String path = "icons/";
        final String baseName = "bolt-";
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
        //</editor-fold>
        //</editor-fold>
        //</editor-fold>

        //</editor-fold>
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                MsfraggerGuiFrame frame = new MsfraggerGuiFrame();
                frame.setVisible(true);
                Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
                frame.setLocation(dim.width/2-frame.getSize().width/2, dim.height/2-frame.getSize().height/2);
            }
        });
    }
    
    private String getDefaultBinMsfragger() {
        String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSFRAGGER);
        return path == null ? "MSFragger.jar" : path;
    }
    
    private String getDefaultBinPhilosopher() {
        String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER);
        if (path != null)
            return path;
        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("umich/msfragger/gui/Bundle"); // NOI18N
        String winName = bundle.getString("default.philosopher.win"); // NOI18N
        String nixName = bundle.getString("default.philosopher.nix"); // NOI18N
        if (OsUtils.isWindows())
            return winName;
        return nixName;
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnAbout;
    private javax.swing.JButton btnAboutInConfig;
    private javax.swing.JButton btnCheckJavaVersion;
    private javax.swing.JButton btnClearCache;
    private javax.swing.JButton btnClearConsole;
    private javax.swing.JButton btnFindTools;
    private javax.swing.JButton btnLoadDefaultsClosed;
    private javax.swing.JButton btnLoadDefaultsOpen;
    private javax.swing.JButton btnMsfraggerBinBrowse;
    private javax.swing.JButton btnMsfraggerBinDownload;
    private javax.swing.JButton btnPepProphDefaultsClosed;
    private javax.swing.JButton btnPepProphDefaultsOpen;
    private javax.swing.JButton btnPhilosopherBinBrowse;
    private javax.swing.JButton btnPhilosopherBinDownload;
    private javax.swing.JButton btnProtProphDefaultsClosed;
    private javax.swing.JButton btnProtProphDefaultsOpen;
    private javax.swing.JButton btnProteinProphetSeqDb;
    private javax.swing.JButton btnRawAddFiles;
    private javax.swing.JButton btnRawAddFolder;
    private javax.swing.JButton btnRawClear;
    private javax.swing.JButton btnRawRemove;
    private javax.swing.JButton btnReportDefaultsClosed;
    private javax.swing.JButton btnReportDefaultsOpen;
    private javax.swing.JButton btnRun;
    private javax.swing.JButton btnSelectPeptideProphetSeqDbPath;
    private javax.swing.JButton btnSelectWrkingDir;
    private javax.swing.JButton btnStop;
    private javax.swing.JCheckBox checkCreateReport;
    private javax.swing.JCheckBox checkDryRun;
    private javax.swing.JCheckBox checkReportDbAnnotate;
    private javax.swing.JCheckBox checkReportFilter;
    private javax.swing.JCheckBox checkReportProteinLevelFdr;
    private javax.swing.JCheckBox chkProteinProphetInteractStar;
    private javax.swing.JCheckBox chkRunPeptideProphet;
    private javax.swing.JCheckBox chkRunProteinProphet;
    private javax.swing.JScrollPane consoleScrollPane;
    private javax.swing.JEditorPane editorMsfraggerCitation;
    private javax.swing.JEditorPane editorPhilosopherLink;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel34;
    private javax.swing.JLabel jLabel35;
    private javax.swing.JLabel jLabel39;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel40;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JLabel lblFindAutomatically;
    private javax.swing.JLabel lblFraggerJavaVer;
    private javax.swing.JLabel lblMsfraggerCitation;
    private javax.swing.JLabel lblOutputDir;
    private javax.swing.JPanel panelConfig;
    private javax.swing.JPanel panelMsFragger;
    private javax.swing.JPanel panelMsfraggerConfig;
    private javax.swing.JPanel panelPeptideProphet;
    private javax.swing.JPanel panelPeptideProphetOptions;
    private javax.swing.JPanel panelPhilosopherConfig;
    private javax.swing.JPanel panelProteinProphet;
    private javax.swing.JPanel panelProteinProphetOptions;
    private javax.swing.JPanel panelReport;
    private javax.swing.JPanel panelRun;
    private javax.swing.JPanel panelSelectFiles;
    private javax.swing.JPanel panelSelectedFiles;
    private javax.swing.JScrollPane scrollPaneMsFragger;
    private javax.swing.JScrollPane scrollPaneRawFiles;
    private javax.swing.JTabbedPane tabPane;
    private javax.swing.JTextField textBinMsfragger;
    private javax.swing.JTextField textBinPhilosopher;
    private javax.swing.JTextField textReportDbAnnotate;
    private javax.swing.JTextField textReportFilter;
    private javax.swing.JTextField txtCombinedProtFile;
    private javax.swing.JTextArea txtPeptideProphetCmdLineOptions;
    private javax.swing.JTextField txtPeptideProphetSeqDb;
    private javax.swing.JTextArea txtProteinProphetCmdLineOpts;
    private javax.swing.JTextField txtProteinProphetSeqDb;
    private javax.swing.JTextField txtWorkingDir;
    // End of variables declaration//GEN-END:variables

}
