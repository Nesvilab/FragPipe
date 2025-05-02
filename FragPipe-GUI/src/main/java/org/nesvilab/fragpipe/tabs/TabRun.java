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

package org.nesvilab.fragpipe.tabs;

import static org.nesvilab.fragpipe.Fragpipe.getStickyStrict;
import static org.nesvilab.fragpipe.cmd.CmdBase.constructClasspathString;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.FragpipeRun;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.FragpipeCacheUtils;
import org.nesvilab.fragpipe.cmd.PbiBuilder;
import org.nesvilab.fragpipe.cmd.ProcessBuilderInfo;
import org.nesvilab.fragpipe.cmd.ToolingUtils;
import org.nesvilab.fragpipe.messages.MessageClearConsole;
import org.nesvilab.fragpipe.messages.MessageKillAll;
import org.nesvilab.fragpipe.messages.MessageKillAll.REASON;
import org.nesvilab.fragpipe.messages.MessagePrintToConsole;
import org.nesvilab.fragpipe.messages.MessageRun;
import org.nesvilab.fragpipe.messages.MessageRunButtonEnabled;
import org.nesvilab.fragpipe.messages.MessageSaveLog;
import org.nesvilab.fragpipe.process.ProcessResult;
import org.nesvilab.fragpipe.tools.philosopher.ReportPanel;
import org.nesvilab.fragpipe.util.BatchRun;
import org.nesvilab.fragpipe.util.SDRFtable;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.FileChooserUtils;
import org.nesvilab.utils.swing.FileChooserUtils.FcMode;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.BufferedWriter;
import java.io.IOException;
import java.net.URI;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
import org.nesvilab.utils.swing.FormEntry;
import org.nesvilab.utils.swing.JPanelWithEnablement;
import org.nesvilab.utils.swing.MigUtils;
import org.nesvilab.utils.swing.TextConsole;
import org.nesvilab.utils.swing.UiCheck;
import org.nesvilab.utils.swing.UiCombo;
import org.nesvilab.utils.swing.UiSpinnerDouble;
import org.nesvilab.utils.swing.UiText;
import org.nesvilab.utils.swing.UiUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
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
  public static final String PDV_NAME = "/FP-PDV/FP-PDV-1.4.4.jar";
  public static final String GENERATE_REPORTS_NAME = "generate_reports_pdf.py";
  private static final String FRAGPIPE_ANALYST_URL = Fragpipe.propsFix().getProperty("fragpipe-analyst-url", "http://fragpipe-analyst.nesvilab.org/");

  public final TextConsole console;
  Color defTextColor;
  public UiText uiTextWorkdir;
  private UiCheck uiCheckDryRun;
  private UiCheck uiCheckDeleteCalibratedFiles;
  private UiCheck uiCheckDeleteTempFiles;
  public UiCheck uiCheckWriteSubMzml;
  public UiSpinnerDouble uiSpinnerProbThreshold;
  private JButton btnRun;
  private JButton btnStop;
  private JButton btnOpenPdv;
  private JButton btnOpenFragPipeAnalyst;
  private JButton btnGenerateSummaryReport;
  private UiCheck uiCheckExportMatchedFragments;
  private Thread pdvThread = null;
  private Thread generateReportThread = null;
  private JPanel pTop;
  private JPanel pConsole;
  private UiCheck uiCheckWordWrap;
  private Process pdvProcess = null;
  private Process generateReportProcess = null;
  private TabDownstream tabDownstream;
  private UiCheck uiCheckSaveSDRF;
  private UiCombo uiComboSDRFtype;
  private UiText uiTextJobName;

  public TabRun(TextConsole console, TabDownstream tabDownstream) {
    this.console = console;
    this.tabDownstream = tabDownstream;
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
    btnStop.setEnabled(!btnRun.isEnabled());
    btnOpenPdv.setEnabled(m.isEnabled); // When Run button is gray, disable the PDV button. When Run button is not gray, also enable the PDV button.
    tabDownstream.btnRun.setEnabled(m.isEnabled);
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void on(MessagePrintToConsole m) {
    if (Fragpipe.headless) {
      synchronized (System.out) {
        System.out.print(m.text);
        if (m.addNewline) {
          System.out.println();
        }
      }
    }

    m.console.append(m.color, m.text);
    if (m.addNewline) {
      m.console.append("\n");
    }
    m.console.getParent().getParent().revalidate();
  }

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageRun m) {
    int returnCode = FragpipeRun.run(m);
    if (Fragpipe.headless && returnCode != 0) {
      System.exit(returnCode);
    }
    Fragpipe.runDone.countDown();
  }

  private JPanel createPanelTop(TextConsole console) {
    uiTextWorkdir = UiUtils.uiTextBuilder().cols(30).create();
    FormEntry feWorkdir = mu.feb("workdir", uiTextWorkdir).label("Output dir:").tooltip("Processing results will be stored in this directory").create();
    JButton btnBrowse = feWorkdir.browseButton(() -> FileChooserUtils.builder("Select output directory").mode(FcMode.DIRS_ONLY).multi(false).paths(Stream.of(uiTextWorkdir.getNonGhostText(), Fragpipe.propsVar().getProperty(LAST_WORK_DIR))).create(), "Select output directory", selected -> {
      uiTextWorkdir.setText(selected.get(0).toString());
    });
    JButton btnOpenInFileManager = UiUtils.createButton("Open in File Manager", e -> {
      String text = uiTextWorkdir.getNonGhostText();
      if (StringUtils.isBlank(text)) {
        SwingUtils.showInfoDialog(TabRun.this, "Empty path", "Does not exist");
        return;
      }
      Path existing = PathUtils.existing(text);
      if (existing == null) {
        SwingUtils.showInfoDialog(TabRun.this, "Path:\n'" + text + "'\nDoes not exist", "Does not exist");
        return;
      }
      try {
        Desktop.getDesktop().open(existing.toFile());
      } catch (IOException ex) {
        SwingUtils.showErrorDialog(TabRun.this, "Could not open path in system file browser.", "Error");
        return;
      }
    });

    if (Fragpipe.headless) {
      uiTextWorkdir.setText(Fragpipe.workdir);
    }

    uiCheckDryRun = UiUtils.createUiCheck("Dry Run", false);

    uiCheckDeleteCalibratedFiles = UiUtils.createUiCheck("Delete calibrated mzML", false);
    uiCheckDeleteCalibratedFiles.setName(TAB_PREFIX + "delete_calibrated_mzml");

    uiCheckDeleteTempFiles = UiUtils.createUiCheck("Delete temp files", false);
    uiCheckDeleteTempFiles.setName(TAB_PREFIX + "delete_temp_files");

    uiCheckWriteSubMzml = UiUtils.createUiCheck("Write sub mzML", false);
    FormEntry feWriteSubMzml = mu.feb(uiCheckWriteSubMzml).name(TAB_PREFIX + "write_sub_mzml").label("Write sub mzML").tooltip("Write unidentified scans to mzML files. Need to run MSFragger.").create();

    uiCheckExportMatchedFragments = UiUtils.createUiCheck("Export matched fragments", false);
    FormEntry feExportMatchedFragments = mu.feb(uiCheckExportMatchedFragments)
        .name(TAB_PREFIX + "export_matched_fragments")
        .label("Export matched fragments")
        .tooltip("Export each PSM's matched fragments to the psm.tsv file.")
        .create();

    uiSpinnerProbThreshold = UiUtils.spinnerDouble(0.5, 0.0, 1.0, 0.01).setCols(4).setFormat("#.##").create();
    FormEntry feProbThreshold = mu.feb(uiSpinnerProbThreshold).name(TAB_PREFIX + "sub_mzml_prob_threshold").label("Probability threshold").tooltip(
        "Used for generating the sub mzML files that contains unidentified scans.<br>"
            + "Scans with the probability larger than the threshold <b>and</b> passing the FDR filtering will be written to the sub mzML files.").create();

    ReportPanel reportPanel = Bus.getStickyEvent(ReportPanel.class);
    if (reportPanel == null) {
      throw new NullPointerException("Sticky note not on the bus: ReportPanel");
    }

    uiCheckWriteSubMzml.addItemListener(e -> {
      uiSpinnerProbThreshold.setEnabled(uiCheckWriteSubMzml.isSelected());
      reportPanel.setPrintDecoys(uiCheckWriteSubMzml.isSelected());
    });

    btnRun = UiUtils.createButton("<html><b>RUN", e -> Bus.post(new MessageRun(isDryRun())));

    btnStop = UiUtils.createButton("Stop", e -> {
      Bus.post(new MessageKillAll(REASON.USER_ACTION, console));
      Path existing = PathUtils.existing(getWorkdirText());
      if (existing != null) {
        Bus.post(MessageSaveLog.saveInDir(existing, console));
        FragpipeRun.saveRuntimeConfig(existing);
      }
    });
    btnStop.setEnabled(false);

    JButton btnSaveJob = UiUtils.createButton("Save as Job", this::actionBtnSaveJob);
    btnSaveJob.setToolTipText("Save the current configuration as a Job for future batch processing.\n" +
        "The job can then be loaded on the Batch tab for batch processing.\n" +
        "The job is saved in the jobs directory.");

    btnOpenPdv = UiUtils.createButton("Open FragPipe-PDV viewer", e -> {
      String[] t = {ToolingUtils.BATMASS_IO_JAR};
      List<Path> pdvPath = FragpipeLocations.checkToolsMissing(Seq.of(PDV_NAME).concat(t));
      if (pdvPath == null || pdvPath.isEmpty()) {
        SwingUtils.showErrorDialog(this, "Cannot find the visualization executable executable file.", "No visualization executable");
      } else {
        final TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
        int nThreads = tabWorkflow.getThreads();

        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        cmd.add("-cp");
        cmd.add(constructClasspathString(pdvPath));
        cmd.add("GUI.GUIMainClass");
        cmd.add(uiTextWorkdir.getNonGhostText());
        cmd.add(nThreads + "");
        cmd.add("v");
        log.debug("Executing: " + String.join(" ", cmd));
        pdvThread = new Thread(() -> {
          try {
            btnOpenPdv.setEnabled(false);
            ProcessBuilder pb = new ProcessBuilder(cmd);
            ProcessBuilderInfo pbi = new PbiBuilder().setPb(pb).setName(pb.toString()).setFnStdOut(null).setFnStdErr(null).setParallelGroup(null).create();
            ProcessResult pr = new ProcessResult(pbi);
            pdvProcess = pr.start();
            if (pdvProcess.waitFor() == 0) {
              log.debug("Process output: {}", pr.getOutput().toString());
              final int exitValue = pr.getProcess().exitValue();
              if (exitValue != 0) {
                String errStr = pr.appendErr(pr.pollStdErr());
                log.debug("Process " + pb + " returned non zero value. Message:\n" + (errStr == null ? "" : errStr));
              }
            } else {
              String errStr = pr.appendErr(pr.pollStdErr());
              log.debug("Process " + pb + " returned non zero value. Message:\n " + (errStr == null ? "" : errStr));
            }

            String outStr = pr.appendOut(pr.pollStdOut());
            log.debug("Process output: {}", (outStr == null ? "" : outStr));
            pr.close();
          } catch (Exception ex) {
            ex.printStackTrace();
            log.error(ExceptionUtils.getStackTrace(ex));
            if (pdvProcess != null) {
              pdvProcess.destroyForcibly();
              btnOpenPdv.setEnabled(true);
            }
          } finally {
            btnOpenPdv.setEnabled(true);
          }
        });
        pdvThread.start();
      }
    });

    btnOpenFragPipeAnalyst = UiUtils.createButton("Open FragPipe-Analyst", e -> {
      SwingUtils.openBrowserOrThrow(FRAGPIPE_ANALYST_URL);
    });

    btnGenerateSummaryReport = UiUtils.createButton("Generate Summary Report", e -> {
      String binPython = Fragpipe.getBinPython();
      if (binPython == null || binPython.isEmpty()) {
        SwingUtils.showErrorDialog(this, "Cannot find the python executable file.", "No python executable");
        return;
      }

      List<Path> generateReportPath = FragpipeLocations.checkToolsMissing(Seq.of(GENERATE_REPORTS_NAME));
      if (generateReportPath == null || generateReportPath.isEmpty()) {
        SwingUtils.showErrorDialog(this, "Cannot find the " + GENERATE_REPORTS_NAME + " file.", "No " + GENERATE_REPORTS_NAME + " file");
      } else {
        List<String> cmd = new ArrayList<>();
        cmd.add(binPython);
        cmd.add(constructClasspathString(generateReportPath));
        cmd.add("-r");
        cmd.add(uiTextWorkdir.getNonGhostText());
        MessagePrintToConsole.toConsole(Fragpipe.COLOR_TOOL, "Generating summary report", true, console);
        MessagePrintToConsole.toConsole(Fragpipe.COLOR_CMDLINE, "Executing: " + String.join(" ", cmd), true, console);
        MessagePrintToConsole.toConsole("Please wait...", console);
        generateReportThread = new Thread(() -> {
          try {
            btnGenerateSummaryReport.setEnabled(false);
            ProcessBuilder pb = new ProcessBuilder(cmd);
            ProcessBuilderInfo pbi = new PbiBuilder().setPb(pb).setName(pb.toString()).setFnStdOut(null).setFnStdErr(null).setParallelGroup(null).create();
            ProcessResult pr = new ProcessResult(pbi);
            generateReportProcess = pr.start();

            while (generateReportProcess.isAlive()) {
              byte[] pollOut = pr.pollStdOut();
              if (pollOut != null && pollOut.length > 0) {
                String outStr = pr.appendOut(pollOut);
                MessagePrintToConsole.toConsole(outStr, console);
              }
              Thread.sleep(100);
            }
            
            int exitCode = generateReportProcess.exitValue();
            if (exitCode == 0) {
              final int exitValue = pr.getProcess().exitValue();
              if (exitValue != 0) {
                String errStr = pr.appendErr(pr.pollStdErr());
                SwingUtils.showErrorDialog(this, "Process " + pb + " returned non zero value. Message:\n" + (errStr == null ? "" : errStr), "Error");
                MessagePrintToConsole.toConsole("Process " + pb + " returned non zero value. Message:\n" + (errStr == null ? "" : errStr), console);
              }
              MessagePrintToConsole.toConsole("Summary report exited with code " + exitCode, console);
            } else {
              String errStr = pr.appendErr(pr.pollStdErr());
              SwingUtils.showErrorDialog(this, "Process " + pb + " returned non zero value. Message:\n " + (errStr == null ? "" : errStr), "Error");
              MessagePrintToConsole.toConsole("Summary report exited with code " + exitCode, console);
            }
                        
            pr.close();
          } catch (Exception ex) {
            SwingUtils.showErrorDialogWithStacktrace(ex, this);
            if (generateReportProcess != null) {
              generateReportProcess.destroyForcibly();
              btnGenerateSummaryReport.setEnabled(true);
            }
          } finally {
            btnGenerateSummaryReport.setEnabled(true);
          }
        });
        generateReportThread.start();
      }
    });

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
        .createUiCheck("Word wrap", true, e -> {
          console.setScrollableTracksViewportWidth(uiCheckWordWrap.isSelected());
          console.setVisible(false);
          console.setVisible(true);
        });

    console.setScrollableTracksViewportWidth(true);

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/SeaGullMass.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    JLabel imageLabel2 = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/org/nesvilab/fragpipe/icons/fragpipe-analyst.jpg")));
      imageLabel2 = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    uiCheckSaveSDRF = new UiCheck("Save SDRF template", null,true);
    uiCheckSaveSDRF.setName("workflow.misc.save-sdrf");
    uiCheckSaveSDRF.setToolTipText("Save a template SDRF file with technical columns (search parameters) for this FragPipe run. \n" +
        "NOTE: this is not a complete SDRF file, information about the samples needs to be added to complete it.");

    List<String> sdrfTypes =  Arrays.stream(SDRFtable.SDRFtypes.values()).map(Enum::name).collect(Collectors.toList());
    uiComboSDRFtype = UiUtils.createUiCombo(sdrfTypes);
    FormEntry feComboSDRFtype = new FormEntry("workflow.misc.sdrf-type", "SDRF Type", uiComboSDRFtype, "SDRF template type to use");

    uiTextJobName = UiUtils.uiTextBuilder().ghost("Enter a name for saving the job (optional).").cols(50).create();

    JPanel p = mu.newPanel(null, true);
    mu.add(p, feWorkdir.label(), false).split().spanX();
    mu.add(p, feWorkdir.comp).growX();
    mu.add(p, btnBrowse);
    mu.add(p, btnOpenInFileManager).wrap();

    // line 1
    mu.add(p, btnRun).split(5);
    mu.add(p, btnStop);
    mu.add(p, uiCheckDryRun);
    mu.add(p, btnSaveJob);
    mu.add(p, uiTextJobName).wrap();

    // line 2
    mu.add(p, imageLabel).split(5);
    mu.add(p, btnOpenPdv);
    mu.add(p, imageLabel2);
    mu.add(p, btnOpenFragPipeAnalyst);
    mu.add(p, btnGenerateSummaryReport).push();

    mu.add(p, btnReportErrors).split(5);
    mu.add(p, btnClearConsole);
    mu.add(p, uiCheckSaveSDRF);
    mu.add(p, feComboSDRFtype.label(), mu.ccR());
    mu.add(p, feComboSDRFtype.comp).wrap();

    // line 3
    mu.add(p, uiCheckWordWrap, mu.ccR()).split().spanX();
    mu.add(p, feExportMatchedFragments.comp, mu.ccR());
    mu.add(p, uiCheckDeleteCalibratedFiles, mu.ccR());
    mu.add(p, uiCheckDeleteTempFiles, mu.ccR());
    mu.add(p, feWriteSubMzml.comp, mu.ccR());
    mu.add(p, feProbThreshold.label(), mu.ccR());
    mu.add(p, feProbThreshold.comp, mu.ccR()).wrap();

    return p;
  }

  public boolean isDryRun() {
    return SwingUtils.isEnabledAndChecked(uiCheckDryRun);
  }

  public boolean isDeleteCalibratedFiles() {
    return SwingUtils.isEnabledAndChecked(uiCheckDeleteCalibratedFiles);
  }

  public boolean isDeleteTempFiles() {
    return SwingUtils.isEnabledAndChecked(uiCheckDeleteTempFiles);
  }

  public boolean isWriteSubMzml() {
    return SwingUtils.isEnabledAndChecked(uiCheckWriteSubMzml);
  }

  public boolean isExportMatchedFragments() {
    return SwingUtils.isEnabledAndChecked(uiCheckExportMatchedFragments);
  }

  public float getSubMzmlProbThreshold() {
    return ((Double) uiSpinnerProbThreshold.getValue()).floatValue();
  }
  public SDRFtable.SDRFtypes getSDRFtype() {
    return SDRFtable.SDRFtypes.valueOf(uiComboSDRFtype.getSelectedItem().toString());
  }

  public boolean isSaveSDRF() {
    return uiCheckSaveSDRF.isSelected();
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
        menu.show(e.getComponent(), e.getX(), e.getY());
      }
    });
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void on(MessageSaveLog m) {
    log.debug("Got MessageSaveLog, trying to save log");
    saveLogToFile(m.console, m.workDir);
  }

  private void actionBtnSaveJob(ActionEvent e) {
    try {
      saveJob(uiTextJobName.getNonGhostText());
      uiTextJobName.setText("");
    } catch (IOException ex) {
      log.error("IO Error while saving job", ex);
      JOptionPane.showMessageDialog(this, "IO Error while saving job: " + ex.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
    }
  }

  public void saveJob(String jobName) throws IOException {
    final TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
    final TabConfig tabConfig = Fragpipe.getStickyStrict(TabConfig.class);
    final TabDatabase tabDatabase = Fragpipe.getStickyStrict(TabDatabase.class);
    SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    Date now = new Date();

    if (jobName == null || jobName.isEmpty()) {
      jobName = "job_" + df.format(now);
    }

    // save current workflow and manifest to file
    Path workflowPath = FragpipeLocations.get().getDirJobs().resolve(String.format("%s.workflow", jobName));
    Fragpipe fp0 = getStickyStrict(Fragpipe.class);
    Properties uiProps = FragpipeCacheUtils.tabsSave0(fp0.tabs, false);
    TabWorkflow.saveWorkflow(workflowPath, String.format("Saved automatically during creation of %s", jobName), uiProps);

    Path manifestPath = FragpipeLocations.get().getDirJobs().resolve(String.format("%s.fp-manifest", jobName));
    tabWorkflow.manifestSave(manifestPath);

    String fastaStr = tabDatabase.getFastaPath();

    // confirm output directory and tools path are not empty
    if (getWorkdirText().isEmpty()) {
      JOptionPane.showMessageDialog(this, "Output directory " + getWorkdirText() + " is empty. Cannot save job.", TAB_PREFIX + " error", JOptionPane.ERROR_MESSAGE);
      return;
    }
    if (tabConfig.uiTextToolsFolder.getNonGhostText().isEmpty()) {
      JOptionPane.showMessageDialog(this, "Tools folder on the config tab is not configured! Cannot save job.", TAB_PREFIX + " error", JOptionPane.ERROR_MESSAGE);
      return;
    }
    // create the job
    BatchRun job = new BatchRun(jobName, workflowPath.toString(), manifestPath.toString(), getWorkdirText(), tabConfig.uiTextToolsFolder.getNonGhostText(), fastaStr, tabWorkflow.getRamGb(), tabWorkflow.getThreads());

    // save job to file
    Path savePath = FragpipeLocations.get().getDirJobs().resolve((String.format("%s.job", jobName)));
    ArrayList<BatchRun> jobs = new ArrayList<>();
    jobs.add(job);
    TabBatch.saveJobsToFile(jobs, savePath);

    // add saved job to batch table automatically
    final TabBatch tabBatch = Fragpipe.getStickyStrict(TabBatch.class);
    tabBatch.addBatchRuns(jobs);

    SwingUtils.showInfoDialog(this, String.format("Saved job to %s and loaded it to the Batch table", savePath), "Job saved");
  }

  private static void saveLogToFileCreateNew(final String text, final Path path) throws FileAlreadyExistsException {
    try (BufferedWriter bufferedWriter = Files.newBufferedWriter(path, StandardOpenOption.CREATE_NEW)) {
      bufferedWriter.write(text);
    } catch (FileAlreadyExistsException e) {
      throw e;
    } catch (IOException e) {
      log.error("Error writing log to file", e);
      e.printStackTrace();
    }
  }

  public static void saveLogToFile(final TextConsole console, final Path path) {
    final String text = console.getText().replaceAll("[^\n]+\u200B" + System.lineSeparator(), "");
    Path pathNew = path;
    for (int i = 1; ; ++i) {
      try {
        saveLogToFileCreateNew(text, pathNew);
      } catch (FileAlreadyExistsException e) {
        pathNew = Paths.get(path.toString() + "_" + i);
        continue;
      }
      break;
    }
  }
}
