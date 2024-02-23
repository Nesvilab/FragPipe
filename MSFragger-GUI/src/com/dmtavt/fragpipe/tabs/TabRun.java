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

package com.dmtavt.fragpipe.tabs;

import static com.dmtavt.fragpipe.cmd.CmdBase.constructClasspathString;
import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;
import static com.dmtavt.fragpipe.tabs.TabWorkflow.workflowExt;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.FragpipeRun;
import com.dmtavt.fragpipe.Version;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.PropsFile;
import com.dmtavt.fragpipe.cmd.PbiBuilder;
import com.dmtavt.fragpipe.cmd.ProcessBuilderInfo;
import com.dmtavt.fragpipe.cmd.ToolingUtils;
import com.dmtavt.fragpipe.messages.MessageClearConsole;
import com.dmtavt.fragpipe.messages.MessageExportLog;
import com.dmtavt.fragpipe.messages.MessageKillAll;
import com.dmtavt.fragpipe.messages.MessageKillAll.REASON;
import com.dmtavt.fragpipe.messages.MessagePrintToConsole;
import com.dmtavt.fragpipe.messages.MessageRun;
import com.dmtavt.fragpipe.messages.MessageRunButtonEnabled;
import com.dmtavt.fragpipe.messages.MessageSaveLog;
import com.dmtavt.fragpipe.messages.MessageShowAboutDialog;
import com.dmtavt.fragpipe.messages.NoteConfigSkyline;
import com.dmtavt.fragpipe.process.ProcessResult;
import com.dmtavt.fragpipe.tools.philosopher.ReportPanel;
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
import com.github.chhh.utils.swing.UiSpinnerDouble;
import com.github.chhh.utils.swing.UiText;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.UIManager;
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
  private static final String PROP_FILECHOOSER_LAST_PATH = TAB_PREFIX + "filechooser.last-path";
  private static final String PDV_NAME = "/FP-PDV/FP-PDV-1.1.8.jar";
  private static final String FRAGPIPE_ANALYST_URL = Fragpipe.propsFix().getProperty("fragpipe-analyst-url", "http://fragpipe-analyst.nesvilab.org/");
  private static final Pattern pattern = Pattern.compile("Converged to [\\d.]+ % FDR with \\d+ Ions\\s+decoy=\\d+ threshold=([\\d.]+) total=\\d+");

  public final TextConsole console;
  Color defTextColor;
  private UiText uiTextWorkdir;
  private UiCheck uiCheckDryRun;
  private UiCheck uiCheckDeleteCalibratedFiles;
  private UiCheck uiCheckDeleteTempFiles;
  public UiCheck uiCheckWriteSubMzml;
  public UiSpinnerDouble uiSpinnerProbThreshold;
  private JButton btnRun;
  private JButton btnStop;
  private JButton btnOpenPdv;
  private JButton btnOpenFragPipeAnalyst;
  public JButton btnOpenSkyline;
  private Thread pdvThread = null;
  private Thread skylineThread = null;
  private JPanel pTop;
  private JPanel pConsole;
  private UiCheck uiCheckWordWrap;
  private Process pdvProcess = null;
  private Process skylineProcess = null;
  private TabDownstream tabDownstream;
  private UiCheck uiCheckSaveSDRF;

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
    JButton btnAbout = UiUtils.createButton("About", e -> Bus.post(new MessageShowAboutDialog()));
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

    uiSpinnerProbThreshold = UiUtils.spinnerDouble(0.5, 0.0, 1.0, 0.01).setCols(4).setFormat("#.##").create();
    FormEntry feProbThreshold = mu.feb(uiSpinnerProbThreshold).name(TAB_PREFIX + "sub_mzml_prob_threshold").label("Probability threshold").tooltip(
        "Used for generating the sub mzML files that contains unidentified scans.<br>"
            + "Scans with the probability larger than the threshold <b>and</b> passing the FDR filtering will be written to the sub mzML files.").create();

    ReportPanel reportPanel = Bus.getStickyEvent(ReportPanel.class);
    if (reportPanel == null) {
      throw new NullPointerException("Sticky note not on the bus: ReportPanel");
    }

    uiCheckWriteSubMzml.addActionListener(e -> {
      uiSpinnerProbThreshold.setEnabled(uiCheckWriteSubMzml.isSelected());
      reportPanel.setPrintDecoys(uiCheckWriteSubMzml.isSelected());
    });

    btnRun = UiUtils.createButton("<html><b>RUN", e -> Bus.post(new MessageRun(isDryRun())));

    btnStop = UiUtils.createButton("Stop", e -> {
      Bus.post(new MessageKillAll(REASON.USER_ACTION, console));
      Path existing = PathUtils.existing(getWorkdirText());
      if (existing != null) {
        Bus.post(MessageSaveLog.saveInDir(existing));
        FragpipeRun.saveRuntimeConfig(existing);
      }
    });

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

    btnOpenSkyline = UiUtils.createButton("Generate Skyline files", e -> {
      NoteConfigSkyline noteConfigSkyline = Bus.getStickyEvent(NoteConfigSkyline.class);
      String skylinePath = noteConfigSkyline.path;

      if (skylinePath == null || skylinePath.isEmpty()) {
        SwingUtils.showErrorDialog(this, "Cannot find SkylineCmd.exe.", "No SkylineCmd.exe");
      } else {
        Path wd = Paths.get(uiTextWorkdir.getNonGhostText());
        Path workflowPath = wd.resolve("fragpipe" + workflowExt);
        PropsFile pf = new PropsFile(workflowPath, "for Skyline");

        List<Path> speclibFiles = new ArrayList<>(1);
        TreeSet<String> lcmsFiles = new TreeSet<>();
        TreeSet<Path> pepxmlFiles = new TreeSet<>();
        float probThreshold = 0.8f;
        List<String> cmd = new ArrayList<>();

        try {
          pf.load();

          String dataType = "None";
          String line;
          BufferedReader reader = Files.newBufferedReader(wd.resolve("fragpipe-files.fp-manifest"));
          while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) {
              continue;
            }
            String[] parts = line.split("\t");
            lcmsFiles.add(parts[0]);
            if (parts[3].contains("DIA")) {
              dataType = "DIA";
            }
          }
          reader.close();

          List<Path> logFiles = Files.walk(wd).filter(p -> p.getFileName().toString().startsWith("log_") && p.getFileName().toString().endsWith(".txt")).sorted().collect(Collectors.toList());
          reader = Files.newBufferedReader(logFiles.get(logFiles.size() - 1));
          while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) {
              continue;
            }
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
              probThreshold = Float.parseFloat(matcher.group(1));
            }
          }
          reader.close();

          pepxmlFiles = Files.walk(wd).filter(p -> p.getFileName().toString().startsWith("interact-") && p.getFileName().toString().endsWith(".pep.xml")).collect(Collectors.toCollection(TreeSet::new));
          speclibFiles = Files.walk(wd).filter(p -> p.getFileName().toString().endsWith(".speclib")).collect(Collectors.toList());

          Path pp = wd.resolve("filelist_skyline.txt");

          BufferedWriter writer = Files.newBufferedWriter(pp);
          writer.write("--overwrite ");
          writer.write("--new=fragpipe.sky ");
          writer.write("--import-search-add-mods ");
          writer.write("--full-scan-acquisition-method=" + dataType + " ");

          if (speclibFiles.isEmpty()) {
            writer.write("--import-search-cutoff-score=" + probThreshold + " ");
            for (Path p : pepxmlFiles) {
              writer.write("--import-search-file=" + p.toAbsolutePath() + " ");
            }
          } else {
            writer.write("--import-search-cutoff-score=0.01 ");
            writer.write("--import-search-file=" + speclibFiles.get(0).toAbsolutePath() + " ");
          }

          if (noteConfigSkyline.compareVersion("23.1.0.380") > 0) {
            // parameters added after released 23.1 version
            writer.write("--pep-max-missed-cleavages=" + pf.getProperty("msfragger.allowed_missed_cleavage_1") + " ");
            writer.write("--pep-min-length=" + pf.getProperty("msfragger.digest_min_length") + " ");
            writer.write("--pep-max-length=" + pf.getProperty("msfragger.digest_max_length") + " ");
            writer.write("--pep-exclude-nterminal-aas=0 ");
          }
          writer.write("--tran-precursor-ion-charges=\"2,3,4,5,6\" ");
          writer.write("--tran-product-ion-charges=\"1,2\" ");
          writer.write("--tran-product-ion-types=\"y,b,p\" ");
          writer.write("--tran-product-start-ion=\"ion 3\" ");
          writer.write("--tran-product-end-ion=\"last ion\" ");
          writer.write("--tran-product-clear-special-ions ");
          if (pf.getProperty("msfragger.fragment_mass_units").contentEquals("1")) {
            writer.write("--library-match-tolerance=" + pf.getProperty("msfragger.fragment_mass_tolerance") + "ppm ");
          } else {
            writer.write("--library-match-tolerance=" + pf.getProperty("msfragger.fragment_mass_tolerance") + "mz ");
          }
          writer.write("--library-product-ions=12 ");
          writer.write("--library-min-product-ions=" + pf.getProperty("msfragger.min_matched_fragments") + " ");
          writer.write("--library-pick-product-ions=filter ");
          writer.write("--full-scan-precursor-analyzer=centroided ");
          writer.write("--full-scan-precursor-isotopes=Count ");
          writer.write("--full-scan-precursor-threshold=3 ");
          writer.write("--full-scan-product-analyzer=centroided ");

          if (pf.getProperty("msfragger.precursor_true_units").contentEquals("1")) {
            writer.write("--full-scan-precursor-res=" + pf.getProperty("msfragger.precursor_true_tolerance") + " ");
          } else {
            writer.write("--full-scan-precursor-res=" + (Float.parseFloat(pf.getProperty("msfragger.precursor_true_tolerance")) * 1000) + " ");
          }

          writer.write("--full-scan-rt-filter=ms2_ids ");
          writer.write("--full-scan-rt-filter-tolerance=2 ");
          writer.write("--instrument-min-mz=50 ");
          writer.write("--instrument-max-mz=2000 ");
          writer.write("--full-scan-precursor-isotopes=Count ");
          writer.write("--full-scan-isolation-scheme=\"Results only\" ");
          for (String s : lcmsFiles) {
            writer.write("--import-file=" + s + " ");
          }
          writer.write("--import-search-exclude-library-sources ");
          writer.write("--import-fasta=" + wd.resolve("protein.fas") + " ");
          writer.write("--associate-proteins-shared-peptides=DuplicatedBetweenProteins ");

          writer.close();

          cmd.add(skylinePath);
          cmd.add("--timestamp");
          cmd.add("--dir=" + wd.toAbsolutePath());
          cmd.add("--batch-commands=" + pp.toAbsolutePath());

          toConsole("Executing: " + String.join(" ", cmd), console);
        } catch (Exception ex) {
          toConsole(Color.red, ExceptionUtils.getStackTrace(ex), true, console);
          btnOpenSkyline.setEnabled(true);
          return;
        }

        skylineThread = new Thread(() -> {
          try {
            btnOpenSkyline.setEnabled(false);
            ProcessBuilder pb = new ProcessBuilder(cmd);
            ProcessBuilderInfo pbi = new PbiBuilder().setPb(pb).setName(pb.toString()).setFnStdOut(null).setFnStdErr(null).setParallelGroup(null).create();
            ProcessResult pr = new ProcessResult(pbi);
            skylineProcess = pr.start();

            redirectOutputToConsole(skylineProcess.getInputStream(), console);

            int exitValue = skylineProcess.waitFor();
            if (exitValue != 0) {
              String errStr = pr.appendErr(pr.pollStdErr());
              toConsole(Color.red, "Process " + pb + " returned non zero value. Message:\n" + (errStr == null ? "" : errStr), true, console);
            } else {
              toConsole("DONE! The Skyline files locate in " + wd.toAbsolutePath(), console);
            }
          } catch (Exception ex) {
            toConsole(Color.red, ExceptionUtils.getStackTrace(ex), true, console);
            if (skylineProcess != null) {
              skylineProcess.destroyForcibly();
            }
            btnOpenSkyline.setEnabled(true);
          } finally {
            btnOpenSkyline.setEnabled(true);
          }
        });
        skylineThread.start();
      }
    });

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
        .createUiCheck("Word wrap", true, e -> {
          console.setScrollableTracksViewportWidth(uiCheckWordWrap.isSelected());
          console.setVisible(false);
          console.setVisible(true);
        });

    console.setScrollableTracksViewportWidth(true);

    JLabel imageLabel = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/com/dmtavt/fragpipe/icons/SeaGullMass.png")));
      imageLabel = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    JLabel imageLabel2 = new JLabel();
    try {
      BufferedImage image = ImageIO.read(Objects.requireNonNull(getClass().getResource("/com/dmtavt/fragpipe/icons/fragpipe-analyst.jpg")));
      imageLabel2 = new JLabel(new ImageIcon(image));
    } catch (Exception ex) {
      ex.printStackTrace();
    }

    uiCheckSaveSDRF = new UiCheck("Save Technical SDRF", null,true);
    uiCheckSaveSDRF.setName("workflow.misc.save-sdrf");
    uiCheckSaveSDRF.setToolTipText("Save a template SDRF file with technical columns (search parameters) for this FragPipe run. \n" +
        "NOTE: this is not a complete SDRF file, information about the samples needs to be added to complete it.");
    JLabel emptySpacer = new JLabel("              ");

    JPanel p = mu.newPanel(null, true);
    mu.add(p, btnAbout).wrap();
    mu.add(p, feWorkdir.label(), false).split().spanX();
    mu.add(p, feWorkdir.comp).growX();
    mu.add(p, btnBrowse);
    mu.add(p, btnOpenInFileManager).wrap();

    mu.add(p, btnRun).split(5);
    mu.add(p, btnStop);
    mu.add(p, uiCheckDryRun);
    mu.add(p, emptySpacer).split();
    mu.add(p, uiCheckSaveSDRF);

    mu.add(p, btnExport, false).split();
    mu.add(p, btnReportErrors);
    mu.add(p, btnClearConsole);
    mu.add(p, uiCheckWordWrap).wrap();

    mu.add(p, imageLabel).split(5);
    mu.add(p, btnOpenPdv).pushX();

    mu.add(p, imageLabel2).gapLeft("30px");
    mu.add(p, btnOpenFragPipeAnalyst);

    mu.add(p, btnOpenSkyline);

    mu.add(p, uiCheckDeleteCalibratedFiles, false).split();
    mu.add(p, uiCheckDeleteTempFiles).gapRight("20px");
    mu.add(p, feWriteSubMzml.comp, false).gapRight("20px");
    mu.add(p, feProbThreshold.label(), false);
    mu.add(p, feProbThreshold.comp, false).wrap();

    return p;
  }

  private void redirectOutputToConsole(InputStream inputStream, TextConsole console) {
    new Thread(() -> {
      try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
        String line;
        while ((line = reader.readLine()) != null) {
          toConsole(line, console);
        }
      } catch (IOException e) {
        toConsole(Color.red, ExceptionUtils.getStackTrace(e), true, console);
      }
    }).start();
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

  public float getSubMzmlProbThreshold() {
    return ((Double) uiSpinnerProbThreshold.getValue()).floatValue();
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
        int overwrite = JOptionPane.showConfirmDialog(parent, "<html>File exists, overwrtie?<br/><br/>" + path, "Overwrite", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION == overwrite) {
          try {
            Files.delete(path);
          } catch (IOException ex) {
            JOptionPane.showMessageDialog(parent, "Could not overwrite", "Overwrite", JOptionPane.ERROR_MESSAGE);
            return;
          }
        }
      }
      saveLogToFile(console, path);
    }
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
