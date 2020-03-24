package umich.msfragger.gui;

import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.StringUtils;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import org.apache.commons.codec.Charsets;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.Version;
import umich.msfragger.cmd.CmdCrystalc;
import umich.msfragger.cmd.CmdIonquant;
import umich.msfragger.cmd.CmdIprophet;
import umich.msfragger.cmd.CmdLabelquant;
import umich.msfragger.cmd.CmdMsAdjuster;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.cmd.CmdPeptideProphet;
import umich.msfragger.cmd.CmdPhilosopherWorkspaceClean;
import umich.msfragger.cmd.CmdPhilosopherWorkspaceCleanInit;
import umich.msfragger.cmd.CmdProteinProphet;
import umich.msfragger.cmd.CmdPtmshepherd;
import umich.msfragger.cmd.CmdPhilosopherAbacus;
import umich.msfragger.cmd.CmdPhilosopherDbAnnotate;
import umich.msfragger.cmd.CmdPhilosopherFilter;
import umich.msfragger.cmd.CmdFreequant;
import umich.msfragger.cmd.CmdPhilosopherReport;
import umich.msfragger.cmd.CmdSpecLibGen;
import umich.msfragger.cmd.CmdTmtIntegrator;
import umich.msfragger.cmd.CmdUmpireSe;
import umich.msfragger.cmd.PbiBuilder;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.cmd.ProcessBuildersDescriptor;
import umich.msfragger.gui.ProcessDescription.Builder;
import com.dmtavt.fragpipe.messages.MessageAppendToConsole;
import com.dmtavt.fragpipe.messages.MessageLastRunWorkDir;
import com.dmtavt.fragpipe.messages.MessageRun;
import com.dmtavt.fragpipe.messages.MessageSaveAllForms;
import com.dmtavt.fragpipe.messages.MessageSaveLog;
import com.dmtavt.fragpipe.messages.MessageStartProcesses;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.params.fragger.FraggerMigPanel;
import umich.msfragger.params.tmtintegrator.QuantLabel;
import umich.msfragger.params.tmtintegrator.TmtiPanel;
import com.github.chhh.utils.FastaUtils;
import com.github.chhh.utils.FastaUtils.FastaContent;
import com.github.chhh.utils.LogUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.UsageTrigger;
import com.github.chhh.utils.swing.TextConsole;

public class FragpipeOnMessages {
  private static final Logger log = LoggerFactory.getLogger(FragpipeOnMessages.class);

  private FragpipeOnMessages() {}


  public static void onMessageRun(MsfraggerGuiFrame msfgf, MessageRun m) {
    EventBus.getDefault().post(MessageSaveAllForms.forCaching());

    final boolean isDryRun = m.isDryRun;
    MsfraggerGuiFrameUtils.saveWorkdirText(msfgf.getTxtWorkingDir());
    msfgf.clearConsole();

    msfgf.resetRunButtons(false);
    final boolean doRunFragger = msfgf.fraggerMigPanel.isRun();
    boolean doRunProphetsAndReport = SwingUtils.isEnabledAndChecked(msfgf.getChkRunPeptideProphet())
        || SwingUtils.isEnabledAndChecked(msfgf.getChkRunProteinProphet())
        || msfgf.getPanelReportOptions().isGenerateReport();

    // check for TSV output when any other downstream tools are requested
    if (doRunFragger && doRunProphetsAndReport) {
      if (msfgf.fraggerMigPanel.getOutputType().equals(FraggerOutputType.TSV)) {
        int confirm = JOptionPane.showConfirmDialog(msfgf,
            "You've chosen TSV output for MSFragger while\n"
                + "also requesting to run other downstream processing\n"
                + "tools. Those tools only support PepXML input.\n\n"
                + "Cancel operation and switch before running (manually)?",
            "Switch to pep.xml?", JOptionPane.YES_NO_OPTION);
        if (JOptionPane.YES_OPTION == confirm) {
          msfgf.resetRunButtons(true);
          return;
        }
      }
    }

    final TextConsole textConsole = msfgf.console;
    final String workingDir = msfgf.getTxtWorkingDir().getText();
    if (workingDir.isEmpty()) {
      JOptionPane.showMessageDialog(msfgf, "Output directory can't be left empty.\n"
              + "Please select an existing directory for the output.", "Bad output directory path",
          JOptionPane.WARNING_MESSAGE);
      msfgf.resetRunButtons(true);
      return;
    }
    Path testWdPath;
    try {
      testWdPath = Paths.get(workingDir);
    } catch (InvalidPathException e) {
      JOptionPane.showMessageDialog(msfgf, "Output directory path is not a valid path.\n"
          + "Please select a directory for the output.", "Bad output directory path", JOptionPane.WARNING_MESSAGE);
      msfgf.resetRunButtons(true);
      return;
    }
    Pattern reWhitespace = Pattern.compile("\\s");
    if (reWhitespace.matcher(testWdPath.toString()).find()) {
      JOptionPane.showMessageDialog(msfgf,
          "Output directory path contains whitespace characters.\n"
              + "Some programs in the pipeline might not work properly in this case.\n\n"
          + "Please change output directory to one without spaces.", "Bad output directory path", JOptionPane.WARNING_MESSAGE);
      msfgf.resetRunButtons(true);
      return;
    }
    final Path wdPath = testWdPath;
    EventBus.getDefault().postSticky(new MessageLastRunWorkDir(wdPath));

    if (!isDryRun) {
      if (!Files.exists(wdPath)) {
        int confirmCreation = JOptionPane.showConfirmDialog(msfgf,
            "Output directory doesn't exist. Create?",
            "Create output directory?", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION != confirmCreation) {
          msfgf.resetRunButtons(true);
          return;
        }
        try {
          Files.createDirectories(wdPath);
        } catch (Exception e) {
          // something went not right during creation of directory structure
          JOptionPane.showMessageDialog(msfgf,
              "Could not create directory structure.\n" + e.getMessage(), "Error",
              JOptionPane.ERROR_MESSAGE);
          msfgf.resetRunButtons(true);
          return;
        }

      } else {
        try (Stream<Path> inWd = Files.list(wdPath)) {
          if (inWd.findAny().isPresent()) {
            int confirm = JOptionPane.showConfirmDialog(msfgf,
                "The output directory is not empty.\n\n"
                    + "Some files might be overwritten in:\n"
                    + " > " + wdPath.toString() + "\n\n"
                    + "Do you want to proceed?", "Confirmation", JOptionPane.YES_NO_OPTION);
            if (JOptionPane.YES_OPTION != confirm) {
              msfgf.resetRunButtons(true);
              return;
            }
          }
        } catch (Exception e) {
          JOptionPane.showMessageDialog(msfgf,
              "Could not create directory structure.\n" + e.getMessage(), "Error",
              JOptionPane.ERROR_MESSAGE);
          msfgf.resetRunButtons(true);
          return;
        }
      }

      // make sure subdirs for experiments are created
      Set<Path> subdirs = msfgf.getLcmsFileGroups().values().stream().map(g -> g.outputDir(wdPath))
          .collect(Collectors.toSet());
      for (Path subdir : subdirs) {
        if (!Files.exists(subdir)) {
          try {
            Files.createDirectories(subdir);
          } catch (IOException e) {
            JOptionPane.showMessageDialog(msfgf,
                "Could not create directory structure.\n" + e.getMessage(), "Error",
                JOptionPane.ERROR_MESSAGE);
            msfgf.resetRunButtons(true);
            return;
          }
        }
      }
    }

    List<InputLcmsFile> lcmsExpEmptyRepNonNull = msfgf.getLcmsFileGroups().values().stream()
        .flatMap(g -> g.lcmsFiles.stream())
        .filter(lcms -> StringUtils.isNullOrWhitespace(lcms.getExperiment())
            && lcms.getReplicate() != null)
        .collect(Collectors.toList());
    if (!lcmsExpEmptyRepNonNull.isEmpty()) {
      int confirm = SwingUtils.showConfirmDialog(msfgf, new JLabel(
          "<html>For " + lcmsExpEmptyRepNonNull.size()
              + " input files Experiment was left empty while Replicate was not.<br/><br/>\n"
              + "<b>Yes</b> - if you want to auto-add 'exp_' prefix and continue as-is.<br/>\n"
              + "<b>No, Cancel</b> - stop and change manually on Select LC/MS Files tab."));
      if (confirm != JOptionPane.YES_OPTION) {
        msfgf.resetRunButtons(true);
        return;
      }
    }

    final Map<String, LcmsFileGroup> lcmsFileGroups = msfgf.getLcmsFileGroups();
    final List<InputLcmsFile> lcmsFilesAll = lcmsFileGroups.values().stream()
        .flatMap(group -> group.lcmsFiles.stream()).collect(Collectors.toCollection(ArrayList::new));

    // check input LCMS files
    if (lcmsFilesAll.isEmpty()) {
      JOptionPane.showMessageDialog(msfgf, "No LC/MS data files selected.\n"
          + "Check 'Select Raw Files' tab.", "Error", JOptionPane.WARNING_MESSAGE);
      msfgf.resetRunButtons(true);
      return;
    } else {
      // check that all input LCMS files have unique filenames
      Map<String, List<Path>> inputFnMap = new HashMap<>();
      for (LcmsFileGroup group : lcmsFileGroups.values()) {
        for (InputLcmsFile inputLcmsFile : group.lcmsFiles) {
          Path path = inputLcmsFile.getPath();
          String fn = path.getFileName().toString();
          inputFnMap.computeIfAbsent(fn, s -> new LinkedList<>()).add(path);
        }
      }

      // check LCMS files only have one dot in the name

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

        JOptionPane.showMessageDialog(msfgf,
            sb.toString(),
            "Input files with same names", JOptionPane.WARNING_MESSAGE);
        msfgf.resetRunButtons(true);
        return;
      }
    }

    DateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    String dateString = df.format(new Date());

    URI jarFragpipeUri = null;
    Path jarFragpipePath = null;
    try {
      jarFragpipeUri = JarUtils.getCurrentJarUri();
      jarFragpipePath = Paths.get(jarFragpipeUri);
    } catch (Exception ignore) {
      // don't care
    }
    if (jarFragpipeUri == null) {
      JOptionPane.showMessageDialog(msfgf, "Could not get the URI of the currently running jar",
          "Errors", JOptionPane.ERROR_MESSAGE);
      msfgf.resetRunButtons(true);
      return;
    }

    // check fasta file
    String fastaPathText = msfgf.getTextSequenceDbPath().getText().trim();
    if (StringUtils.isNullOrWhitespace(fastaPathText)) {
      JOptionPane.showMessageDialog(msfgf, "Fasta file path (Database tab) can't be empty",
          "Warning", JOptionPane.WARNING_MESSAGE);
      msfgf.resetRunButtons(true);
      return;
    }
    final String fastaPath = PathUtils.testFilePath(fastaPathText, workingDir);
    if (fastaPath == null) {
      JOptionPane.showMessageDialog(msfgf,
          String.format("Could not find fasta file (Database) at:\n%s", fastaPathText),
          "Errors", JOptionPane.ERROR_MESSAGE);
      msfgf.resetRunButtons(true);
      return;
    }

    final String binPhilosopher = msfgf.getTextBinPhilosopher().getText().trim();
    final List<ProcessBuildersDescriptor> pbDescsToFill = new ArrayList<>();

    // main call to generate all the process builders
    if (!processBuildersNew(msfgf, wdPath, jarFragpipePath, binPhilosopher, isDryRun, pbDescsToFill)) {
      msfgf.resetRunButtons(true);
      return;
    }


    String sbSysinfo = OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo() + "\n";
    LogUtils.println(msfgf.console, String.format(Locale.ROOT, "System info:\n%s",
        sbSysinfo));

    StringBuilder sbVer = new StringBuilder();
    sbVer.append(Version.PROGRAM_TITLE).append(" version ").append(Version.version()).append("\n");
    sbVer.append("MSFragger version ").append(msfgf.fraggerVer).append("\n");
    sbVer.append("Philosopher version ").append(msfgf.philosopherVer).append("\n");
    LogUtils.println(msfgf.console, String.format(Locale.ROOT, "Version info:\n%s", sbVer.toString()));
    LogUtils.println(msfgf.console, "");

    LogUtils.println(msfgf.console, "LCMS files:");
    for (Entry<String, LcmsFileGroup> e : lcmsFileGroups.entrySet()) {
      LogUtils.println(msfgf.console,
          String.format(Locale.ROOT, "  Experiment/Group: %s", e.getValue().name));
      for (InputLcmsFile f : e.getValue().lcmsFiles) {
        LogUtils.println(msfgf.console, String.format(Locale.ROOT, "  - %s", f.getPath().toString()));
      }
    }
    LogUtils.println(msfgf.console, "");

    // Converting process builders descriptors to process builder infos
    final List<ProcessBuilderInfo> pbis = pbDescsToFill.stream()
        .flatMap(pbd -> pbd.pbis.stream().map(pbi ->
        {
          PbiBuilder b = new PbiBuilder();
          b.setPb(pbi.pb);
          b.setName(pbi.name != null ? pbi.name : pbd.name);
          b.setFnStdOut(pbi.fnStdout != null ? pbi.fnStdout : pbd.fnStdout);
          b.setFnStdErr(pbi.fnStderr != null ? pbi.fnStderr : pbd.fnStderr);
          b.setParallelGroup(pbi.parallelGroup != null ? pbi.parallelGroup : pbd.getParallelGroup());
          return b.create();
        }))
        .collect(Collectors.toList());

    LogUtils.println(msfgf.console, String.format(Locale.ROOT, "%d commands to execute:", pbis.size()));

    for (final ProcessBuilderInfo pbi : pbis) {
      MsfraggerGuiFrameUtils
          .printProcessDescription(msfgf.COLOR_CMDLINE, msfgf.COLOR_TOOL, msfgf.COLOR_WORKDIR,
              msfgf.console, pbi);

    }
    LogUtils.println(msfgf.console, "~~~~~~~~~~~~~~~~~~~~~~");
    LogUtils.println(msfgf.console, "");
    LogUtils.println(msfgf.console, "");

    if (isDryRun) {
      LogUtils.println(msfgf.console, "It's a dry-run, not running the commands.");
      msfgf.resetRunButtons(true);
      return;
    }

    // save all the options
    LocalDateTime time = LocalDateTime.now();
    String timestamp = time.format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss"));
    Path path = wdPath.resolve("fragpipe" + "_" + timestamp + ".config");
    try {
      Files.deleteIfExists(path);
    } catch (IOException e) {
      log.error("Could not delete old fragpipe.config at: {}", path.toString());
    }
    EventBus.getDefault().post(new MessageSaveAllForms(path));


    // print all the options
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try {
      msfgf.formWrite(baos);
      LogUtils.println(msfgf.console, "~~~~~~~~~ fragpipe.config ~~~~~~~~~");
      LogUtils.println(msfgf.console, baos.toString(Charsets.UTF_8.name()));
      LogUtils.println(msfgf.console, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
    } catch (IOException e) {
      log.error("Could not collect form text representation for printing to console");
    }

    // run everything
    List<RunnableDescription> toRun = new ArrayList<>();
    for (final ProcessBuilderInfo pbi : pbis) {
      Runnable runnable = ProcessBuilderInfo.toRunnable(pbi, wdPath, pbi1 -> MsfraggerGuiFrameUtils
          .printProcessDescription(msfgf.COLOR_CMDLINE, msfgf.COLOR_TOOL, msfgf.COLOR_WORKDIR,
              msfgf.console, pbi1));
      ProcessDescription.Builder b = new ProcessDescription.Builder().setName(pbi.name);
      if (pbi.pb.directory() != null) {
        b.setWorkDir(pbi.pb.directory().toString());
      }
      if (pbi.pb.command() != null && !pbi.pb.command().isEmpty()) {
        b.setCommand(String.join(" ", pbi.pb.command()));
      }
      toRun.add(new RunnableDescription(b.create(), runnable, pbi.parallelGroup));
    }

    // add finalizer process
    final JButton btnStartPtr = msfgf.getBtnRun();
    final JButton btnStopPtr = msfgf.getBtnStop();
    Runnable finalizerRun = () -> {
      btnStartPtr.setEnabled(true);
      btnStopPtr.setEnabled(false);
      String msg =
          "=========================\n" +
              "===\n" +
              "===      Done\n" +
              "===\n" +
              "=========================\n";
      EventBus.getDefault()
          .post(new MessageAppendToConsole(msg, MsfraggerGuiFrame.COLOR_RED_DARKEST));
      EventBus.getDefault().post(new MessageSaveLog(wdPath));
    };
    String finalizerDesc = "Finalizer task";
    toRun.add(new RunnableDescription(new Builder().setName("Finalizer Task").create(), finalizerRun));
    EventBus.getDefault().post(new MessageStartProcesses(toRun));
  }

  /**
   * @param msfgf The old main huge gui frame class.
   * @param wd Global working directory. LCMS file groups' output will be created inside this one.
   */
  static boolean processBuildersNew(MsfraggerGuiFrame msfgf, Path wd, Path jarFragpipe,
      String binPhilosopher, boolean isDryRun,
      final List<ProcessBuildersDescriptor> pbDescsToFill) {

    final List<ProcessBuildersDescriptor> pbDescs = new ArrayList<>();

    // Collect input LCMS files
    final Map<String, LcmsFileGroup> lcmsFileGroups = msfgf.getLcmsFileGroups();
    List<InputLcmsFile> lcmsFiles = lcmsFileGroups.values().stream()
        .flatMap(g -> g.lcmsFiles.stream())
        .collect(Collectors.toList());

    final UsageTrigger usePhi = new UsageTrigger(binPhilosopher, "Philosopher");

    // confirm with user that multi-experiment report is not needed
    final boolean isProcessGroupsSeparately = msfgf.getCheckProcessGroupsSeparately().isSelected();
    if (!isProcessGroupsSeparately && lcmsFileGroups.size() > 1 && !msfgf.getPanelReportOptions().isMultiExpReport()) {
      String[] options = {"Turn on and continue", "Continue as-is", "Cancel"};
      int choice = SwingUtils.showChoiceDialog(msfgf, new JLabel(
          "<html>LCMS files are grouped into more than one experiment.<br/>\n" +
              "However, multi-experiment report was turned off.<br/>\n" +
              "<br/>\n" +
              "<b>What would you like to do with multi-experiment report?</b>\n"), options, 0);
      if (choice == 0) {
        msfgf.getPanelReportOptions().setMultiExpReport(true);
      } else if (choice != 1) {
        return false; // either Window Closed, or Cancel option
      }
    }
    final boolean isMuiltiExperimentReport = msfgf.getPanelReportOptions().isMultiExpReport();

    // run DIA-Umpire SE
    final CmdUmpireSe cmdUmpireSe = new CmdUmpireSe(msfgf.isRunUmpireSe(), wd);
    if (cmdUmpireSe.isRun()) {
      if (!cmdUmpireSe.configure(msfgf, isDryRun, jarFragpipe, usePhi,
          msfgf.umpirePanel, lcmsFiles))
        return false;
      pbDescs.add(cmdUmpireSe.getBuilderDescriptor());
      lcmsFiles = cmdUmpireSe.outputs(lcmsFiles);
    }


    final FraggerMigPanel fp = msfgf.fraggerMigPanel;

    // run MSAdjuster
    final CmdMsAdjuster cmdMsAdjuster = new CmdMsAdjuster(fp.isRun() && fp.isMsadjuster(), wd);
    if (cmdMsAdjuster.isRun()) {
      if (!cmdMsAdjuster.configure(msfgf,
          jarFragpipe, fp, lcmsFiles, false, 49)) {
        return false;
      }
      pbDescs.add(cmdMsAdjuster.getBuilderDescriptor());
      // MsAdjuster only makes files that are discovered by MsFragger
      // automatically, so no file-list changes are needed
    }


    // run MsFragger
    final String fastaFile = msfgf.getFastaPath();
    final UsageTrigger binMsfragger = new UsageTrigger(
        msfgf.getTextBinMsfragger().getText().trim(), "MsFragger");
    final CmdMsfragger cmdMsfragger = new CmdMsfragger(fp.isRun(), wd, fp.getParams().getPrecursorMassUnits(), fp.getParams().getOutputReportTopN());
    if (cmdMsfragger.isRun()) {
      final String decoyTag = msfgf.getTextDecoyTagSeqDb().getText().trim();
      if (!cmdMsfragger.configure(msfgf,
          isDryRun, fp, jarFragpipe, binMsfragger, fastaFile, lcmsFiles, decoyTag)) {
        return false;
      }
      pbDescs.add(cmdMsfragger.getBuilderDescriptor());

      String warn = ThisAppProps.load(ThisAppProps.PROP_MGF_WARNING, Boolean.TRUE.toString());
      if (Boolean.parseBoolean(warn)) {
        for (InputLcmsFile f : lcmsFiles) {
          if (f.getPath().toString().toLowerCase().endsWith(".mgf")) {
            JCheckBox checkbox = new JCheckBox("Do not show this message again.");
            String msg = "The list of input files contains MGF entries.\n"
                + "MSFragger has limited MGF support (ProteoWizard output is OK).\n"
                + "The search might fail unexpectedly with errors.\n"
                + "Please consider converting files to mzML/mzXML with ProteoWizard.";
            Object[] params = {msg, checkbox};
            JOptionPane.showMessageDialog(msfgf, params, "Warning",
                JOptionPane.WARNING_MESSAGE);
            if (checkbox.isSelected()) {
              ThisAppProps.save(ThisAppProps.PROP_MGF_WARNING, Boolean.FALSE.toString());
            }
            break;
          }
        }
      }
    }
    Map<InputLcmsFile, List<Path>> pepxmlFiles = cmdMsfragger.outputs(
        lcmsFiles, fp.getOutputFileExt(), wd);
    final Map<InputLcmsFile, List<Path>> pepxmlFilesFromMsfragger = new HashMap<>(pepxmlFiles);


    // run MsAdjuster Cleanup
    if (cmdMsAdjuster.isRun()) {
      if (!cmdMsAdjuster.configure(msfgf,
          jarFragpipe, fp, lcmsFiles, true, 51)) {
        return false;
      }
      pbDescs.add(cmdMsAdjuster.getBuilderDescriptor());
    }


    // run Crystalc
    final CmdCrystalc cmdCrystalc = new CmdCrystalc(msfgf.getPanelCrystalc().isRun(), wd);
    if (cmdCrystalc.isRun()) {
      CrystalcParams ccParams = msfgf.getPanelCrystalc().toParams();
      final int fraggerThreads = msfgf.fraggerMigPanel.getThreads();
      if (fraggerThreads > 0) {
        ccParams.setThread(fraggerThreads);
      }
      if (!cmdCrystalc.configure(msfgf,
          fp, isDryRun, Paths.get(binMsfragger.getBin()), ccParams, fastaFile, pepxmlFiles)) {
        return false;
      }
      pbDescs.add(cmdCrystalc.getBuilderDescriptor());
      pepxmlFiles = cmdCrystalc.outputs(pepxmlFiles, fp.getOutputFileExt());
    }

    // run Peptide Prophet
    final boolean isRunPeptideProphet = SwingUtils.isEnabledAndChecked(msfgf.getChkRunPeptideProphet());
    final boolean isCombinedPepxml = msfgf.getCheckCombinedPepxml().isSelected();
    final String decoyTag = msfgf.getTextDecoyTagSeqDb().getText().trim();
    CmdPeptideProphet cmdPeptideProphet = new CmdPeptideProphet(isRunPeptideProphet, wd);
    if (cmdPeptideProphet.isRun()) {
      final String pepProphCmd = msfgf.getTextPepProphCmd().getText().trim();
      final String enzymeName = msfgf.fraggerMigPanel.getEnzymeName();
      if (!cmdPeptideProphet.configure(msfgf, usePhi, jarFragpipe, isDryRun,
          fastaFile, decoyTag, pepProphCmd, isCombinedPepxml, enzymeName, pepxmlFiles)) {
        return false;
      }
      pbDescs.add(cmdPeptideProphet.getBuilderDescriptor());
    }
    pepxmlFiles = cmdPeptideProphet.outputs(pepxmlFiles, fp.getOutputFileExt(), isCombinedPepxml);

    // run Protein Prophet
    final boolean isRunProteinProphet = SwingUtils.isEnabledAndChecked(
        msfgf.getChkRunProteinProphet());
    final CmdProteinProphet cmdProteinProphet = new CmdProteinProphet(isRunProteinProphet, wd);
    if (cmdProteinProphet.isRun()) {
      final String protProphCmdStr = msfgf.getTxtProteinProphetCmdLineOpts().getText().trim();
      if (!cmdProteinProphet.configure(msfgf,
          usePhi, protProphCmdStr, isMuiltiExperimentReport,
          isProcessGroupsSeparately, pepxmlFiles)) {
        return false;
      }
      pbDescs.add(cmdProteinProphet.getBuilderDescriptor());
    }
    Map<LcmsFileGroup, Path> mapGroupsToProtxml = cmdProteinProphet.outputs(pepxmlFiles, isProcessGroupsSeparately, isMuiltiExperimentReport);

    // Check Decoy tags if any of the downstream tools are requested
    if (cmdPeptideProphet.isRun() || cmdProteinProphet.isRun()) {
      if (StringUtils.isNullOrWhitespace(msfgf.getTextDecoyTagSeqDb().getText())) {
        int confirm = JOptionPane.showConfirmDialog(msfgf,
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

    final boolean isReport = msfgf.getPanelReportOptions().isGenerateReport();
    final boolean isFreequant = msfgf.getPanelQuant().isFreequant();
    if (isReport) {
      // run Report - DbAnnotate
      final boolean isDbAnnotate = true;
      final CmdPhilosopherDbAnnotate cmdPhilosopherDbAnnotate = new CmdPhilosopherDbAnnotate(isDbAnnotate, wd);
      if (cmdPhilosopherDbAnnotate.isRun()) {
        if (!cmdPhilosopherDbAnnotate
            .configure(msfgf, usePhi, fastaFile, decoyTag, pepxmlFiles, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdPhilosopherDbAnnotate.getBuilderDescriptor());
      }

      // run Report - Filter
      final boolean isFilter = isReport;
      final CmdPhilosopherFilter cmdPhilosopherFilter = new CmdPhilosopherFilter(isFilter, wd);
      if (cmdPhilosopherFilter.isRun()) {
        final boolean isCheckFilterNoProtxml = msfgf.getPanelReportOptions().isNoProtXml();

        // if ProtProph is not run but protxml is there - query the user
        boolean dontUseProtxmlInFilter;
        if (!isRunProteinProphet) {
          dontUseProtxmlInFilter = true; // default, but we will ask the user if the files are already there
          boolean allProtxmlsExist = true;
          String paths = mapGroupsToProtxml.values().stream().map(path -> "- " + path.toString()).collect(Collectors.joining("\n"));
          log.debug("Checking for existence of all protxml files:\n{}\n", paths);
          for (Entry<LcmsFileGroup, Path> kv : mapGroupsToProtxml.entrySet()) {
            Path protxml = kv.getValue();
            try {
              if (protxml == null || !Files.exists(protxml)) {
                allProtxmlsExist = false;
                break;
              }
            } catch (Exception e) {
              allProtxmlsExist = false;
              break;
            }
          }
          if (allProtxmlsExist) {
            // ProtProph is not run, but all protxmls are there
            int confirm = JOptionPane.showConfirmDialog(msfgf,
                "Protein Prophet is not run, but prot.xml files for all groups\n"
                    + "do already exist:\n\n"
                    + paths
                    + "\n\n"
                    + "Do you want to use them for the Filter command?\n",
                "Use previously existing prot.xml files?\n", JOptionPane.YES_NO_OPTION);
            if (JOptionPane.YES_OPTION == confirm) {
              dontUseProtxmlInFilter = false;
            }
          }
        } else { // if (!isRunProteinProphet) {
          // protein prophet is run, respenct the checkFilterNoProtxml checkbox
          dontUseProtxmlInFilter = isCheckFilterNoProtxml;
        }

        if (!cmdPhilosopherFilter.configure(msfgf, usePhi,
            decoyTag, msfgf.getPanelReportOptions().getFilterCmdText(), dontUseProtxmlInFilter, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdPhilosopherFilter.getBuilderDescriptor());
      }

      // run Report - Report command itself
      final CmdPhilosopherReport cmdPhilosopherReport = new CmdPhilosopherReport(isReport, wd);
      final boolean doPrintDecoys = msfgf.getPanelReportOptions().isPrintDecoys();
//      final boolean doMzid = comboReportOutputFormat.getSelectedItem().toString().toLowerCase().contains("mzid");
      final boolean doMzid = msfgf.getPanelReportOptions().isWriteMzid();
      if (cmdPhilosopherReport.isRun()) {
        if (!cmdPhilosopherReport.configure(msfgf, usePhi, doPrintDecoys, doMzid, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdPhilosopherReport.getBuilderDescriptor());
      }

      // run Report - Multi-Experiment report
      final int nThreads = msfgf.fraggerMigPanel.getThreads();
      final CmdPhilosopherAbacus cmdPhilosopherAbacus = new CmdPhilosopherAbacus(isMuiltiExperimentReport, wd);
      final boolean isMultiexpPepLevelSummary = msfgf.getPanelReportOptions().isPepSummary();
      if (cmdPhilosopherAbacus.isRun()) {

        // run iProphet, will run right after Peptide Prophet because of priority setting
        if (isMultiexpPepLevelSummary) { // iProphet is not needed if we don't generate peptide level summry
          final CmdIprophet cmdIprophet = new CmdIprophet(cmdPhilosopherAbacus.isRun(), wd);
          if (!cmdIprophet.configure(msfgf, usePhi, decoyTag, nThreads, pepxmlFiles)) {
            return false;
          }
          pbDescs.add(cmdIprophet.getBuilderDescriptor());
        }

        // run Abacus
        if (!cmdPhilosopherAbacus.configure(msfgf, usePhi, msfgf.getPanelReportOptions().getFilterCmdText(),
            isMultiexpPepLevelSummary, decoyTag, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdPhilosopherAbacus.getBuilderDescriptor());
      }

      // run Report - Freequant (Labelfree)
      final CmdFreequant cmdFreequant = new CmdFreequant(isFreequant, wd);
      if (cmdFreequant.isRun()) {
        if (!cmdFreequant.configure(msfgf, usePhi, msfgf.getPanelQuant().getFreequantOptsAsText(), mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(cmdFreequant.getBuilderDescriptor());
      }

      // run Report - IonQuant (Labelfree)
      final boolean isIonquant = msfgf.getPanelQuant().isIonquant();
      final CmdIonquant cmdIonquant = new CmdIonquant(isIonquant, wd);
      if (cmdIonquant.isRun()) {
        final int ramGb = fp.getRamGb() > 0 ? fp.getRamGb() : OsUtils.getDefaultXmx();
        if (!cmdIonquant.configure(
            msfgf, Paths.get(binMsfragger.getBin()), ramGb, msfgf.getPanelQuant().toMap(),
            pepxmlFilesFromMsfragger, mapGroupsToProtxml, nThreads)) {
          return false;
        }
        pbDescs.add(cmdIonquant.getBuilderDescriptor());
      }
    }


    final TmtiPanel tmtPanel = msfgf.getTmtPanel();
    final boolean isTmt = tmtPanel.isRun();
    if (isTmt) {
      // check file compatibility separately, as single tools will report errors
      // that look like they are unrelated to TMT
      if (lcmsFiles.stream().anyMatch(f -> !f.getPath().getFileName().toString().toLowerCase().endsWith(".mzml"))) {
        SwingUtils.showWarningDialog(msfgf,
            CmdTmtIntegrator.NAME + " only supports mzML files.\n"
                + "Please remove other files from the input list.", CmdTmtIntegrator.NAME + "error");
        return false;
      }

      // run FreeQuant - as part of TMT-I
      if (isFreequant) {
        String msg = "<html>FreeQuant needs to be run as part of TMT analysis.<br/>\n"
            + "You have chosen to run FreeQuant separately as weel.<br/>\n"
            + "This will interfere with FreeQuant files generated as part of TMT<br/>\n"
            + "processing.<br/>\n"
            + "Please turn off standalone FreeQuant.<br/>\n";
        JOptionPane.showMessageDialog(msfgf, msg, "Warning", JOptionPane.WARNING_MESSAGE);
        return false;
      }
      { // run freequant
        CmdFreequant fq = new CmdFreequant(true, wd);
        String opts = tmtPanel.getFreequantOptsAsText();
        if (!fq.configure(msfgf, usePhi, opts, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(fq.getBuilderDescriptor());
      }
      {
        // run LabelQuant - as part of TMT-I
        List<String> forbiddenOpts = Arrays.asList("--plex", "--annot", "--dir");
        CmdLabelquant lq = new CmdLabelquant(true, wd);
        String opts = tmtPanel.getLabelquantOptsAsText();
        QuantLabel label = tmtPanel.getSelectedLabel();
        Map<LcmsFileGroup, Path> annotations = tmtPanel.getAnnotations();
        if (!lq.configure(msfgf, isDryRun, usePhi, opts, label, forbiddenOpts, annotations, mapGroupsToProtxml)) {
          return false;
        }
        pbDescs.add(lq.getBuilderDescriptor());
      }
      // run TMT-Integrator
      CmdTmtIntegrator cmdTmt = new CmdTmtIntegrator(isTmt, wd);
      if (!cmdTmt.configure(msfgf.getTmtPanel(), isDryRun,
          msfgf.fraggerMigPanel.getRamGb(), msfgf.getFastaPath(), mapGroupsToProtxml)) {
        return false;
      }
      pbDescs.add(cmdTmt.getBuilderDescriptor());
    }

    // check fasta file for presence of decoys
    if (isRunPeptideProphet || isReport) {
      FastaContent fasta;
      try {
        fasta = FastaUtils.readFasta(Paths.get(fastaFile));
      } catch (IOException e) {
        SwingUtils.showErrorDialogWithStacktrace(e, msfgf);
        return false;
      }
      double decoysPercentage = FastaUtils.checkDecoysPercentage(fasta.ordered.get(0), decoyTag);
      if (decoysPercentage <= 0) {
        int confirm = SwingUtils.showConfirmDialog(msfgf, new JLabel(
            "<html>No decoys found in the FASTA file.<br/>\n" +
                "You have possibly set incorrect decoy tag. Check protein database tab.<br/><br/>\n" +
                "<br/>\n" +
                "You can also continue as-is, but FDR analysis will fail. Do you want to continue?"));
        if (JOptionPane.YES_OPTION != confirm) {
          return false;
        }
      } else if (decoysPercentage >= 1) {
        int confirm = SwingUtils.showConfirmDialog(msfgf, new JLabel(
            "<html>All FASTA entries seem to be decoys.<br/>\n" +
                "You have possibly set incorrect decoy tag. Check protein database tab.<br/><br/>\n" +
                "<br/>\n" +
                "You can also continue as-is, but FDR analysis will fail. Do you want to continue?"));
        if (JOptionPane.YES_OPTION != confirm) {
          return false;
        }
      } else if (decoysPercentage < 0.4 || decoysPercentage > 0.6) {
        DecimalFormat dfPct = new DecimalFormat("##.#'%'");
        int confirm = SwingUtils.showConfirmDialog(msfgf, new JLabel(
            "<html>FASTA file contains " + dfPct.format(decoysPercentage * 100)  + ".<br/>\n" +
                "You have possibly set incorrect decoy tag. Check protein database tab.<br/><br/>\n" +
                "<br/>\n" +
                "You can also continue as-is, if that's what you're expected.</br>\n"
                + "Do you want to continue?"));
        if (JOptionPane.YES_OPTION != confirm) {
          return false;
        }
      }
    }

    // run PTMShepherd
    final boolean isRunShepherd = msfgf.getPtmshepherdPanel().isRunShepherd();
    final boolean isPtmsFormValid = msfgf.getPtmshepherdPanel().validateForm();
    final CmdPtmshepherd cmdPtmshepherd = new CmdPtmshepherd(isRunShepherd, wd);
    if (cmdPtmshepherd.isRun()) {
      if (!isPtmsFormValid) {
        JOptionPane.showMessageDialog(msfgf,
            "There are errors in PTM-Shepherd configuraiton panel on Report tab.",
            "PTMShepherd Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
      Path fastaPath = Paths.get(fastaFile);
      int ramGb = fp.getRamGb();
      int threads = fp.getThreads();
      Map<String, String> additionalShepherdParams = msfgf.getPtmshepherdPanel().toMap();
      if (threads > 0) {
        additionalShepherdParams.put("threads", Integer.toString(threads));
      }
      String massOffsets = msfgf.fraggerMigPanel.getMassOffsets();
      if (!StringUtils.isNullOrWhitespace(massOffsets)) {
        additionalShepherdParams.put("mass_offsets", massOffsets);
      }
      if (!cmdPtmshepherd.configure(msfgf, isDryRun, Paths.get(binMsfragger.getBin()),
          ramGb, fastaPath, mapGroupsToProtxml, additionalShepherdParams)) {
        return false;
      }
      pbDescs.add(cmdPtmshepherd.getBuilderDescriptor());
    }


    // run Spectral library generation
    final boolean isRunSpeclibgen = msfgf.getSpeclibPanel1().isRunSpeclibgen();
    final boolean useEasypqp = msfgf.getSpeclibPanel1().useEasypqp();
    final CmdSpecLibGen cmdSpecLibGen = new CmdSpecLibGen(isRunSpeclibgen, wd);
    if (cmdSpecLibGen.isRun()) {
      if (!cmdSpecLibGen.configure(msfgf, usePhi, jarFragpipe,
          mapGroupsToProtxml, fastaFile, isRunProteinProphet, useEasypqp)) {
        return false;
      }
      pbDescs.add(cmdSpecLibGen.getBuilderDescriptor());
    }


    // run Philosopher clean/init in all directories where Philosopher will be invoked
    for (Path pathPhiIsRunIn : usePhi.getWorkDirs()) {
      CmdPhilosopherWorkspaceCleanInit cmdPhiCleanInit = new CmdPhilosopherWorkspaceCleanInit(
          true, pathPhiIsRunIn);
      cmdPhiCleanInit.configure(usePhi);
      pbDescs.add(cmdPhiCleanInit.getBuilderDescriptor());
      CmdPhilosopherWorkspaceClean cmdPhiClean = new CmdPhilosopherWorkspaceClean(
          true, pathPhiIsRunIn);
      cmdPhiClean.configure(usePhi);
      pbDescs.add(cmdPhiClean.getBuilderDescriptor());
    }

    // make sure that all subfolders are created for groups/experiments
    if (!isDryRun) {
      List<Path> paths = Stream
          .concat(pepxmlFiles.values().stream().flatMap(List::stream), mapGroupsToProtxml.values().stream())
          .map(Path::getParent).collect(Collectors.toList());
      try {
        for (Path path : paths) {
          if (!Files.exists(path)) {
            Files.createDirectories(path);
          }
        }
      } catch (IOException e) {
        JOptionPane.showMessageDialog(msfgf,
            "Not all directories could be created:\n" + e.getMessage());
        return false;
      }
    }

    final StringBuilder sb = new StringBuilder();
    pbDescs.forEach(pbd -> sb.append(String.format("%03d", pbd.priority)).append(" : ").append(pbd.name).append("\n"));
    log.debug("Descriptors before sorting:\n{}", sb.toString());

    pbDescs.sort(Comparator.comparing(pbDesc -> pbDesc.priority, Integer::compare));
    sb.setLength(0);
    pbDescs.forEach(pbd -> sb.append(String.format("%03d", pbd.priority)).append(" : ").append(pbd.name).append("\n"));
    log.debug("Descriptors after sorting:\n{}", sb.toString());

    pbDescsToFill.addAll(pbDescs);
    return true;
  }
}
