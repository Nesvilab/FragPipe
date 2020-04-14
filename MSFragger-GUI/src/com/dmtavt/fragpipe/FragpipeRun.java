package com.dmtavt.fragpipe;

import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageClearConsole;
import com.dmtavt.fragpipe.messages.MessageRun;
import com.dmtavt.fragpipe.messages.MessageRunButtonEnabled;
import com.dmtavt.fragpipe.messages.MessageSaveCache;
import com.dmtavt.fragpipe.messages.MessageSaveLog;
import com.dmtavt.fragpipe.messages.MessageSaveUiState;
import com.dmtavt.fragpipe.messages.MessageStartProcesses;
import com.dmtavt.fragpipe.messages.NoteConfigDatabase;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.dmtavt.fragpipe.tabs.TabDatabase;
import com.dmtavt.fragpipe.tabs.TabRun;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tools.protproph.ProtProphPanel;
import com.github.chhh.utils.FastaUtils;
import com.github.chhh.utils.FastaUtils.FastaContent;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.UsageTrigger;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import org.apache.commons.codec.Charsets;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.CmdCrystalc;
import umich.msfragger.cmd.CmdFreequant;
import umich.msfragger.cmd.CmdIonquant;
import umich.msfragger.cmd.CmdIprophet;
import umich.msfragger.cmd.CmdLabelquant;
import umich.msfragger.cmd.CmdMsAdjuster;
import umich.msfragger.cmd.CmdMsfragger;
import umich.msfragger.cmd.CmdPeptideProphet;
import umich.msfragger.cmd.CmdPhilosopherAbacus;
import umich.msfragger.cmd.CmdPhilosopherDbAnnotate;
import umich.msfragger.cmd.CmdPhilosopherFilter;
import umich.msfragger.cmd.CmdPhilosopherReport;
import umich.msfragger.cmd.CmdPhilosopherWorkspaceClean;
import umich.msfragger.cmd.CmdPhilosopherWorkspaceCleanInit;
import umich.msfragger.cmd.CmdProteinProphet;
import umich.msfragger.cmd.CmdPtmshepherd;
import umich.msfragger.cmd.CmdSpecLibGen;
import umich.msfragger.cmd.CmdTmtIntegrator;
import umich.msfragger.cmd.CmdUmpireSe;
import umich.msfragger.cmd.PbiBuilder;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.cmd.ProcessBuildersDescriptor;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.gui.ProcessDescription;
import umich.msfragger.gui.ProcessDescription.Builder;
import umich.msfragger.gui.RunnableDescription;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcPanel;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.fragger.FraggerMigPanel;
import umich.msfragger.params.philosopher.ReportPanel;
import umich.msfragger.params.tmtintegrator.QuantLabel;
import umich.msfragger.params.tmtintegrator.TmtiPanel;
import umich.msfragger.params.umpire.UmpirePanel;

public class FragpipeRun {
  private static final Logger log = LoggerFactory.getLogger(FragpipeRun.class);

  private FragpipeRun() {}

  public static void run(MessageRun m) {
    Bus.post(new MessageSaveCache());
    Bus.post(new MessageClearConsole());
    Bus.post(new MessageRunButtonEnabled(false));

    boolean runConfigurationDone = false;

    try {
      final boolean isDryRun = m.isDryRun;

      final TabRun tabRun = Bus.getStickyEvent(TabRun.class);
      if (tabRun == null)
        throw new IllegalStateException("TabRun has not been posted to the bus");

      // workdir
      String wdStr = tabRun.getWorkdirText();
      Fragpipe.propsVarSet(ThisAppProps.PROP_FILE_OUT, wdStr);
      final Path wd = validateWd(tabRun, wdStr);
      if (wd == null) {
        log.debug("validateWd() failed");
        return;
      }
      final TabWorkflow tabWorkflow = Bus.getStickyEvent(TabWorkflow.class);
      if (tabWorkflow == null) {
        throw new IllegalStateException("TabWorkflow has not been posted to the bus");
      }
      if (!isDryRun) {
        Path preparedWd = prepareWd(tabRun, wd, tabWorkflow);
        if (preparedWd == null) {
          log.debug("prepareWd() failed");
          return;
        }
      }

      // check input LCMS files
      final Map<String, LcmsFileGroup> lcmsFileGroups = checkInputLcmsFiles1(tabRun, tabWorkflow);
      if (lcmsFileGroups == null) {
        log.debug("checkInputLcmsFiles1() failed");
        return;
      }
      final List<InputLcmsFile> inputLcmsFiles = checkInputLcmsFiles2(tabRun, lcmsFileGroups);
      if (inputLcmsFiles == null) {
        log.debug("checkInputLcmsFiles2() failed");
        return;
      }

      final Path jarPath = FragpipeLocations.get().getJarPath();
      if (jarPath == null) {
        JOptionPane.showMessageDialog(tabRun, "Could not get the URI of the currently running jar",
            "Errors", JOptionPane.ERROR_MESSAGE);
        return;
      }

      // check fasta file
      NoteConfigDatabase configDb = Bus.getStickyEvent(NoteConfigDatabase.class);
      final String fastaPath = checkFasta(tabRun, configDb);
      if (fastaPath == null) {
        log.debug("checkFasta() failed");
        return;
      }

      final List<ProcessBuildersDescriptor> pbDescsToFill = new ArrayList<>();
      // main call to generate all the process builders
      if (!createProcessBuilders(tabRun, wd, jarPath, isDryRun, fastaPath, pbDescsToFill)) {
        log.debug("createProcessBuilders() failed");
        return;
      }


      // =========================================================================================================

      toConsole(OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo() + "\n");
      toConsole("");
      toConsole("Version info:\n" + createVersionsString());
      toConsole("");
      toConsole("LCMS files:\n" + createLcmsFilesString(lcmsFileGroups));
      toConsole("");

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

      toConsole(String.format(Locale.ROOT, "%d commands to execute:", pbis.size()));
      for (final ProcessBuilderInfo pbi : pbis) {
        printProcessDescription(pbi);
      }
      toConsole("~~~~~~~~~~~~~~~~~~~~~~");
      toConsole("");
      toConsole("");

      if (isDryRun) {
        toConsole("It's a dry-run, not running the commands.");
        return;
      }

      // save all the options to output dir
      saveRuntimeConfig(wd);

      // print all the options to the screen as well
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      try {
        Fragpipe.propsUi().save(baos);
        toConsole("~~~~~~~~~ fragpipe.config ~~~~~~~~~");
        toConsole(baos.toString(Charsets.UTF_8.name()));
        toConsole("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
      } catch (IOException e) {
        log.error("Could not collect form text representation for printing to console");
      }

      // run everything
      final List<RunnableDescription> toRun = new ArrayList<>();
      for (final ProcessBuilderInfo pbi : pbis) {
        Runnable runnable = ProcessBuilderInfo.toRunnable(pbi, wd, FragpipeRun::printProcessDescription);
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
      final Runnable finalizerRun = () -> {
        Bus.post(new MessageRunButtonEnabled(true));
        String msg =
            "=========================\n" +
            "===\n" +
            "===      Done\n" +
            "===\n" +
            "=========================\n";
        toConsole(Fragpipe.COLOR_RED_DARKEST, msg, true);
        Bus.post(new MessageSaveLog(wd));
      };
      toRun.add(new RunnableDescription(new Builder().setName("Finalizer Task").create(), finalizerRun));

      Bus.post(new MessageStartProcesses(toRun));

      // =========================================================================================================

      runConfigurationDone = true;
    } finally {
      if (!runConfigurationDone) {
        Bus.post(new MessageRunButtonEnabled(true));
      }
    }
  }

  private static void saveRuntimeConfig(Path wd) {
    LocalDateTime time = LocalDateTime.now();
    String timestamp = time.format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss"));
    Path path = wd.resolve("fragpipe" + "_" + timestamp + ".config");
    try {
      Files.deleteIfExists(path);
    } catch (IOException e) {
      log.error("Could not delete old fragpipe.config at: {}", path.toString());
    }
    Bus.post(new MessageSaveUiState(path));
  }

  private static Path validateWd(JComponent parent, String workingDir) {
    if (StringUtils.isBlank(workingDir)) {
      JOptionPane.showMessageDialog(parent, "Output directory can't be left empty.\n"
              + "Please select an existing directory for the output.", "Bad output directory path",
          JOptionPane.WARNING_MESSAGE);
      return null;
    }
    Path testWdPath;
    try {
      testWdPath = Paths.get(workingDir);
    } catch (InvalidPathException e) {
      JOptionPane.showMessageDialog(parent, "Output directory path is not a valid path.\n"
          + "Please select a directory for the output.", "Bad output directory path", JOptionPane.WARNING_MESSAGE);
      return null;
    }
    Pattern reWhitespace = Pattern.compile("\\s");
    if (reWhitespace.matcher(testWdPath.toString()).find()) {
      JOptionPane.showMessageDialog(parent,
          "Output directory path contains whitespace characters.\n"
              + "Some programs in the pipeline might not work properly in this case.\n\n"
              + "Please change output directory to one without spaces.", "Bad output directory path", JOptionPane.WARNING_MESSAGE);
      return null;
    }
    return testWdPath;
  }

  private static Path prepareWd(JComponent parent, Path wd, TabWorkflow tabWorkflow) {
    if (!Files.exists(wd)) {
      int confirmCreation = JOptionPane.showConfirmDialog(parent,
          "Output directory doesn't exist. Create?",
          "Create output directory?", JOptionPane.OK_CANCEL_OPTION);
      if (JOptionPane.OK_OPTION != confirmCreation) {
        return null;
      }
      try {
        Files.createDirectories(wd);
      } catch (Exception e) {
        // something went not right during creation of directory structure
        JOptionPane.showMessageDialog(parent,
            "Could not create directory structure.\n" + e.getMessage(), "Error",
            JOptionPane.ERROR_MESSAGE);
        return null;
      }

    } else {
      try (Stream<Path> inWd = Files.list(wd)) {
        if (inWd.findAny().isPresent()) {
          int confirm = JOptionPane.showConfirmDialog(parent,
              "The output directory is not empty.\n\n"
                  + "Some files might be overwritten in:\n"
                  + " > " + wd.toString() + "\n\n"
                  + "Do you want to proceed?", "Confirmation", JOptionPane.YES_NO_OPTION);
          if (JOptionPane.YES_OPTION != confirm) {
            return null;
          }
        }
      } catch (Exception e) {
        JOptionPane.showMessageDialog(parent,
            "Could not create directory structure.\n" + e.getMessage(), "Error",
            JOptionPane.ERROR_MESSAGE);
        return null;
      }
    }

    // make sure subdirs for experiments are created
    Set<Path> subdirs = tabWorkflow.getLcmsFileGroups().values().stream().map(g -> g.outputDir(wd))
        .collect(Collectors.toSet());
    for (Path subdir : subdirs) {
      if (!Files.exists(subdir)) {
        try {
          Files.createDirectories(subdir);
        } catch (IOException e) {
          JOptionPane.showMessageDialog(parent,
              "Could not create directory structure.\n" + e.getMessage(), "Error",
              JOptionPane.ERROR_MESSAGE);
          return null;
        }
      }
    }

    return wd;
  }

  private static Map<String, LcmsFileGroup> checkInputLcmsFiles1(JComponent parent, TabWorkflow tabWorkflow) {
    final Map<String, LcmsFileGroup> lcmsFileGroups = tabWorkflow.getLcmsFileGroups();
    List<InputLcmsFile> lcmsExpEmptyRepNonNull = lcmsFileGroups.values().stream()
        .flatMap(g -> g.lcmsFiles.stream())
        .filter(lcms -> StringUtils.isNullOrWhitespace(lcms.getExperiment())
            && lcms.getReplicate() != null)
        .collect(Collectors.toList());
    if (!lcmsExpEmptyRepNonNull.isEmpty()) {
      int confirm = SwingUtils.showConfirmDialog(parent, new JLabel(
          "<html>For " + lcmsExpEmptyRepNonNull.size()
              + " input files Experiment was left empty while Replicate was not.<br/><br/>\n"
              + "<b>Yes</b> - if you want to auto-add 'exp_' prefix and continue as-is.<br/>\n"
              + "<b>No, Cancel</b> - stop and change manually on Select LC/MS Files tab."));
      if (confirm != JOptionPane.YES_OPTION) {
        log.debug("User chose not to auto-rename experiment");
        return null;
      }
      throw new UnsupportedOperationException("Renaming not implemented"); // TODO: implement renaming
    }
    return lcmsFileGroups;
  }

  private static List<InputLcmsFile> checkInputLcmsFiles2(JComponent parent, Map<String, LcmsFileGroup> lcmsFileGroups) {
    final List<InputLcmsFile> lcmsFilesAll = lcmsFileGroups.values().stream()
        .flatMap(group -> group.lcmsFiles.stream()).collect(Collectors.toCollection(ArrayList::new));

    if (lcmsFilesAll.isEmpty()) {
      JOptionPane.showMessageDialog(parent, "No LC/MS data files selected.\n"
          + "Check 'Select Raw Files' tab.", "Error", JOptionPane.WARNING_MESSAGE);
      return null;
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

        JOptionPane.showMessageDialog(parent,
            sb.toString(),
            "Input files with same names", JOptionPane.WARNING_MESSAGE);
        return null;
      }
    }
    return lcmsFilesAll;
  }

  private static String checkFasta(JComponent parent, NoteConfigDatabase configDb) {
    String fastaPathText = configDb.path.toString();
    if (StringUtils.isNullOrWhitespace(fastaPathText)) {
      JOptionPane.showMessageDialog(parent, "Fasta file path (Database tab) can't be empty",
          "Warning", JOptionPane.WARNING_MESSAGE);
      return null;
    }
    final Path existing = PathUtils.existing(fastaPathText);
    if (existing == null) {
      JOptionPane.showMessageDialog(parent,
          String.format("Could not find fasta file (Database) at:\n%s", fastaPathText),
          "Errors", JOptionPane.ERROR_MESSAGE);
      return null;
    }
    return existing.toString();
  }

  private static String createVersionsString() {
    StringBuilder sbVer = new StringBuilder();
    sbVer.append(Version.PROGRAM_TITLE).append(" version ").append(Version.version()).append("\n");
    sbVer.append("MSFragger version ").append(Fragpipe.getStickyStrict(NoteConfigMsfragger.class).version).append("\n");
    sbVer.append("Philosopher version ").append(Fragpipe.getStickyStrict(NoteConfigPhilosopher.class).version).append("\n");
    return sbVer.toString();
  }

  private static String createLcmsFilesString(Map<String, LcmsFileGroup> lcmsFileGroups) {
    StringBuilder sb = new StringBuilder();
    for (Entry<String, LcmsFileGroup> e : lcmsFileGroups.entrySet()) {
      sb.append(String.format(Locale.ROOT, "  Experiment/Group: %s", e.getValue().name)).append("\n");
      for (InputLcmsFile f : e.getValue().lcmsFiles) {
        sb.append(String.format(Locale.ROOT, "  - %s", f.getPath().toString())).append("\n");
      }
    }
    return sb.toString();
  }

  public static void printProcessDescription(ProcessBuilderInfo pbi) {
    if (!StringUtils.isNullOrWhitespace(pbi.name)) {
      toConsole(Fragpipe.COLOR_TOOL, pbi.name, false);
    }
    if (pbi.pb.directory() != null) {
      toConsole(Fragpipe.COLOR_WORKDIR, " [Work dir: " + pbi.pb.directory() + "]", false);
    }
    toConsole("");
    final String cmd = org.apache.commons.lang3.StringUtils.join(pbi.pb.command(), " ");
    toConsole(Fragpipe.COLOR_CMDLINE, cmd, true);
  }

  private static boolean createProcessBuilders(JComponent parent, Path wd, Path jarPath,
      boolean isDryRun, String fastaFile, List<ProcessBuildersDescriptor> pbDescsToFill) {

    final List<ProcessBuildersDescriptor> pbDescs = new ArrayList<>();

    // Collect input LCMS files
    TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
    final Map<String, LcmsFileGroup> lcmsFileGroups = tabWorkflow.getLcmsFileGroups();
    List<InputLcmsFile> lcmsFiles = lcmsFileGroups.values().stream()
        .flatMap(g -> g.lcmsFiles.stream())
        .collect(Collectors.toList());

    // confirm with user that multi-experiment report is not needed
    final ProtProphPanel protProphPanel = Fragpipe.getStickyStrict(ProtProphPanel.class);
    ReportPanel reportPanel = Fragpipe.getStickyStrict(ReportPanel.class);
    final boolean isProcessGroupsSeparately = protProphPanel.isProcessGroupsSeparately();
    if (!isProcessGroupsSeparately && lcmsFileGroups.size() > 1 && !reportPanel.isMultiExpReport()) {
      String[] options = {"Turn on and continue", "Continue as-is", "Cancel"};
      int choice = SwingUtils.showChoiceDialog(parent, new JLabel(
          "<html>LCMS files are grouped into more than one experiment.<br/>\n" +
              "However, multi-experiment report was turned off.<br/>\n" +
              "<br/>\n" +
              "<b>What would you like to do with multi-experiment report?</b>\n"), options, 0);
      if (choice == 0) {
        reportPanel.setMultiExpReport(true);
      } else if (choice != 1) {
        return false; // either Window Closed, or Cancel option
      }
    }

    NoteConfigPhilosopher configPhi = Fragpipe.getStickyStrict(NoteConfigPhilosopher.class);
    final UsageTrigger usePhi = new UsageTrigger(configPhi.path, "Philosopher");
    final boolean isMuiltiExperimentReport = reportPanel.isMultiExpReport();

    // run DIA-Umpire SE
    final UmpirePanel umpirePanel = Fragpipe.getStickyStrict(UmpirePanel.class);
    final CmdUmpireSe cmdUmpire = new CmdUmpireSe(umpirePanel.isRunUmpire(), wd);
    if (cmdUmpire.isRun()) {
      if (!cmdUmpire.configure(parent, isDryRun, jarPath, usePhi, umpirePanel, lcmsFiles))
        return false;
      pbDescs.add(cmdUmpire.getBuilderDescriptor());
      lcmsFiles = cmdUmpire.outputs(lcmsFiles);
    }

    final FraggerMigPanel fp = Fragpipe.getStickyStrict(FraggerMigPanel.class);

    // run MSAdjuster
    final CmdMsAdjuster cmdMsAdjuster = new CmdMsAdjuster(fp.isRun() && fp.isMsadjuster(), wd);
    if (cmdMsAdjuster.isRun()) {
      if (!cmdMsAdjuster.configure(parent, jarPath, fp, lcmsFiles, false, 49)) {
        return false;
      }
      pbDescs.add(cmdMsAdjuster.getBuilderDescriptor());
      // MsAdjuster only makes files that are discovered by MsFragger
      // automatically, so no file-list changes are needed
    }

    // run MsFragger
    final NoteConfigMsfragger configMsfragger = Fragpipe.getStickyStrict(NoteConfigMsfragger.class);
    final UsageTrigger binMsfragger = new UsageTrigger(configMsfragger.path, "MsFragger");
    TabDatabase tabDatabase = Fragpipe.getStickyStrict(TabDatabase.class);

    final CmdMsfragger cmdMsfragger = new CmdMsfragger(fp.isRun(), wd, fp.getParams().getPrecursorMassUnits(), fp.getParams().getOutputReportTopN());
    if (cmdMsfragger.isRun()) {
      final String decoyTag = tabDatabase.getDecoyTag();
      if (!cmdMsfragger.configure(parent, isDryRun, fp, jarPath, binMsfragger, fastaFile, lcmsFiles, decoyTag)) {
        return false;
      }
      pbDescs.add(cmdMsfragger.getBuilderDescriptor());

      String warn = Fragpipe.propsVarGet(ThisAppProps.PROP_MGF_WARNING, Boolean.TRUE.toString());
      if (Boolean.parseBoolean(warn)) {
        for (InputLcmsFile f : lcmsFiles) {
          if (f.getPath().toString().toLowerCase().endsWith(".mgf")) {
            JCheckBox checkbox = new JCheckBox("Do not show this message again.");
            String msg = "The list of input files contains MGF entries.\n"
                + "MSFragger has limited MGF support (ProteoWizard output is OK).\n"
                + "The search might fail unexpectedly with errors.\n"
                + "Please consider converting files to mzML/mzXML with ProteoWizard.";
            Object[] params = {msg, checkbox};
            JOptionPane.showMessageDialog(parent, params, "Warning", JOptionPane.WARNING_MESSAGE);
            if (checkbox.isSelected()) {
              Fragpipe.propsVarSet(ThisAppProps.PROP_MGF_WARNING, Boolean.FALSE.toString());
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
      if (!cmdMsAdjuster.configure(parent, jarPath, fp, lcmsFiles, true, 51)) {
        return false;
      }
      pbDescs.add(cmdMsAdjuster.getBuilderDescriptor());
    }


    // run Crystalc
    CrystalcPanel crystalcPanel = Fragpipe.getStickyStrict(CrystalcPanel.class);
    final CmdCrystalc cmdCrystalc = new CmdCrystalc(crystalcPanel.isRun(), wd);
    if (cmdCrystalc.isRun()) {
      CrystalcParams ccParams = crystalcPanel.toParams();
      final int fraggerThreads = fp.getThreads();
      if (fraggerThreads > 0) {
        ccParams.setThread(fraggerThreads);
      }
      if (!cmdCrystalc.configure(parent, fp, isDryRun, Paths.get(binMsfragger.getBin()), ccParams, fastaFile, pepxmlFiles)) {
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
      double decoysPercentage = FastaUtils.getDecoysPct(fasta.ordered.get(0), decoyTag);
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
      Optional.ofNullable(msfgf.fraggerMigPanel.getUiTextIsoErr().getNonGhostText())
          .filter(StringUtils::isNotBlank).ifPresent(v -> additionalShepherdParams.put("isotope_error", v));
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


    if (!configPhi.isValid()) {
      SwingUtils.showErrorDialog(parent, "Philosopher configuraiton invalid, check Config tab", "Config Error");
      return false;
    }


    return true;
  }
}
