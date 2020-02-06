package umich.msfragger.gui;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
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
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import org.apache.commons.codec.Charsets;
import org.greenrobot.eventbus.EventBus;
import umich.msfragger.Version;
import umich.msfragger.cmd.PbiBuilder;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.cmd.ProcessBuildersDescriptor;
import umich.msfragger.gui.ProcessDescription.Builder;
import umich.msfragger.messages.MessageAppendToConsole;
import umich.msfragger.messages.MessageLastRunWorkDir;
import umich.msfragger.messages.MessageRun;
import umich.msfragger.messages.MessageSaveAllForms;
import umich.msfragger.messages.MessageSaveLog;
import umich.msfragger.messages.MessageStartProcesses;
import umich.msfragger.params.enums.FraggerOutputType;
import umich.msfragger.util.LogUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.swing.console.TextConsole;

public class FragpipeOnMessages {
  private FragpipeOnMessages() {}


  public static void onMessageRun(MsfraggerGuiFrame msfgf, MessageRun m) {
    EventBus.getDefault().post(MessageSaveAllForms.forCaching());

    final boolean isDryRun = m.isDryRun;
    msfgf.saveWorkdirText();
    msfgf.clearConsole();

    msfgf.resetRunButtons(false);
    final boolean doRunFragger = msfgf.fraggerMigPanel.isRun();
    boolean doRunProphetsAndReport = msfgf.chkRunPeptideProphet.isSelected()
        || msfgf.chkRunProteinProphet.isSelected()
        || msfgf.panelReportOptions.isGenerateReport();

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
    final String workingDir = msfgf.txtWorkingDir.getText();
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
    final ArrayList<InputLcmsFile> lcmsFilesAll = lcmsFileGroups.values().stream()
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
      jarFragpipeUri = PathUtils.getCurrentJarUri();
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
    String fastaPathText = msfgf.textSequenceDbPath.getText().trim();
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

    final String binPhilosopher = msfgf.textBinPhilosopher.getText().trim();
    final List<ProcessBuildersDescriptor> pbDescsToFill = new ArrayList<>();

    // main call to generate all the process builders
    if (!msfgf
        .processBuildersNew(wdPath, jarFragpipePath, binPhilosopher, isDryRun, pbDescsToFill)) {
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
        LogUtils.println(f.console, String.format(Locale.ROOT, "  - %s", f.getPath().toString()));
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
      msfgf.printProcessDescription(pbi);

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
      MsfraggerGuiFrame.log.error("Could not delete old fragpipe.config at: {}", path.toString());
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
      MsfraggerGuiFrame.log.error("Could not collect form text representation for printing to console");
    }

    // run everything
    List<RunnableDescription> toRun = new ArrayList<>();
    for (final ProcessBuilderInfo pbi : pbis) {
      Runnable runnable = ProcessBuilderInfo.toRunnable(pbi, wdPath, msfgf::printProcessDescription);
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
    final JButton btnStartPtr = msfgf.btnRun;
    final JButton btnStopPtr = msfgf.btnStop;
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
}
