package com.dmtavt.fragpipe;

import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageAppendToConsole;
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
import com.dmtavt.fragpipe.tabs.TabRun;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.github.chhh.utils.LogUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
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
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import org.apache.commons.codec.Charsets;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.PbiBuilder;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.cmd.ProcessBuildersDescriptor;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.gui.MsfraggerGuiFrame;
import umich.msfragger.gui.MsfraggerGuiFrameUtils;
import umich.msfragger.gui.ProcessDescription;
import umich.msfragger.gui.ProcessDescription.Builder;
import umich.msfragger.gui.RunnableDescription;
import umich.msfragger.params.ThisAppProps;

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

      NoteConfigPhilosopher configPhi = Bus.getStickyEvent(NoteConfigPhilosopher.class);
      if (!configPhi.isValid()) {
        SwingUtils.showErrorDialog(tabRun, "Philosopher configuraiton invalid, check Config tab", "Config Error");
        return;
      }
      final String binPhi = configPhi.path;

      final List<ProcessBuildersDescriptor> pbDescsToFill = new ArrayList<>();
      // main call to generate all the process builders
      if (!createProcessBuilders(tabRun, wd, jarPath, binPhi, isDryRun, pbDescsToFill)) {
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

      // save all the options
      saveRuntimeConfig(wd);

      // print all the options
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      try {
        Fragpipe.propsUi()
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
}
