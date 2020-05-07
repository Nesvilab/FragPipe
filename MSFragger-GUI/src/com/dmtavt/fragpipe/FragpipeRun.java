package com.dmtavt.fragpipe;

import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.IConfig;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.cmd.CmdBase;
import com.dmtavt.fragpipe.cmd.CmdCrystalc;
import com.dmtavt.fragpipe.cmd.CmdFreequant;
import com.dmtavt.fragpipe.cmd.CmdIonquant;
import com.dmtavt.fragpipe.cmd.CmdIprophet;
import com.dmtavt.fragpipe.cmd.CmdLabelquant;
import com.dmtavt.fragpipe.cmd.CmdMsAdjuster;
import com.dmtavt.fragpipe.cmd.CmdMsfragger;
import com.dmtavt.fragpipe.cmd.CmdPeptideProphet;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherAbacus;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherDbAnnotate;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherFilter;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherReport;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherWorkspaceClean;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherWorkspaceCleanInit;
import com.dmtavt.fragpipe.cmd.CmdProteinProphet;
import com.dmtavt.fragpipe.cmd.CmdPtmshepherd;
import com.dmtavt.fragpipe.cmd.CmdSpecLibGen;
import com.dmtavt.fragpipe.cmd.CmdStart;
import com.dmtavt.fragpipe.cmd.CmdTmtIntegrator;
import com.dmtavt.fragpipe.cmd.CmdUmpireSe;
import com.dmtavt.fragpipe.cmd.PbiBuilder;
import com.dmtavt.fragpipe.cmd.ProcessBuilderInfo;
import com.dmtavt.fragpipe.cmd.ProcessBuildersDescriptor;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
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
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.process.ProcessDescription;
import com.dmtavt.fragpipe.process.ProcessDescription.Builder;
import com.dmtavt.fragpipe.process.RunnableDescription;
import com.dmtavt.fragpipe.tabs.TabDatabase;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tabs.TabRun;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tools.crystalc.CrystalcPanel;
import com.dmtavt.fragpipe.tools.crystalc.CrystalcParams;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerParams;
import com.dmtavt.fragpipe.tools.ionquant.QuantPanelLabelfree;
import com.dmtavt.fragpipe.tools.pepproph.PepProphPanel;
import com.dmtavt.fragpipe.tools.philosopher.ReportPanel;
import com.dmtavt.fragpipe.tools.protproph.ProtProphPanel;
import com.dmtavt.fragpipe.tools.ptmshepherd.PtmshepherdPanel;
import com.dmtavt.fragpipe.tools.speclibgen.SpecLibGen2;
import com.dmtavt.fragpipe.tools.speclibgen.SpeclibPanel;
import com.dmtavt.fragpipe.tools.tmtintegrator.QuantLabel;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiPanel;
import com.dmtavt.fragpipe.tools.umpire.UmpirePanel;
import com.github.chhh.utils.FastaUtils;
import com.github.chhh.utils.FastaUtils.FastaContent;
import com.github.chhh.utils.MapUtils;
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
import java.util.HashMap;
import java.util.LinkedHashMap;
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
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.traverse.ClosestFirstIterator;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.jooq.lambda.Seq;
import org.jooq.lambda.function.Consumer3;
import org.jooq.lambda.tuple.Tuple2;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeRun {

  private static final Logger log = LoggerFactory.getLogger(FragpipeRun.class);

  private FragpipeRun() {
  }

  public static void run(MessageRun m) {
    log.debug("Started main FragpipeRun.run() method");
    Thread.setDefaultUncaughtExceptionHandler(Fragpipe.uncaughtExceptionHandler());

    Bus.post(new MessageSaveCache());
    Bus.post(new MessageClearConsole());
    Bus.post(new MessageRunButtonEnabled(false));

    boolean runConfigurationDone = false;

    try {
      final boolean isDryRun = m.isDryRun;

      final TabRun tabRun = Bus.getStickyEvent(TabRun.class);
      if (tabRun == null) {
        throw new IllegalStateException("TabRun has not been posted to the bus");
      }

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

      final Graph<CmdBase, DefaultEdge> dag = new DirectedAcyclicGraph<>(DefaultEdge.class);
      // main call to generate all the process builders
      if (!configureTaskGraph(tabRun, wd, jarPath, isDryRun, fastaPath, dag)) {
        log.debug("createProcessBuilders() failed");
        return;
      }

      if (log.isDebugEnabled()) {
        TopologicalOrderIterator<CmdBase, DefaultEdge> it =
            new TopologicalOrderIterator<>(dag);
        String s = Seq.seq(it).map(o -> (CmdBase) o)
            .map(cmd -> String.format("Cmd: [%s], IsRun: [%s] Prority: [%d]", cmd.getCmdName(),
                cmd.isRun(), cmd.getPriority()))
            .toString("\n");
        log.debug("Ordered tasks:\n{}", s);
      }

      final List<ProcessBuildersDescriptor> pbDescsBuilderDescs = Seq
          .seq(new TopologicalOrderIterator<>(dag))
          .map(o -> (CmdBase) o).filter(CmdBase::isRun)
          .map(CmdBase::getBuilderDescriptor).toList();

      // =========================================================================================================
      toConsole(OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo() + "\n");
      toConsole("");
      toConsole("Version info:\n" + createVersionsString());
      toConsole("");
      toConsole("LCMS files:\n" + createLcmsFilesString(lcmsFileGroups));
      toConsole("");

      // Converting process builders descriptors to process builder infos
      final List<ProcessBuilderInfo> pbis = pbDescsBuilderDescs.stream()
          .flatMap(pbd -> pbd.pbis.stream().map(pbi ->
          {
            PbiBuilder b = new PbiBuilder();
            b.setPb(pbi.pb);
            b.setName(pbi.name != null ? pbi.name : pbd.name);
            b.setFnStdOut(pbi.fnStdout != null ? pbi.fnStdout : pbd.fnStdout);
            b.setFnStdErr(pbi.fnStderr != null ? pbi.fnStderr : pbd.fnStderr);
            b.setParallelGroup(
                pbi.parallelGroup != null ? pbi.parallelGroup : pbd.getParallelGroup());
            return b.create();
          }))
          .collect(Collectors.toList());

      toConsole(String.format(Locale.ROOT, "%d commands to execute:", pbis.size()));
      for (final ProcessBuilderInfo pbi : pbis) {
        printProcessDescription(pbi);
      }
      toConsole("~~~~~~~~~~~~~~~~~~~~~~");
      toConsole("");

      // execution order
//      String execOrder = Seq.seq(new TopologicalOrderIterator<>(dag)).map(o -> (CmdBase) o)
//          .filter(CmdBase::isRun)
//          .map(cmd -> String.format("Cmd: [%s], WorkDir: [%s]", cmd.getCmdName(), cmd.getWd()))
//          .toString("\n    ");
      toConsole("Execution order:\n");
      Seq.seq(new TopologicalOrderIterator<>(dag)).map(o -> (CmdBase) o)
          .filter(CmdBase::isRun)
          .forEach(cmd -> {
            toConsole(Fragpipe.COLOR_TOOL, String.format("    Cmd: [%s], ", cmd.getCmdName()),
                false);
            toConsole(Fragpipe.COLOR_WORKDIR, String.format("Work dir: [%s]", cmd.getWd()), true);
          });

      toConsole("");
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
        Runnable runnable = ProcessBuilderInfo
            .toRunnable(pbi, wd, FragpipeRun::printProcessDescription);
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
        Bus.post(MessageSaveLog.saveInDir(wd));
      };
      toRun.add(
          new RunnableDescription(new Builder().setName("Finalizer Task").create(), finalizerRun));

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
              + "Please select a directory for the output.", "Bad output directory path",
          JOptionPane.WARNING_MESSAGE);
      return null;
    }
    Pattern reWhitespace = Pattern.compile("\\s");
    if (reWhitespace.matcher(testWdPath.toString()).find()) {
      JOptionPane.showMessageDialog(parent,
          "Output directory path contains whitespace characters.\n"
              + "Some programs in the pipeline might not work properly in this case.\n\n"
              + "Please change output directory to one without spaces.",
          "Bad output directory path", JOptionPane.WARNING_MESSAGE);
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

  private static Map<String, LcmsFileGroup> checkInputLcmsFiles1(JComponent parent,
      TabWorkflow tabWorkflow) {
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
      throw new UnsupportedOperationException(
          "Renaming not yet implemented"); // TODO: implement renaming
    }
    return lcmsFileGroups;
  }

  private static List<InputLcmsFile> checkInputLcmsFiles2(JComponent parent,
      Map<String, LcmsFileGroup> lcmsFileGroups) {
    final List<InputLcmsFile> lcmsFilesAll = lcmsFileGroups.values().stream()
        .flatMap(group -> group.lcmsFiles.stream())
        .collect(Collectors.toCollection(ArrayList::new));

    if (lcmsFilesAll.isEmpty()) {
      JOptionPane.showMessageDialog(parent, "No LC/MS data files selected.\n"
              + "Check 'Workflow' tab, 'Input LC/MS Files' section.", "Error",
          JOptionPane.WARNING_MESSAGE);
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
    if (configDb == null || configDb.path == null || StringUtils
        .isBlank(configDb.path.toString())) {
      JOptionPane.showMessageDialog(parent, "Fasta file path (Database tab) can't be empty",
          "Warning", JOptionPane.WARNING_MESSAGE);
      return null;
    }
    final Path existing = PathUtils.existing(configDb.path.toString());
    if (existing == null) {
      JOptionPane.showMessageDialog(parent,
          String.format("Could not find fasta file (Database) at:\n%s", configDb.path.toString()),
          "Errors", JOptionPane.ERROR_MESSAGE);
      return null;
    }
    return existing.toString();
  }

  private static String createVersionsString() {
    StringBuilder sb = new StringBuilder();
    sb.append(Version.PROGRAM_TITLE).append(" version ").append(Version.version()).append("\n");
    sb.append("MSFragger version ")
        .append(Fragpipe.getStickyStrict(NoteConfigMsfragger.class).version).append("\n");
    sb.append("Philosopher version ")
        .append(Fragpipe.getStickyStrict(NoteConfigPhilosopher.class).version).append("\n");
    return sb.toString();
  }

  private static String createLcmsFilesString(Map<String, LcmsFileGroup> lcmsFileGroups) {
    StringBuilder sb = new StringBuilder();
    for (Entry<String, LcmsFileGroup> e : lcmsFileGroups.entrySet()) {
      sb.append(String.format(Locale.ROOT, "  Experiment/Group: %s", e.getValue().name))
          .append("\n");
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

  private static void addToDepGraph(Graph<? super CmdBase, DefaultEdge> graph, CmdBase node,
      CmdBase... deps) {
    if (!graph.containsVertex(node)) {
      graph.addVertex(node);
    }
    for (CmdBase dep : deps) {
      if (!graph.containsEdge(dep, node)) {
        graph.addEdge(dep, node);
      }
    }
  }

  private static class TaskGraphNode {

    CmdBase cmd;
    IConfig config = () -> true;

    public TaskGraphNode(CmdBase cmd, IConfig config) {
      this.cmd = cmd;
      if (config != null) {
        this.config = config;
      }
    }
  }

  private static boolean configureTaskGraph(JComponent parent, Path wd, Path jarPath,
      boolean isDryRun, String fastaFile, final Graph<CmdBase, DefaultEdge> graphOrder) {

    // Collect input LCMS files
    final TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);

    // confirm with user that multi-experiment report is not needed
    final ProtProphPanel protProphPanel = Fragpipe.getStickyStrict(ProtProphPanel.class);
    final ReportPanel reportPanel = Fragpipe.getStickyStrict(ReportPanel.class);

    final NoteConfigPhilosopher configPhi = Fragpipe.getStickyStrict(NoteConfigPhilosopher.class);
    final UsageTrigger usePhi = new UsageTrigger(configPhi.path, "Philosopher");

    // all the configurations are aggregated before being executed
    // because some commands might require others to run
    // (even though those commands might be turned off)
    final Graph<CmdBase, DefaultEdge> graphDeps = new DirectedAcyclicGraph<>(DefaultEdge.class);
    final List<IConfig> preConfigs = new ArrayList<>();
    final List<CmdBase> cmds = new ArrayList<>();
    final Consumer3<CmdBase, IConfig, IConfig> conf = (cmd, preConfig, config) -> {
      //new TaskGraphNode(cmd, config);
      cmds.add(cmd);
      if (config != null) {
        cmd.setConfig(config);
      }
      if (preConfig != null) {
        preConfigs.add(preConfig);
      }
    };

    final Map<String, LcmsFileGroup> sharedLcmsFileGroups = new LinkedHashMap<>();
    final List<InputLcmsFile> sharedLcmsFiles = new ArrayList<>();

    // Add LCMS files
    final CmdStart cmdStart = new CmdStart(true, wd);
    conf.accept(cmdStart, null, () -> {
      MapUtils.refill(sharedLcmsFileGroups, tabWorkflow.getLcmsFileGroups());
      sharedLcmsFiles.clear();
      sharedLcmsFiles.addAll(Seq.seq(sharedLcmsFileGroups.values())
          .flatMap(group -> group.lcmsFiles.stream())
          .toList());
      if (sharedLcmsFiles.isEmpty()) {
        SwingUtils.showErrorDialog(parent,
            "No LCMS files provided.", "Add LCMS files");
        return false;
      }
      return true;
    });

    // run DIA-Umpire SE
    final UmpirePanel umpirePanel = Fragpipe.getStickyStrict(UmpirePanel.class);
    final CmdUmpireSe cmdUmpire = new CmdUmpireSe(umpirePanel.isRunUmpire(), wd);
    conf.accept(cmdUmpire, null, () -> {
      if (cmdUmpire.isRun()) {
        if (!cmdUmpire.configure(parent, isDryRun, jarPath, usePhi, umpirePanel, sharedLcmsFiles)) {
          return false;
        }
        List<InputLcmsFile> outputs = cmdUmpire.outputs(sharedLcmsFiles);
        sharedLcmsFiles.clear();
        sharedLcmsFiles.addAll(outputs);
      }
      return true;
    });

    final TabMsfragger tabMsf = Fragpipe.getStickyStrict(TabMsfragger.class);
    final int ramGb = tabWorkflow.getRamGb();
    final int ramGbNonzero = ramGb > 0 ? ramGb : OsUtils.getDefaultXmx();
    final int threads = tabWorkflow.getThreads();

    // run MSAdjuster
    final CmdMsAdjuster cmdMsAdjuster = new CmdMsAdjuster(tabMsf.isRun() && tabMsf.isMsadjuster(),
        wd);

    conf.accept(cmdMsAdjuster, null, () -> {
      if (cmdMsAdjuster.isRun()) {
        if (!cmdMsAdjuster.configure(parent, jarPath, ramGbNonzero, sharedLcmsFiles, false, 49)) {
          return false;
        }
        // MsAdjuster only makes files that are discovered by MsFragger
        // automatically, so no file-list changes are needed
      }
      return true;
    });

    // run MsAdjuster Cleanup
    final CmdMsAdjuster cmdMsAdjusterCleanup = new CmdMsAdjuster(cmdMsAdjuster.isRun(), wd);

    conf.accept(cmdMsAdjusterCleanup, null, () -> {
      if (cmdMsAdjusterCleanup.isRun()) {
        if (!cmdMsAdjusterCleanup
            .configure(parent, jarPath, ramGbNonzero, sharedLcmsFiles, true, 51)) {
          return false;
        }
      }
      return true;
    });

    // run MsFragger
    final NoteConfigMsfragger configMsfragger;
    try {
      configMsfragger = Fragpipe.getSticky(NoteConfigMsfragger.class);
    } catch (NoStickyException e) {
      SwingUtils.showErrorDialog(parent,
          "Looks like fragger was not configured.\nFragger is currently required.", "No MSFragger");
      return false;
    }
    final UsageTrigger binMsfragger = new UsageTrigger(configMsfragger.path, "MsFragger");

    final TabDatabase tabDatabase = Fragpipe.getStickyStrict(TabDatabase.class);
    final String decoyTag = tabDatabase.getDecoyTag();

    final CmdMsfragger cmdMsfragger;
    {
      MsfraggerParams p = tabMsf.getParams();
      cmdMsfragger = new CmdMsfragger(tabMsf.isRun(), wd,
          p.getPrecursorMassUnits(), p.getOutputReportTopN());
    }
    final Map<InputLcmsFile, List<Path>> sharedPepxmlFilesFromMsfragger = new HashMap<>();
    final Map<InputLcmsFile, List<Path>> sharedPepxmlFiles = new HashMap<>();

    conf.accept(cmdMsfragger, () -> {
      SpeclibPanel speclibPanel = Fragpipe.getStickyStrict(SpeclibPanel.class);
      // check that Write Calibrated MGF is On in Fragger for easyPQP + timsTOF
      if (SpeclibPanel.EASYPQP_TIMSTOF.equals(speclibPanel.getEasypqpDataType())) {
        if (!SwingUtils.showConfirmDialogShort(parent,
            "Spectral library generation via EasyPQP requires that MSFragger\n"
                + "writes a calibrated MGF file. There is a checkbox on MSFragger tab.\n\n"
                + "Do you want to turn writing MGF on and continue?")) {
          log.debug("User chose not to continue with auto-enabled MGF writing");
          return false;
        }
        tabMsf.setWriteCalMgf(true);
      }
      return true;
    }, () -> {
      if (cmdMsfragger.isRun()) {
        if (!cmdMsfragger.configure(parent, isDryRun, jarPath, binMsfragger, fastaFile,
            tabMsf.getParams(), tabMsf.getNumDbSlices(), ramGbNonzero,
            sharedLcmsFiles, decoyTag)) {
          return false;
        }

        String warn = Fragpipe.propsVarGet(ThisAppProps.PROP_MGF_WARNING, Boolean.TRUE.toString());
        if (Boolean.parseBoolean(warn)) {
          for (InputLcmsFile f : sharedLcmsFiles) {
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

      Map<InputLcmsFile, List<Path>> outputs = cmdMsfragger.outputs(
          sharedLcmsFiles, tabMsf.getOutputFileExt(), wd);
      MapUtils.refill(sharedPepxmlFilesFromMsfragger, outputs);
      MapUtils.refill(sharedPepxmlFiles, outputs);

      return true;
    });

    // run Crystalc
    CrystalcPanel crystalcPanel = Fragpipe.getStickyStrict(CrystalcPanel.class);
    final CmdCrystalc cmdCrystalc = new CmdCrystalc(crystalcPanel.isRun(), wd);

    conf.accept(cmdCrystalc, null, () -> {
      if (cmdCrystalc.isRun()) {
        CrystalcParams ccParams = crystalcPanel.toParams();
        if (threads > 0) {
          ccParams.setThread(threads);
        }
        if (!cmdCrystalc.configure(parent, isDryRun, Paths.get(binMsfragger.getBin()),
            tabMsf.getParams().getOutputFileExtension(), ramGbNonzero,
            ccParams, fastaFile, sharedPepxmlFiles)) {
          return false;
        }
        Map<InputLcmsFile, List<Path>> outputs = cmdCrystalc
            .outputs(sharedPepxmlFiles, tabMsf.getOutputFileExt());
        sharedPepxmlFiles.clear();
        sharedPepxmlFiles.putAll(outputs);
      }
      return true;
    });

    // run Peptide Prophet
    final PepProphPanel pepProphPanel = Fragpipe.getStickyStrict(PepProphPanel.class);
    final boolean isRunPeptideProphet = pepProphPanel.isRun();
    final boolean isCombinedPepxml = pepProphPanel.isCombinePepxml();

    CmdPeptideProphet cmdPeptideProphet = new CmdPeptideProphet(isRunPeptideProphet, wd);

    conf.accept(cmdPeptideProphet, () -> {
      if (cmdPeptideProphet.isRun()) {
        return checkDbConfig(parent);
      }
      return true;

    }, () -> {
      if (cmdPeptideProphet.isRun()) {
        final String pepProphCmd = pepProphPanel.getCmdOpts();
        final String enzymeName = tabMsf.getEnzymeName();
        if (!cmdPeptideProphet.configure(parent, usePhi, jarPath, isDryRun,
            fastaFile, decoyTag, pepProphCmd, isCombinedPepxml, enzymeName, sharedPepxmlFiles)) {
          return false;
        }
      }
      Map<InputLcmsFile, List<Path>> pepProphOutputs = cmdPeptideProphet
          .outputs(sharedPepxmlFiles, tabMsf.getOutputFileExt(), isCombinedPepxml);
      sharedPepxmlFiles.clear();
      sharedPepxmlFiles.putAll(pepProphOutputs);

      if (cmdPeptideProphet.isRun()) {
        // peptide prophet is run, so we run adjustments of the pepxml files.
        List<Tuple2<InputLcmsFile, Path>> lcmsToPepxml = Seq.seq(sharedPepxmlFiles)
            .flatMap(t2 -> Seq.seq(t2.v2).map(pepxml -> new Tuple2<>(t2.v1, pepxml)))
            .distinct(t2 -> t2.v2)
            .toList();
        for (Tuple2<InputLcmsFile, Path> kv : lcmsToPepxml) {
          // TODO: process builder to to call: FixPepProphLcmsPath.fixPathInplace(kv.v2, kv.v1, wd);
        }
      }

      return true;
    });

    // run Protein Prophet
    final boolean isMuiltiExperimentReport = sharedLcmsFileGroups.size() > 1;
    final boolean isProcessGroupsSeparately = Fragpipe.getStickyStrict(TabWorkflow.class)
        .isProcessEachExpSeparately();
    final boolean isRunProteinProphet = protProphPanel.isRun();
    final Map<LcmsFileGroup, Path> sharedMapGroupsToProtxml = new LinkedHashMap<>();

    final CmdProteinProphet cmdProteinProphet = new CmdProteinProphet(isRunProteinProphet, wd);

    conf.accept(cmdProteinProphet, () -> {
      if (cmdProteinProphet.isRun()) {
        return checkDbConfig(parent);
      }
      return true;
    }, () -> {
      if (cmdProteinProphet.isRun()) {
        final String protProphCmdStr = protProphPanel.getCmdOpts();
        if (!cmdProteinProphet.configure(parent,
            usePhi, protProphCmdStr, isMuiltiExperimentReport,
            isProcessGroupsSeparately, sharedPepxmlFiles)) {
          return false;
        }
      }
      Map<LcmsFileGroup, Path> outputs = cmdProteinProphet
          .outputs(sharedPepxmlFiles, isProcessGroupsSeparately,
              isMuiltiExperimentReport);
      MapUtils.refill(sharedMapGroupsToProtxml, outputs);
      return true;
    });

    final boolean isReport = reportPanel.isGenerateReport();
    final QuantPanelLabelfree quantPanelLabelfree = Fragpipe
        .getStickyStrict(QuantPanelLabelfree.class);
    final boolean isFreequant = quantPanelLabelfree.isFreequant();

    // run Report - DbAnnotate
    final boolean isDbAnnotate = isReport;
    final CmdPhilosopherDbAnnotate cmdPhilosopherDbAnnotate = new CmdPhilosopherDbAnnotate(
        isDbAnnotate, wd);

    conf.accept(cmdPhilosopherDbAnnotate, null, () -> {
      if (cmdPhilosopherDbAnnotate.isRun()) {
        return cmdPhilosopherDbAnnotate
            .configure(parent, usePhi, fastaFile, decoyTag, sharedPepxmlFiles,
                sharedMapGroupsToProtxml);
      }
      return true;
    });

    // run Report - Filter
    final CmdPhilosopherFilter cmdPhilosopherFilter = new CmdPhilosopherFilter(isReport, wd);

    conf.accept(cmdPhilosopherFilter, null, () -> {
      if (cmdPhilosopherFilter.isRun()) {
        final boolean isCheckFilterNoProtxml = reportPanel.isNoProtXml();

        // if ProtProph is not run but protxml is there - query the user
        boolean dontUseProtxmlInFilter;
        if (!isRunProteinProphet) {
          dontUseProtxmlInFilter = true; // default, but we will ask the user if the files are already there
          boolean allProtxmlsExist = true;
          String paths = sharedMapGroupsToProtxml.values().stream()
              .map(path -> "- " + path.toString()).collect(Collectors.joining("\n"));
          log.debug("Checking for existence of all protxml files:\n{}\n", paths);
          for (Entry<LcmsFileGroup, Path> kv : sharedMapGroupsToProtxml.entrySet()) {
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
            int confirm = JOptionPane.showConfirmDialog(parent,
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

        return cmdPhilosopherFilter.configure(parent, usePhi,
            decoyTag, reportPanel.getFilterCmdText(), dontUseProtxmlInFilter,
            sharedMapGroupsToProtxml);
      }
      return true;
    });

    // run Report - Report command itself
    final CmdPhilosopherReport cmdPhilosopherReport = new CmdPhilosopherReport(isReport, wd);
    final boolean doPrintDecoys = reportPanel.isPrintDecoys();
    final boolean doMzid = reportPanel.isWriteMzid();

    conf.accept(cmdPhilosopherReport, null, () -> {
      if (cmdPhilosopherReport.isRun()) {
        return cmdPhilosopherReport
            .configure(parent, usePhi, doPrintDecoys, doMzid, sharedMapGroupsToProtxml);
      }
      return true;
    });

    // run Report - Multi-Experiment report
    final boolean isMultiexpPepLevelSummary = reportPanel.isPepSummary();
    final CmdPhilosopherAbacus cmdPhilosopherAbacus = new CmdPhilosopherAbacus(
        isReport && isMuiltiExperimentReport, wd);

    conf.accept(cmdPhilosopherAbacus, null, () -> {
      if (cmdPhilosopherAbacus.isRun()) {
        return cmdPhilosopherAbacus.configure(parent, usePhi, reportPanel.getFilterCmdText(),
            isMultiexpPepLevelSummary, decoyTag, sharedMapGroupsToProtxml);
      }
      return true;
    });

    final CmdIprophet cmdIprophet = new CmdIprophet(cmdPhilosopherAbacus.isRun(), wd);
    conf.accept(cmdIprophet, null, () -> {
      if (cmdIprophet.isRun()) {
        return cmdIprophet.configure(parent, usePhi, decoyTag, threads, sharedPepxmlFiles);
      }
      return true;
    });

    // run Report - Freequant (Labelfree)
    final CmdFreequant cmdFreequant = new CmdFreequant(isReport && isFreequant, wd);
    conf.accept(cmdFreequant, null, () -> {
      if (cmdFreequant.isRun()) {
        return cmdFreequant.configure(parent, usePhi, quantPanelLabelfree.getFreequantOptsAsText(),
            sharedMapGroupsToProtxml);
      }
      return true;
    });

    // run Report - IonQuant (Labelfree)
    final boolean isIonquant = quantPanelLabelfree.isIonquant();
    final CmdIonquant cmdIonquant = new CmdIonquant(isReport && isIonquant, wd);

    conf.accept(cmdIonquant, null, () -> {
      if (cmdIonquant.isRun()) {
        return cmdIonquant.configure(
            parent, Paths.get(binMsfragger.getBin()), ramGbNonzero, quantPanelLabelfree.toMap(),
            sharedPepxmlFilesFromMsfragger, sharedMapGroupsToProtxml, threads);
      }
      return true;
    });

    final TmtiPanel tmtiPanel = Fragpipe.getStickyStrict(TmtiPanel.class);
    final boolean isTmt = tmtiPanel.isRun();
    final boolean isTmtLqFq = tmtiPanel.isRunFqLq();
    final CmdTmtIntegrator cmdTmt = new CmdTmtIntegrator(isTmt, wd);

    conf.accept(cmdTmt, null, () -> {
      // run TMT-Integrator
      if (isTmt) {
        if (sharedLcmsFiles.stream()
            .anyMatch(f -> !f.getPath().getFileName().toString().toLowerCase().endsWith(".mzml"))) {
          SwingUtils.showWarningDialog(parent,
              CmdTmtIntegrator.NAME + " only supports mzML files.\n"
                  + "Please remove other files from the input list.",
              CmdTmtIntegrator.NAME + "error");
          return false;
        }
        return cmdTmt.configure(tmtiPanel, isDryRun, ramGb, fastaFile, sharedMapGroupsToProtxml);
      }
      return true;
    });

    // run FreeQuant - as part of TMT-I
    final CmdFreequant cmdTmtFreequant = new CmdFreequant(isTmtLqFq, wd);

    conf.accept(cmdTmtFreequant, () -> {
      if (isTmtLqFq && isFreequant) {
        String msg =
            "<html>FreeQuant needs to be run needs to be run as part of TMT analysis.\n"
                + "You have chosen to run FreeQuant separately as weel.\n"
                + "This will interfere with FreeQuant files generated as part of TMT\n"
                + "processing."
                + "Please turn off standalone FreeQuant.\n";
        JOptionPane.showMessageDialog(parent, msg, "Warning", JOptionPane.WARNING_MESSAGE);
        return false;
      }
      return true;
    }, () -> {
      if (cmdTmtFreequant.isRun()) {
        String optsFq = tmtiPanel.getFreequantOptsAsText();
        return cmdTmtFreequant.configure(parent, usePhi, optsFq, sharedMapGroupsToProtxml);
      }
      return true;
    });


    // run LabelQuant - as part of TMT-I
    final CmdLabelquant cmdTmtLabelQuant = new CmdLabelquant(isTmtLqFq, wd);

    conf.accept(cmdTmtLabelQuant, null, () -> {
      if (cmdTmtLabelQuant.isRun()) {
        List<String> forbiddenOpts = Arrays.asList("--plex", "--annot", "--dir");
        String optsLq = tmtiPanel.getLabelquantOptsAsText();
        QuantLabel label = tmtiPanel.getSelectedLabel();
        Map<LcmsFileGroup, Path> annotations = tmtiPanel.getAnnotations();
        if (!cmdTmtLabelQuant
            .configure(parent, isDryRun, usePhi, optsLq, label, forbiddenOpts, annotations,
                sharedMapGroupsToProtxml)) {
          return false;
        }
      }
      return true;
    });

    // run PTMShepherd
    PtmshepherdPanel ptmsPanel = Fragpipe.getStickyStrict(PtmshepherdPanel.class);
    final boolean isRunShepherd = ptmsPanel.isRunShepherd();
    final boolean isPtmsFormValid = ;
    final CmdPtmshepherd cmdPtmshepherd = new CmdPtmshepherd(isRunShepherd, wd);

    conf.accept(cmdPtmshepherd, () -> {
      if (!ptmsPanel.validateForm()) {
        JOptionPane.showMessageDialog(parent,
            "There are errors in PTM-Shepherd configuraiton panel on Report tab.",
            "PTMShepherd Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
      return true;
    }, () -> {
      if (cmdPtmshepherd.isRun()) {
        Path fastaPath = Paths.get(fastaFile);
        Map<String, String> additionalShepherdParams = ptmsPanel.toPtmsParamsMap();
        if (threads > 0) {
          additionalShepherdParams.put("threads", Integer.toString(threads));
        }
        String massOffsets = tabMsf.getMassOffsets();
        if (!StringUtils.isNullOrWhitespace(massOffsets)) {
          additionalShepherdParams.put("mass_offsets", massOffsets);
        }
        Optional.ofNullable(tabMsf.getUiTextIsoErr().getNonGhostText())
            .filter(StringUtils::isNotBlank)
            .ifPresent(v -> additionalShepherdParams.put("isotope_error", v));
        if (!cmdPtmshepherd.configure(parent, isDryRun, Paths.get(binMsfragger.getBin()),
            ramGb, fastaPath, sharedMapGroupsToProtxml, additionalShepherdParams)) {
          return false;
        }
      }
      return true;
    });


    // run Spectral library generation
    final SpeclibPanel speclibPanel = Fragpipe.getStickyStrict(SpeclibPanel.class);
    final boolean isRunSpeclibgen = speclibPanel.isRunSpeclibgen();
    final boolean useEasypqp = speclibPanel.useEasypqp();
    final CmdSpecLibGen cmdSpecLibGen = new CmdSpecLibGen(isRunSpeclibgen, wd);
    if (cmdSpecLibGen.isRun()) {

      NoteConfigSpeclibgen speclibConf = Fragpipe.getStickyStrict(NoteConfigSpeclibgen.class);
      if (!speclibConf.isValid()) {
        JOptionPane.showMessageDialog(parent,
            "Spectral Library Generation scripts did not initialize correctly.",
            "Spectral Library Generation Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
      final SpecLibGen2 slg = speclibConf.instance;

      if (!cmdSpecLibGen.configure(parent, usePhi, jarPath, slg,
          sharedMapGroupsToProtxml, fastaFile, isRunProteinProphet, useEasypqp)) {
        return false;
      }
    }

    addToDepGraph(graphOrder, cmdStart);
    addToDepGraph(graphOrder, cmdUmpire, cmdStart);
    addToDepGraph(graphOrder, cmdMsAdjuster, cmdStart, cmdUmpire);
    addToDepGraph(graphOrder, cmdMsfragger, cmdStart, cmdUmpire, cmdMsAdjuster);

    addToDepGraph(graphOrder, cmdMsAdjusterCleanup, cmdMsAdjuster);
    addToDepGraph(graphOrder, cmdCrystalc, cmdMsfragger);
    addToDepGraph(graphOrder, cmdPeptideProphet, cmdMsfragger, cmdCrystalc);
    addToDepGraph(graphOrder, cmdProteinProphet, cmdPeptideProphet);
    //addToDepGraph(graphOrder, cmdPhilosopherDbAnnotate, cmdProteinProphet); // TODO: try removing that, it needs to be before Filter, but that's it no relation to Protein Prophet
    addToDepGraph(graphOrder, cmdPhilosopherFilter, cmdPhilosopherDbAnnotate);
    addToDepGraph(graphOrder, cmdFreequant, cmdPhilosopherFilter);
    addToDepGraph(graphOrder, cmdIprophet, cmdPhilosopherReport, cmdPeptideProphet);
    addToDepGraph(graphOrder, cmdPhilosopherAbacus, cmdPhilosopherReport, cmdIprophet,
        cmdProteinProphet);
    addToDepGraph(graphOrder, cmdIonquant, cmdFreequant);
    addToDepGraph(graphOrder, cmdTmtFreequant, cmdPhilosopherFilter, cmdIonquant);
    addToDepGraph(graphOrder, cmdTmtLabelQuant, cmdPhilosopherFilter, cmdTmtFreequant);
    addToDepGraph(graphOrder, cmdPhilosopherReport, cmdPhilosopherFilter, cmdFreequant,
        cmdTmtFreequant, cmdTmtLabelQuant);
    addToDepGraph(graphOrder, cmdTmt, cmdPhilosopherReport, cmdTmtFreequant, cmdTmtLabelQuant,
        cmdPhilosopherAbacus);
    addToDepGraph(graphOrder, cmdPtmshepherd, cmdPhilosopherReport, cmdTmt);
    addToDepGraph(graphOrder, cmdSpecLibGen, cmdPhilosopherReport, cmdPtmshepherd);

    // run pre-checks
    for (IConfig preConfig : preConfigs) {
      if (!preConfig.config()) {
        return false;
      }
    }

    // turn on all required dependencies
    addToDepGraph(graphDeps, cmdPhilosopherAbacus, cmdIprophet);
    addToDepGraph(graphDeps, cmdMsAdjuster, cmdMsAdjusterCleanup);
    addToDepGraph(graphDeps, cmdPhilosopherFilter, cmdPhilosopherDbAnnotate);
    addToDepGraph(graphDeps, cmdTmtFreequant, cmdPhilosopherFilter, cmdPhilosopherReport);
    addToDepGraph(graphDeps, cmdTmtLabelQuant, cmdPhilosopherFilter, cmdPhilosopherReport);
    addToDepGraph(graphDeps, cmdTmt, cmdPhilosopherFilter);

    List<CmdBase> startingPoints = Seq.seq(graphDeps.vertexSet())
        .filter(CmdBase::isRun) // those that are run
        .filter(cmd -> graphDeps.inDegreeOf(cmd) > 0) // and are dependent on something
        .toList();
    for (CmdBase startingPoint : startingPoints) {
      for (ClosestFirstIterator<CmdBase, DefaultEdge> it = new ClosestFirstIterator<>(graphDeps,
          startingPoint); it.hasNext(); ) {
        CmdBase cmdDependency = it.next();
        if (!cmdDependency.isRun()) {
          log.warn(
              "Command [{}] is a required dependency of [{}]. Will be run despite being switched off.",
              cmdDependency.getCmdName(), startingPoint);
          cmdDependency.isRun(true);
        }
      }
    }

    // run all configs, this will determine which commadns use Phi and in which directories
    for (CmdBase cmd : cmds) {
      IConfig config = cmd.getConfig();
      if (config != null && cmd.isRun()) {
        if (!config.config()) {
          return false;
        }
      }
    }
    // now the shared `usePhi` object should have all the paths where Phi is invoked.

    // special treatment of phi workspace
    // run Philosopher clean/init in all directories where Philosopher will be invoked
    for (Path pathPhiIsRunIn : usePhi.getWorkDirs()) {

      CmdPhilosopherWorkspaceCleanInit cmdPhiCleanInit = new CmdPhilosopherWorkspaceCleanInit(
          usePhi.isUsed(), pathPhiIsRunIn);
      cmdPhiCleanInit.configure(usePhi);
      addToDepGraph(graphOrder, cmdPhiCleanInit, cmdStart); // needs to run at start
      for (CmdBase cmd : cmds) {                            // and before any command that uses phi
        if (cmd.usesPhi()) {
          graphOrder.addEdge(cmdPhiCleanInit, cmd);
        }
      }

      CmdPhilosopherWorkspaceClean cmdPhiClean = new CmdPhilosopherWorkspaceClean(
          usePhi.isUsed(), pathPhiIsRunIn);
      cmdPhiClean.configure(usePhi);

      CmdBase lastPhiDependentCmd = null;
      for (TopologicalOrderIterator<CmdBase, DefaultEdge> it = new TopologicalOrderIterator<>(
          graphOrder); it.hasNext(); ) {
        CmdBase cmd = it.next();
        if (cmd.usesPhi()) {
          lastPhiDependentCmd = cmd;
        }
      }
      if (lastPhiDependentCmd != null) {
        // clean runs after last command that uses Phi
        log.debug("Determined that the last command in graph to use Phi is: [{}]",
            lastPhiDependentCmd.getCmdName());
        addToDepGraph(graphOrder, cmdPhiClean, lastPhiDependentCmd);
      } else {
        log.warn("No command was found to use Philosopher.");
      }
    }

    // make sure that all subfolders are created for groups/experiments
    if (!isDryRun) {
      List<Path> paths = Stream
          .concat(sharedPepxmlFiles.values().stream().flatMap(List::stream),
              sharedMapGroupsToProtxml.values().stream())
          .map(Path::getParent).collect(Collectors.toList());
      try {
        for (Path path : paths) {
          if (!Files.exists(path)) {
            Files.createDirectories(path);
          }
        }
      } catch (IOException e) {
        JOptionPane.showMessageDialog(parent,
            "Not all directories could be created:\n" + e.getMessage());
        return false;
      }
    }

    if (!configPhi.isValid()) {
      SwingUtils.showErrorDialog(parent, "Philosopher configuraiton invalid, check Config tab",
          "Config Error");
      return false;
    }

    return true;
  }

  private static boolean checkDbConfig(JComponent parent) {
    NoteConfigDatabase n;
    try {
      n = Fragpipe.getSticky(NoteConfigDatabase.class);
    } catch (NoStickyException e) {
      SwingUtils.showErrorDialog(parent, "Database not configured", "Database config error");
      return false;
    }

    String s = checkFasta(parent, n);

    if (n.numEntries == 0) {
      SwingUtils.showErrorDialog(parent, "Database looks to be empty", "Database config error");
      return false;
    }

    double decoysPercentage = (n.decoysCnt / (double) n.numEntries);
    if (decoysPercentage <= 0) {
      int confirm = SwingUtils.showConfirmDialog(parent, new JLabel(
          "<html>No decoys found in the FASTA file.<br/>\n" +
              "You have possibly set incorrect decoy tag. Check protein database tab.<br/><br/>\n" +
              "<br/>\n" +
              "You can also continue as-is, but FDR analysis will fail. Do you want to continue?"));
      if (JOptionPane.YES_OPTION != confirm) {
        return false;
      }
    } else if (decoysPercentage >= 1) {
      int confirm = SwingUtils.showConfirmDialog(parent, new JLabel(
          "<html>All FASTA entries seem to be decoys.<br/>\n" +
              "You have possibly set incorrect decoy tag. Check protein database tab.<br/><br/>\n" +
              "<br/>\n" +
              "You can also continue as-is, but FDR analysis will fail. Do you want to continue?"));
      if (JOptionPane.YES_OPTION != confirm) {
        return false;
      }
    } else if (decoysPercentage < 0.4 || decoysPercentage > 0.6) {
      DecimalFormat dfPct = new DecimalFormat("##.#'%'");
      int confirm = SwingUtils.showConfirmDialog(parent, new JLabel(
          "<html>FASTA file contains " + dfPct.format(decoysPercentage * 100) + ".<br/>\n" +
              "You have possibly set incorrect decoy tag. Check protein database tab.<br/><br/>\n" +
              "<br/>\n" +
              "You can also continue as-is, if that's what you're expected.</br>\n"
              + "Do you want to continue?"));
      if (JOptionPane.YES_OPTION != confirm) {
        return false;
      }
    }

    return true;
  }
}
