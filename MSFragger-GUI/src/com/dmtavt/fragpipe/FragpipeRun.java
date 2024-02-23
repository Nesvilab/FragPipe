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

package com.dmtavt.fragpipe;

import static com.dmtavt.fragpipe.messages.MessagePrintToConsole.toConsole;
import static com.dmtavt.fragpipe.tabs.TabDatabase.databaseSizeLimit;
import static com.dmtavt.fragpipe.tabs.TabWorkflow.manifestExt;
import static com.dmtavt.fragpipe.tabs.TabWorkflow.workflowExt;
import static com.dmtavt.fragpipe.tools.diann.DiannPanel.NEW_VERSION;
import static com.github.chhh.utils.FileDelete.deleteFileOrFolder;
import static com.github.chhh.utils.SwingUtils.wrapInScrollForDialog;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.IConfig;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.cmd.CmdAppendFile;
import com.dmtavt.fragpipe.cmd.CmdBase;
import com.dmtavt.fragpipe.cmd.CmdCheckCentroid;
import com.dmtavt.fragpipe.cmd.CmdCrystalc;
import com.dmtavt.fragpipe.cmd.CmdDiaPasefSCentric;
import com.dmtavt.fragpipe.cmd.CmdDiann;
import com.dmtavt.fragpipe.cmd.CmdFpopQuant;
import com.dmtavt.fragpipe.cmd.CmdFreequant;
import com.dmtavt.fragpipe.cmd.CmdIonquant;
import com.dmtavt.fragpipe.cmd.CmdIprophet;
import com.dmtavt.fragpipe.cmd.CmdLabelquant;
import com.dmtavt.fragpipe.cmd.CmdMSBooster;
import com.dmtavt.fragpipe.cmd.CmdMsfragger;
import com.dmtavt.fragpipe.cmd.CmdOPair;
import com.dmtavt.fragpipe.cmd.CmdPairScans;
import com.dmtavt.fragpipe.cmd.CmdPeptideProphet;
import com.dmtavt.fragpipe.cmd.CmdPercolator;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherAbacus;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherDbAnnotate;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherFilter;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherReport;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherWorkspaceClean;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherWorkspaceCleanInit;
import com.dmtavt.fragpipe.cmd.CmdProteinProphet;
import com.dmtavt.fragpipe.cmd.CmdPtmProphet;
import com.dmtavt.fragpipe.cmd.CmdPtmshepherd;
import com.dmtavt.fragpipe.cmd.CmdSpecLibGen;
import com.dmtavt.fragpipe.cmd.CmdStart;
import com.dmtavt.fragpipe.cmd.CmdTmtIntegrator;
import com.dmtavt.fragpipe.cmd.CmdUmpireSe;
import com.dmtavt.fragpipe.cmd.CmdWriteSubMzml;
import com.dmtavt.fragpipe.cmd.PbiBuilder;
import com.dmtavt.fragpipe.cmd.ProcessBuilderInfo;
import com.dmtavt.fragpipe.cmd.ProcessBuildersDescriptor;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.internal.DefEdge;
import com.dmtavt.fragpipe.messages.MessageClearConsole;
import com.dmtavt.fragpipe.messages.MessageManifestSave;
import com.dmtavt.fragpipe.messages.MessageRun;
import com.dmtavt.fragpipe.messages.MessageRunButtonEnabled;
import com.dmtavt.fragpipe.messages.MessageSDRFsave;
import com.dmtavt.fragpipe.messages.MessageSaveCache;
import com.dmtavt.fragpipe.messages.MessageSaveLog;
import com.dmtavt.fragpipe.messages.MessageSaveUiState;
import com.dmtavt.fragpipe.messages.MessageStartProcesses;
import com.dmtavt.fragpipe.messages.NoteConfigDatabase;
import com.dmtavt.fragpipe.messages.NoteConfigDiann;
import com.dmtavt.fragpipe.messages.NoteConfigIonQuant;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.dmtavt.fragpipe.messages.NoteConfigPhilosopher;
import com.dmtavt.fragpipe.messages.NoteConfigSpeclibgen;
import com.dmtavt.fragpipe.messages.NoteConfigSkyline;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.process.ProcessDescription;
import com.dmtavt.fragpipe.process.ProcessDescription.Builder;
import com.dmtavt.fragpipe.process.ProcessManager;
import com.dmtavt.fragpipe.process.RunnableDescription;
import com.dmtavt.fragpipe.tabs.TabDatabase;
import com.dmtavt.fragpipe.tabs.TabDownstream;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tabs.TabRun;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tabs.TabWorkflow.InputDataType;
import com.dmtavt.fragpipe.tools.crystalc.CrystalcPanel;
import com.dmtavt.fragpipe.tools.crystalc.CrystalcParams;
import com.dmtavt.fragpipe.tools.diann.DiannPanel;
import com.dmtavt.fragpipe.tools.diapasefscentric.DiaPasefSCentricPanel;
import com.dmtavt.fragpipe.tools.fpop.FpopQuantPanel;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerParams;
import com.dmtavt.fragpipe.tools.ionquant.QuantPanelLabelfree;
import com.dmtavt.fragpipe.tools.msbooster.MSBoosterPanel;
import com.dmtavt.fragpipe.tools.opair.OPairPanel;
import com.dmtavt.fragpipe.tools.opair.OPairParams;
import com.dmtavt.fragpipe.tools.pepproph.PepProphPanel;
import com.dmtavt.fragpipe.tools.percolator.PercolatorPanel;
import com.dmtavt.fragpipe.tools.philosopher.ReportPanel;
import com.dmtavt.fragpipe.tools.protproph.ProtProphPanel;
import com.dmtavt.fragpipe.tools.ptmprophet.PtmProphetPanel;
import com.dmtavt.fragpipe.tools.ptmshepherd.PTMSGlycanAssignPanel;
import com.dmtavt.fragpipe.tools.ptmshepherd.PtmshepherdPanel;
import com.dmtavt.fragpipe.tools.speclibgen.SpecLibGen2;
import com.dmtavt.fragpipe.tools.speclibgen.SpeclibPanel;
import com.dmtavt.fragpipe.tools.tmtintegrator.QuantLabel;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiPanel;
import com.dmtavt.fragpipe.tools.umpire.UmpirePanel;
import com.github.chhh.utils.MapUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.UsageTrigger;
import com.github.chhh.utils.swing.TextConsole;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jgrapht.Graph;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.traverse.ClosestFirstIterator;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.jooq.lambda.Seq;
import org.jooq.lambda.function.Consumer1;
import org.jooq.lambda.function.Consumer2;
import org.jooq.lambda.tuple.Tuple2;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeRun {

  private static final Logger log = LoggerFactory.getLogger(FragpipeRun.class);
  private static final Pattern[] filesToDelete = new Pattern[]{
      Pattern.compile(".+\\.pepXML"),
      Pattern.compile(".+\\.pin"),
      Pattern.compile(".+\\.pair"),
      Pattern.compile("db\\.bin.*"),
      Pattern.compile("meta\\.bin"),
      Pattern.compile("psm\\.bin"),
      Pattern.compile("ion\\.bin"),
      Pattern.compile("pep\\.bin"),
      Pattern.compile("pro\\.bin"),
      Pattern.compile("pepxml\\.bin"),
      Pattern.compile("protxml\\.bin"),
      Pattern.compile("razor\\.bin"),
      Pattern.compile("raw\\.bin"),
      Pattern.compile("lfq\\.bin"),
      Pattern.compile(".+temp-psm\\.tsv")
  };
  private static final Pattern fppdvDbPattern = Pattern.compile(".+\\.db");

  private FragpipeRun() {
  }

  public static int run(MessageRun m) {
    final TabWorkflow tabWorkflow = Bus.getStickyEvent(TabWorkflow.class);
    if (tabWorkflow == null) {
      throw new IllegalStateException("TabWorkflow has not been posted to the bus");
    }

    // Update the number of threads before running everything.
    ProcessManager processManager = Bus.getStickyEvent(ProcessManager.class);
    if (processManager == null) {
      throw new IllegalStateException("ProcessManager has not been posted to the bus");
    }
    processManager.setThreads(tabWorkflow.getThreads());

    log.debug("Started main FragpipeRun.run() method");
    Thread.setDefaultUncaughtExceptionHandler(Fragpipe::uncaughtExceptionHandler);

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

      final TabMsfragger tabMsf = Fragpipe.getStickyStrict(TabMsfragger.class);
      final PercolatorPanel percolatorPanel = Fragpipe.getStickyStrict(PercolatorPanel.class);
      final PepProphPanel peptideProphetPanel = Fragpipe.getStickyStrict(PepProphPanel.class);
      if (percolatorPanel.isRun() && (!tabMsf.getOutputType().valueInParamsFile().toLowerCase().contains("pin") || !tabMsf.getOutputType().valueInParamsFile().toLowerCase().contains("pepxml"))) {
        if (Fragpipe.headless) {
          log.error("Percolator was enabled but MSFragger's output formats did not contain pin or pepXML.");
        } else {
          JOptionPane.showMessageDialog(tabRun, "Percolator was enabled but MSFragger's output formats did not contain pin or pepXML.", "Errors", JOptionPane.ERROR_MESSAGE);
        }
        return 1;
      }
      if (peptideProphetPanel.isRun() && !tabMsf.getOutputType().valueInParamsFile().toLowerCase().contains("pepxml")) {
        if (Fragpipe.headless) {
          log.error("PeptideProphet was enabled but MSFragger's output formats did not contain pepXML.");
        } else {
          JOptionPane.showMessageDialog(tabRun, "PeptideProphet was enabled but MSFragger's output formats did not contain pepXML.", "Errors", JOptionPane.ERROR_MESSAGE);
        }
        return 1;
      }

      // workdir
      String wdStr = tabRun.getWorkdirText();
      Fragpipe.propsVarSet(ThisAppProps.PROP_FILE_OUT, wdStr);
      final Path wd = validateWd(tabRun, wdStr);
      if (wd == null) {
        log.debug("validateWd() failed");
        return 1;
      }

      if (!isDryRun) {
        Path preparedWd = prepareWd(tabRun, wd, tabWorkflow);
        if (preparedWd == null) {
          log.debug("prepareWd() failed");
          return 1;
        }

        // Delete FragPipe-PDV's *.db file before running anything
        try {
          Files.list(preparedWd).filter(Files::isRegularFile).filter(p -> fppdvDbPattern.matcher(p.getFileName().toString()).matches()).forEach(p -> {
            try {
              Files.deleteIfExists(p);
            } catch (Exception e) {
              throw new RuntimeException("Could not delete " + p.toAbsolutePath() + ". Please close all visualization (FragPipe-PDV) windows and try again.");
            }
          });
        } catch (Exception ex) {
          throw new RuntimeException(ex);
        }
      }

      // check input LCMS files
      final Map<String, LcmsFileGroup> lcmsFileGroups = tabWorkflow.getLcmsFileGroups();
      if (lcmsFileGroups == null) {
        log.debug("checkInputLcmsFiles1() failed");
        return 1;
      }
      final List<InputLcmsFile> inputLcmsFiles = checkInputLcmsFiles2(tabRun, lcmsFileGroups);
      if (inputLcmsFiles == null) {
        log.debug("checkInputLcmsFiles2() failed");
        return 1;
      }

      final Path jarPath = FragpipeLocations.get().getJarPath();
      if (jarPath == null) {
        if (Fragpipe.headless) {
          log.error("Could not get the URI of the currently running jar");
        } else {
          JOptionPane.showMessageDialog(tabRun, "Could not get the URI of the currently running jar", "Errors", JOptionPane.ERROR_MESSAGE);
        }
        return 1;
      }

      // check fasta file
      NoteConfigDatabase configDb = Bus.getStickyEvent(NoteConfigDatabase.class);
      if (configDb == null) {
        throw new IllegalStateException("NoteConfigDatabase has not been posted to the bus");
      }

      final String fastaPath = checkFasta(tabRun, configDb);
      if (fastaPath == null) {
        log.debug("checkFasta() failed");
        return 1;
      }

      if (!configDb.isBigDatabase && tabMsf.getNumDbSlices() > configDb.numEntries) {
        if (Fragpipe.headless) {
          log.error("Number of split database is larger than total proteins.");
        } else {
          JOptionPane.showMessageDialog(tabRun, "Number of split database is larger than total proteins.", "Errors", JOptionPane.ERROR_MESSAGE);
        }
        return 1;
      }

      final Graph<CmdBase, DefEdge> dag = new DirectedAcyclicGraph<>(DefEdge.class);
      // main call to generate all the process builders
      if (!configureTaskGraph(tabRun, wd, jarPath, isDryRun, fastaPath, dag)) {
        log.debug("createProcessBuilders() failed");
        return 1;
      }

      if (log.isDebugEnabled()) {
        TopologicalOrderIterator<CmdBase, DefEdge> it =
            new TopologicalOrderIterator<>(dag);
        String s = Seq.seq(it)
            .map(cmd -> String.format("Cmd: [%s], IsRun: [%s]", cmd.getCmdName(), cmd.isRun()))
            .toString("\n");
        log.debug("Ordered tasks:\n{}", s);
      }

      final List<ProcessBuildersDescriptor> pbDescsBuilderDescs = Seq
          .seq(new TopologicalOrderIterator<>(dag))
          .filter(CmdBase::isRun)
          .map(CmdBase::getBuilderDescriptor).toList();

      // =========================================================================================================
      toConsole(OsUtils.OsInfo() + "\n" + OsUtils.JavaInfo() + "\n" + OsUtils.NetCoreInfo() + "\n", tabRun.console);
      toConsole("", tabRun.console);
      toConsole("Version info:\n" + createVersionsString(), tabRun.console);
      toConsole("", tabRun.console);
      toConsole("LCMS files:\n" + createLcmsFilesString(lcmsFileGroups), tabRun.console);
      toConsole("", tabRun.console);

      // save manifest file in both GUI and headless mode
      Path pp = wd.resolve("fragpipe-files" + manifestExt);
      Bus.post(new MessageManifestSave(pp, true));

      saveRuntimeConfig(wd);


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

      toConsole(String.format(Locale.ROOT, "%d commands to execute:", pbis.size()), tabRun.console);
      for (final ProcessBuilderInfo pbi : pbis) {
        printProcessDescription(pbi, tabRun.console);
      }
      toConsole("~~~~~~~~~~~~~~~~~~~~~~", tabRun.console);
      toConsole("", tabRun.console);

      // execution order
//      String execOrder = Seq.seq(new TopologicalOrderIterator<>(dag)).map(o -> (CmdBase) o)
//          .filter(CmdBase::isRun)
//          .map(cmd -> String.format("Cmd: [%s], WorkDir: [%s]", cmd.getCmdName(), cmd.getWd()))
//          .toString("\n    ");
      toConsole("Execution order:\n", tabRun.console);
      Seq.seq(new TopologicalOrderIterator<>(dag))
          .filter(CmdBase::isRun)
          .forEach(cmd -> {
            toConsole(Fragpipe.COLOR_TOOL, String.format("    Cmd: [%s], ", cmd.getCmdName()), false, tabRun.console);
            toConsole(Fragpipe.COLOR_WORKDIR, String.format("Work dir: [%s]", cmd.getWd()), true, tabRun.console);
          });

      toConsole("", tabRun.console);
      toConsole("~~~~~~~~~~~~~~~~~~~~~~", tabRun.console);

      // Write top 20 lines of the fasta file to the console.
      final TabDatabase tabDatabase = Fragpipe.getStickyStrict(TabDatabase.class);
      toConsole("", tabRun.console);
      toConsole("~~~~~~Sample of " + tabDatabase.getFastaPath() + "~~~~~~~", tabRun.console);
      try {
        Path p = Paths.get(tabDatabase.getFastaPath());
        if (Files.size(p) < databaseSizeLimit) {
          List<String> proteinHeaders = Files.lines(p).filter(e -> e.startsWith(">")).sorted().collect(Collectors.toList());
          int gap = proteinHeaders.size() < 21 ? 1 : (proteinHeaders.size() - 1) / 20;    // make sure gap is always at least 1 to prevent an infinite loop
          int idx = 0;
          while (idx < proteinHeaders.size()) {
            toConsole(proteinHeaders.get(idx).trim(), tabRun.console);
            idx += gap;
          }
        } else {
          toConsole("The fasta file " + p.toAbsolutePath() + " is very large.\nSkip printing the head samples.", tabRun.console);
        }
      } catch (Exception e) {
        toConsole("Cannot get the sample of " + tabDatabase.getFastaPath(), tabRun.console);
        toConsole(ExceptionUtils.getStackTrace(e), tabRun.console);
      }
      toConsole("~~~~~~~~~~~~~~~~~~~~~~", tabRun.console);
      toConsole("", tabRun.console);

      // Write top annotation files to the console.
      final TmtiPanel tmtiPanel = Fragpipe.getStickyStrict(TmtiPanel.class);
      if (tmtiPanel.isRun()) {
        toConsole("~~~~~~annotation files~~~~~~~", tabRun.console);
        for (Path p : tmtiPanel.getAnnotations().values()) {
          try {
            toConsole(p.toFile().getCanonicalPath() + ":", tabRun.console);
            Files.lines(p.toFile().getCanonicalFile().toPath()).forEach(e -> toConsole(e.trim(), tabRun.console));
          } catch (Exception e) {
            toConsole("Cannot read " + p.toAbsolutePath(), tabRun.console);
            toConsole(ExceptionUtils.getStackTrace(e), tabRun.console);
          }
        }
        toConsole("~~~~~~~~~~~~~~~~~~~~~~", tabRun.console);
        toConsole("", tabRun.console);
      }

      if (isDryRun) {
        toConsole(Fragpipe.COLOR_RED_DARKEST, "\nIt's a dry-run, not running the commands.\n", true, tabRun.console);
        printReference(tabRun.console);
        return 0;
      }

      // save all the options to output dir
      // saveRuntimeConfig(wd);

      // print all the options to the screen as well
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      try {
        Fragpipe.propsUi().save(baos);
        toConsole("~~~~~~~~~ fragpipe.config ~~~~~~~~~", tabRun.console);
        toConsole(baos.toString(StandardCharsets.UTF_8.name()), tabRun.console);
        toConsole("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", tabRun.console);
      } catch (IOException e) {
        log.error("Could not collect form text representation for printing to console");
      }

      // run everything
      long startTime = System.nanoTime();
      final List<RunnableDescription> toRun = new ArrayList<>();
      for (final ProcessBuilderInfo pbi : pbis) {
        Runnable runnable = ProcessBuilderInfo.toRunnable(pbi, wd, FragpipeRun::printProcessDescription, tabRun.console, false);
        ProcessDescription.Builder b = new ProcessDescription.Builder().setName(pbi.name);
        if (pbi.pb.directory() != null) {
          b.setWorkDir(pbi.pb.directory().toString());
        }
        if (pbi.pb.command() != null && !pbi.pb.command().isEmpty()) {
          b.setCommand(String.join(" ", pbi.pb.command()));
        }
        toRun.add(new RunnableDescription(b.create(), runnable, pbi.parallelGroup, pbi));
      }

      // add finalizer process
      final Runnable finalizerRun = () -> {
        if (tabRun.isDeleteCalibratedFiles() || tabRun.isDeleteTempFiles()) {
          toConsole(Fragpipe.COLOR_TOOL, "\nDelete calibrated or temp files", true, tabRun.console);
        }

        if (tabRun.isDeleteCalibratedFiles()) {
          for (LcmsFileGroup lcmsFileGroup : lcmsFileGroups.values()) {
            for (InputLcmsFile inputLcmsFile : lcmsFileGroup.lcmsFiles) {
              String baseName = FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString());
              Path path = inputLcmsFile.getPath().getParent().resolve(baseName + "_calibrated.mzML");
              if (Files.exists(path)) {
                try {
                  toConsole(Fragpipe.COLOR_TOOL, "Delete ", false, tabRun.console);
                  toConsole(Fragpipe.COLOR_BLACK, path.toAbsolutePath().toString(), true, tabRun.console);
                  deleteFileOrFolder(path);
                } catch (Exception ex) {
                  toConsole(Fragpipe.COLOR_RED, "Could not delete " + path.toAbsolutePath() + ". It won't affect the result.", true, tabRun.console);
                }
              }
            }
          }
        }

        if (tabRun.isDeleteTempFiles()) {
          try {
            Files.walk(wd).filter(p -> {
              for (Pattern pattern : filesToDelete) {
                if (pattern.matcher(p.getFileName().toString()).matches()) {
                  return true;
                } else {
                  if (p.getFileName().toString().endsWith(".tsv")) {
                    for (LcmsFileGroup lcmsFileGroup : lcmsFileGroups.values()) {
                      for (InputLcmsFile inputLcmsFile : lcmsFileGroup.lcmsFiles) {
                        String baseName = FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString());
                        if (p.getFileName().toString().endsWith(baseName + ".tsv")) {
                          return true;
                        }
                      }
                    }
                  }
                }
              }
              return false;
            }).forEach(path -> {
              try {
                toConsole(Fragpipe.COLOR_TOOL, "Delete ", false, tabRun.console);
                toConsole(Fragpipe.COLOR_BLACK, path.toAbsolutePath().toString(), true, tabRun.console);
                deleteFileOrFolder(path);
              } catch (Exception ex) {
                toConsole(Fragpipe.COLOR_RED, "Could not delete " + path.toAbsolutePath() + ". It won't affect the result.", true, tabRun.console);
              }
            });
          } catch (Exception ex) {
            toConsole(Fragpipe.COLOR_RED, ExceptionUtils.getStackTrace(ex), true, tabRun.console);
          }
        }

        // save SDRF after run to be able to check MSFragger optimized params from log
        if (tabRun.isSaveSDRF()) {
          QuantLabel label = tmtiPanel.isRun() ? tmtiPanel.getSelectedLabel() : null;
          Path sdrfPath = wd.resolve("sdrf.tsv");
          Bus.post(new MessageSDRFsave(sdrfPath, true, label, tabRun.console.getText()));
        }

        if (tabRun.isWriteSubMzml()) { // write sub workflow and manifest files for the second-pass
          try {
            Float newFragmentMassTolerance = null;
            Integer newUseTopNPeaks = null;
            Float newMinimumRatio = null;
            Integer newIntensityTransform = null;
            Integer newRemovePrecursorPeak = null;
            Integer newMaxFragmentCharge = null;
            Float newPrecursorTrueTolerance = null;

            final String text = tabRun.console.getText().replaceAll("[^\n]+\u200B" + System.lineSeparator(), "");
            Matcher matcher = Pattern.compile("New fragment_mass_tolerance = ([\\d.]+) PPM").matcher(text);
            if (matcher.find()) {
              newFragmentMassTolerance = Float.parseFloat(matcher.group(1));
            }

            matcher = Pattern.compile("New use_topN_peaks = (\\d+)").matcher(text);
            if (matcher.find()) {
              newUseTopNPeaks = Integer.parseInt(matcher.group(1));
            }

            matcher = Pattern.compile("New minimum_ratio = ([\\d.]+)").matcher(text);
            if (matcher.find()) {
              newMinimumRatio = Float.parseFloat(matcher.group(1));
            }

            matcher = Pattern.compile("New intensity_transform = (\\d+)").matcher(text);
            if (matcher.find()) {
              newIntensityTransform = Integer.parseInt(matcher.group(1));
            }

            matcher = Pattern.compile("New remove_precursor_peak = (\\d+)").matcher(text);
            if (matcher.find()) {
              newRemovePrecursorPeak = Integer.parseInt(matcher.group(1));
            }

            matcher = Pattern.compile("New max_fragment_charge = (\\d+)").matcher(text);
            if (matcher.find()) {
              newMaxFragmentCharge = Integer.parseInt(matcher.group(1));
            }

            matcher = Pattern.compile("New precursor_true_tolerance = ([\\d.]+) PPM").matcher(text);
            if (matcher.find()) {
              newPrecursorTrueTolerance = Float.parseFloat(matcher.group(1));
            }

            Path workflowFilePath = wd.resolve("fragpipe" + workflowExt);
            Path workflowFileSecondPassPath = wd.resolve("fragpipe-second-pass" + workflowExt);
            if (Files.exists(workflowFilePath) && Files.isRegularFile(workflowFilePath) && Files.isReadable(workflowFilePath)) {
              BufferedReader reader = Files.newBufferedReader(workflowFilePath);
              BufferedWriter writer = Files.newBufferedWriter(workflowFileSecondPassPath);
              String line;
              while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("tab-run.write_sub_mzml")) {
                  line = "tab-run.write_sub_mzml=false";
                } else if (line.startsWith("percolator.run-percolator")) {
                  line = "percolator.run-percolator=true";
                } else if (line.startsWith("peptide-prophet.run-peptide-prophet")) {
                  line = "peptide-prophet.run-peptide-prophet=false";
                } else if (line.startsWith("phi-report.print-decoys")) {
                  line = "phi-report.print-decoys=false";
                } else if (line.startsWith("msfragger.calibrate_mass")) {
                  line = "msfragger.calibrate_mass=0";
                } else if (newFragmentMassTolerance != null && line.startsWith("msfragger.fragment_mass_tolerance")) {
                  line = "msfragger.fragment_mass_tolerance=" + newFragmentMassTolerance;
                } else if (newFragmentMassTolerance != null && line.startsWith("msfragger.fragment_mass_units")) {
                  line = "msfragger.fragment_mass_units=1";
                } else if (newUseTopNPeaks != null && line.startsWith("msfragger.use_topN_peaks")) {
                  line = "msfragger.use_topN_peaks=" + newUseTopNPeaks;
                } else if (newMinimumRatio != null && line.startsWith("msfragger.minimum_ratio")) {
                  line = "msfragger.minimum_ratio=" + newMinimumRatio;
                } else if (newIntensityTransform != null && line.startsWith("msfragger.intensity_transform")) {
                  line = "msfragger.intensity_transform=" + newIntensityTransform;
                } else if (newRemovePrecursorPeak != null && line.startsWith("msfragger.remove_precursor_peak")) {
                  line = "msfragger.remove_precursor_peak=" + newRemovePrecursorPeak;
                } else if (newMaxFragmentCharge != null && line.startsWith("msfragger.max_fragment_charge")) {
                  line = "msfragger.max_fragment_charge=" + newMaxFragmentCharge;
                } else if (newPrecursorTrueTolerance != null && line.startsWith("msfragger.precursor_true_tolerance")) {
                  line = "msfragger.precursor_true_tolerance=" + newPrecursorTrueTolerance;
                } else if (newPrecursorTrueTolerance != null && line.startsWith("msfragger.precursor_true_units")) {
                  line = "msfragger.precursor_true_units=1";
                } else if (line.startsWith("database.db-path")) {
                  line = "database.db-path=";
                } else if (line.startsWith("database.decoy-tag")) {
                  line = "database.decoy-tag=";
                } else if (line.startsWith("workdir=")) {
                  line = "workdir=";
                }
                
                writer.write(line + "\n");
              }
              writer.close();
              reader.close();
            }

            Path manifestFilePath = wd.resolve("fragpipe-files" + manifestExt);
            Path manifestFileSecondPassPath = wd.resolve("fragpipe-files-second-pass" + manifestExt);
            if (Files.exists(manifestFilePath) && Files.isRegularFile(manifestFilePath) && Files.isReadable(manifestFilePath)) {
              BufferedReader reader = Files.newBufferedReader(manifestFilePath);
              BufferedWriter writer = Files.newBufferedWriter(manifestFileSecondPassPath);
              String line;
              while ((line = reader.readLine()) != null) {
                line = line.trim();
                String[] parts = line.split("\t");
                parts[0] = wd.resolve(StringUtils.upToLastDot(Paths.get(parts[0]).getFileName().toString()) + "_sub.mzML").toAbsolutePath().toString();
                writer.write(String.join("\t", parts) + "\n");
              }
              writer.close();
              reader.close();
            }
          } catch (Exception ex) {
            ex.printStackTrace();
          }
        }

        NoteConfigSkyline noteConfigSkyline = Bus.getStickyEvent(NoteConfigSkyline.class);
        if (noteConfigSkyline.isValid()) {
          if (noteConfigSkyline.compareVersion("23.1.0.380") <= 0) {
            // 23.1 released version of Skyline requires moving the speclib file to where the DIANN report.tsv is located
            DiannPanel diannPanel = Bus.getStickyEvent(DiannPanel.class);
            if (diannPanel != null && diannPanel.isRun()) {
              try {
                Files.walk(wd).filter(p -> p.getFileName().toString().endsWith(".speclib")).forEach(p -> {
                  try {
                    Path ppp = wd.resolve("diann-output");
                    if (Files.exists(ppp) && Files.isDirectory(ppp)) {
                      Files.move(p, ppp.resolve("report.tsv.speclib"));
                    }
                  } catch (Exception ignored) {
                  }
                });
              } catch (Exception ignored) {
              }
            }
          }
        }

        printReference(tabRun.console);
        String totalTime = String.format("%.1f", (System.nanoTime() - startTime) * 1e-9 / 60);
        toConsole(Fragpipe.COLOR_RED_DARKEST, "\n=============================================================ALL JOBS DONE IN " + totalTime + " MINUTES=============================================================", true, tabRun.console);
        Bus.post(MessageSaveLog.saveInDir(wd));

        Bus.post(new MessageRunButtonEnabled(true));
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
    return 0;
  }

  private static void printReference(TextConsole console) {
    toConsole(Fragpipe.COLOR_RED_DARKEST, "\nPlease cite:", true, console);

    UmpirePanel umpirePanel = Bus.getStickyEvent(UmpirePanel.class);
    if (umpirePanel != null && umpirePanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(pseudo-MS/MS generation with DIA-Umpire) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "DIA-Umpire: comprehensive computational framework for data-independent acquisition proteomics. Nat Methods 12:258 (2015)", true, console);
    }

    TabMsfragger tabMsfragger = Bus.getStickyEvent(TabMsfragger.class);
    if (tabMsfragger != null && tabMsfragger.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Any searches) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry–based proteomics. Nat Methods 14:513 (2017)", true, console);
      toConsole(Fragpipe.COLOR_CMDLINE, "(Any searches) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Fast deisotoping algorithm and its implementation in the MSFragger search engine. J. Proteome Res. 20:498 (2021)", true, console);
      toConsole(Fragpipe.COLOR_CMDLINE, "(timsTOF PASEF) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Fast quantitative analysis of timsTOF PASEF data with MSFragger and IonQuant. Mol Cell Proteomics 19:1575 (2020)", true, console);
      toConsole(Fragpipe.COLOR_CMDLINE, "(Open search) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Identification of modified peptides using localization-aware open search. Nat Commun. 11:4065 (2020)", true, console);
      toConsole(Fragpipe.COLOR_CMDLINE, "(Glyco search) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Fast and comprehensive N- and O-glycoproteomics analysis with MSFragger-Glyco. Nat Methods 17:1125 (2020)", true, console);
      toConsole(Fragpipe.COLOR_CMDLINE, "(Labile search) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "MSFragger-Labile: A Flexible Method to Improve Labile PTM Analysis in Proteomics. Mol Cell Proteomics 22:100538 (2023)", true, console);
    }

    CrystalcPanel crystalcPanel = Bus.getStickyEvent(CrystalcPanel.class);
    if (crystalcPanel != null && crystalcPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Open search) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Crystal-C: A Computational tool for refinement of open search results. J. Proteome Res. 19.6:2511 (2020)", true, console);
    }

    MSBoosterPanel msBoosterPanel = Bus.getStickyEvent(MSBoosterPanel.class);
    if (msBoosterPanel != null && msBoosterPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(MSBooster) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "MSBooster: improving peptide identification rates using deep learning-based features. Nat Commun. 14:4539 (2023)", true, console);
    }

    PepProphPanel pepProphPanel = Bus.getStickyEvent(PepProphPanel.class);
    if (pepProphPanel != null && pepProphPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(PSM validation with PeptideProphet)", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Empirical statistical model to estimate the accuracy of peptide identifications made by MS/MS and database search. Anal. Chem. 74:5383 (2002)", true, console);
    }

    PercolatorPanel percolatorPanel = Bus.getStickyEvent(PercolatorPanel.class);
    if (percolatorPanel != null && percolatorPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(PSM validation with Percolator) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Semi-supervised learning for peptide identification from shotgun proteomics datasets. Nat Methods 4:923 (2007)", true, console);
    }

    PtmProphetPanel ptmProphetPanel = Bus.getStickyEvent(PtmProphetPanel.class);
    if (ptmProphetPanel != null && ptmProphetPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(PTM localization with PTMProphet)", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "PTMProphet: fast and accurate mass modification localization for the trans-proteomic pipeline\n. J. Proteome Res. 18:4262 (2019)", true, console);
    }

    ProtProphPanel protProphPanel = Bus.getStickyEvent(ProtProphPanel.class);
    if (protProphPanel != null && protProphPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Protein inference with ProteinProphet)", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "A statistical model for identifying proteins by tandem mass spectrometry. Anal. Chem. 75:4646 (2003)", true, console);
    }

    ReportPanel reportPanel = Bus.getStickyEvent(ReportPanel.class);
    if (reportPanel != null && reportPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(FDR filtering and reporting) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Philosopher: a versatile toolkit for shotgun proteomics data analysis. Nat Methods 17:869 (2020)", true, console);
    }

    PtmshepherdPanel ptmshepherdPanel = Bus.getStickyEvent(PtmshepherdPanel.class);
    if (ptmshepherdPanel != null && ptmshepherdPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Open search) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "PTM-Shepherd: analysis and summarization of post-translational and chemical modifications from open search results. Mol Cell Proteomics 20:100018 (2020)", true, console);
      toConsole(Fragpipe.COLOR_CMDLINE, "(Diagnostic ion mining) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Detecting diagnostic features in MS/MS spectra of post-translationally modified peptides. Nat Commun. 14:4132 (2023)", true, console);
    }

    PTMSGlycanAssignPanel ptmsGlycanAssignPanel = Bus.getStickyEvent(PTMSGlycanAssignPanel.class);
    if (ptmsGlycanAssignPanel != null && ptmsGlycanAssignPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Glycan identification and FDR control) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Multiattribute glycan identification and FDR control for Glycoproteomics. Mol Cell Proteomics 21:100105 (2022)", true, console);
    }

    OPairPanel oPairPanel = Bus.getStickyEvent(OPairPanel.class);
    if (oPairPanel != null && oPairPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(O-Glycan localization) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "O-Pair Search with MetaMorpheus for O-glycopeptide characterization. Nat Methods 17:1133 (2020)", true, console);
    }

    QuantPanelLabelfree quantPanelLabelfree = Bus.getStickyEvent(QuantPanelLabelfree.class);
    if (quantPanelLabelfree != null && quantPanelLabelfree.isRunIonQuant()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Label-free/isotopic-labeling quantification) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "IonQuant Enables Accurate and Sensitive Label-Free Quantification With FDR-Controlled Match-Between-Runs. Mol Cell Proteomics 20:100077 (2021)", true, console);
    }

    TmtiPanel tmtiPanel = Bus.getStickyEvent(TmtiPanel.class);
    if (tmtiPanel != null && tmtiPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Isobaric-labeling quantification) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Quantitative proteomic landscape of metaplastic breast carcinoma pathological subtypes and their relationship to triple-negative tumors. Nat Commun. 11:1723 (2020)", true, console);
    }

    SpeclibPanel speclibPanel = Bus.getStickyEvent(SpeclibPanel.class);
    DiannPanel diannPanel = Bus.getStickyEvent(DiannPanel.class);

    if ((speclibPanel != null && speclibPanel.isRun()) || (diannPanel != null && diannPanel.isRun())) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(Spectral library generation and DIA analysis) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Analysis of DIA proteomics data using MSFragger-DIA and FragPipe computational platform. Nat Commun. 14:4154 (2023)", true, console);
    }

    if (diannPanel != null && diannPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(DIA quantification with DIA-NN) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "dia-PASEF data analysis using FragPipe and DIA-NN for deep proteomics of low sample amounts. Nat Commun. 13:3944 (2022)", true, console);
    }

    FpopQuantPanel fpopQuantPanel = Bus.getStickyEvent(FpopQuantPanel.class);
    if (fpopQuantPanel != null && fpopQuantPanel.isRun()) {
      toConsole(Fragpipe.COLOR_CMDLINE, "(FPOP) ", false, console);
      toConsole(Fragpipe.COLOR_BLACK, "Efficient Analysis of Proteome-Wide FPOP Data by FragPipe. Anal Chem. 95:16131 (2023)", true, console);
    }

    toConsole(Fragpipe.COLOR_CMDLINE, "(Visualization with FragPipe-PDV) ", false, console);
    toConsole(Fragpipe.COLOR_BLACK, "PDV: an integrative proteomics data viewer. Bioinformatics. 35(7):1249 (2019)", true, console);
  }


  public static void saveRuntimeConfig(Path wd) {
    Path path = wd.resolve("fragpipe" + workflowExt);
    Bus.post(new MessageSaveUiState(path));
  }

  private static Path validateWd(JComponent parent, String workingDir) {
    if (StringUtils.isBlank(workingDir)) {
      if (Fragpipe.headless){
        log.error("Output directory can't be left empty.");
      } else {
        JOptionPane.showMessageDialog(parent, "Output directory can't be left empty.\n"
                + "Please select an existing directory for the output.", "Bad output directory path",
            JOptionPane.WARNING_MESSAGE);
      }
      return null;
    }
    Path testWdPath;
    try {
      testWdPath = Paths.get(workingDir);
    } catch (InvalidPathException e) {
      if (Fragpipe.headless){
        log.error("Output directory path is not a valid path.");
      } else {
        JOptionPane.showMessageDialog(parent, "Output directory path is not a valid path.\n"
                + "Please select a directory for the output.", "Bad output directory path",
            JOptionPane.WARNING_MESSAGE);
      }
      return null;
    }
    Pattern reWhitespace = Pattern.compile("\\s");
    if (reWhitespace.matcher(testWdPath.toString()).find()) {
      if (Fragpipe.headless) {
        log.error("Output directory path contains whitespace characters. Some programs in the pipeline might not work properly in this case. Please change output directory to one without spaces.");
      } else {
        JOptionPane.showMessageDialog(parent,
            "Output directory path contains whitespace characters.\n"
                + "Some programs in the pipeline might not work properly in this case.\n\n"
                + "Please change output directory to one without spaces.",
            "Bad output directory path", JOptionPane.ERROR_MESSAGE);
      }
      return null;
    }
    return testWdPath;
  }

  private static Path prepareWd(JComponent parent, Path wd, TabWorkflow tabWorkflow) {
    if (!Files.exists(wd)) {
      if (Fragpipe.headless){
        log.warn("Output directory doesn't exist. Creating it.");
      } else {
        int confirmCreation = JOptionPane.showConfirmDialog(parent, "Output directory doesn't exist. Create?", "Create output directory?", JOptionPane.OK_CANCEL_OPTION);
        if (JOptionPane.OK_OPTION != confirmCreation) {
          return null;
        }
      }

      try {
        Files.createDirectories(wd);
      } catch (Exception e) {
        // something went not right during creation of directory structure
        if (Fragpipe.headless) {
          log.error("Could not create directory structure. " + e.getMessage());
        } else {
          JOptionPane.showMessageDialog(parent, "Could not create directory structure.\n" + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
        }
        return null;
      }
    } else {
      try (Stream<Path> inWd = Files.list(wd)) {
        if (inWd.findAny().isPresent()) {
          if (Fragpipe.headless) {
            log.warn("The output directory is not empty. Some files might be overwritten in: " + wd);
          } else {
            int confirm = JOptionPane.showConfirmDialog(parent, "The output directory is not empty.\n\n"
                + "Some files might be overwritten in:\n"
                + " > " + wd + "\n\n"
                + "Do you want to proceed?", "Confirmation", JOptionPane.YES_NO_OPTION);
            if (JOptionPane.YES_OPTION != confirm) {
              return null;
            }
          }
        }
      } catch (Exception e) {
        if (Fragpipe.headless) {
          log.error(e.getMessage());
        } else {
          JOptionPane.showMessageDialog(parent, e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
        }
        return null;
      }
    }

    // make sure subdirs for experiments are created
    Set<Path> subdirs = tabWorkflow.getLcmsFileGroups().values().stream().map(g -> g.outputDir(wd)).collect(Collectors.toSet());
    for (Path subdir : subdirs) {
      if (!Files.exists(subdir)) {
        try {
          Files.createDirectories(subdir);
        } catch (IOException e) {
          if (Fragpipe.headless) {
            log.error("Could not create directory structure. " + e.getMessage());
          } else {
            JOptionPane.showMessageDialog(parent, "Could not create directory structure.\n" + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
          }
          return null;
        }
      }
    }

    return wd;
  }

  private static List<InputLcmsFile> checkInputLcmsFiles2(JComponent parent,
      Map<String, LcmsFileGroup> lcmsFileGroups) {
    final List<InputLcmsFile> lcmsFilesAll = lcmsFileGroups.values().stream()
        .flatMap(group -> group.lcmsFiles.stream())
        .collect(Collectors.toCollection(ArrayList::new));

    if (lcmsFilesAll.isEmpty()) {
      if (Fragpipe.headless) {
        log.error("No LC/MS data files selected.");
      } else {
        JOptionPane.showMessageDialog(parent, "No LC/MS data files selected.\n"
                + "Check 'Workflow' tab, 'Input LC/MS Files' section.", "Error",
            JOptionPane.WARNING_MESSAGE);
      }
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

        if (Fragpipe.headless) {
          log.error(sb.toString());
        } else {
          JOptionPane.showMessageDialog(parent, sb.toString(), "Input files with same names", JOptionPane.WARNING_MESSAGE);
        }

        return null;
      }
    }
    return lcmsFilesAll;
  }

  private static String checkFasta(JComponent parent, NoteConfigDatabase configDb) {
    if (configDb == null || configDb.path == null || StringUtils.isBlank(configDb.path.toString())) {
      if (Fragpipe.headless) {
        log.error("FASTA file path is empty or the file is corrupted");
      } else {
        JOptionPane.showMessageDialog(parent, "FASTA file path is empty or the file is corrupted", "Warning", JOptionPane.WARNING_MESSAGE);
      }
      return null;
    }
    final Path existing = PathUtils.existing(configDb.path.toString());
    if (existing == null) {
      if (Fragpipe.headless) {
        log.error(String.format("Could not find fasta file at: %s", configDb.path));
      } else {
        JOptionPane.showMessageDialog(parent, String.format("Could not find fasta file (Database) at:\n%s", configDb.path), "Errors", JOptionPane.ERROR_MESSAGE);
      }
      return null;
    }
    return existing.toString();
  }

  public static String createVersionsString() {
    String msfraggerVersion;
    String ionQuantVersion = NoteConfigIonQuant.version;
    String philosopherVersion;

    try {
      msfraggerVersion = Fragpipe.getStickyStrict(NoteConfigMsfragger.class).version;
      if (msfraggerVersion == null || msfraggerVersion.trim().isEmpty()) {
        throw new NullPointerException();
      }
    } catch (Exception e) {
      msfraggerVersion = "N/A";
    }

    if (ionQuantVersion == null || ionQuantVersion.trim().isEmpty()) {
      ionQuantVersion = "N/A";
    }

    try {
      philosopherVersion = Fragpipe.getStickyStrict(NoteConfigPhilosopher.class).version;
      if (philosopherVersion == null || philosopherVersion.trim().isEmpty()) {
        throw new NullPointerException();
      }
    } catch (Exception e) {
      philosopherVersion = "N/A";
    }

    StringBuilder sb = new StringBuilder();
    sb.append(Version.PROGRAM_TITLE).append(" version ").append(Version.version()).append("\n");
    sb.append("MSFragger version ").append(msfraggerVersion).append("\n");
    sb.append("IonQuant version ").append(ionQuantVersion).append("\n");
    sb.append("Philosopher version ").append(philosopherVersion).append("\n");

    return sb.toString();
  }

  private static String createLcmsFilesString(Map<String, LcmsFileGroup> lcmsFileGroups) {
    StringBuilder sb = new StringBuilder();
    for (Entry<String, LcmsFileGroup> e : lcmsFileGroups.entrySet()) {
      sb.append(String.format(Locale.ROOT, "  Experiment/Group: %s", e.getValue().name))
          .append("\n  (if \"spectral library generation\" is enabled, all files will be analyzed together)\n");
      for (InputLcmsFile f : e.getValue().lcmsFiles) {
        sb.append(String.format(Locale.ROOT, "  - %s\t%s", f.getPath().toString(), f.getDataType())).append("\n");
      }
    }
    return sb.toString();
  }

  public synchronized static void printProcessDescription(ProcessBuilderInfo pbi, TextConsole console) {
    if (!StringUtils.isNullOrWhitespace(pbi.name)) {
      toConsole(Fragpipe.COLOR_TOOL, pbi.name, false, console);
    }
    if (pbi.pb.directory() != null) {
      toConsole(Fragpipe.COLOR_WORKDIR, " [Work dir: " + pbi.pb.directory() + "]", false, console);
    }
    toConsole("", true, console);
    final String cmd = pbi.pb.command().stream()
            .map(e -> OsUtils.isUnix() && Pattern.matches(".*\\s.*", e) ? "\"" + e + "\"" : e) // insert quotes for arguments with whitespace
            .collect(Collectors.joining(" "));
    toConsole(Fragpipe.COLOR_CMDLINE, cmd, true, console);
  }

  private static enum DIRECTION {IN, OUT, BOTH}

  private static void addToGraph(Graph<? super CmdBase, DefEdge> graph, CmdBase node, DIRECTION direction,
      CmdBase... deps) {
    if (!graph.containsVertex(node)) {
      graph.addVertex(node);
    }
    for (CmdBase dep : deps) {
      if (!graph.containsVertex(dep)) {
        log.debug("Source node was not present when adding ordering edge, addint it: [{}]", dep);
        graph.addVertex(dep);
      }
      if (!graph.containsEdge(dep, node)) {
        switch (direction) {
          case IN:
            graph.addEdge(dep, node);
            break;
          case OUT:
            graph.addEdge(node, dep);
            break;
          case BOTH:
          default:
            throw new IllegalArgumentException("Direction both not allowed in this code");
        }
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
      boolean isDryRun, String fastaFile, final Graph<CmdBase, DefEdge> graphOrder) {

    // Collect input LCMS files
    final TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);

    // confirm with user that multi-experiment report is not needed
    final ProtProphPanel protProphPanel = Fragpipe.getStickyStrict(ProtProphPanel.class);
    final ReportPanel reportPanel = Fragpipe.getStickyStrict(ReportPanel.class);

    final NoteConfigPhilosopher configPhi = Fragpipe.getStickyStrict(NoteConfigPhilosopher.class);

    if (!configPhi.isValid()) {
      SwingUtils.showErrorDialog(parent, "Philosopher was not configured properly. Please check the config tab if you want to use it.", "Philosopher is not available");
    }

    final UsageTrigger usePhi = new UsageTrigger(configPhi.path, "Philosopher");

    // all the configurations are aggregated before being executed
    // because some commands might require others to run
    // (even though those commands might be turned off)
    final List<IConfig> checks = new ArrayList<>();
    final List<CmdBase> commands = new ArrayList<>();
    final Consumer1<IConfig> addCheck = (check) -> {
      if (check != null)
        checks.add(check);
    };
    final Consumer2<CmdBase, IConfig> addConfig = (cmd, config) -> {
      //new TaskGraphNode(cmd, config);
      commands.add(cmd);
      if (config != null) {
        cmd.setConfig(config);
      }
    };

    final Map<String, LcmsFileGroup> sharedLcmsFileGroups = new LinkedHashMap<>();
    final List<InputLcmsFile> sharedLcmsFiles = new ArrayList<>();
    final Map<String, LcmsFileGroup> sharedLcmsFileGroupsAll = new LinkedHashMap<>();
    final List<InputLcmsFile> sharedLcmsFilesAll = new ArrayList<>();

    // Add LCMS files
    final CmdStart cmdStart = new CmdStart(true, wd);
    addConfig.accept(cmdStart, () -> {
      MapUtils.refill(sharedLcmsFileGroupsAll, tabWorkflow.getLcmsFileGroups());
      sharedLcmsFilesAll.clear();
      sharedLcmsFilesAll.addAll(Seq.seq(sharedLcmsFileGroupsAll.values()).flatMap(group -> group.lcmsFiles.stream()).toList());
      if (sharedLcmsFilesAll.isEmpty()) {
        SwingUtils.showErrorDialog(parent, "No LCMS files provided.", "Add LCMS files");
        return false;
      }

      if (sharedLcmsFileGroupsAll.size() > 1) {
        for (String s : sharedLcmsFileGroupsAll.keySet()) {
          if (s.isEmpty()) {
            SwingUtils.showErrorDialog(parent, "There are multiple experimental groups, but one of them has empty name.", "Empty group name");
            return false;
          }
        }
      }

      // Don't include DIA-Quant and diaPASEF.
      for (Map.Entry<String, LcmsFileGroup> e : tabWorkflow.getLcmsFileGroups().entrySet()) {
        List<InputLcmsFile> ttt = e.getValue().lcmsFiles.stream().filter(f -> !f.getDataType().contentEquals("DIA-Quant")).collect(Collectors.toList());
        if (!ttt.isEmpty()) {
          sharedLcmsFileGroups.put(e.getKey(), new LcmsFileGroup(e.getValue().name, ttt));
        }
      }
      sharedLcmsFiles.clear();
      sharedLcmsFiles.addAll(Seq.seq(sharedLcmsFileGroups.values()).flatMap(group -> group.lcmsFiles.stream()).toList());
      if (sharedLcmsFiles.isEmpty()) {
        DiannPanel diannPanel = Fragpipe.getStickyStrict(DiannPanel.class);
        if (!diannPanel.isRun()) {
          SwingUtils.showErrorDialog(parent, "There are only DIA-Quant runs, but the DIA-NN quant was not enabled.", "Add LCMS files");
          return false;
        } else if (diannPanel.getLibraryPath().isEmpty()) {
          SwingUtils.showErrorDialog(parent, "There are only DIA-Quant runs, but there is no spectral library provided to the DIA-NN quant.", "Add LCMS files");
          return false;
        }
      }

      return true;
    });

    // Check if the scans are centroided.
    final TabMsfragger tabMsf = Fragpipe.getStickyStrict(TabMsfragger.class);
    final int ramGb = tabWorkflow.getRamGb() > 0 ? tabWorkflow.getRamGb() : OsUtils.getDefaultXmx();
    final int threads = tabWorkflow.getThreads();

    CmdCheckCentroid cmdCheckCentroid = new CmdCheckCentroid(true, wd);
    addConfig.accept(cmdCheckCentroid, () -> {
      cmdCheckCentroid.setRun(cmdCheckCentroid.isRun() && !sharedLcmsFiles.isEmpty());;
      if (cmdCheckCentroid.isRun()) {
        return cmdCheckCentroid.configure(jarPath, ramGb, threads, sharedLcmsFiles);
      }
      return true;
    });

    // run DIA-Umpire SE
    final NoteConfigMsfragger configMsfragger;
    try {
      configMsfragger = Fragpipe.getSticky(NoteConfigMsfragger.class);
    } catch (NoStickyException e) {
      SwingUtils.showErrorDialog(parent, "Looks like MSFragger was not configured.\nMSFragger is currently required.", "No MSFragger");
      return false;
    }

    if (!configMsfragger.isValid()) {
      SwingUtils.showErrorDialog(parent, "MSFragger was not configured properly. Please check the config tab.", "MSFragger is not available");
      return false;
    }

    final UsageTrigger binMsfragger = new UsageTrigger(configMsfragger.path, "MSFragger");

    final UmpirePanel umpirePanel = Fragpipe.getStickyStrict(UmpirePanel.class);
    final CmdUmpireSe cmdUmpire = new CmdUmpireSe(umpirePanel.isRun(), wd);
    addConfig.accept(cmdUmpire, () -> {
      cmdUmpire.setRun(cmdUmpire.isRun() && !sharedLcmsFiles.isEmpty());
      if (cmdUmpire.isRun()) {
        if (!cmdUmpire.configure(parent, isDryRun, jarPath, ramGb, Paths.get(binMsfragger.getBin()), umpirePanel, sharedLcmsFiles)) {
          return false;
        }
        List<InputLcmsFile> outputs = cmdUmpire.outputs(sharedLcmsFiles, umpirePanel.generateQ1(), umpirePanel.generateQ2(), umpirePanel.generateQ3());
        sharedLcmsFiles.clear();
        sharedLcmsFiles.addAll(outputs);
      }
      return true;
    });

    final DiaPasefSCentricPanel diaPasefSCentricPanel = Fragpipe.getStickyStrict(DiaPasefSCentricPanel.class);
    final CmdDiaPasefSCentric cmdDiaPasefSCentric = new CmdDiaPasefSCentric(diaPasefSCentricPanel.isRun(), wd);
    addConfig.accept(cmdDiaPasefSCentric, () -> {
      cmdDiaPasefSCentric.setRun(cmdDiaPasefSCentric.isRun() && !sharedLcmsFiles.isEmpty());
      if (cmdDiaPasefSCentric.isRun()) {
        if (!cmdDiaPasefSCentric.configure(
            parent,
            ramGb,
            threads,
            Paths.get(binMsfragger.getBin()),
            diaPasefSCentricPanel.writeIntermediateFiles(),
            diaPasefSCentricPanel.imTolerance(),
            diaPasefSCentricPanel.mzToleranceUnit(),
            diaPasefSCentricPanel.mzTolerance(),
            diaPasefSCentricPanel.apexScanDeltaRange(),
            diaPasefSCentricPanel.imToleranceMs1Ms2(),
            diaPasefSCentricPanel.apexScanDeltaRangeMs1Ms2(),
            sharedLcmsFiles)) {
          return false;
        }
        List<InputLcmsFile> outputs = cmdDiaPasefSCentric.outputs(sharedLcmsFiles);
        sharedLcmsFiles.clear();
        sharedLcmsFiles.addAll(outputs);
      }
      return true;
    });

    final TabRun tabRun = Bus.getStickyEvent(TabRun.class);
    if (tabRun == null) {
      throw new IllegalStateException("TabRun has not been posted to the bus");
    }

    // run MsFragger
    final TabDatabase tabDatabase = Fragpipe.getStickyStrict(TabDatabase.class);

    String ss = tabDatabase.checkFastaPath();
    if (ss != null) {
      SwingUtils.showErrorDialog(parent, ss, "Invalid FASTA file path");
      return false;
    }

    final String decoyTag = tabDatabase.getDecoyTag();
    MsfraggerParams p = tabMsf.getParams();
    final CmdMsfragger cmdMsfragger = new CmdMsfragger(tabMsf.isRun(), wd, p.getOutputFormat(), tabMsf.getOutputReportTopNDia1(), tabMsf.getOutputReportTopNDdaPlus());

    final Map<InputLcmsFile, List<Path>> sharedPepxmlFilesFromMsfragger = new TreeMap<>();
    final TreeMap<InputLcmsFile, List<Path>> sharedPepxmlFiles = new TreeMap<>();

    addConfig.accept(cmdMsfragger, () -> {
      cmdMsfragger.setRun(cmdMsfragger.isRun() && !sharedLcmsFiles.isEmpty());
      if (cmdMsfragger.isRun()) {
        if (!cmdMsfragger.configure(parent, isDryRun, jarPath, binMsfragger, fastaFile, tabMsf.getParams(), tabMsf.getNumDbSlices(), ramGb, sharedLcmsFiles, decoyTag, tabWorkflow.hasDataType("DDA"), tabWorkflow.hasDataType("DIA"), tabWorkflow.hasDataType("GPF-DIA"), tabWorkflow.hasDataType("DIA-Lib"), tabWorkflow.hasDataType("DDA+"), cmdUmpire.isRun(), cmdDiaPasefSCentric.isRun(), tabRun.isWriteSubMzml())) {
          return false;
        }

        if (!tabMsf.isLocalizeDeltaMass() && (tabMsf.isMassOffsetSearch() || tabMsf.isOpenSearch()) && !tabWorkflow.hasDataType("DIA") && !tabWorkflow.hasDataType("GPF-DIA") && !tabWorkflow.hasDataType("DDA+")) {
          if (Fragpipe.headless) {
            log.error("Mass-offset or open search with 'Localize mass shift (LOS)` was disabled. It is recommended to enable 'Localize mass shift (LOS)`.");
          } else {
            int confirmCreation = SwingUtils.showChoiceDialog(parent, "Warning",
                wrapInScrollForDialog(new JLabel("<html>Mass-offset or open search with <b>Localize mass shift (LOS)</b> disabled.<br>"
                + "It is recommended to enable it.<br><br>"
                + "Do you want to continue?")), new String[]{"Yes", "No"}, 1);
            if (confirmCreation == JOptionPane.NO_OPTION) {
              return false;
            }
          }
        }

        String warn = Fragpipe.propsVarGet(ThisAppProps.PROP_MGF_WARNING, Boolean.TRUE.toString());
        if (!Fragpipe.headless && Boolean.parseBoolean(warn)) {
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

      Map<InputLcmsFile, List<Path>> outputs = cmdMsfragger.outputs(sharedLcmsFiles, tabMsf.getOutputFileExt(), wd);
      MapUtils.refill(sharedPepxmlFilesFromMsfragger, outputs);
      MapUtils.refill(sharedPepxmlFiles, outputs);

      return true;
    });

    // run Crystalc
    CrystalcPanel crystalcPanel = Fragpipe.getStickyStrict(CrystalcPanel.class);
    final CmdCrystalc cmdCrystalc = new CmdCrystalc(crystalcPanel.isRun(), wd);

    addConfig.accept(cmdCrystalc, () -> {
      cmdCrystalc.setRun(cmdCrystalc.isRun() && !sharedPepxmlFiles.isEmpty());
      if (cmdCrystalc.isRun()) {
        CrystalcParams ccParams = crystalcPanel.toParams();
        if (threads > 0) {
          ccParams.setThread(threads);
        }
        if (!cmdCrystalc.configure(parent, jarPath, isDryRun, Paths.get(binMsfragger.getBin()), "pepXML", ramGb, ccParams, fastaFile, sharedPepxmlFiles)) {
          return false;
        }
        Map<InputLcmsFile, List<Path>> outputs = cmdCrystalc
            .outputs(sharedPepxmlFiles, tabMsf.getOutputFileExt());
        sharedPepxmlFiles.clear();
        sharedPepxmlFiles.putAll(outputs);
      }
      return true;
    });

    // Run MSBooster
    final MSBoosterPanel MSBoosterPanel = Fragpipe.getStickyStrict(MSBoosterPanel.class);
    final CmdMSBooster cmdMSBooster = new CmdMSBooster(MSBoosterPanel.isRun(), wd);
    addConfig.accept(cmdMSBooster, () -> {
      cmdMSBooster.setRun(cmdMSBooster.isRun() && !sharedPepxmlFilesFromMsfragger.isEmpty());
      if (cmdMSBooster.isRun()) {
        return cmdMSBooster.configure(parent, ramGb, threads, sharedPepxmlFilesFromMsfragger, MSBoosterPanel.predictRt(), MSBoosterPanel.predictSpectra(), MSBoosterPanel.useCorrelatedFeatures(), tabWorkflow.hasDataType("DDA"), tabWorkflow.hasDataType("DIA"), tabWorkflow.hasDataType("GPF-DIA"), tabWorkflow.hasDataType("DIA-Lib"), tabWorkflow.hasDataType("DDA+"), cmdUmpire.isRun(), cmdDiaPasefSCentric.isRun(), tabMsf.isOpenSearch(), MSBoosterPanel.rtModel(), MSBoosterPanel.spectraModel());
      }
      return true;
    });

    // run PeptideProphet
    final PepProphPanel pepProphPanel = Fragpipe.getStickyStrict(PepProphPanel.class);
    final boolean isRunPeptideProphet = pepProphPanel.isRun();
    final boolean isCombinedPepxml = pepProphPanel.isCombinePepxml();

    final CmdPeptideProphet cmdPeptideProphet = new CmdPeptideProphet(isRunPeptideProphet, wd);

    final PercolatorPanel percolatorPanel = Fragpipe.getStickyStrict(PercolatorPanel.class);

    addCheck.accept(() -> {
      if (cmdPeptideProphet.isRun()) {
        return checkDbConfig(parent, percolatorPanel.isRun(), isRunPeptideProphet, reportPanel.isRun());
      }
      return true;
    });

    final Map<InputLcmsFile, List<Path>> sharedPepxmlFilesBeforePeptideValidation = new HashMap<>();
    addConfig.accept(cmdPeptideProphet, () -> {
      cmdPeptideProphet.setRun(cmdPeptideProphet.isRun() && !sharedPepxmlFiles.isEmpty());
      sharedPepxmlFilesBeforePeptideValidation.putAll(sharedPepxmlFiles);
      if (cmdPeptideProphet.isRun()) {
        final String pepProphCmd = pepProphPanel.getCmdOpts();
        final String enzymeName = tabMsf.getEnzymeName();
        if (!cmdPeptideProphet.configure(parent, usePhi, jarPath, isDryRun,
            fastaFile, decoyTag, pepProphCmd, isCombinedPepxml, enzymeName, sharedPepxmlFiles, tabMsf.isWriteCalMzml() && tabMsf.getMassCalibration() > 0, threads)) {
          return false;
        }
      }
      if(pepProphPanel.isSelected()) {
        Map<InputLcmsFile, List<Path>> pepProphOutputs = cmdPeptideProphet
                .outputs(sharedPepxmlFiles, tabMsf.getOutputFileExt(), isCombinedPepxml);
        sharedPepxmlFiles.clear();
        sharedPepxmlFiles.putAll(pepProphOutputs);
      }
      return true;
    });

    // run Percolator
    final boolean isRunPercolator = percolatorPanel.isRun();
    final boolean isCombinedPepxml_percolator = percolatorPanel.isCombinePepxml();

    final CmdPercolator cmdPercolator = new CmdPercolator(isRunPercolator, wd);

    addCheck.accept(() -> {
      if (cmdPercolator.isRun()) {
        return checkDbConfig(parent, percolatorPanel.isRun(), isRunPeptideProphet, reportPanel.isRun());
      }
      return true;
    });

    addConfig.accept(cmdPercolator, () -> {
      cmdPercolator.setRun(cmdPercolator.isRun() && !sharedPepxmlFilesBeforePeptideValidation.isEmpty());
      if (cmdPercolator.isRun()) {
        final String percolatorCmd = percolatorPanel.getCmdOpts();
        if (!cmdPercolator.configure(parent, jarPath, percolatorCmd, isCombinedPepxml_percolator, sharedPepxmlFilesBeforePeptideValidation, crystalcPanel.isRun(), percolatorPanel.getMinProb(), decoyTag, tabMsf.isWriteCalMzml() && tabMsf.getMassCalibration() > 0, tabRun.isWriteSubMzml())) {
          return false;
        }
      }
      if (percolatorPanel.isSelected()) {
        Map<InputLcmsFile, List<Path>> percolatorOutputs = cmdPercolator.outputs(sharedPepxmlFilesBeforePeptideValidation, tabMsf.getOutputFileExt(), isCombinedPepxml_percolator);
        sharedPepxmlFilesBeforePeptideValidation.putAll(sharedPepxmlFiles);
        sharedPepxmlFiles.clear();
        sharedPepxmlFiles.putAll(percolatorOutputs);
      }
      return true;
    });

    // Run PTM-Prophet.
    final PtmProphetPanel panelPtmProphet = Fragpipe.getStickyStrict(PtmProphetPanel.class);
    final CmdPtmProphet cmdPtmProphet = new CmdPtmProphet(panelPtmProphet.isRun(), wd);
    addConfig.accept(cmdPtmProphet, () -> {
      // PeptideProphet is run, so we run adjustments of the pepxml files.
      List<Tuple2<InputLcmsFile, Path>> lcmsToPepxml = Seq.seq(sharedPepxmlFiles)
          .flatMap(tuple -> tuple.v2.stream().map(o -> new Tuple2<>(tuple.v1, o)))
          .toList();

      cmdPtmProphet.setRun(cmdPtmProphet.isRun() && !lcmsToPepxml.isEmpty());
      if (cmdPtmProphet.isRun()) {
        return cmdPtmProphet.configure(parent, panelPtmProphet.getCmdLineOpts(), lcmsToPepxml, threads);
      }
      return true;
    });

    // run ProteinProphet
    final boolean isRunProteinProphet = protProphPanel.isRun();
    final TreeMap<LcmsFileGroup, Path> sharedMapGroupsToProtxml = new TreeMap<>();

    final CmdProteinProphet cmdProteinProphet = new CmdProteinProphet(isRunProteinProphet, wd);

    addConfig.accept(cmdProteinProphet, () -> {
      cmdProteinProphet.setRun(cmdProteinProphet.isRun() && !sharedPepxmlFiles.isEmpty());
      if (cmdProteinProphet.isRun()) {
        final String protProphCmdStr = protProphPanel.getCmdOpts();
        if (!cmdProteinProphet.configure(parent, usePhi, protProphCmdStr, sharedPepxmlFiles)) {
          return false;
        }
      }
      Map<LcmsFileGroup, Path> outputs = cmdProteinProphet.outputs(sharedPepxmlFiles);
      MapUtils.refill(sharedMapGroupsToProtxml, outputs);
      return true;
    });

    final QuantPanelLabelfree quantPanelLabelfree = Fragpipe.getStickyStrict(QuantPanelLabelfree.class);

    // run Report - DbAnnotate
    final CmdPhilosopherDbAnnotate cmdPhilosopherDbAnnotate = new CmdPhilosopherDbAnnotate(reportPanel.isRun(), wd);

    addConfig.accept(cmdPhilosopherDbAnnotate, () -> {
      cmdPhilosopherDbAnnotate.setRun(cmdPhilosopherDbAnnotate.isRun() && !sharedPepxmlFiles.isEmpty());
      if (cmdPhilosopherDbAnnotate.isRun()) {
        return cmdPhilosopherDbAnnotate
            .configure(parent, ramGb, threads, usePhi, fastaFile, decoyTag, sharedPepxmlFiles.firstKey());
      }
      return true;
    });

    // run Report - Filter
    final CmdPhilosopherFilter cmdPhilosopherFilter = new CmdPhilosopherFilter(reportPanel.isRun(), wd);

    addCheck.accept(() -> {
      if (cmdPhilosopherFilter.isRun()) {
        return checkDbConfig(parent, percolatorPanel.isRun(), isRunPeptideProphet, reportPanel.isRun());
      }
      return true;
    });

    addConfig.accept(cmdPhilosopherFilter, () -> {
      cmdPhilosopherFilter.setRun(cmdPhilosopherFilter.isRun() && !sharedPepxmlFiles.isEmpty());
      if (cmdPhilosopherFilter.isRun()) {
        final boolean isCheckFilterNoProtxml = reportPanel.isNoProtXml();

        // if ProteinProphet is not run but prot.xml is there - query the user
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
            // ProteinProphet is not run, but all prot.xml are there
            if (Fragpipe.headless) {
              log.warn("ProteinProphet is not run, but prot.xml files for all groups do already exist: " + paths);
              dontUseProtxmlInFilter = false;
            } else {
              int confirm = JOptionPane.showConfirmDialog(parent,
                  "ProteinProphet is not run, but prot.xml files for all groups do already exist:\n\n"
                      + paths
                      + "\n\n"
                      + "Do you want to use them for the Filter command?\n",
                  "Use previously existing prot.xml files?\n", JOptionPane.YES_NO_OPTION);
              if (JOptionPane.YES_OPTION == confirm) {
                dontUseProtxmlInFilter = false;
              }
            }
          }
        } else {
          // ProteinProphet is run, respect the checkFilterNoProtxml checkbox
          dontUseProtxmlInFilter = isCheckFilterNoProtxml;
        }

        return cmdPhilosopherFilter.configure(parent, ramGb, threads, usePhi,
            decoyTag, reportPanel.getFilterCmdText(), dontUseProtxmlInFilter,
            sharedMapGroupsToProtxml, sharedPepxmlFiles.firstKey());
      }
      return true;
    });

    final TmtiPanel tmtiPanel = Fragpipe.getStickyStrict(TmtiPanel.class);

    // run Report - Report command itself
    final CmdPhilosopherReport cmdPhilosopherReport = new CmdPhilosopherReport(reportPanel.isRun() || quantPanelLabelfree.isRunFreeQuant(), wd);
    final boolean doPrintDecoys = reportPanel.isPrintDecoys();
    final boolean doMSstats = reportPanel.isMsstats() && !quantPanelLabelfree.isRunIonQuant(); // Don't let Philosopher generate MSstats files if IonQuant is going to run because IonQuant will generate them.

    addConfig.accept(cmdPhilosopherReport, () -> {
      cmdPhilosopherReport.setRun(cmdPhilosopherReport.isRun() && !sharedMapGroupsToProtxml.isEmpty());
      if (cmdPhilosopherReport.isRun()) {
        return cmdPhilosopherReport.configure(parent, ramGb, threads, usePhi, doPrintDecoys, doMSstats, sharedLcmsFileGroups.size() > 1, reportPanel.isRemoveContaminants(), sharedMapGroupsToProtxml);
      }
      return true;
    });

    // run Report - Multi-Experiment report
    final CmdPhilosopherAbacus cmdPhilosopherAbacus = new CmdPhilosopherAbacus(false, wd);
    addConfig.accept(cmdPhilosopherAbacus, () -> {
      final boolean doRunAbacus = cmdPhilosopherReport.isRun() && (sharedLcmsFileGroups.size() > 1) && !quantPanelLabelfree.isRunIonQuant() && (doMSstats || (!reportPanel.isNoProtXml() && reportPanel.isProtSummary()) || reportPanel.isPepSummary());
      cmdPhilosopherAbacus.setRun(doRunAbacus);
      if (cmdPhilosopherAbacus.isRun()) {
        int plex = 0;
        if (tmtiPanel.isRun() && tmtiPanel.getIntensityExtractionTool() == 1) {
          QuantLabel label = tmtiPanel.getSelectedLabel();
          plex = label.getReagentNames().size();
        }
        return cmdPhilosopherAbacus.configure(parent, usePhi, reportPanel.getFilterCmdText(), reportPanel.isProtSummary(), reportPanel.isPepSummary(), reportPanel.isNoProtXml(), decoyTag, plex, sharedMapGroupsToProtxml);
      }
      return true;
    });


    final CmdIprophet cmdIprophet = new CmdIprophet(false, wd);
    addConfig.accept(cmdIprophet, () -> {
      cmdIprophet.setRun(cmdPhilosopherAbacus.isRun() && !quantPanelLabelfree.isRunIonQuant() && reportPanel.isPepSummary());
      if (cmdIprophet.isRun()) {
        return cmdIprophet.configure(parent, usePhi, decoyTag, threads, sharedPepxmlFiles);
      }
      return true;
    });

    // run Report - Freequant (Labelfree)
    final CmdFreequant cmdFreequant = new CmdFreequant(quantPanelLabelfree.isRunFreeQuant(), wd);
    addConfig.accept(cmdFreequant, () -> {
      cmdFreequant.setRun(cmdFreequant.isRun() && !sharedMapGroupsToProtxml.isEmpty());
      if (cmdFreequant.isRun()) {
        return cmdFreequant.configure(parent, usePhi, quantPanelLabelfree.getFreequantOptsAsText(), sharedMapGroupsToProtxml, tmtiPanel.isRun(), tabMsf.isOpenSearch());
      }
      return true;
    });

    // PTM-S glycan assignment can assign additional masses to PSMs. Append those masses to the mass list file after PTM-S is run for IonQuant (including IonQuant for MS2 quant extraction)
    PTMSGlycanAssignPanel ptmsGlycanPanel = Fragpipe.getStickyStrict(PTMSGlycanAssignPanel.class);
    final CmdAppendFile cmdAppendFile = new CmdAppendFile(ptmsGlycanPanel.isRun() && (quantPanelLabelfree.isRunIonQuant() || (tmtiPanel.isRun() && tmtiPanel.getIntensityExtractionTool() == 0)), wd);
    addConfig.accept(cmdAppendFile,  () -> {
      cmdAppendFile.setRun(cmdAppendFile.isRun() && !sharedPepxmlFilesFromMsfragger.isEmpty());
      if (cmdAppendFile.isRun()) {
        return cmdAppendFile.configure(parent, jarPath,"modmasses_ionquant.txt", "ptm-shepherd-output/glyco_masses_list.txt");
      }
      return true;
    });

    // run Report - IonQuant (Labelfree)
    final CmdIonquant cmdIonquant = new CmdIonquant(quantPanelLabelfree.isRunIonQuant(), wd);
    if (quantPanelLabelfree.isChecked() && quantPanelLabelfree.isIonQuantChecked() && (!quantPanelLabelfree.isIonQuantEnabled() || !quantPanelLabelfree.isCheckRunEnabled()) && !tabWorkflow.hasDataType("DIA") && !tabWorkflow.hasDataType("GPF-DIA") && !tabWorkflow.hasDataType("DIA-Quant") && !tabWorkflow.hasDataType("DIA-Lib")) {
      SwingUtils.showErrorDialog(parent, "Looks like IonQuant was not configured.\nIonQuant is currently required.", "No IonQuant");
      return false;
    }

    final UsageTrigger binIonQuant = new UsageTrigger(NoteConfigIonQuant.path, "IonQuant");

    Set<Float> modMassSet = new TreeSet<>();
    modMassSet.addAll(tabMsf.getVarModMassSet());
    modMassSet.addAll(tabMsf.getFixedModMassSet());
    modMassSet.addAll(tabMsf.getMassOffsetSet());

    if (cmdIonquant.isRun()) {
      final NoteConfigIonQuant configIonQuant;
      try {
        configIonQuant = Fragpipe.getSticky(NoteConfigIonQuant.class);
      } catch (NoStickyException e) {
        SwingUtils.showErrorDialog(parent, "Looks like IonQuant was not configured.\nIonQuant is currently required.", "No IonQuant");
        return false;
      }

      if (!configIonQuant.isValid()) {
        SwingUtils.showErrorDialog(parent, "Looks like IonQuant was not configured.\nIonQuant is currently required.", "IonQuant not available");
        return false;
      }

      addConfig.accept(cmdIonquant,  () -> {
        cmdIonquant.setRun(cmdIonquant.isRun() && !sharedPepxmlFilesFromMsfragger.isEmpty());
        if (cmdIonquant.isRun()) {
          OPairPanel oPairPanel = Bus.getStickyEvent(OPairPanel.class);
          if (oPairPanel == null) {
            throw new IllegalStateException("OPairPanel has not been posted to the bus");
          }

          return cmdIonquant.configure(parent, Paths.get(binMsfragger.getBin()), Paths.get(binIonQuant.getBin()), ramGb, quantPanelLabelfree.toMap(), tabWorkflow.getInputDataType(), sharedPepxmlFilesFromMsfragger, sharedMapGroupsToProtxml, threads, oPairPanel.isRun() ? null : modMassSet, isDryRun, true, true);
        }
        return true;
      });
    }

    // run TMT-Integrator
    final CmdTmtIntegrator cmdTmt = new CmdTmtIntegrator(tmtiPanel.isRun(), wd);
    addConfig.accept(cmdTmt, () -> {
      if (tmtiPanel.isRun() && !sharedMapGroupsToProtxml.isEmpty()) {
        if (sharedLcmsFiles.stream().anyMatch(f -> !f.getPath().getFileName().toString().toLowerCase().endsWith(".mzml") && !f.getPath().getFileName().toString().toLowerCase().endsWith(".raw"))) {
          SwingUtils.showWarningDialog(parent, CmdTmtIntegrator.NAME + " only supports mzML and raw files.\nPlease remove other files from the input list.", CmdTmtIntegrator.NAME + " error");
          return false;
        }
        return cmdTmt.configure(tmtiPanel, isDryRun, ramGb, decoyTag, sharedMapGroupsToProtxml, doMSstats, tmtiPanel.getAnnotations(), false);
      }
      return true;
    });

    final CmdIonquant cmdTmtIonquant = new CmdIonquant(tmtiPanel.isRun() && tmtiPanel.getIntensityExtractionTool() == 0, wd);
    cmdTmtIonquant.setTitle(CmdIonquant.NAME + " MS1 (TMT)");

    final CmdIonquant cmdTmtIonquantIsobaric = new CmdIonquant(tmtiPanel.isRun() && tmtiPanel.getIntensityExtractionTool() == 0, wd);
    cmdTmtIonquantIsobaric.setTitle(CmdIonquant.NAME + " Isobaric (TMT)");

    final CmdFreequant cmdTmtFreequant = new CmdFreequant(tmtiPanel.isRun() && tmtiPanel.getIntensityExtractionTool() == 1, wd);
    cmdTmtFreequant.setTitle(CmdFreequant.NAME + " (TMT)");

    final CmdLabelquant cmdTmtLabelQuant = new CmdLabelquant(tmtiPanel.isRun() && tmtiPanel.getIntensityExtractionTool() == 1, wd);
    cmdTmtLabelQuant.setTitle(CmdLabelquant.NAME + " (TMT)");

    if (tmtiPanel.isRun()) {
      final TabDownstream tabDownstream = Fragpipe.getStickyStrict(TabDownstream.class);
      addCheck.accept(() -> {
        if (!tmtiPanel.isRun()) {
          if (!tabDownstream.pFpop.isRunFpopQuant()){
            return true;
          } else {
            if (!tabDownstream.pFpop.isFpopTmt()){
              return true;
            }
          }
        }
        Map<LcmsFileGroup, Path> annotations = tmtiPanel.getAnnotations();
        boolean hasMissing = Seq.seq(annotations.values()).map(PathUtils::existing).anyMatch(Objects::isNull);
        if (hasMissing) {
          SwingUtils.showErrorDialog(parent,
              "Not all TMT groups have annotation files set.\n"
                  + "Check <b>Quant (Isobaric)</b> tab, <b>TMT-Integrator config</b>, or <b>Downstream tab (FPOP Quant)</b>.", "TMT-Integrator config error");
          return false;
        }
        return true;
      });

      QuantLabel label = tmtiPanel.getSelectedLabel();
      String quantLevel = tmtiPanel.getQuantLevel();
      int tolerance = tmtiPanel.getTolerance();
      double minprob = tmtiPanel.getMinprob();
      double purity = tmtiPanel.getPurity();
      double minIntensityPercant = tmtiPanel.getMinIntensityPercent();
      Map<LcmsFileGroup, Path> annotations = tmtiPanel.getAnnotations();

      if (tmtiPanel.getIntensityExtractionTool() == 0) {
        addConfig.accept(cmdTmtIonquant, () -> {
          cmdTmtIonquant.setRun(!sharedPepxmlFilesFromMsfragger.isEmpty());
          if (cmdTmtIonquant.isRun()) {
            OPairPanel oPairPanel = Bus.getStickyEvent(OPairPanel.class);
            if (oPairPanel == null) {
              throw new IllegalStateException("OPairPanel has not been posted to the bus");
            }
            return cmdTmtIonquant.configure(parent, Paths.get(binMsfragger.getBin()), Paths.get(binIonQuant.getBin()), ramGb, null, tabWorkflow.getInputDataType(), sharedPepxmlFilesFromMsfragger, sharedMapGroupsToProtxml, threads, oPairPanel.isRun() ? null : modMassSet, isDryRun, false, false);
          }
          return true;
        });
        addConfig.accept(cmdTmtIonquantIsobaric, () -> {
          cmdTmtIonquantIsobaric.setRun(!sharedPepxmlFilesFromMsfragger.isEmpty());
          if (cmdTmtIonquantIsobaric.isRun()) {
            OPairPanel oPairPanel = Bus.getStickyEvent(OPairPanel.class);
            if (oPairPanel == null) {
              throw new IllegalStateException("OPairPanel has not been posted to the bus");
            }
            return cmdTmtIonquantIsobaric.configure(parent, Paths.get(binMsfragger.getBin()), Paths.get(binIonQuant.getBin()), ramGb, null, tabWorkflow.getInputDataType(), sharedPepxmlFilesFromMsfragger, sharedMapGroupsToProtxml, threads, oPairPanel.isRun() ? null : modMassSet, isDryRun, false, true, false, tolerance, Integer.parseInt(quantLevel), label.getName(), annotations, true);
          }
          return true;
        });
      } else if (tmtiPanel.getIntensityExtractionTool() == 1) {
        addCheck.accept(() -> {
          if (quantPanelLabelfree.isRunFreeQuant()) {
            if (Fragpipe.headless) {
              log.error("Both FreeQuant and TMT-Integrator were enabled. If you want to perform TMT analysis, please disable FreeQuant. If you want to perform LFQ analysis, please disable TMT-Integrator.");
            } else {
              String msg =
                  "<html>Both FreeQuant and TMT-Integrator were enabled.\n"
                      + "If you want to perform TMT analysis, please disable FreeQuant.\n"
                      + "If you want to perform LFQ analysis, please disable TMT-Integrator.\n";
              JOptionPane.showMessageDialog(parent, msg, "Warning", JOptionPane.ERROR_MESSAGE);
            }
            return false;
          }
          return true;
        });

        addConfig.accept(cmdTmtFreequant, () -> {
          cmdTmtFreequant.setRun(cmdTmtFreequant.isRun() && !sharedMapGroupsToProtxml.isEmpty());
          if (cmdTmtFreequant.isRun()) {
            return cmdTmtFreequant.configure(parent, usePhi, quantPanelLabelfree.getFreequantOptsAsText(), sharedMapGroupsToProtxml, tmtiPanel.isRun(), tabMsf.isOpenSearch());
          }
          return true;
        });

        addConfig.accept(cmdTmtLabelQuant, () -> {
          cmdTmtLabelQuant.setRun(cmdTmtLabelQuant.isRun() && !sharedMapGroupsToProtxml.isEmpty());
          if (cmdTmtLabelQuant.isRun()) {
            return cmdTmtLabelQuant.configure(parent, isDryRun, usePhi, quantLevel, tolerance, minprob, purity, minIntensityPercant, label, annotations, sharedMapGroupsToProtxml);
          }
          return true;
        });
      }
    }

    // Run scan pairing - make scan pair files if O-Pair is run
    final OPairPanel oPairPanel = Fragpipe.getStickyStrict(OPairPanel.class);
    CmdPairScans cmdPairScans = new CmdPairScans(oPairPanel.isRun(), wd);
    addConfig.accept(cmdPairScans, () -> {
      cmdPairScans.setRun(cmdPairScans.isRun() && !sharedLcmsFiles.isEmpty());
      if (cmdPairScans.isRun()) {
        return cmdPairScans.configure(parent, Paths.get(binMsfragger.getBin()), jarPath, ramGb, threads, sharedLcmsFiles, oPairPanel.getOPairParams());
      }
      return true;
    });

    // run O-Pair
    CmdOPair cmdOPair = new CmdOPair(oPairPanel.isRun(), wd);
    OPairParams oPairParams = oPairPanel.getOPairParams();
    addConfig.accept(cmdOPair, () -> {
      cmdOPair.setRun(cmdOPair.isRun() && !sharedMapGroupsToProtxml.isEmpty());
      if (cmdOPair.isRun()) {
        return cmdOPair.configure(parent, wd, sharedMapGroupsToProtxml, oPairParams, isDryRun, tabMsf.isWriteCalMzml() && tabMsf.getMassCalibration() > 0, threads);
      }
      return true;
    });

    // run PTMShepherd
    PtmshepherdPanel ptmsPanel = Fragpipe.getStickyStrict(PtmshepherdPanel.class);
    final boolean isRunShepherd = ptmsPanel.isRun() || ptmsGlycanPanel.isRun();
    final CmdPtmshepherd cmdPtmshepherd = new CmdPtmshepherd(isRunShepherd, wd);

    addCheck.accept(() -> {
      if (!ptmsPanel.validateForm()) {
        if (Fragpipe.headless) {
          log.error("There are errors in PTM-Shepherd configuration panel on Report tab.");
        } else {
          JOptionPane.showMessageDialog(parent, "There are errors in PTM-Shepherd configuration panel on Report tab.", "PTMShepherd Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
      return true;
    });
    addConfig.accept(cmdPtmshepherd, () -> {
      cmdPtmshepherd.setRun(cmdPtmshepherd.isRun() && !sharedMapGroupsToProtxml.isEmpty());
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
        if (ptmsGlycanPanel.isRun()) {
          additionalShepherdParams.putAll(ptmsGlycanPanel.getGlycanAssignParams());
        }
        Optional.ofNullable(tabMsf.getUiTextIsoErr().getNonGhostText())
            .filter(StringUtils::isNotBlank)
            .ifPresent(v -> additionalShepherdParams.put("isotope_error", v));
        return cmdPtmshepherd.configure(parent, isDryRun, Paths.get(binMsfragger.getBin()),
            ramGb, fastaPath, sharedMapGroupsToProtxml, additionalShepherdParams);
      }
      return true;
    });


    // run FPOP script
    final TabDownstream tabDownstream = Fragpipe.getStickyStrict(TabDownstream.class);
    final CmdFpopQuant cmdFpopQuant = new CmdFpopQuant(tabDownstream.pFpop.isRunFpopQuant(), wd);
    final CmdTmtIntegrator cmdTmtFpop = new CmdTmtIntegrator(tmtiPanel.isRun() && cmdFpopQuant.isRun() && tabDownstream.pFpop.isFpopTmt(), wd);

    if (tabDownstream.pFpop.isFpopTmt() && cmdFpopQuant.isRun()) {
      addCheck.accept(() -> {
        if (!tmtiPanel.isRun()) {
          if (Fragpipe.headless) {
            log.error("FPOP TMT Quant was requested on Downstream tab, but TMT-Integrator is not run. Please enable TMT-Integrator");
          } else {
            JOptionPane.showMessageDialog(parent, "FPOP TMT Quant was requested on Downstream tab, but TMT-Integrator is not run. Please enable TMT-Integrator", "FPOP TMT Error", JOptionPane.ERROR_MESSAGE);
          }
          return false;
        }
        return true;
      });

      // run TMT-Integrator a second time to provide unmodified peptide data as well as modified (does NOT rerun freequant/labelquant)
      addConfig.accept(cmdTmtFpop, () -> {
        cmdTmtFpop.setRun(cmdTmtFpop.isRun() && !sharedMapGroupsToProtxml.isEmpty());
        if (sharedLcmsFiles.stream().anyMatch(f -> !f.getPath().getFileName().toString().toLowerCase().endsWith(".mzml") && !f.getPath().getFileName().toString().toLowerCase().endsWith(".raw"))) {
          SwingUtils.showWarningDialog(parent, CmdTmtIntegrator.NAME + " only supports mzML and raw files.\nPlease remove other files from the input list.", CmdTmtIntegrator.NAME + " error");
          return false;
        }
        return cmdTmtFpop.configure(tmtiPanel, isDryRun, ramGb, decoyTag, sharedMapGroupsToProtxml, doMSstats, tmtiPanel.getAnnotations(), true);
      });
    }

    addConfig.accept(cmdFpopQuant, () -> {
      cmdFpopQuant.setRun(cmdFpopQuant.isRun() && !sharedMapGroupsToProtxml.isEmpty());
      if (cmdFpopQuant.isRun()) {
        return cmdFpopQuant.configure(parent);
      }
      return true;
    });


    // run Spectral library generation
    final SpeclibPanel speclibPanel = Fragpipe.getStickyStrict(SpeclibPanel.class);
    final CmdSpecLibGen cmdSpecLibGen = new CmdSpecLibGen(speclibPanel.isRun(), wd);

    addConfig.accept(cmdSpecLibGen, () -> {
      if (speclibPanel.isChecked() && !speclibPanel.isCheckRunEnabled()) {
        if (Fragpipe.headless) {
          log.error("Spectral Library Generation module was not configured correctly. Please make sure that Python and EasyPQP have been installed.");
        } else {
          JOptionPane.showMessageDialog(parent, "Spectral Library Generation module was not configured correctly.\nPlease make sure that Python and EasyPQP have been installed, then restart FragPipe.", "Spectral Library Generation Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }

      cmdSpecLibGen.setRun(cmdSpecLibGen.isRun() && !sharedMapGroupsToProtxml.isEmpty());
      if (cmdSpecLibGen.isRun()) {
        NoteConfigSpeclibgen speclibConf = Fragpipe.getStickyStrict(NoteConfigSpeclibgen.class);
        if (!speclibConf.isValid()) {
          if (Fragpipe.headless) {
            log.error("Spectral Library Generation scripts did not initialize correctly. Please make sure that Python and EasyPQP have been installed.");
          } else {
            JOptionPane.showMessageDialog(parent, "Spectral Library Generation scripts did not initialize correctly.\nPlease make sure that Python and EasyPQP have been installed, then restart FragPipe.", "Spectral Library Generation Error", JOptionPane.ERROR_MESSAGE);
          }
          return false;
        }
        final SpecLibGen2 slg = speclibConf.instance;

        return cmdSpecLibGen.configure(parent, slg, sharedMapGroupsToProtxml, fastaFile, isRunProteinProphet, tabWorkflow.getInputDataType(), threads, decoyTag);
      }
      return true;
    });


    // run DIA-NN
    boolean isNew;
    NoteConfigDiann noteConfigDiann = Fragpipe.getStickyStrict(NoteConfigDiann.class);
    if (noteConfigDiann != null && noteConfigDiann.version != null && noteConfigDiann.compareVersion(NEW_VERSION) >= 0) {
      isNew = true;
    } else {
      isNew = false;
    }

    final DiannPanel diannPanel = Fragpipe.getStickyStrict(DiannPanel.class);
    final CmdDiann cmdDiann = new CmdDiann(diannPanel.isRun(), wd);
    addConfig.accept(cmdDiann,  () -> {
      if (cmdDiann.isRun()) {
        return cmdDiann.configure(parent, sharedLcmsFileGroupsAll.values(), threads, diannPanel.getDiannQuantificationStrategy(isNew), diannPanel.usePredict(), diannPanel.unrelatedRuns(), diannPanel.getDiannQvalue(), diannPanel.useRunSpecificProteinQvalue(), diannPanel.getLibraryPath(), diannPanel.getCmdOpts(), isDryRun, diannPanel.isRunPlex(), diannPanel.generateMsstats(), diannPanel.getLight(), diannPanel.getMedium(), diannPanel.getHeavy(), jarPath);
      }
      return true;
    });


    // write sub mzML files
    final CmdWriteSubMzml cmdWriteSubMzml = new CmdWriteSubMzml(tabRun.isWriteSubMzml(), wd);
    addConfig.accept(cmdWriteSubMzml, () -> {
      cmdWriteSubMzml.setRun(cmdWriteSubMzml.isRun() && !sharedLcmsFileGroups.isEmpty());
      if (cmdWriteSubMzml.isRun()) {
        return cmdWriteSubMzml.configure(parent, jarPath, ramGb, threads, sharedLcmsFileGroups, tabRun.getSubMzmlProbThreshold(), tabMsf.isRun(), tabWorkflow.hasDataType("DIA"), tabWorkflow.hasDataType("GPF-DIA"), tabWorkflow.hasDataType("DIA-Lib"), tabWorkflow.hasDataType("DDA+"));
      }
      return true;
    });


    // check if any incompatible tools are requested
    addCheck.accept(() -> {
      if (InputDataType.ImMsTimsTof == tabWorkflow.getInputDataType()) {
        // timsTOF data compatibility check
        List<CmdBase> incompatible = Seq.seq(graphOrder.vertexSet())
            .filter(CmdBase::isRun)
            .filter(cmd -> (cmd instanceof CmdCrystalc) || (cmd instanceof CmdFreequant) || (cmd instanceof CmdLabelquant)
                || (cmd instanceof CmdTmtIntegrator)).toList();
        if (!incompatible.isEmpty()) {
          String s = Seq.seq(incompatible).map(CmdBase::getCmdName).distinct().toString(", ");
          if (Fragpipe.headless) {
            log.error("timsTOF data is currently not compatible with some of tools to be run: " + s);
            return false;
          } else {
            int confirmation = SwingUtils.showConfirmDialog(parent, new JLabel(SwingUtils.makeHtml(
                "timsTOF data is currently not compatible with some of tools to be run:\n"
                    + s + "\nTurn them off and continue?")));
            if (JOptionPane.YES_OPTION != confirmation) {
              return false;
            }
          }
          incompatible.forEach(cmd -> cmd.setRun(false));
        }
      }
      return true;
    });


    addToGraph(graphOrder, cmdStart, DIRECTION.IN);
    addToGraph(graphOrder, cmdCheckCentroid, DIRECTION.IN, cmdStart);
    addToGraph(graphOrder, cmdUmpire, DIRECTION.IN, cmdCheckCentroid);
    addToGraph(graphOrder, cmdDiaPasefSCentric, DIRECTION.IN, cmdCheckCentroid);
    addToGraph(graphOrder, cmdMsfragger, DIRECTION.IN, cmdCheckCentroid, cmdUmpire);
    addToGraph(graphOrder, cmdMsfragger, DIRECTION.IN, cmdCheckCentroid, cmdDiaPasefSCentric);

    addToGraph(graphOrder, cmdCrystalc, DIRECTION.IN, cmdMsfragger);
    addToGraph(graphOrder, cmdMSBooster, DIRECTION.IN, cmdMsfragger);
    addToGraph(graphOrder, cmdPeptideProphet, DIRECTION.IN, cmdMsfragger, cmdCrystalc);
    addToGraph(graphOrder, cmdPercolator, DIRECTION.IN, cmdMsfragger, cmdCrystalc, cmdMSBooster);
    for (final CmdBase cmdPeptideValidation : new CmdBase[]{cmdPeptideProphet, cmdPercolator}) {
      addToGraph(graphOrder, cmdPtmProphet, DIRECTION.IN, cmdPeptideValidation);
      addToGraph(graphOrder, cmdProteinProphet, DIRECTION.IN, cmdPeptideValidation, cmdPtmProphet);
    }
    addToGraph(graphOrder, cmdPhilosopherDbAnnotate, DIRECTION.IN, cmdProteinProphet);
    addToGraph(graphOrder, cmdPhilosopherFilter, DIRECTION.IN, cmdPhilosopherDbAnnotate, cmdProteinProphet);
    addToGraph(graphOrder, cmdFreequant, DIRECTION.IN, cmdPhilosopherFilter);
    for (final CmdBase cmdPeptideValidation : new CmdBase[]{cmdPeptideProphet, cmdPercolator})
      addToGraph(graphOrder, cmdIprophet, DIRECTION.IN, cmdPhilosopherReport, cmdPeptideValidation);
    addToGraph(graphOrder, cmdPhilosopherAbacus, DIRECTION.IN, cmdPhilosopherReport, cmdIprophet, cmdProteinProphet);
    addToGraph(graphOrder, cmdTmtFreequant, DIRECTION.IN, cmdPhilosopherFilter);
    addToGraph(graphOrder, cmdTmtLabelQuant, DIRECTION.IN, cmdPhilosopherFilter, cmdTmtFreequant);
    addToGraph(graphOrder, cmdPhilosopherReport, DIRECTION.IN, cmdPhilosopherFilter, cmdFreequant, cmdTmtFreequant, cmdTmtLabelQuant);
    addToGraph(graphOrder, cmdPairScans, DIRECTION.IN, cmdPhilosopherReport, cmdPhilosopherAbacus);
    addToGraph(graphOrder, cmdOPair, DIRECTION.IN, cmdPairScans);
    addToGraph(graphOrder, cmdPtmshepherd, DIRECTION.IN, cmdPhilosopherReport, cmdPhilosopherAbacus);
    addToGraph(graphOrder, cmdAppendFile, DIRECTION.IN, cmdPtmshepherd);
    addToGraph(graphOrder, cmdIonquant, DIRECTION.IN, cmdPhilosopherReport, cmdPhilosopherAbacus, cmdPtmshepherd);
    addToGraph(graphOrder, cmdTmtIonquant, DIRECTION.IN, cmdPhilosopherReport, cmdPhilosopherAbacus, cmdPtmshepherd);
    addToGraph(graphOrder, cmdTmtIonquantIsobaric, DIRECTION.IN, cmdPhilosopherReport, cmdPhilosopherAbacus, cmdPtmshepherd, cmdIonquant);
    addToGraph(graphOrder, cmdTmt, DIRECTION.IN, cmdPhilosopherReport, cmdTmtFreequant, cmdTmtLabelQuant, cmdPhilosopherAbacus, cmdPtmshepherd, cmdTmtIonquant, cmdTmtIonquantIsobaric);
    addToGraph(graphOrder, cmdTmtFpop, DIRECTION.IN, cmdPhilosopherReport, cmdTmtFreequant, cmdTmtLabelQuant, cmdPhilosopherAbacus, cmdPtmshepherd, cmdTmtIonquant, cmdTmtIonquantIsobaric);
    addToGraph(graphOrder, cmdFpopQuant, DIRECTION.IN, cmdIonquant, cmdTmt, cmdTmtFpop);
    addToGraph(graphOrder, cmdSpecLibGen, DIRECTION.IN, cmdPhilosopherReport, cmdOPair);
    addToGraph(graphOrder, cmdDiann, DIRECTION.IN, cmdSpecLibGen);
    addToGraph(graphOrder, cmdWriteSubMzml, DIRECTION.IN, cmdPhilosopherReport);

    // compose graph of required dependencies
    final Graph<CmdBase, DefEdge> graphDeps = new DirectedAcyclicGraph<>(DefEdge.class);
    addToGraph(graphDeps, cmdPhilosopherFilter, DIRECTION.OUT, cmdPhilosopherDbAnnotate);
    addToGraph(graphDeps, cmdTmtFreequant, DIRECTION.OUT, cmdPhilosopherFilter, cmdPhilosopherReport);
    addToGraph(graphDeps, cmdTmtLabelQuant, DIRECTION.OUT, cmdPhilosopherFilter, cmdPhilosopherReport);
    //addToGraph(graphDeps, cmdTmt, DIRECTION.OUT, cmdPhilosopherFilter);


    // run pre-checks (after both graphs are built
    for (IConfig preConfig : checks) {
      if (!preConfig.config()) {
        return false;
      }
    }

    // turn on all required dependencies
    List<CmdBase> origins = Seq.seq(graphDeps.vertexSet())
        .filter(CmdBase::isRun) // those that are run
        .filter(cmd -> graphDeps.outDegreeOf(cmd) > 0) // and are dependent on something
        .toList();
    log.debug("Starting points:\n\t{}", Seq.seq(origins).toString("\n\t"));

    for (CmdBase origin : origins) {
      log.debug("Traversing from starting point: [{}]", origin);
      ClosestFirstIterator<CmdBase, DefEdge> it = new ClosestFirstIterator<>(graphDeps, origin);
      while (it.hasNext()) {
        CmdBase next = it.next();
        log.debug("Next traversal node: [{}]", next);
        if (!next.isRun()) {
          log.warn(
              "Command [{}] is a required dependency of [{}]. Will be run despite being switched off.",
              next.getCmdName(), origin.getCmdName());
          next.setRun(true);
        }
      }
    }

    // run all configs, this will determine which commadns use Phi and in which directories
    for (CmdBase cmd : commands) {
      IConfig config = cmd.getConfig();
//      if (config != null && cmd.isRun()) {
      if (config != null) { // each config action takes care by itself about checking if isRun is true
        if (!config.config()) {
          return false;
        }
      }
    }
    // now the shared `usePhi` object should have all the paths where Phi is invoked.

    // special treatment of phi workspace
    // run Philosopher clean/init in all directories where Philosopher will be invoked

    CmdBase firstPhiDependentCmd = null;
    CmdBase lastPhiDependentCmd = null;
    for (TopologicalOrderIterator<CmdBase, DefEdge> it = new TopologicalOrderIterator<>(
        graphOrder); it.hasNext(); ) {
      CmdBase cmd = it.next();
      if (cmd.usesPhi()) {
        if (firstPhiDependentCmd == null) {
          firstPhiDependentCmd = cmd;
        }
        lastPhiDependentCmd = cmd;
      }
    }

    if (usePhi != null && usePhi.isUsed()) {
      for (Path pathPhiIsRunIn : usePhi.getWorkDirs()) {

        CmdPhilosopherWorkspaceCleanInit cmdPhiCleanInit = new CmdPhilosopherWorkspaceCleanInit(
            usePhi.isUsed(), "Phi wrk clean, init", pathPhiIsRunIn);
        cmdPhiCleanInit.configure(usePhi);
        addToGraph(graphOrder, cmdPhiCleanInit, DIRECTION.IN, cmdStart); // needs to run at start
        if (firstPhiDependentCmd != null) {
          addToGraph(graphOrder, firstPhiDependentCmd, DIRECTION.IN, cmdPhiCleanInit); // and before first command that uses phi
        }
//      for (CmdBase cmd : commands) {
//        if (cmd.usesPhi()) {
//          graphOrder.addEdge(cmdPhiCleanInit, cmd);
//        }
//      }

        CmdPhilosopherWorkspaceClean cmdPhiClean = new CmdPhilosopherWorkspaceClean(
            usePhi.isUsed(), "Phi wrk clean", pathPhiIsRunIn);
        cmdPhiClean.configure(usePhi);

        if (lastPhiDependentCmd != null) {
          // clean runs after last command that uses Phi
          log.debug("Determined that the last command in graph to use Phi is: [{}]",
              lastPhiDependentCmd.getCmdName());
          addToGraph(graphOrder, cmdPhiClean, DIRECTION.IN, lastPhiDependentCmd);
        } else {
          log.warn("No command was found to use Philosopher.");
        }
      }
    }


    // all graphs are constructed
//    if (Version.isDevBuild()) {
//      // org.jgrapht.nio.graphml.GraphMLExporter
//
//      final JGraphXAdapter2<CmdBase, DefEdge> adapter = new JGraphXAdapter2<>(graphOrder);
//
//      mxGraph mxGraph = new mxGraph(adapter.getModel()) {
//        @Override public String convertValueToString(Object cell) {
////          log.info("convertValueToString called for type: {}", cell.getClass().getCanonicalName());
//          if (cell instanceof mxCell) {
//            Object value = ((mxCell) cell).getValue();
//            if (value instanceof CmdBase) {
//              CmdBase cmd = (CmdBase) value;
//              String relWd = wd.relativize(cmd.getWd()).toString();
//              //return StringUtils.isBlank(relWd) ? String.format("%s", value.toString()) : String.format("%s\n[%s]", value.toString(), relWd);
//              return String.format("%s", cmd.getTitle());
//            }
////            log.info("Object cell mxCell getValue is type: {}", value.getClass().getCanonicalName());
//          }
//
//          return super.convertValueToString(cell);
//        }
//
//        /**
//         * Returns an array of key, value pairs representing the cell style for the
//         * given cell. If no string is defined in the model that specifies the style,
//         * then the default style for the cell is returned or <EMPTY_ARRAY>, if not
//         * style can be found.
//         *
//         * @param cell Cell whose style should be returned.
//         * @return Returns the style of the cell.
//         */
//        @Override
//        public Map<String, Object> getCellStyle(Object cell) {
//          Map<String, Object> cellStyle = super.getCellStyle(cell);
//          if (cell instanceof mxCell) {
//            Object value = ((mxCell) cell).getValue();
//            if (value instanceof CmdBase) {
//              CmdBase cmd = (CmdBase) value;
//              // example: fillColor=green
//              cellStyle = new HashMap<>(cellStyle);
//              String color = cmd.isRun() ? "#d5f7de" : "#f5e7d5";
//              cellStyle.put("fillColor", color);
//            }
//          }
//          return cellStyle;
//        }
//      };
//      mxGraph.setAutoSizeCells(true);
//
//
//      // Overrides method to create the editing value
//      final mxGraphComponent graphComponent = new mxGraphComponent(mxGraph) {
//
//      };
//
//      ActionListener actionLayout = e -> {
//        mxHierarchicalLayout layout = new mxHierarchicalLayout(adapter);
//        Object cell = adapter.getSelectionCell();
//        if (cell == null || adapter.getModel().getChildCount(cell) == 0) {
//          cell = adapter.getDefaultParent();
//        }
//        adapter.getModel().beginUpdate();
//        try {
//          long t0 = System.currentTimeMillis();
//          layout.execute(cell);
//        } finally {
//          mxMorphing morph = new mxMorphing(graphComponent, 20, 1.2, 20);
//          morph.addListener(mxEvent.DONE, (sender, evt) -> adapter.getModel().endUpdate());
//          morph.startAnimation();
//        }
//      };
//
//      JFrame graphFrame = new JFrame("Graph");
//      Fragpipe.decorateFrame(graphFrame);
//      graphFrame.setLayout(new BorderLayout());
//
//      mxHierarchicalLayout layout = new mxHierarchicalLayout(adapter);
//      layout.execute(adapter.getDefaultParent());
//
//      graphFrame.add(UiUtils.createButton("Layout", actionLayout), BorderLayout.NORTH);
//      graphFrame.add(graphComponent, BorderLayout.CENTER);
//
//      graphFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
//      graphFrame.pack();
//      graphFrame.setVisible(true);
//    }


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
        if (Fragpipe.headless) {
          log.error("Not all directories could be created: " + e.getMessage());
        } else {
          JOptionPane.showMessageDialog(parent, "Not all directories could be created:\n" + e.getMessage());
        }
        return false;
      }
    }

    return true;
  }

  private static boolean checkDbConfig(JComponent parent, boolean runPerconator, boolean runPeptideProphet, boolean runReport) {
    NoteConfigDatabase n;
    try {
      n = Fragpipe.getSticky(NoteConfigDatabase.class);
    } catch (NoStickyException e) {
      SwingUtils.showErrorDialog(parent, "Database not configured", "Database config error");
      return false;
    }

    String fastaPath = checkFasta(parent, n);
    if (fastaPath == null) {
      return false;
    }

    if (n.numEntries == 0) {
      SwingUtils.showErrorDialog(parent, "Database looks to be empty", "Database config error");
      return false;
    }

    if (!n.isBigDatabase) {
      double decoysPercentage = (n.decoysCnt / (double) n.numEntries);
      if (decoysPercentage <= 0) {
        if (Fragpipe.headless) {
          log.error("No decoys found in the FASTA file.");
          return false;
        } else {
          if (runPerconator) {
            SwingUtils.showErrorDialog(parent, "No decoys found in the FASTA file.<br>Percolator was enabled.<br>Please check protein database tab.", "No decoys");
            return false;
          } else if (runPeptideProphet || runReport) {
            int confirm = SwingUtils.showConfirmDialog(parent, new JLabel(
                "<html>No decoys found in the FASTA file.<br>PeptideProphet or FDR filter and report was enabled.<br>Please check protein database tab.<br><br>You can also continue as-is, but FDR analysis will fail. Do you want to continue?"));
            return JOptionPane.YES_OPTION == confirm;
          }
        }
      } else if (decoysPercentage >= 1) {
        if (Fragpipe.headless) {
          log.error("All FASTA entries seem to be decoys.");
          return false;
        } else {
          int confirm = SwingUtils.showConfirmDialog(parent, new JLabel(
              "<html>All FASTA entries seem to be decoys.<br>\n" +
                  "Please check protein database tab.<br><br><br>\n" +
                  "You can also continue as-is, but FDR analysis will fail. Do you want to continue?"));
          return JOptionPane.YES_OPTION == confirm;
        }
      } else if (decoysPercentage < 0.4 || decoysPercentage > 0.6) {
        DecimalFormat dfPct = new DecimalFormat("##.#'%'");
        if (Fragpipe.headless) {
          log.error("FASTA file contains " + dfPct.format(decoysPercentage * 100) + " decoys.");
          return false;
        } else {
          int confirm = SwingUtils.showConfirmDialog(parent, new JLabel(
              "<html>FASTA file contains " + dfPct.format(decoysPercentage * 100) + " decoys.<br>\n" +
                  "Please check protein database tab.<br><br><br>\n" +
                  "You can also continue as-is, if that's what you're expected.<br>\n"
                  + "Do you want to continue?"));
          return JOptionPane.YES_OPTION == confirm;
        }
      }
    }

    return true;
  }
}
