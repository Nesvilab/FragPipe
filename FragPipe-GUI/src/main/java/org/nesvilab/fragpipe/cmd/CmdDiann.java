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

package org.nesvilab.fragpipe.cmd;

import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.BATMASS_IO_JAR;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.generateLFQExperimentAnnotation;
import static org.nesvilab.fragpipe.tabs.TabWorkflow.manifestExt;
import static org.nesvilab.utils.OsUtils.isUnix;
import static org.nesvilab.utils.OsUtils.isWindows;
import static org.nesvilab.utils.SwingUtils.createClickableHtml;
import static org.nesvilab.utils.SwingUtils.showErrorDialogWithStacktrace;

import java.awt.Component;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.JOptionPane;

import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.messages.NoteConfigDiann;
import org.nesvilab.fragpipe.tools.diann.Diann;
import org.nesvilab.fragpipe.tools.diann.DiannToMsstats;
import org.nesvilab.fragpipe.tools.diann.ParquetToTsv;
import org.nesvilab.fragpipe.tools.diann.PlexDiaHelper;
import org.nesvilab.fragpipe.tools.diann.Propagation;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdDiann extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdDiann.class);
  private static final String NAME = "DIA-Quant";
  private static final List<String> SUPPORTED_FORMATS_WIN = Arrays.asList("mzML", "d", "dia", "wiff", "raw");
  private static final List<String> SUPPORTED_FORMATS_LINUX = Arrays.asList("mzML", "d", "dia", "raw");
  public static final String FRAG_REPORTER = "FragReporter-1.1.0.jar";
  public static final Pattern labelPattern = Pattern.compile("([A-Znc*]+)([\\d.+-]+)");

  private final String diannPath;
  private final String LD_PRELOAD_str;

  public CmdDiann(boolean isRun, Path workDir) {
    super(isRun, workDir);
    NoteConfigDiann noteConfigDiann = Bus.getStickyEvent(NoteConfigDiann.class);
    if (noteConfigDiann == null) {
      diannPath = Diann.fallbackDiannPath;
    } else {
      diannPath = noteConfigDiann.path;
    }
    LD_PRELOAD_str = Diann.LD_PRELOAD_str;
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp,
      Collection<LcmsFileGroup> lcmsFileGroups,
      int nThreads,
      int ramGb,
      Set<String> quantificationStrategy,
      String channelNormalizationStrategy,
      boolean unrelatedRuns,
      float qvalue,
      boolean useRunSpecificProteinQvalue,
      boolean useMbr,
      boolean redoProteinInference,
      String fastaFile,
      String libraryPath,
      String additionalCmdOpts,
      boolean isDryRun,
      boolean isRunPlex,
      boolean generateMsstats,
      String lightString,
      String mediumString,
      String heavyString,
      Path jarFragpipe,
      NoteConfigDiann noteConfigDiann,
      boolean isDiaUmpireRun,
      String modTag,
      float siteProb,
      boolean isTransferLearningRun,
      boolean isTransferLearningPrediction,
      int transferLearningPeptidesToPredict,
      String transferLearningOutputFormat) {
    initPreConfig();

    String predictedSpeclibPath = null;
    if (isTransferLearningRun && isTransferLearningPrediction) {
      Path predictedSpeclib = wd.resolve("fragpipe-predicted-speclib." + transferLearningOutputFormat);
      predictedSpeclibPath = predictedSpeclib.toAbsolutePath().normalize().toString();
    }

    if (libraryPath != null && !libraryPath.trim().isEmpty()) {
      System.out.println("There are external spectral library. Will not generate the MSstats file.");
      generateMsstats = false;
    }

    if (isRunPlex) {
      if ((lightString == null || lightString.isEmpty()) && (mediumString == null || mediumString.isEmpty()) && (heavyString == null || heavyString.isEmpty())) {
        SwingUtils.showErrorDialog(comp, "All light, medium, and heavy labels are empty.<br>Please disable/uncheck <b>plexDIA</b> in <b>Quant (DIA)</b> tab or provide label info.", "No label info");
        return false;
      }

      if (predictedSpeclibPath != null && !predictedSpeclibPath.toLowerCase().endsWith(".tsv")) {
        SwingUtils.showErrorDialog(comp, "plexDIA requires a TSV format spectral library.", "Incompatible library format");
        return false;
      }

      final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(BATMASS_IO_JAR));
      if (classpathJars == null) {
        return false;
      }

      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      String libsDir = root.resolve("lib").toAbsolutePath().normalize() + "/*";
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib").toAbsolutePath().normalize() + "/*";
      }

      Set<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toSet());
      toJoin.add(libsDir);
      final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));

      for (LcmsFileGroup group : lcmsFileGroups) {
        final Path groupWd = group.outputDir(wd);
        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        cmd.add("-cp");
        cmd.add(classpath);
        cmd.add(PlexDiaHelper.class.getCanonicalName());
        cmd.add("--threads");
        cmd.add(String.valueOf(nThreads));
        if (lightString != null && !lightString.isEmpty()) {
          cmd.add("--light");
          cmd.add(lightString);
        }
        if (mediumString != null && !mediumString.isEmpty()) {
          cmd.add("--medium");
          cmd.add(mediumString);
        }
        if (heavyString != null && !heavyString.isEmpty()) {
          cmd.add("--heavy");
          cmd.add(heavyString);
        }
        cmd.add("--library");
        cmd.add(predictedSpeclibPath != null ? predictedSpeclibPath : "library.tsv");
        cmd.add("--out");
        cmd.add("library_2.tsv");
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(groupWd.toFile());
        pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " prepare plex library").create());
      }
    }

    List<String> sup;
    if (isWindows()) {
      sup = SUPPORTED_FORMATS_WIN;
    } else if (isUnix()) {
      sup = SUPPORTED_FORMATS_LINUX;
    } else {
      System.err.println("DIA-NN only works in Windows and Linux.");
      return false;
    }

    Collection<LcmsFileGroup> lcmsFileGroups2;
    if (isDiaUmpireRun) {
      lcmsFileGroups2 = lcmsFileGroups;
    } else {
      lcmsFileGroups2 = new ArrayList<>(lcmsFileGroups.size());
      for (LcmsFileGroup group : lcmsFileGroups) {
        List<InputLcmsFile> lcmsFiles2 = new ArrayList<>(group.lcmsFiles.size());
        for (InputLcmsFile lcmsFile : group.lcmsFiles) {
          if (lcmsFile.getDataType().contentEquals("DIA-Quant")) {
            lcmsFiles2.add(lcmsFile);   // do not use uncalibrated mzML for DIA-Quant since it will not be generated (the file is not sent to MSFragger)
          } else {
            Path path2 = Paths.get(lcmsFile.getPath().toString().replaceAll("(?i)\\.raw", "_uncalibrated.mzML"));
            InputLcmsFile lcmsFile2 = new InputLcmsFile(path2, lcmsFile.getExperiment(), lcmsFile.getReplicate(), lcmsFile.getDataType());
            lcmsFiles2.add(lcmsFile2);
          }
        }
        lcmsFileGroups2.add(new LcmsFileGroup(group.name, lcmsFiles2));
      }
    }

    for (LcmsFileGroup group : lcmsFileGroups2) {
      final Path groupWd = group.outputDir(wd);

      Set<Path> inputLcmsPaths = group.lcmsFiles.stream().filter(f -> !f.getDataType().contentEquals("DDA") && !f.getDataType().contentEquals("GPF-DIA") && !f.getDataType().contentEquals("DIA-Lib") && !f.getDataType().contentEquals("DDA+")).map(InputLcmsFile::getPath).collect(Collectors.toSet());

      if (inputLcmsPaths.isEmpty()) {
        continue;
      }

      List<InputLcmsFile> inputLcmsFiles = group.lcmsFiles.stream().filter(f -> !f.getDataType().contentEquals("DDA") && !f.getDataType().contentEquals("GPF-DIA") && !f.getDataType().contentEquals("DIA-Lib") && !f.getDataType().contentEquals("DDA+")).collect(Collectors.toList());

      if (!checkCompatibleFormats(comp, inputLcmsFiles, sup)) {
        return false;
      }

      List<String> exts = inputLcmsFiles.stream().map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase())).distinct().filter(ext -> ext.equalsIgnoreCase("raw")).collect(Collectors.toList());
      if (!exts.isEmpty()) {
        if (Fragpipe.headless) {
          log.warn("Running DIA-NN with Thermo raw files requires Thermo MS File Reader is installed. If you do not have it installed, please convert to mzML prior to running " + PROGRAM_TITLE + ".");
        } else {
          int res = JOptionPane.showConfirmDialog(comp, createClickableHtml("Running DIA-NN with Thermo raw files requires <a href=\"https://thermo.flexnetoperations.com/control/thmo/login?nextURL=%2Fcontrol%2Fthmo%2Fdownload%3Felement%3D6306677\">Thermo MS File Reader</a> is installed.<br>If you do not have it installed, please convert to mzML prior to running " + PROGRAM_TITLE + ".<br>\"Yes\" to continue. \"No\" to cancel."), "Data format warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
          if (JOptionPane.YES_OPTION != res) {
            return false;
          }
        }
      }

      exts = inputLcmsFiles.stream().map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase())).distinct().filter(ext -> ext.equals("wiff")).collect(Collectors.toList());
      if (!exts.isEmpty()) {
        if (Fragpipe.headless) {
          log.warn("Running DIA-NN with WIFF files requires additional library is installed. If you do not have it installed, please convert to mzML prior to running " + PROGRAM_TITLE + ".");
        } else {
          int res = JOptionPane.showConfirmDialog(comp, createClickableHtml("Running DIA-NN with WIFF files requires <a href=\"https://github.com/vdemichev/DiaNN/blob/master/README.md#raw-data-formats\">additional library</a> is installed.<br>If you do not have it installed, please convert to mzML prior to running " + PROGRAM_TITLE + ".<br>\"Yes\" to continue. \"No\" to cancel."), "Data format warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
          if (JOptionPane.YES_OPTION != res) {
            return false;
          }
        }
      }

      TreeMap<Long, List<Path>> sizeInputLcms = new TreeMap<>(Comparator.reverseOrder());
      for (Path inputLcmsPath : inputLcmsPaths) {
        long size = 0;
        try {
          if (Files.exists(inputLcmsPath) && Files.isReadable(inputLcmsPath)) {
            if (Files.isDirectory(inputLcmsPath)) {
              for (Path p : Files.walk(inputLcmsPath).collect(Collectors.toList())) {
                if (Files.isReadable(p) && Files.isRegularFile(p)) {
                  size += Files.size(p);
                }
              }
            } else if (Files.isRegularFile(inputLcmsPath)) {
              size = Files.size(inputLcmsPath);
            }
          }
        } catch (Exception e) {
          e.printStackTrace();
        }
        List<Path> t = sizeInputLcms.get(size);
        if (t == null) {
          t = new ArrayList<>(1);
          t.add(inputLcmsPath);
          sizeInputLcms.put(size, t);
        } else {
          sizeInputLcms.get(size).add(inputLcmsPath);
        }
      }

      try {
        final Path diannOutputDirectory = groupWd.resolve("dia-quant-output");
        if (Files.exists(diannOutputDirectory.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
          Files.createDirectories(diannOutputDirectory);
        }
      } catch (IOException ex) {
        throw new UncheckedIOException(ex);
      }

      if (!isRunPlex && libraryPath != null && !libraryPath.isEmpty()) {
        Path tt = Paths.get(libraryPath);
        if (!Files.exists(tt) || !Files.isRegularFile(tt) || !Files.isReadable(tt)) {
          SwingUtils.showErrorDialog(comp, "The custom library file does not exist or is not readable: " + libraryPath + "<br>Please double check the <b>spectral library (optinal)</b> box in the <b>Quant (DIA)</b> tab.", libraryPath + " is not readable");
          return false;
        }
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(diannPath);
      cmd.add("--lib");
      String libraryToUse;
      if (!isRunPlex && libraryPath != null && !libraryPath.isEmpty()) {
        libraryToUse = libraryPath;
      } else if (isRunPlex) {
        libraryToUse = "library_2.tsv";
      } else if (predictedSpeclibPath != null) {
        libraryToUse = predictedSpeclibPath;
      } else {
        libraryToUse = "library.tsv";
      }
      cmd.add(libraryToUse);
      cmd.add("--threads");
      cmd.add(String.valueOf(nThreads));
      cmd.add("--verbose");
      cmd.add("1");
      cmd.add("--out");
      cmd.add("dia-quant-output" + File.separator + "report.tsv");
      cmd.add("--qvalue");
      cmd.add(String.valueOf(qvalue));
      if (useMbr) {
        cmd.add("--reanalyse");
      }
      if (useRunSpecificProteinQvalue) {
        cmd.add("--matrix-spec-q");
        cmd.add(String.valueOf(qvalue));
      }
      cmd.add("--matrix-qvalue");
      cmd.add(String.valueOf(qvalue));
      cmd.add("--matrices");
      if (noteConfigDiann.compareVersion("2.0") < 0) {
        if (redoProteinInference) {
          cmd.add("--relaxed-prot-inf");
          cmd.add("--fasta");
          cmd.add(fastaFile);
        } else {
          cmd.add("--no-prot-inf");
        }
      } else {
        if (redoProteinInference) {
          cmd.add("--fasta");
          cmd.add(fastaFile);
        } else {
          cmd.add("--no-prot-inf");
        }
      }
      cmd.add("--smart-profiling");
      cmd.add("--no-quant-files");
      cmd.addAll(quantificationStrategy);
      if (unrelatedRuns) {
        cmd.add("--individual-mass-acc");
        cmd.add("--individual-windows");
      }
      if (generateMsstats) {
        if (noteConfigDiann.compareVersion("2.0") < 0) {
          cmd.add("--report-lib-info");
        } else {
          cmd.add("--export-quant");
        }
      }
      if (isRunPlex) {
        try {
          cmd.addAll(getPlexDiannFlags(lightString, mediumString, heavyString));
        } catch (Exception e) {
          SwingUtils.showErrorDialog(comp, e.getMessage(), "Error parsing plex settings");
          return false;
        }
        if (noteConfigDiann.compareVersion("1.9") >= 0) {
          cmd.add(channelNormalizationStrategy);
        }
      }
      if (!additionalCmdOpts.isEmpty()) {
        cmd.add(additionalCmdOpts);
      }

      try {
        final Path filelist = groupWd.resolve("filelist_diann.txt");
        if (Files.exists(filelist.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
          BufferedWriter bufferedWriter = Files.newBufferedWriter(filelist);
          for (Path p : sizeInputLcms.values().stream().flatMap(List::stream).collect(Collectors.toList())) {
            bufferedWriter.write("--f " + p.toAbsolutePath() + "\n");
          }
          bufferedWriter.close();
        }
        cmd.add("--cfg");
        cmd.add(isUnix() ? (filelist.toAbsolutePath() + "--") : filelist.toAbsolutePath().normalize().toString()); // https://github.com/vdemichev/DiaNN/issues/641#issuecomment-1479416716
      } catch (IOException ex) {
        throw new UncheckedIOException(ex);
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());
      if (LD_PRELOAD_str != null) {
        pb.environment().put("LD_PRELOAD", LD_PRELOAD_str);
      }
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " run DIA-NN").create());

      if (isWindows()) {
        // Plotting
        List<String> cmd2 = new ArrayList<>();
        if (noteConfigDiann.compareVersion("2.0") < 0) {
          cmd2.add(diannPath.replaceAll("DiaNN\\.exe$", "dia-nn-plotter.exe"));
          cmd2.add("dia-quant-output" + File.separator + "report.stats.tsv");
          cmd2.add("dia-quant-output" + File.separator + "report.tsv");
          cmd2.add("dia-quant-output" + File.separator + "report.pdf");
        } else {
          cmd2.add(diannPath.replaceAll("(?i)diann\\.exe$", "diann-stats.exe"));
          cmd2.add("dia-quant-output" + File.separator + "report.parquet");
        }
        ProcessBuilder pb2 = new ProcessBuilder(cmd2);
        pb2.directory(groupWd.toFile());
        pbis.add(new PbiBuilder().setPb(pb2).setName(getCmdName() + " plot DIA-NN output").create());
      }

      Path speclibForSkyline = wd.resolve("dia-quant-output").resolve("report-tsv.speclib");
      List<ProcessBuilder> pbsMove = ToolingUtils.pbsMoveFilesWithExtension(jarFragpipe, speclibForSkyline, wd, ".speclib");
      pbis.addAll(PbiBuilder.from(pbsMove, getCmdName() + " move and rename speclib for skyline"));
    }

    if (noteConfigDiann.compareVersion("2.0") >= 0) {
      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      Path libsDir = root.resolve("lib").toAbsolutePath().normalize();
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib").toAbsolutePath().normalize();
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-cp");
      cmd.add(libsDir + File.separator + "*");
      cmd.add(ParquetToTsv.class.getCanonicalName());
      cmd.add(wd.resolve("dia-quant-output").resolve("report.parquet").toAbsolutePath().normalize().toString());
      cmd.add(wd.resolve("dia-quant-output").resolve("report.tsv").toAbsolutePath().normalize().toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.resolve("dia-quant-output").toFile());
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " convert Parquet to Tsv").create());
    }

    if (!isRunPlex) {
      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      String libsDir = root.resolve("lib").toAbsolutePath().normalize() + "/*";
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib").toAbsolutePath().normalize() + "/*";
      }

      List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(BATMASS_IO_JAR));
      if (classpathJars == null) {
        return false;
      }

      Set<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toSet());
      toJoin.add(libsDir);
      final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));

      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-cp");
      cmd.add(classpath);
      cmd.add(Propagation.class.getCanonicalName());
      cmd.add(wd.toAbsolutePath().normalize().toString());
      cmd.add((lightString == null || lightString.isEmpty()) ? "-" : lightString);
      cmd.add(mediumString == null || mediumString.isEmpty() ? "-" : mediumString);
      cmd.add(heavyString == null || heavyString.isEmpty() ? "-" : heavyString);
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.resolve("dia-quant-output").toFile());
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " propagate information").create());
    }

    if (!isRunPlex && generateMsstats && noteConfigDiann.compareVersion("2.0") < 0) {
      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      String libsDir = root.resolve("lib").toAbsolutePath().normalize() + "/*";
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib").toAbsolutePath().normalize() + "/*";
      }

      Set<String> toJoin = new TreeSet<>();
      toJoin.add(libsDir);
      final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));

      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-cp");
      cmd.add(classpath);
      cmd.add(DiannToMsstats.class.getCanonicalName());
      cmd.add("report.tsv");
      cmd.add("./");
      cmd.add(wd.resolve("psm.tsv").toAbsolutePath().normalize().toString());
      cmd.add(String.valueOf(qvalue));
      if (useRunSpecificProteinQvalue) {
        cmd.add(String.valueOf(qvalue));
      } else {
        cmd.add("1");
      }
      cmd.add(String.valueOf(qvalue));
      cmd.add(String.valueOf(qvalue));
      cmd.add(wd.resolve("fragpipe-files" + manifestExt).toAbsolutePath().normalize().toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.resolve("dia-quant-output").toFile());
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " convert DIA-NN output to MSstats.csv").create());
    }

    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Stream.of(FRAG_REPORTER));
    if (classpathJars == null) {
      System.err.println("Could not find " + FRAG_REPORTER);
    } else {
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-Xmx" + ramGb + "G");
      cmd.add("-jar");
      cmd.add(constructClasspathString(classpathJars));
      cmd.add("--pr");
      cmd.add(wd.resolve("dia-quant-output").resolve("report.tsv").toAbsolutePath().normalize().toString());
      cmd.add("--exp-ann");
      cmd.add(wd.resolve("fragpipe-files" + manifestExt).toAbsolutePath().normalize().toString());
      cmd.add("--out-dir");
      cmd.add(wd.resolve("dia-quant-output").toAbsolutePath().normalize().toString());
      cmd.add("--mod-tag");
      cmd.add(modTag);
      cmd.add("--min-site-prob");
      cmd.add(String.valueOf(siteProb));
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.resolve("dia-quant-output").toFile());
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " generate site reports").create());
    }

//    if (isRunPlex) {
//      final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(BATMASS_IO_JAR));
//      if (classpathJars == null) {
//        return false;
//      }
//
//      Path root = FragpipeLocations.get().getDirFragpipeRoot();
//      Path libsDir = root.resolve("lib");
//      if (Files.isDirectory(jarFragpipe)) {
//        libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib");
//        log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
//      }
//
//      Set<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toSet());
//      try {
//        toJoin.addAll(Files.walk(libsDir).
//            filter(p -> p.getFileName().toString().endsWith(".jar")).
//            filter(p -> p.getFileName().toString().startsWith("fragpipe-")).
//            map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList())
//        );
//      } catch (IOException ex) {
//        ex.printStackTrace();
//        return false;
//      }
//
//      toJoin.add(jarFragpipe.toAbsolutePath().normalize().toString());
//      final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));
//
//      for (LcmsFileGroup group : lcmsFileGroups) {
//        final Path groupWd = group.outputDir(wd);
//        List<String> cmd = new ArrayList<>();
//        cmd.add(Fragpipe.getBinJava());
//        cmd.add("-cp");
//        cmd.add(classpath);
//        cmd.add(PlexDiaHelper.class.getCanonicalName());
//        cmd.add("--threads");
//        cmd.add(String.valueOf(nThreads));
//        if (lightString != null && !lightString.isEmpty()) {
//          cmd.add("--light");
//          cmd.add(lightString);
//        }
//        if (mediumString != null && !mediumString.isEmpty()) {
//          cmd.add("--medium");
//          cmd.add(mediumString);
//        }
//        if (heavyString != null && !heavyString.isEmpty()) {
//          cmd.add("--heavy");
//          cmd.add(heavyString);
//        }
//        cmd.add("--library");
//        cmd.add("library_2.tsv");
//        cmd.add("--diann-report");
//        cmd.add("dia-quant-output" + File.separator + "report.tsv");
//        cmd.add("--output-dir");
//        cmd.add(groupWd.toAbsolutePath().normalize().toString());
//        ProcessBuilder pb = new ProcessBuilder(cmd);
//        pb.directory(groupWd.toFile());
//        pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " process DIA-NN output").create());
//      }
//    }

    if (!isDryRun) {
      try {
        generateLFQExperimentAnnotation(wd, 1);
      } catch (Exception ex) {
        showErrorDialogWithStacktrace(ex, comp);
        return false;
      }
    }

    isConfigured = true;
    return true;
  }

  private static List<String> getPlexDiannFlags(String lightString, String mediumString, String heavyString) throws Exception {
    List<String> cmds = new ArrayList<>(6);

    if (lightString == null || lightString.isEmpty()) {
      throw new Exception("Light string must not be empty in plexDIA mode.");
    }

    Map<Character, Float> lightAaMap = parseLabel(lightString);
    Map<Character, Float> mediumAaMap = null;
    Map<Character, Float> heavyAaMap = null;

    if (lightAaMap == null) {
      throw new Exception("Light string must not be empty in plexDIA mode.");
    }

    if (mediumString != null && !mediumString.isEmpty()) {
      mediumAaMap = parseLabel(mediumString);
      if (mediumAaMap == null) {
        throw new Exception("Medium label setting is not empty but could not parse it: " + mediumString);
      }
      if (!lightAaMap.keySet().equals(mediumAaMap.keySet())) {
        throw new Exception("Light and medium labels must have the same amino acids.");
      }
    }

    if (heavyString != null && !heavyString.isEmpty()) {
      heavyAaMap = parseLabel(heavyString);
      if (heavyAaMap == null) {
        throw new Exception("Heavy label setting is not empty but could not parse it: " + heavyString);
      }
      if (!lightAaMap.keySet().equals(heavyAaMap.keySet())) {
        throw new Exception("Light and heavy labels must have the same amino acids.");
      }
    }

    Matcher matcher = labelPattern.matcher(lightString.trim());
    while (matcher.find()) {
      cmds.add("--fixed-mod");
      cmds.add("label," + matcher.group(2) + "," + matcher.group(1) + ",label");
    }

    cmds.add("--channels");
    cmds.add(getChannel(lightAaMap, mediumAaMap, heavyAaMap));

    cmds.add("--peak-translation");
    cmds.add("--original-mods");

    return cmds;
  }

  private static String getChannel(Map<Character, Float> lightAaMap, Map<Character, Float> mediumAaMap, Map<Character, Float> heavyAaMap) {
    List<String> channelList = new ArrayList<>();

    channelList.add(buildChannelString("L", lightAaMap, lightAaMap));

    if (mediumAaMap != null && !mediumAaMap.isEmpty()) {
      channelList.add(buildChannelString("M", mediumAaMap, lightAaMap));
    }

    if (heavyAaMap != null && !heavyAaMap.isEmpty()) {
      channelList.add(buildChannelString("H", heavyAaMap, lightAaMap));
    }

    return String.join(";", channelList);
  }

  private static String buildChannelString(String label, Map<Character, Float> labelAaMap, Map<Character, Float> lightAaMap) {
    String aaKeys = labelAaMap.keySet().stream().map(String::valueOf).collect(Collectors.joining());
    String deltaMasses = labelAaMap.entrySet().stream()
        .map(entry -> {
          if (!lightAaMap.containsKey(entry.getKey())) {
            throw new NullPointerException("The " + label + " labeled amino acid " + entry.getKey() + " with delta mass" + entry.getValue() + " was not found in the base labels.");
          }
          return String.valueOf(entry.getValue() - lightAaMap.get(entry.getKey()));
        })
        .collect(Collectors.joining(":"));

    return String.format("label,%s,%s,%s", label, aaKeys, deltaMasses);
  }

  private static Map<Character, Float> parseLabel(String inputStr) {
    Map<Character, Float> outputMap = new TreeMap<>();
    Matcher matcher = labelPattern.matcher(inputStr.trim());
    while (matcher.find()) {
      char[] aas = matcher.group(1).toCharArray();
      float modMass = Float.parseFloat(matcher.group(2));
      for (char aa : aas) {
        if (aa == '*') {
          for (int i = 65; i < 91; ++i) {
            outputMap.put((char) i, modMass);
          }
          outputMap.put('n', modMass);
          outputMap.put('c', modMass);
        } else {
          outputMap.put(aa, modMass);
        }
      }
    }

    if (outputMap.isEmpty()) {
      return null;
    } else {
      return outputMap;
    }
  }

  private boolean checkCompatibleFormats(Component comp, List<InputLcmsFile> inputLcmsFiles, List<String> supportedFormats) {
    List<String> notSupportedExts = getNotSupportedExts(inputLcmsFiles, supportedFormats);
    if (!notSupportedExts.isEmpty()) {
      if (Fragpipe.headless) {
        log.error(String.format("%s can't work with '.%s' files. You can convert files using msconvert from ProteoWizard.", getCmdName(), String.join(", ", notSupportedExts)));
      } else {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("<html>%s can't work with '.%s' files.<br/>", getCmdName(), String.join(", ", notSupportedExts)));
        if (notSupportedExts.contains(".raw") || notSupportedExts.contains("raw")) {
          sb.append("Support for raw files requires Windows and <a href=\"https://thermo.flexnetoperations.com/control/thmo/login?nextURL=%2Fcontrol%2Fthmo%2Fdownload%3Felement%3D6306677\">Thermo MS File Reader</a> to be installed.<br/>It is essential to use specifically the version by the link above (3.0 SP3).<br/>");
        }
        if (notSupportedExts.contains(".wiff") || notSupportedExts.contains("wiff")) {
          sb.append("Support for wiff files requires Windows.<br>");
        }
        sb.append(String.format("Compatible formats are: %s<br/>", String.join(", ", supportedFormats)));
        sb.append(String.format("Either remove files from input or disable %s<br/>", getCmdName()));
        sb.append("You can also convert files using <i>msconvert</i> from ProteoWizard.");

        JOptionPane.showMessageDialog(comp, sb.toString(), getCmdName() + " error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }
    return true;
  }

  private static String getSpeclibExtension(String outputFormat) {
    if (outputFormat == null) {
      return null;
    }
    switch (outputFormat.toLowerCase()) {
      case "speclib":
        return ".speclib";
      case "tsv":
        return ".tsv";
      case "parquet":
        return ".parquet";
      case "mgf":
        return ".mgf";
      default:
        return null;
    }
  }
}
