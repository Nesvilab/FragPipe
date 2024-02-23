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

package com.dmtavt.fragpipe.cmd;

import static com.dmtavt.fragpipe.cmd.ToolingUtils.BATMASS_IO_JAR;
import static com.dmtavt.fragpipe.cmd.ToolingUtils.generateLFQExperimentAnnotation;
import static com.dmtavt.fragpipe.tabs.TabWorkflow.manifestExt;
import static com.github.chhh.utils.OsUtils.isUnix;
import static com.github.chhh.utils.OsUtils.isWindows;
import static com.github.chhh.utils.SwingUtils.createClickableHtml;
import static com.github.chhh.utils.SwingUtils.showErrorDialogWithStacktrace;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.messages.NoteConfigDiann;
import com.dmtavt.fragpipe.tools.diann.Diann;
import com.dmtavt.fragpipe.tools.diann.DiannToMsstats;
import com.dmtavt.fragpipe.tools.diann.Localization;
import com.dmtavt.fragpipe.tools.diann.PlexDiaHelper;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdDiann extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdDiann.class);
  private static final String NAME = "DIA-NN";
  private static final List<String> SUPPORTED_FORMATS_WIN = Arrays.asList("mzML", "d", "dia", "wiff", "raw");
  private static final List<String> SUPPORTED_FORMATS_LINUX = Arrays.asList("mzML", "d", "dia");
  private static final Pattern labelPattern = Pattern.compile("([A-Znc*]+)([\\d.+-]+)");

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

  public boolean configure(Component comp, Collection<LcmsFileGroup> lcmsFileGroups, int nThreads, Set<String> quantificationStrategy, boolean usePredict, boolean unrelatedRuns, float qvalue, boolean useRunSpecificProteinQvalue, String libraryPath, String additionalCmdOpts, boolean isDryRun, boolean isRunPlex, boolean generateMsstats, String lightString, String mediumString, String heavyString, Path jarFragpipe) {

    initPreConfig();

    if (isRunPlex) {
      if ((lightString == null || lightString.isEmpty()) && (mediumString == null || mediumString.isEmpty()) && (heavyString == null || heavyString.isEmpty())) {
        SwingUtils.showErrorDialog(comp, "All light, medium, and heavy labels are empty.<br>Please disable/uncheck <b>plexDIA</b> in <b>Quant (DIA)</b> tab or provide label info.", "No label info");
        return false;
      }

      final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(BATMASS_IO_JAR));
      if (classpathJars == null) {
        return false;
      }

      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      Path libsDir = root.resolve("lib");
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib");
        log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
      }

      List<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList());
      try {
        toJoin.addAll(Files.walk(libsDir).
            filter(p -> p.getFileName().toString().endsWith(".jar")).
            filter(p -> p.getFileName().toString().startsWith("fragpipe-")).
            map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList())
        );
      } catch (IOException ex) {
        ex.printStackTrace();
        return false;
      }

      toJoin.add(jarFragpipe.toAbsolutePath().normalize().toString());
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
        cmd.add("library.tsv");
        cmd.add("--out");
        cmd.add("library_2.tsv");
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(groupWd.toFile());
        pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + ": Prepare plex library").create());
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

    for (LcmsFileGroup group : lcmsFileGroups) {
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
          log.warn("Running DIA-NN with Thermo raw files requires Thermo MS File Reader is installed. If you do not have it installed, please convert to mzML prior to running FragPipe.");
        } else {
          int res = JOptionPane.showConfirmDialog(comp, createClickableHtml("Running DIA-NN with Thermo raw files requires <a href=\"https://thermo.flexnetoperations.com/control/thmo/login?nextURL=%2Fcontrol%2Fthmo%2Fdownload%3Felement%3D6306677\">Thermo MS File Reader</a> is installed.<br>If you do not have it installed, please convert to mzML prior to running FragPipe.<br>\"Yes\" to continue. \"No\" to cancel."), "Data format warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
          if (JOptionPane.YES_OPTION != res) {
            return false;
          }
        }
      }

      exts = inputLcmsFiles.stream().map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase())).distinct().filter(ext -> ext.equals("wiff")).collect(Collectors.toList());
      if (!exts.isEmpty()) {
        if (Fragpipe.headless) {
          log.warn("Running DIA-NN with WIFF files requires additional library is installed. If you do not have it installed, please convert to mzML prior to running FragPipe.");
        } else {
          int res = JOptionPane.showConfirmDialog(comp, createClickableHtml("Running DIA-NN with WIFF files requires <a href=\"https://github.com/vdemichev/DiaNN/blob/master/README.md#raw-data-formats\">additional library</a> is installed.<br>If you do not have it installed, please convert to mzML prior to running FragPipe.<br>\"Yes\" to continue. \"No\" to cancel."), "Data format warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
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
        final Path diannOutputDirectory = groupWd.resolve("diann-output");
        if (Files.exists(diannOutputDirectory.getParent())) { // Dry run does not make directories, so does not write the file.
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
      cmd.add((!isRunPlex && libraryPath != null && !libraryPath.isEmpty()) ? libraryPath : (isRunPlex ? "library_2.tsv" : "library.tsv"));
      cmd.add("--threads");
      cmd.add(String.valueOf(nThreads));
      cmd.add("--verbose");
      cmd.add("1");
      cmd.add("--out");
      cmd.add("diann-output" + File.separator + "report.tsv");
      cmd.add("--qvalue");
      cmd.add(String.valueOf(qvalue));
      if (useRunSpecificProteinQvalue) {
        cmd.add("--matrix-spec-q");
      }
      cmd.add("--matrices");
      cmd.add("--no-prot-inf");
      cmd.add("--smart-profiling");
      cmd.add("--no-quant-files");
      cmd.addAll(quantificationStrategy);
      if (unrelatedRuns) {
        cmd.add("--individual-mass-acc");
        cmd.add("--individual-windows");
      }
      if (usePredict) {
        cmd.add("--predictor");
        cmd.add("--dl-no-rt");
        cmd.add("--dl-no-im");
        cmd.add("--strip-unknown-mods");
      }
      if (generateMsstats) {
        cmd.add("--report-lib-info");
      }
      if (isRunPlex) {
        try {
          cmd.addAll(getPlexDiannFlags(lightString, mediumString, heavyString));
        } catch (Exception e) {
          SwingUtils.showErrorDialog(comp, e.getMessage(), "Error parsing plex settings");
          return false;
        }
      }
      if (!additionalCmdOpts.isEmpty()) {
        cmd.add(additionalCmdOpts);
      }

      try {
        final Path filelist = groupWd.resolve("filelist_diann.txt");
        if (Files.exists(filelist.getParent())) { // Dry run does not make directories, so does not write the file.
          BufferedWriter bufferedWriter = Files.newBufferedWriter(filelist);
          for (Path p : sizeInputLcms.values().stream().flatMap(List::stream).collect(Collectors.toList())) {
            bufferedWriter.write("--f " + p.toAbsolutePath() + "\n");
          }
          bufferedWriter.close();
        }
        cmd.add("--cfg");
        cmd.add(isUnix() ? (filelist.toAbsolutePath() + "--") : filelist.toAbsolutePath().toString()); // https://github.com/vdemichev/DiaNN/issues/641#issuecomment-1479416716
      } catch (IOException ex) {
        throw new UncheckedIOException(ex);
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());
      if (LD_PRELOAD_str != null) {
        pb.environment().put("LD_PRELOAD", LD_PRELOAD_str);
      }
      pbis.add(PbiBuilder.from(pb));

      if (isWindows()) {
        // Plotting
        List<String> cmd2 = new ArrayList<>();
        cmd2.add(diannPath.replaceAll("DiaNN\\.exe$", "dia-nn-plotter.exe"));
        cmd2.add("diann-output" + File.separator + "report.stats.tsv");
        cmd2.add("diann-output" + File.separator + "report.tsv");
        cmd2.add("diann-output" + File.separator + "report.pdf");
        ProcessBuilder pb2 = new ProcessBuilder(cmd2);
        pb2.directory(groupWd.toFile());
        pbis.add(PbiBuilder.from(pb2));
      }
    }

    {
      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      Path libsDir = root.resolve("lib");
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib");
        log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
      }

      List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(BATMASS_IO_JAR));
      if (classpathJars == null) {
        return false;
      }

      List<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList());

      try {
        toJoin.addAll(Files.walk(libsDir).filter(p -> p.getFileName().toString().endsWith(".jar")).filter(p -> {
          String t = p.getFileName().toString();
          return t.startsWith("fragpipe-");
        }).map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList()));
      } catch (Exception ex) {
        ex.printStackTrace();
        return false;
      }

      toJoin.add(jarFragpipe.toAbsolutePath().normalize().toString());
      final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));

      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-cp");
      cmd.add(classpath);
      cmd.add(Localization.class.getCanonicalName());
      cmd.add(wd.toAbsolutePath().toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.resolve("diann-output").toFile());
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + ": Propagate localization").create());
    }

    if (generateMsstats) {
      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      Path libsDir = root.resolve("lib");
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib");
        log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
      }

      List<String> toJoin = new ArrayList<>();
      try {
        toJoin.addAll(Files.walk(libsDir).
            filter(p -> p.getFileName().toString().endsWith(".jar")).
            filter(p -> {
              String t = p.getFileName().toString();
              return t.startsWith("fragpipe-") || t.startsWith("commons-io");
            }).
            map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList())
        );
      } catch (IOException ex) {
        ex.printStackTrace();
        return false;
      }

      toJoin.add(jarFragpipe.toAbsolutePath().normalize().toString());
      final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));

      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-cp");
      cmd.add(classpath);
      cmd.add(DiannToMsstats.class.getCanonicalName());
      cmd.add("report.tsv");
      cmd.add("./");
      cmd.add(wd.resolve("psm.tsv").toAbsolutePath().toString());
      cmd.add(String.valueOf(qvalue));
      if (useRunSpecificProteinQvalue) {
        cmd.add(String.valueOf(qvalue));
      } else {
        cmd.add("1");
      }
      cmd.add(String.valueOf(qvalue));
      cmd.add(String.valueOf(qvalue));
      cmd.add(wd.resolve("fragpipe-files" + manifestExt).toAbsolutePath().toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.resolve("diann-output").toFile());
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + ": Convert DIA-NN output to MSstats.csv").create());
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
//        libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib");
//        log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
//      }
//
//      List<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList());
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
//        cmd.add("diann-output" + File.separator + "report.tsv");
//        cmd.add("--output-dir");
//        cmd.add(groupWd.toAbsolutePath().toString());
//        ProcessBuilder pb = new ProcessBuilder(cmd);
//        pb.directory(groupWd.toFile());
//        pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + ": Process DIA-NN output").create());
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
        log.error(String.format("%s can't work with '.%s' files. You can convert files using msconvert from ProteoWizard.", NAME, String.join(", ", notSupportedExts)));
      } else {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("<html>%s can't work with '.%s' files.<br/>", NAME, String.join(", ", notSupportedExts)));
        if (notSupportedExts.contains(".raw") || notSupportedExts.contains("raw")) {
          sb.append("Support for raw files requires Windows and <a href=\"https://thermo.flexnetoperations.com/control/thmo/login?nextURL=%2Fcontrol%2Fthmo%2Fdownload%3Felement%3D6306677\">Thermo MS File Reader</a> to be installed.<br/>It is essential to use specifically the version by the link above (3.0 SP3).<br/>");
        }
        if (notSupportedExts.contains(".wiff") || notSupportedExts.contains("wiff")) {
          sb.append("Support for wiff files requires Windows.<br>");
        }
        sb.append(String.format("Compatible formats are: %s<br/>", String.join(", ", supportedFormats)));
        sb.append(String.format("Either remove files from input or disable %s<br/>", NAME));
        sb.append("You can also convert files using <i>msconvert</i> from ProteoWizard.");

        JOptionPane.showMessageDialog(comp, sb.toString(), NAME + " error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }
    return true;
  }
}
