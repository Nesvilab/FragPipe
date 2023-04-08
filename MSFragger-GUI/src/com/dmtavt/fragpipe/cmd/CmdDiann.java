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
import static com.github.chhh.utils.OsUtils.isUnix;
import static com.github.chhh.utils.OsUtils.isWindows;
import static com.github.chhh.utils.SwingUtils.createClickableHtml;
import static com.github.chhh.utils.SwingUtils.showErrorDialogWithStacktrace;
import static org.apache.commons.lang3.StringUtils.getCommonPrefix;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.diann.PlexDiaHelper;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.apache.commons.io.FilenameUtils;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdDiann extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdDiann.class);
  private static final String NAME = "DIA-NN";
  private static final String[] DIANN_SO_DEPS = {"diann_so/libm.so.6", "diann_so/libstdc++.so.6"};
  private static final String[] DIANN_SO_DEPS_libgomp = {"diann_so/libgomp.so.1.0.0"};
  public static final String DIANN_VERSION = "1.8.2_beta_8";
  public static final String DIANN_WIN = "diann/1.8.2_beta_8/win/DiaNN.exe";
  public static final String DIANN_LINUX = "diann/1.8.2_beta_8/linux/diann-1.8.1.8";
  private static final List<String> SUPPORTED_FORMATS_WIN = Arrays.asList("mzML", "d", "dia", "wiff", "raw");
  private static final List<String> SUPPORTED_FORMATS_LINUX = Arrays.asList("mzML", "d", "dia");


  public CmdDiann(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Collection<LcmsFileGroup> lcmsFileGroups, int nThreads, Set<String> quantificationStrategy, boolean usePredict, boolean unrelatedRuns, float qvalue, boolean useRunSpecificProteinQvalue, String libraryPath, String additionalCmdOpts, boolean isDryRun, boolean isRunPlex, String lightString, String mediumString, String heavyString, Path jarFragpipe) {

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

    final List<Path> diannPath;
    if (isWindows()) {
      diannPath = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_WIN));
    } else if (isUnix()) {
      diannPath = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_LINUX));
    } else {
      System.err.println("DIA-NN only works in Windows and Linux.");
      return false;
    }

    if (diannPath == null || diannPath.isEmpty()) {
      System.err.println("Cannot find DIA-NN executable file.");
      return false;
    }

    if (diannPath.size() > 1) {
      System.err.print("There are more than one DIA-NN executable file: ");
      for (Path p : diannPath) {
        System.err.print(p.toAbsolutePath() + "; ");
      }
      System.err.println();
      return false;
    }

    String LD_PRELOAD_str = getLDPRELOAD(diannPath);

    for (LcmsFileGroup group : lcmsFileGroups) {
      final Path groupWd = group.outputDir(wd);

      Set<Path> inputLcmsPaths = group.lcmsFiles.stream().filter(f -> !f.getDataType().contentEquals("DDA") && !f.getDataType().contentEquals("GPF-DIA") && !f.getDataType().contentEquals("DIA-Lib")).map(InputLcmsFile::getPath).collect(Collectors.toSet());

      if (inputLcmsPaths.isEmpty()) {
        continue;
      }

      List<InputLcmsFile> inputLcmsFiles = group.lcmsFiles.stream().filter(f -> !f.getDataType().contentEquals("DDA") && !f.getDataType().contentEquals("GPF-DIA") && !f.getDataType().contentEquals("DIA-Lib")).collect(Collectors.toList());

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
      cmd.add(diannPath.get(0).toAbsolutePath().toString());
      cmd.add("--lib");
      cmd.add((!isRunPlex && libraryPath != null && !libraryPath.isEmpty()) ? libraryPath : (isRunPlex ? "library_2.tsv" : "library.tsv"));
      cmd.add("--threads");
      cmd.add(String.valueOf(nThreads));
      cmd.add("--verbose");
      cmd.add("1");
      cmd.add("--out");
      cmd.add("diann-output" + File.separator + "diann-output.tsv");
      cmd.add("--qvalue");
      cmd.add(String.valueOf(qvalue));
      cmd.add("--matrix-qvalue");
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
        cmd2.add(diannPath.get(0).toAbsolutePath().toString().replaceAll("DiaNN\\.exe$", "dia-nn-plotter.exe"));
        cmd2.add("diann-output" + File.separator + "diann-output.stats.tsv");
        cmd2.add("diann-output" + File.separator + "diann-output.tsv");
        cmd2.add("diann-output" + File.separator + "diann-output.pdf");
        ProcessBuilder pb2 = new ProcessBuilder(cmd2);
        pb2.directory(groupWd.toFile());
        pbis.add(PbiBuilder.from(pb2));
      }
    }

    if (isRunPlex) {
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
        cmd.add("library_2.tsv");
        cmd.add("--diann-report");
        cmd.add("diann-output" + File.separator + "diann-output.tsv");
        cmd.add("--output-dir");
        cmd.add(groupWd.toAbsolutePath().toString());
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(groupWd.toFile());
        pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + ": Process DIA-NN output").create());
      }
    }

    if (!isDryRun) {
      try {
        Set<String> fileNameSet = new HashSet<>();
        for (LcmsFileGroup lcmsFileGroup : lcmsFileGroups) {
          for (InputLcmsFile inputLcmsFile : lcmsFileGroup.lcmsFiles) {
            String baseName = FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString());
            fileNameSet.add(baseName);
          }
        }

        String commonPrefix = getCommonPrefix(fileNameSet.toArray(new String[0]));
        int a = commonPrefix.length();

        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(wd.resolve("experiment_annotation.tsv").toFile()));
        bufferedWriter.write("file\tsample\tsample_name\tcondition\treplicate\n");
        for (LcmsFileGroup lcmsFileGroup : lcmsFileGroups) {
          for (InputLcmsFile inputLcmsFile : lcmsFileGroup.lcmsFiles) {
            String baseName = FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString());
            String sampleName = baseName.substring(a);
            String[] tt = sampleName.split("_");
            if (tt.length == 3) {
              try {
                int replicate = Integer.parseInt(tt[2]);
                bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + tt[0] + "_" + tt[1] + "\t" + tt[1] + "\t" + replicate + "\n");
              } catch (Exception ex) {
                bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + tt[0] + "_" + tt[1] + "\t" + tt[2] + "\t1\n");
              }
            } else if (tt.length == 2) {
              try {
                int replicate = Integer.parseInt(tt[1]);
                bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + sampleName + "\t" + tt[0] + "\t" + replicate + "\n");
              } catch (Exception ex) {
                bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + sampleName + "\t" + tt[1] + "\t1\n");
              }
            } else {
              bufferedWriter.write(inputLcmsFile.getPath().toAbsolutePath() + "\t" + inputLcmsFile.getPath().toAbsolutePath() + "\t" + sampleName + "\t\t1\n");
            }
          }
        }
        bufferedWriter.close();
      } catch (Exception ex) {
        showErrorDialogWithStacktrace(ex, comp);
        return false;
      }
    }

    isConfigured = true;
    return true;
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

  public static String getLDPRELOAD(List<Path> diannPath) {
    boolean ld_preload;
    String LD_PRELOAD_str = null;
    if (isUnix()) {
      final ProcessBuilder pb = new ProcessBuilder("ldd", diannPath.get(0).toString());
      final java.io.InputStream inputStream;
      try {
        final Process proc = pb.redirectErrorStream(true).start();
        inputStream = proc.getInputStream();
      } catch (IOException e) {
        System.err.println("Failed in checking " + diannPath.get(0).toString());
        return null;
      }
      final String s = new java.util.Scanner(inputStream).useDelimiter("\\A").next();
      ld_preload = s.contains("not found");

      if (ld_preload) {
        final List<Path> diann_so_path = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_SO_DEPS));
        if (diann_so_path == null || diann_so_path.size() != DIANN_SO_DEPS.length) {
          System.err.print(".so files missing");
          return null;
        }
        LD_PRELOAD_str = diann_so_path.get(0).toString() + ":" + diann_so_path.get(1).toString();
        final ProcessBuilder ldd2 = new ProcessBuilder("ldd", diannPath.get(0).toString());
        ldd2.environment().put("LD_PRELOAD", LD_PRELOAD_str);
        final java.io.InputStream ldd2_inputStream;
        try {
          final Process proc = ldd2.redirectErrorStream(true).start();
          ldd2_inputStream = proc.getInputStream();
        } catch (IOException e) {
          System.err.println("Failed in checking " + diannPath.get(0).toString());
          return null;
        }
        final String ldd2_output = new java.util.Scanner(ldd2_inputStream).useDelimiter("\\A").next();
        if (ldd2_output.contains("not found")) {
          final List<Path> diann_so_path2 = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_SO_DEPS_libgomp));
          if (diann_so_path2 == null || diann_so_path2.size() != DIANN_SO_DEPS_libgomp.length) {
            System.err.print(".so files missing");
            return null;
          }
          LD_PRELOAD_str += ":" + diann_so_path2.get(0).toString();
        }
      }
    }
    return LD_PRELOAD_str;
  }
}
