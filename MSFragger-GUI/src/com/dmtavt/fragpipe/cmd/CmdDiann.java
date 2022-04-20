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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.cmd;

import static com.github.chhh.utils.OsUtils.isUnix;
import static com.github.chhh.utils.OsUtils.isWindows;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdDiann extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdDiann.class);
  private static final String NAME = "DIA-NN";
  private static final String[] DIANN_SO_DEPS = {"diann_so/libm.so.6", "diann_so/libstdc++.so.6"};
  private static final String[] DIANN_SO_DEPS_libgomp = {"diann_so/libgomp.so.1.0.0"};
  public static final String DIANN_VERSION = "1.8.1";
  public static final String DIANN_WIN = "diann/1.8.1/win/DiaNN.exe";
  public static final String DIANN_LINUX = "diann/1.8.1/linux/diann-1.8.1";
  private static final List<String> SUPPORTED_FORMATS_WIN = Arrays.asList("mzML", "d", "dia", "wiff", "raw");
  private static final List<String> SUPPORTED_FORMATS_LINUX = Arrays.asList("mzML", "d", "dia");


  public CmdDiann(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Collection<LcmsFileGroup> lcmsFileGroups, int nThreads, Set<String> quantificationStrategy, boolean usePredict, float qvalue, float runSpecificProteinQvalue, String libraryPath, String additionalCmdOpts) {

    initPreConfig();

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

      List<String> cmd = new ArrayList<>();
      cmd.add(diannPath.get(0).toAbsolutePath().toString());
      cmd.add("--lib");
      cmd.add((libraryPath != null && !libraryPath.isEmpty()) ? libraryPath : "library.tsv");
      cmd.add("--threads");
      cmd.add(String.valueOf(nThreads));
      cmd.add("--verbose");
      cmd.add("1");
      cmd.add("--out");
      cmd.add("diann-output.tsv");
      cmd.add("--qvalue");
      cmd.add(String.valueOf(qvalue));
      cmd.add("--matrix-spec-q");
      cmd.add(String.valueOf(runSpecificProteinQvalue));
      cmd.add("--matrices");
      cmd.add("--no-prot-inf");
      cmd.add("--smart-profiling");
      cmd.add("--no-quant-files");
      cmd.addAll(quantificationStrategy);
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
        cmd.add(filelist.toAbsolutePath().toString());
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
        cmd2.add("diann-output.stats.tsv");
        cmd2.add("diann-output.tsv");
        cmd2.add("diann-output.pdf");
        ProcessBuilder pb2 = new ProcessBuilder(cmd2);
        pb2.directory(groupWd.toFile());
        pbis.add(PbiBuilder.from(pb2));
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
