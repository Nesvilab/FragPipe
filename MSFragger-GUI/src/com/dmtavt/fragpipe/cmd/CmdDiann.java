package com.dmtavt.fragpipe.cmd;

import static com.dmtavt.fragpipe.cmd.CmdMSBooster.getLDPRELOAD;
import static com.github.chhh.utils.OsUtils.isUnix;
import static com.github.chhh.utils.OsUtils.isWindows;

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
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;

public class CmdDiann extends CmdBase {

  private static final String NAME = "DIA-NN";
  private static final String[] DIANN_SO_DEPS = {"diann_so/libm.so.6", "diann_so/libstdc++.so.6"};
  private static final String DIANN_WIN = "diann/1.8/win/DiaNN.exe";
  private static final String DIANN_LINUX = "diann/1.8/linux/diann-1.8";
  private static final List<String> SUPPORTED_FORMATS_WIN = Arrays.asList("mzML", "d", "dia", "wiff", "raw");
  private static final List<String> SUPPORTED_FORMATS_LINUX = Arrays.asList("mzML", "d", "dia");


  public CmdDiann(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Collection<LcmsFileGroup> lcmsFileGroups, int nThreads, Set<String> quantificationStrategy, float qvalue, String libraryPath, String additionalCmdOpts) {

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

      Set<String> inputLcmsPaths = group.lcmsFiles.stream().filter(f -> !f.getDataType().contentEquals("DDA")).map(f -> f.getPath().toAbsolutePath().toString()).collect(Collectors.toSet());

      if (inputLcmsPaths.isEmpty()) {
        continue;
      }

      List<InputLcmsFile> inputLcmsFiles = group.lcmsFiles.stream().filter(f -> !f.getDataType().contentEquals("DDA")).collect(Collectors.toList());

      if (!checkCompatibleFormats(comp, inputLcmsFiles, sup)) {
        return false;
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
      cmd.add("--matrices");
      cmd.add("--no-prot-inf");
      cmd.add("--smart-profiling");
      cmd.addAll(quantificationStrategy);
      cmd.add(additionalCmdOpts);

      try {
        final Path filelist = groupWd.resolve("filelist_diann.txt");
        final Path filelist_fixdiann = isUnix() ? groupWd.resolve("filelist_diann.txt ") : filelist;
        if (Files.exists(filelist_fixdiann.getParent())) { // Dry run does not make directories, so does not write the file.
          BufferedWriter bufferedWriter = Files.newBufferedWriter(filelist_fixdiann);
          for (String f : inputLcmsPaths) {
            bufferedWriter.write("--f " + f + "\n");
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
      return false;
    }
    return true;
  }
}
