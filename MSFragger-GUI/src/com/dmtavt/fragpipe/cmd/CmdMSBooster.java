package com.dmtavt.fragpipe.cmd;

import static com.dmtavt.fragpipe.cmd.ToolingUtils.BATMASS_IO_JAR;
import static com.dmtavt.fragpipe.cmd.ToolingUtils.SMILE_CORE_JAR;
import static com.dmtavt.fragpipe.cmd.ToolingUtils.SMILE_MATH_JAR;
import static com.github.chhh.utils.OsUtils.isUnix;
import static com.github.chhh.utils.OsUtils.isWindows;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdMSBooster extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdMSBooster.class);
  public static String NAME = "MSBooster";
  public static final String JAR_MSBOOSTER_NAME = "msbooster-1.0.jar";
  public static final String JAR_MSBOOSTER_MAIN_CLASS = "Features.MainClass";
  private static final String[] JAR_DEPS = {SMILE_CORE_JAR, SMILE_MATH_JAR, BATMASS_IO_JAR};
  private static final String[] DIANN_SO_DEPS = {"diann_so/libm.so.6", "diann_so/libstdc++.so.6"};
  private static final String[] DIANN_SO_DEPS_libgomp = {"diann_so/libgomp.so.1.0.0"};
  private static final String DIANN_WIN = "diann/1.8/win/DiaNN.exe";
  private static final String DIANN_LINUX = "diann/1.8/linux/diann-1.8";
  private static final Pattern pattern1 = Pattern.compile("\\.pepXML$");
  private static final Pattern pattern2 = Pattern.compile("_rank[0-9]+\\.pepXML$");

  public CmdMSBooster(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, int ramGb, int threads, Map<InputLcmsFile, List<Path>> lcmsToFraggerPepxml, boolean predictRT, boolean predictSpectra, boolean hasDda, boolean hasDia, boolean hasGpfDia) {
    initPreConfig();

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_MSBOOSTER_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
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

    String fraggerParams;
    if (hasDda) {
      if (hasDia || hasGpfDia) {
        fraggerParams = wd.resolve("fragger_dda.params").toAbsolutePath().toString();
      } else {
        fraggerParams = wd.resolve("fragger.params").toAbsolutePath().toString();
      }
    } else if (hasDia) {
      fraggerParams = wd.resolve("fragger_dia.params").toAbsolutePath().toString();
    } else if (hasGpfDia) {
      fraggerParams = wd.resolve("fragger_gpfdia.params").toAbsolutePath().toString();
    } else {
      System.err.println("There are not DDA, DIA, or GPF-DIA.");
      return false;
    }

    final Path paramPath = wd.resolve("msbooster_params.txt");

    if (Files.exists(paramPath.getParent())) { // Dry run does not make directories, so does not write the file.
      try {
        BufferedWriter bufferedWriter = Files.newBufferedWriter(paramPath);
        bufferedWriter.write("useDetect = false\n");
        bufferedWriter.write("numThreads = " + threads + "\n");
        bufferedWriter.write("DiaNN = " + diannPath.get(0) + "\n");
        bufferedWriter.write("renamePin = 1\n");
        bufferedWriter.write("useRT = " + (predictRT ? "true" : "false") + "\n");
        bufferedWriter.write("useSpectra = " + (predictSpectra ? "true" : "false") + "\n");
        bufferedWriter.write("fragger = " + fraggerParams + "\n");

        // compute unique lcms file directories
        bufferedWriter.write("mzmlDirectory = ");
        Set<Path> lcmsDirsUnique = Seq.seq(lcmsToFraggerPepxml.keySet()).map(lcms -> lcms.getPath().getParent()).toSet();
        for (Path path : lcmsDirsUnique) {
          bufferedWriter.write(path.toString() + " ");
        }
        bufferedWriter.write("\n");

        bufferedWriter.write("pinPepXMLDirectory = ");
        Set<String> pinFiles = new HashSet<>();
        for (Entry<InputLcmsFile, List<Path>> e : lcmsToFraggerPepxml.entrySet()) {
          for (Path pepxml : e.getValue()) {
            if (e.getKey().getDataType().contentEquals("DDA")) {
              Matcher matcher = pattern1.matcher(wd.relativize(pepxml).toString());
              pinFiles.add(matcher.replaceAll(".pin"));
            } else {
              Matcher matcher = pattern2.matcher(wd.relativize(pepxml).toString());
              pinFiles.add(matcher.replaceAll(".pin"));
            }
          }
        }
        bufferedWriter.write(String.join(" ", pinFiles) + "\n");
        bufferedWriter.close();
      } catch (IOException ex) {
        ex.printStackTrace();
        return false;
      }
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    if (ramGb > 0) {
      cmd.add("-Xmx" + ramGb + "G");
    }

    cmd.add("-cp");
    cmd.add(constructClasspathString(classpathJars));
    cmd.add(JAR_MSBOOSTER_MAIN_CLASS);
    cmd.add("--paramsList");
    cmd.add(paramPath.toAbsolutePath().toString());

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    if (LD_PRELOAD_str != null)
      pb.environment().put("LD_PRELOAD", LD_PRELOAD_str);
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
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
