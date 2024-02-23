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

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.messages.NoteConfigDiann;
import com.dmtavt.fragpipe.tools.diann.Diann;
import com.github.chhh.utils.SwingUtils;
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
  public static final String JAR_MSBOOSTER_NAME = "msbooster-1.2.2.jar";
  public static final String JAR_MSBOOSTER_MAIN_CLASS = "Features.MainClass";
  private static final String[] JAR_DEPS = {BATMASS_IO_JAR};
  private static final Pattern pattern1 = Pattern.compile("\\.pepXML$");
  private static final Pattern pattern2 = Pattern.compile("_rank[0-9]+\\.pepXML$");

  private final String diannPath;
  private final String LD_PRELOAD_str;

  public CmdMSBooster(boolean isRun, Path workDir) {
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

  public boolean configure(Component comp, int ramGb, int threads, Map<InputLcmsFile, List<Path>> lcmsToFraggerPepxml, boolean predictRT, boolean predictSpectra, boolean useCorrelatedFeatures, boolean hasDda, boolean hasDia, boolean hasGpfDia, boolean hasDiaLib, boolean hasDdaPlus, boolean isRunDiaU, boolean isRunDiaPasefSCentric, boolean isOpenSearch, String rtModel, String spectraModel) {
    initPreConfig();

    // MSBooster does not compatible with open search and mass-offset search.
    if (isOpenSearch) {
      SwingUtils.showErrorDialog(comp, "MSBooster is incompatible with open search. Please disable MSBooster.", NAME + " error");
      return false;
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_MSBOOSTER_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    String fraggerParams;
    if (hasDda || isRunDiaU || isRunDiaPasefSCentric) {
      if (hasDia || hasDiaLib || hasGpfDia) {
        fraggerParams = wd.resolve("fragger_dda.params").toAbsolutePath().toString();
      } else {
        fraggerParams = wd.resolve("fragger.params").toAbsolutePath().toString();
      }
    } else if (hasDia || hasDiaLib) {
      fraggerParams = wd.resolve("fragger_dia.params").toAbsolutePath().toString();
    } else if (hasGpfDia) {
      fraggerParams = wd.resolve("fragger_gpfdia.params").toAbsolutePath().toString();
    } else if (hasDdaPlus) {
      fraggerParams = wd.resolve("fragger_dda_plus.params").toAbsolutePath().toString();
    } else {
      System.err.println("There are no DDA, DIA, GPF-DIA, GPF-Lib, or DDA+ data.");
      return false;
    }

    final Path paramPath = wd.resolve("msbooster_params.txt");

    if (Files.exists(paramPath.getParent())) { // Dry run does not make directories, so does not write the file.
      try {
        BufferedWriter bufferedWriter = Files.newBufferedWriter(paramPath);
        bufferedWriter.write("useDetect = false\n");
        bufferedWriter.write("numThreads = " + threads + "\n");
        bufferedWriter.write("DiaNN = " + diannPath + "\n");
        bufferedWriter.write("renamePin = 1\n");
        bufferedWriter.write("useRT = " + (predictRT ? "true" : "false") + "\n");
        bufferedWriter.write("useSpectra = " + (predictSpectra ? "true" : "false") + "\n");
        bufferedWriter.write("fragger = " + fraggerParams + "\n");
        bufferedWriter.write("deletePreds = false\n"); // FragPipe-PDV need the prediction files.
        bufferedWriter.write("rtModel = " + rtModel + "\n");
        bufferedWriter.write("spectraModel = " + spectraModel + "\n");

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
        bufferedWriter.write("useMultipleCorrelatedFeatures = " + useCorrelatedFeatures);
        bufferedWriter.close();
      } catch (IOException ex) {
        ex.printStackTrace();
        return false;
      }
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    if (Fragpipe.headless) {
      cmd.add("-Djava.awt.headless=true"); // In some rare case, the server does not have X11 but DISPLAY env var is set, which crashes the headless mode. Setting the headless env to true to prevent the crash.
    }
    cmd.add("-Xmx" + ramGb + "G");
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
}
