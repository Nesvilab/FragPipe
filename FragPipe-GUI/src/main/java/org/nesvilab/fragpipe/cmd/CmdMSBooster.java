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

import static org.nesvilab.fragpipe.cmd.ToolingUtils.BATMASS_IO_JAR;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.UNIMOD_OBO;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.getUnimodOboPath;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.messages.NoteConfigDiann;
import org.nesvilab.fragpipe.tools.diann.Diann;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.io.BufferedWriter;
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
  public static final String JAR_MSBOOSTER_NAME = "MSBooster-1.2.68.jar";
  public static final String JAR_MSBOOSTER_MAIN_CLASS = "mainsteps.MainClass";
  private static final String[] JAR_DEPS = {BATMASS_IO_JAR};
  private static final Pattern pattern1 = Pattern.compile("\\.pepXML$");
  private static final Pattern pattern2 = Pattern.compile("_rank[0-9]+\\.pepXML$");

  private final String diannPath;
  private final String LD_PRELOAD_str;

  public CmdMSBooster(boolean isRun, Path workDir) {
    super(isRun, workDir);
    NoteConfigDiann noteConfigDiann = Bus.getStickyEvent(NoteConfigDiann.class);
    if (noteConfigDiann == null || noteConfigDiann.version.contentEquals("1.9.2")) { // DIA-NN 1.9.2 has a bug.
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
                           int ramGb,
                           int threads,
                           Map<InputLcmsFile, List<Path>> lcmsToFraggerPepxml,
                           boolean predictRT,
                           boolean predictSpectra,
                           boolean predictIm,
                           boolean hasDda,
                           boolean hasDia,
                           boolean hasGpfDia,
                           boolean hasDiaLib,
                           boolean hasDdaPlus,
                           boolean isRunDiaU,
                           boolean isRunDiaTracer,
                           boolean isOpenSearch,
                           String rtModel,
                           String spectraModel,
                           String imModel,
                           boolean findBestRtModel,
                           boolean findBestSpectraModel,
                           boolean findBestImModel,
                           String koinaURL,
                           String libraryPath,
                           String testRtModels,
                           String testSpectraModels,
                           String testImModels) {

    initPreConfig();

    // MSBooster does not compatible with open search and mass-offset search.
    if (isOpenSearch) {
      SwingUtils.showErrorDialog(comp, "MSBooster is incompatible with open search. Please disable MSBooster.", NAME + " error");
      return false;
    }

    if (!predictRT && !predictSpectra) {
      SwingUtils.showErrorDialog(comp, "At least one of <b>Predict RT</b> or <b>Predict spectra</b> must be enabled if you want to run MSBooster.", NAME + " parameter error");
      return false;
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_MSBOOSTER_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    if (koinaURL.isEmpty() && (!rtModel.contentEquals("DIA-NN") || !spectraModel.contentEquals("DIA-NN") || !imModel.contentEquals("DIA-NN"))) {
      SwingUtils.showErrorDialog(comp, "Koina URL is required for non DIA-NN models.\nPlease go to <b>Validation</b> tab and adjust the settings.", NAME + " error");
      return false;
    }

    if (koinaURL.isEmpty() && (findBestRtModel || findBestSpectraModel || findBestImModel)) {
      SwingUtils.showErrorDialog(comp, "Koina URL is required for <b>Find best RT/spectral/IM model</b>.\nPlease go to <b>Validation</b> tab and adjust the settings.", NAME + " error");
      return false;
    }

    String fraggerParams;
    if (hasDda || isRunDiaU || isRunDiaTracer) {
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

    boolean hasTimsTof = false;
    for (InputLcmsFile t : lcmsToFraggerPepxml.keySet()) {
      if (t.getPath().getFileName().toString().endsWith(".d")) {
        hasTimsTof = true;
        break;
      }
    }

    final Path paramPath = wd.resolve("msbooster_params.txt");

    if (Files.exists(paramPath.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
      try {
        BufferedWriter bufferedWriter = Files.newBufferedWriter(paramPath);
        bufferedWriter.write("useDetect = false\n");
        bufferedWriter.write("numThreads = " + threads + "\n");
        bufferedWriter.write("DiaNN = " + diannPath + "\n");
        bufferedWriter.write("renamePin = 1\n");
        bufferedWriter.write("useRT = " + (predictRT ? "true" : "false") + "\n");
        bufferedWriter.write("useSpectra = " + (predictSpectra ? "true" : "false") + "\n");
        bufferedWriter.write("useIM = " + ((hasTimsTof && predictIm) ? "true" : "false") + "\n");
        bufferedWriter.write("fragger = " + fraggerParams + "\n");
        bufferedWriter.write("deletePreds = false\n"); // FragPipe-PDV need the prediction files.
        bufferedWriter.write("rtModel = " + rtModel + "\n");
        bufferedWriter.write("spectraModel = " + spectraModel + "\n");
        bufferedWriter.write("imModel = " + imModel + "\n");
        bufferedWriter.write("findBestRtModel = " + (findBestRtModel ? "true" : "false") + "\n");
        bufferedWriter.write("findBestSpectraModel = " + (findBestSpectraModel ? "true" : "false") + "\n");
        bufferedWriter.write("findBestImModel = " + ((hasTimsTof && findBestImModel) ? "true" : "false") + "\n");
        bufferedWriter.write("KoinaURL = " + koinaURL + "\n");
        if (!libraryPath.isEmpty()) {
          bufferedWriter.write("spectraPredFile = " + libraryPath + "\n");
          bufferedWriter.write("RTPredFile = " + libraryPath + "\n");
        }
        bufferedWriter.write("rtSearchModelsString = " + testRtModels + "\n");
        bufferedWriter.write("ms2SearchModelsString = " + testSpectraModels + "\n");
        bufferedWriter.write("imSearchModelsString = " + testImModels + "\n");
        bufferedWriter.write("unimodObo = " + getUnimodOboPath(UNIMOD_OBO).toAbsolutePath() + "\n");

        // compute unique lcms file directories
        bufferedWriter.write("mzmlDirectory = ");
        Set<Path> lcmsDirsUnique = Seq.seq(lcmsToFraggerPepxml.keySet()).map(lcms -> lcms.getPath().toAbsolutePath().getParent()).toSet();
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
      } catch (Exception ex) {
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
