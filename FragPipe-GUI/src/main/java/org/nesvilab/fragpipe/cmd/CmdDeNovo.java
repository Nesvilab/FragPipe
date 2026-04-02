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

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.*;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

public class CmdDeNovo extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdDeNovo.class);
  public static final String JAR_FRAGNOVO_CLIENT = "fragnovo-client-1.0.0.jar";
  public static String NAME = "FragNovo";

  public CmdDeNovo(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp,
      String credentialPath,
      List<InputLcmsFile> lcmsFiles,
      Map<String, LcmsFileGroup> lcmsFileGroups,
      boolean isRunFineTuning,
      boolean isRunPrediction,
      boolean isRunLoraPrediction,
      int precursorMassTol,
      int isotopeErrorMin,
      int isotopeErrorMax,
      boolean useIrt,
      String newTokens,
      String loraWeightsPath,
      String calFilePath,
      String modelName,
      int timeout) {

    initPreConfig();

    if (isRunPrediction && (isRunFineTuning || isRunLoraPrediction)) {
      SwingUtils.showErrorDialog(comp, "Base prediction and LoRA fine-tuning/prediction cannot be enabled at the same time.", NAME + " error");
      return false;
    }

    if (credentialPath == null || credentialPath.isEmpty()) {
      SwingUtils.showErrorDialog(comp, "A credential file is required for De Novo sequencing.", NAME + " error");
      return false;
    }

    CmdTransferLearning.Credential credential;
    try {
      credential = CmdTransferLearning.parseCredential(credentialPath);
    } catch (Exception e) {
      SwingUtils.showErrorDialog(comp, "Failed to read the credential file: " + e.getMessage(), NAME + " error");
      return false;
    }

    String url = credential.url;
    String apiKey = credential.apiKey;

    if (url == null || url.isEmpty()) {
      SwingUtils.showErrorDialog(comp, "URL is required for De Novo sequencing.", NAME + " error");
      return false;
    }

    if (apiKey == null || apiKey.isEmpty()) {
      SwingUtils.showErrorDialog(comp, "API key is required for De Novo sequencing.", NAME + " error");
      return false;
    }

    final List<Path> clientJars = FragpipeLocations.checkToolsMissing(Stream.of(JAR_FRAGNOVO_CLIENT));
    if (clientJars == null) {
      return false;
    }
    Path fragNovoClientJar = clientJars.get(0);

    // Collect all DDA mzML file paths
    List<Path> ddaMzmlFiles = new ArrayList<>();
    for (InputLcmsFile lcms : lcmsFiles) {
      if (lcms.getDataType().contentEquals("DDA")) {
        ddaMzmlFiles.add(resolveUncalibratedMzml(lcms));
      }
    }

    if (ddaMzmlFiles.isEmpty()) {
      SwingUtils.showErrorDialog(comp, "No DDA LCMS files found for de novo sequencing.", NAME + " error");
      return false;
    }

    Path outputDir = wd.resolve("fragnovo");
    try {
      Files.createDirectories(outputDir);
    } catch (Exception ex) {
      log.error("Failed to create FragNovo output directory", ex);
      SwingUtils.showErrorDialog(comp, "Failed to create FragNovo output directory: " + ex.getMessage(), NAME + " error");
      return false;
    }

    // Generate the config YAML file from GUI parameters
    Path configFile = wd.resolve("fragnovo_config.yml");
    if (Files.exists(configFile.toAbsolutePath().getParent())) {
      try {
        BufferedWriter writer = Files.newBufferedWriter(configFile);
        writer.write("precursor_mass_tol: " + precursorMassTol + "\n");
        writer.write("isotope_error_range: [" + isotopeErrorMin + ", " + isotopeErrorMax + "]\n");
        writer.write("use_irt: " + (useIrt ? "True" : "False") + "\n");
        writer.write("load_file_name: \"./" + modelName + ".ckpt\"\n");
        if (newTokens != null && !newTokens.trim().isEmpty()) {
          writer.write("new_tokens:\n");
          for (String entry : newTokens.trim().split(";")) {
            String e = entry.trim();
            if (e.isEmpty()) {
              continue;
            }
            int colonIdx = e.lastIndexOf(':');
            if (colonIdx > 0) {
              String tokenName = e.substring(0, colonIdx).trim();
              String mass = e.substring(colonIdx + 1).trim();
              writer.write("  \"" + tokenName + "\": " + mass + "\n");
            }
          }
        }
        writer.close();
      } catch (Exception ex) {
        log.error("Failed to write FragNovo config file", ex);
        SwingUtils.showErrorDialog(comp, "Failed to write FragNovo config file: " + ex.getMessage(), NAME + " error");
        return false;
      }
    }

    // Write the mzML list file (shared by all commands)
    Path mzmlListFile = wd.resolve("filelist_fragnovo.txt");
    if (Files.exists(mzmlListFile.toAbsolutePath().getParent())) {
      try {
        BufferedWriter writer = Files.newBufferedWriter(mzmlListFile);
        for (Path mzml : ddaMzmlFiles) {
          writer.write(mzml.toAbsolutePath().normalize() + "\n");
        }
        writer.close();
      } catch (Exception ex) {
        log.error("Failed to write FragNovo mzML list file", ex);
        SwingUtils.showErrorDialog(comp, "Failed to write FragNovo mzML list file: " + ex.getMessage(), NAME + " error");
        return false;
      }
    }

    // RT calibration file (optional, used by prediction, fine-tuning, and LoRA prediction)
    Path calFile = null;
    if (calFilePath != null && !calFilePath.trim().isEmpty()) {
      calFile = Paths.get(calFilePath.trim());
    } else if (useIrt) {
      calFile = wd.resolve("MSBooster").resolve("_RTcalibration.txt");
    }

    // Find psm.tsv from the first DDA file's group (for fine-tuning)
    LcmsFileGroup firstDdaGroup = null;
    for (InputLcmsFile lcms : lcmsFiles) {
      if (!lcms.getDataType().contentEquals("DDA")) {
        continue;
      }
      for (LcmsFileGroup group : lcmsFileGroups.values()) {
        if (group.lcmsFiles.contains(lcms)) {
          firstDdaGroup = group;
          break;
        }
      }
      break;
    }

    // LoRA fine-tuning
    if (isRunFineTuning) {
      if (firstDdaGroup == null) {
        SwingUtils.showErrorDialog(comp, "Could not locate the psm.tsv file for fine-tuning.", NAME + " error");
        return false;
      }

      Path psmFile = firstDdaGroup.outputDir(wd).resolve("psm.tsv");

      List<String> cmdTrain = new ArrayList<>();
      cmdTrain.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmdTrain.add("-Djava.awt.headless=true");
      }
      cmdTrain.add("-jar");
      cmdTrain.add(fragNovoClientJar.toAbsolutePath().normalize().toString());
      cmdTrain.add("lora-fine-tune");
      cmdTrain.add("--url");
      cmdTrain.add(url);
      cmdTrain.add("--api-key");
      cmdTrain.add(apiKey);
      cmdTrain.add("--mzml-list");
      cmdTrain.add(mzmlListFile.toAbsolutePath().normalize().toString());
      cmdTrain.add("--psm");
      cmdTrain.add(psmFile.toAbsolutePath().normalize().toString());

      if (calFile != null) {
        cmdTrain.add("--cal");
        cmdTrain.add(calFile.toAbsolutePath().normalize().toString());
      }

      cmdTrain.add("--config");
      cmdTrain.add(configFile.toAbsolutePath().normalize().toString());
      cmdTrain.add("--timeout");
      cmdTrain.add(String.valueOf(timeout));
      cmdTrain.add("--output-dir");
      cmdTrain.add(outputDir.toAbsolutePath().normalize().toString());

      ProcessBuilder pbTrain = new ProcessBuilder(cmdTrain);
      pbTrain.directory(wd.toFile());
      pbis.add(new PbiBuilder().setPb(pbTrain).setName(getCmdName() + " fine-tuning").create());
    }

    // Base model prediction
    if (isRunPrediction) {
      List<String> cmdPredict = new ArrayList<>();
      cmdPredict.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmdPredict.add("-Djava.awt.headless=true");
      }
      cmdPredict.add("-jar");
      cmdPredict.add(fragNovoClientJar.toAbsolutePath().normalize().toString());
      cmdPredict.add("predict");
      cmdPredict.add("--url");
      cmdPredict.add(url);
      cmdPredict.add("--api-key");
      cmdPredict.add(apiKey);
      cmdPredict.add("--mzml-list");
      cmdPredict.add(mzmlListFile.toAbsolutePath().normalize().toString());
      if (calFile != null) {
        cmdPredict.add("--cal");
        cmdPredict.add(calFile.toAbsolutePath().normalize().toString());
      }
      cmdPredict.add("--config");
      cmdPredict.add(configFile.toAbsolutePath().normalize().toString());
      cmdPredict.add("--timeout");
      cmdPredict.add(String.valueOf(timeout));
      cmdPredict.add("--output-dir");
      cmdPredict.add(outputDir.toAbsolutePath().normalize().toString());

      ProcessBuilder pbPredict = new ProcessBuilder(cmdPredict);
      pbPredict.directory(wd.toFile());
      pbis.add(new PbiBuilder().setPb(pbPredict).setName(getCmdName() + " prediction").create());
    }

    // LoRA prediction
    if (isRunLoraPrediction) {
      Path loraWeights = null;
      if (loraWeightsPath != null && !loraWeightsPath.trim().isEmpty()) {
        loraWeights = Paths.get(loraWeightsPath.trim());
      } else if (isRunFineTuning) {
        loraWeights = outputDir.resolve("lora_weights.pt");
      }

      if (loraWeights == null) {
        SwingUtils.showErrorDialog(comp, "LoRA prediction requires LoRA weights. Either specify a weights file or enable fine-tuning.", NAME + " error");
        return false;
      }

      List<String> cmdLoraPredict = new ArrayList<>();
      cmdLoraPredict.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmdLoraPredict.add("-Djava.awt.headless=true");
      }
      cmdLoraPredict.add("-jar");
      cmdLoraPredict.add(fragNovoClientJar.toAbsolutePath().normalize().toString());
      cmdLoraPredict.add("lora-predict");
      cmdLoraPredict.add("--url");
      cmdLoraPredict.add(url);
      cmdLoraPredict.add("--api-key");
      cmdLoraPredict.add(apiKey);
      cmdLoraPredict.add("--mzml-list");
      cmdLoraPredict.add(mzmlListFile.toAbsolutePath().normalize().toString());
      cmdLoraPredict.add("--lora-weights");
      cmdLoraPredict.add(loraWeights.toAbsolutePath().normalize().toString());
      if (calFile != null) {
        cmdLoraPredict.add("--cal");
        cmdLoraPredict.add(calFile.toAbsolutePath().normalize().toString());
      }
      cmdLoraPredict.add("--config");
      cmdLoraPredict.add(configFile.toAbsolutePath().normalize().toString());
      cmdLoraPredict.add("--timeout");
      cmdLoraPredict.add(String.valueOf(timeout));
      cmdLoraPredict.add("--output-dir");
      cmdLoraPredict.add(outputDir.toAbsolutePath().normalize().toString());

      ProcessBuilder pbLoraPredict = new ProcessBuilder(cmdLoraPredict);
      pbLoraPredict.directory(wd.toFile());
      pbis.add(new PbiBuilder().setPb(pbLoraPredict).setName(getCmdName() + " LoRA prediction").create());
    }

    isConfigured = true;
    return true;
  }

  private Path resolveUncalibratedMzml(InputLcmsFile lcmsFile) {
    String fileName = lcmsFile.getPath().getFileName().toString().toLowerCase();
    if (fileName.endsWith(".mzml")) {
      return lcmsFile.getPath().toAbsolutePath().normalize();
    }
    // For raw/d/wiff files, use the _uncalibrated.mzML generated by MSFragger
    String pathStr = lcmsFile.getPath().toAbsolutePath().normalize().toString();
    return Paths.get(StringUtils.upToLastDot(pathStr) + "_uncalibrated.mzML");
  }
}
