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

import java.awt.Component;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdTransferLearning extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdTransferLearning.class);
  private static final String JAR_MSBOOSTER_TRAINER = "transferlearn.Trainer";
  private static final String JAR_MSBOOSTER_PREDICTOR = "transferlearn.Predictor";
  private static final Pattern pattern1 = Pattern.compile("\\.pepXML$");
  private static final Pattern pattern2 = Pattern.compile("_rank[0-9]+\\.pepXML$");

  public static String NAME = "Transfer Learning";

  public static class Credential {
    public final String url;
    public final String apiKey;
    
    public Credential(String url, String apiKey) {
      this.url = url;
      this.apiKey = apiKey;
    }
  }

  public static Credential parseCredential(String credentialPath) throws Exception {
    List<String> lines = Files.readAllLines(Paths.get(credentialPath));
    List<String> nonEmptyLines = new ArrayList<>();
    for (String line : lines) {
      if (!line.trim().isEmpty()) {
        nonEmptyLines.add(line.trim());
      }
    }
    
    String url = null;
    String apiKey = null;
    
    if (nonEmptyLines.size() > 0) {
      url = nonEmptyLines.get(0);
      // Handle BOM (Byte Order Mark) which might be present in Windows files
      if (url.startsWith("\uFEFF")) {
        url = url.substring(1);
      }
    }
    if (nonEmptyLines.size() > 1) {
      apiKey = nonEmptyLines.get(1);
    }
    
    return new Credential(url, apiKey);
  }


  public CmdTransferLearning(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, 
    Path jarFragpipe, 
    int ramGb,
    Map<InputLcmsFile, List<Path>> lcmsToFraggerPepxml,
    String credentialPath,
    boolean isRunTraining,
    boolean isRunPrediction,
    String libraryPath,
    String modelPath, 
    boolean predictMS2,
    boolean predictRT,
    boolean predictIM,
    String outputFormat,
    int peptidesToPredict,
    String customPeptideListPath,
    int minCharge,
    int maxCharge,
    String instrument,
    int nce,
    boolean keepDecoys,
    String fastaPath) {

    initPreConfig();

    if (credentialPath == null || credentialPath.isEmpty()) {
      SwingUtils.showErrorDialog(comp, "A credential file is required for transfer learning.", NAME + " error");
      return false;
    }
    
    Credential credential = null;
    try {
      credential = parseCredential(credentialPath);
    } catch (Exception e) {
      SwingUtils.showErrorDialog(comp, "Failed to read the credential file: " + e.getMessage(), NAME + " error");
      return false;
    }

    String url = credential.url;
    String apiKey = credential.apiKey;

    if (url == null || url.isEmpty()) {
      SwingUtils.showErrorDialog(comp, "URL is required for transfer learning.", NAME + " error");
      return false;
    }

    if (apiKey == null || apiKey.isEmpty()) {
      SwingUtils.showErrorDialog(comp, "API key is required for transfer learning.", NAME + " error");
      return false;
    }

    if (isRunPrediction && peptidesToPredict == 2 && (customPeptideListPath == null || customPeptideListPath.isEmpty())) {
      SwingUtils.showErrorDialog(comp, "Custom peptide list path is empty.", NAME + " error");
      return false;
    }

    if (isRunTraining && (libraryPath == null || libraryPath.isEmpty())) {
      libraryPath = wd.resolve("library.tsv").toAbsolutePath().normalize().toString();
    }

    if (isRunPrediction && (modelPath == null || modelPath.isEmpty())) {
      modelPath = wd.resolve("fragpipe-transfer-learning-model.zip").toAbsolutePath().normalize().toString();
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(CmdMSBooster.JAR_MSBOOSTER_NAME).concat(CmdMSBooster.JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    // training
    if (isRunTraining) {
      List<String> cmdTrain = new ArrayList<>();
      cmdTrain.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmdTrain.add("-Djava.awt.headless=true");
      }
      cmdTrain.add("-Xmx" + ramGb + "G");
      cmdTrain.add("-cp");
      cmdTrain.add(constructClasspathString(classpathJars));
      cmdTrain.add(JAR_MSBOOSTER_TRAINER);
      cmdTrain.add("--url");
      cmdTrain.add(url);
      cmdTrain.add("--api-key");
      cmdTrain.add(apiKey);
      cmdTrain.add("--library");
      cmdTrain.add(libraryPath);
      cmdTrain.add("--output-dir");
      cmdTrain.add(wd.toAbsolutePath().normalize().toString());
      cmdTrain.add("--basename");
      cmdTrain.add("fragpipe-transfer-learning-model");

      ProcessBuilder pbTrain = new ProcessBuilder(cmdTrain);
      pbTrain.directory(wd.toFile());
      pbis.add(new PbiBuilder().setPb(pbTrain).setName(getCmdName() + " training").create());
    }

    // prediction
    if (isRunPrediction) {
      final Path paramPath = wd.resolve("transfer_learning_params.txt");
      if (Files.exists(paramPath.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
        try {
          BufferedWriter bufferedWriter = Files.newBufferedWriter(paramPath);
          if (peptidesToPredict == 0) {
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
          } else {
            bufferedWriter.write("instrument = " + instrument + "\n");
            bufferedWriter.write("NCE = " + nce + "\n");
          }
          bufferedWriter.close();
        } catch (Exception ex) {
          ex.printStackTrace();
          return false;
        }
      }

      List<String> cmdPredict = new ArrayList<>();
      cmdPredict.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmdPredict.add("-Djava.awt.headless=true");
      }
      cmdPredict.add("-Xmx" + ramGb + "G");
      cmdPredict.add("-cp");
      cmdPredict.add(constructClasspathString(classpathJars));
      cmdPredict.add(JAR_MSBOOSTER_PREDICTOR);
      cmdPredict.add("--url");
      cmdPredict.add(url);
      cmdPredict.add("--api-key");
      cmdPredict.add(apiKey);
      cmdPredict.add("--model");
      cmdPredict.add(modelPath);
      cmdPredict.add("--ms2");
      cmdPredict.add(String.valueOf(predictMS2));
      cmdPredict.add("--rt");
      cmdPredict.add(String.valueOf(predictRT));
      cmdPredict.add("--im");
      cmdPredict.add(String.valueOf(predictIM));
      cmdPredict.add("--output-format");
      cmdPredict.add(outputFormat);
      cmdPredict.add("--paramsList");
      cmdPredict.add(paramPath.toAbsolutePath().normalize().toString());
      if (peptidesToPredict > 0) {
        cmdPredict.add("--min-charge");
        cmdPredict.add(String.valueOf(minCharge));
        cmdPredict.add("--max-charge");
        cmdPredict.add(String.valueOf(maxCharge));
        cmdPredict.add("--peptide-list-to-predict");
        if (peptidesToPredict == 1) {
          cmdPredict.add(wd.resolve("peptide_list.parquet").toAbsolutePath().normalize().toString());
        } else if (peptidesToPredict == 2) {
          cmdPredict.add(customPeptideListPath);
        }
      } else if (peptidesToPredict == 0) {
        cmdPredict.add("--keep-decoys");
        cmdPredict.add(keepDecoys ? "1" : "0");
      }
      cmdPredict.add("--output-dir");
      cmdPredict.add(wd.toAbsolutePath().normalize().toString());
      cmdPredict.add("--basename");
      cmdPredict.add("fragpipe-predicted-speclib");
      cmdPredict.add("--fasta");
      cmdPredict.add(fastaPath);

      ProcessBuilder pbPredict = new ProcessBuilder(cmdPredict);
      pbPredict.directory(wd.toFile());
      pbis.add(new PbiBuilder().setPb(pbPredict).setName(getCmdName() + " prediction").create());
    }

    isConfigured = true;
    return true;
  }
}

