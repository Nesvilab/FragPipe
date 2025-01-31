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

import static com.github.chhh.utils.SwingUtils.showErrorDialogWithStacktrace;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.tmtintegrator.QuantLabelAnnotation;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiConfProps;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiPanel;
import com.github.chhh.utils.FileDelete;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Component;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JOptionPane;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdTmtIntegrator extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdTmtIntegrator.class);

  public static final String NAME = "TmtIntegrator";
  public static final String JAR_NAME = "TMT-Integrator-6.0.5.jar";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "raw", "d");
  public static final String CONFIG_FN = "tmt-integrator-conf.yml";
  public static final String CONFIG_FN_2 = "tmt-integrator-conf-unmod.yml";

  public CmdTmtIntegrator(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, SUPPORTED_FORMATS);
    if (!notSupportedExts.isEmpty()) {
      if (Fragpipe.headless) {
        log.error(String.format("TMT-Integrator doesn't support '.%s' files.", String.join(", ", notSupportedExts)));
      } else {
        JOptionPane.showMessageDialog(comp, String.format("<html>TMT-Integrator doesn't support '.%s' files.", String.join(", ", notSupportedExts)), NAME + " error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }
    return true;
  }

  public boolean configure(TmtiPanel panel, boolean isDryRun, int ramGb, Map<LcmsFileGroup, Path> mapGroupsToProtxml, boolean doMsstats, Map<LcmsFileGroup, Path> groupAnnotationMap, boolean isSecondUnmodRun) {
    isConfigured = false;

    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Stream.of(JAR_NAME));
    if (classpathJars == null) {
      return false;
    }

//    // check Thermo compatibility
//    final List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
//    final Path extLibsThermo = CmdMsfragger.searchExtLibsThermo(Collections.singletonList(binFragger.getParent()));
//    if (extLibsThermo != null) {
//      sup.add("RAW");
//    }

    // see if input files match compatibility table
    if (!checkCompatibleFormats(panel, mapGroupsToProtxml)) {
      return false;
    }

    // write MSstatsTMT_annotation
    if (!isDryRun && doMsstats) {
      try {
        String line;
        BufferedWriter bw = Files.newBufferedWriter(wd.resolve("MSstatsTMT_annotation.csv"));
        bw.write("Run,Fraction,TechRepMixture,Mixture,Channel,BioReplicate,Condition\n");
        for (Map.Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
          Path annotationPath = groupAnnotationMap.get(e.getKey());
          BufferedReader br = new BufferedReader(new FileReader(annotationPath.toFile()));
          while ((line = br.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) {
              continue;
            }
            String[] parts = line.split("\\s");
            if (parts[1].trim().equalsIgnoreCase("na")) {
              continue;
            }
            String[] parts2 = parts[1].trim().split("_");

            int fraction = 1;
            int replicate = 1;
            for (InputLcmsFile inputLcmsFile : e.getKey().lcmsFiles) {
              if (parts2.length == 2) {
                bw.write(FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString()) + "," + fraction + "," + replicate + "," + e.getKey().name + "," + parts[0].trim() + "," + parts[1].trim() + "," + parts2[1].trim() + "\n");
              } else {
                bw.write(FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString()) + "," + fraction + "," + replicate + "," + e.getKey().name + "," + parts[0].trim() + "," + parts[1].trim() + ",\n");
              }
              ++fraction;
            }
          }
          br.close();
        }
        bw.close();
      } catch (Exception ex) {
        showErrorDialogWithStacktrace(ex, panel);
        return false;
      }
    }

    // write and check TMT-I config file
    Path pathConf;
    final String tmtOutDirName;
    if (isSecondUnmodRun) {
      tmtOutDirName = "tmt-report-unmod";   // special second run for unmodified peptides to compare
    } else {
      tmtOutDirName = "tmt-report";
    }

    final Path outDir = getWd().resolve(tmtOutDirName);
    try {
      if (isSecondUnmodRun) {
        pathConf = wd.resolve(CONFIG_FN_2);
      } else {
        pathConf = wd.resolve(CONFIG_FN);
      }
      Files.deleteIfExists(pathConf);
      if (!isDryRun) {
        FileDelete.deleteFileOrFolder(outDir);
        Files.createDirectories(outDir);
      }
      log.debug("Writing {} config file", NAME);
      if (!Files.exists(pathConf.toAbsolutePath().getParent())) {
        log.debug(NAME + " config required presence of output work dir, creating: {}", pathConf.toAbsolutePath().getParent());
        Files.createDirectories(pathConf.toAbsolutePath().getParent());
      }
      Map<String, String> conf = panel.formToConfig(outDir.toString(), panel.getSelectedLabel(), isSecondUnmodRun);

      // check that there is a reference channel set in each annotation file
      String refTag = conf.get("ref_tag");
      if (StringUtils.isNullOrWhitespace(refTag)) {
        SwingUtils.showWarningDialog(panel, "Reference channel tag can't be left empty\n"
            + "in " + NAME + " config, unless selecting Virtual reference option.", NAME + " Config");
      }

      Set<String> ss = new HashSet<>();
      for (Path path : groupAnnotationMap.values()) {
        List<QuantLabelAnnotation> annotations = TmtiPanel.parseTmtAnnotationFile(path.toFile());
        for (QuantLabelAnnotation a : annotations) {
          if (!a.getSample().equalsIgnoreCase("na") && !ss.add(a.getSample())) {
            SwingUtils.showErrorDialog(panel, "Duplicate samples found in annotation files. The sample names must be unique among all experiments.", "ERROR: duplicate label");
            return false;
          }
        }
      }

      List<Path> filesWithoutRefChannel = new ArrayList<>();
      // only check for presence of reference channels if "Define Reference is set to "Reference Sample"
      if (TmtiConfProps.COMBO_ADD_REF_CHANNEL.equalsIgnoreCase(panel.getDefineReference())) {
        for (Path path : groupAnnotationMap.values()) {
          List<QuantLabelAnnotation> annotations = TmtiPanel
              .parseTmtAnnotationFile(path.toFile());
          if (annotations.stream().noneMatch(a -> a.getSample().contains(refTag))) {
            filesWithoutRefChannel.add(path);
          }
        }
      }
      if (!filesWithoutRefChannel.isEmpty()) {
        String files = filesWithoutRefChannel.stream().map(Path::toString)
            .collect(Collectors.joining("\n"));
        SwingUtils.showWarningDialog(panel, "Found annotation files without reference channel\n"
            + "specified:\n" + files
            + "\n\nOne sample name in each annotation file must start with\n"
            + "the reference tag you set. Currently it is set to: '" + refTag + "'.\n"
                + "You can change that in Quant tab, [Ref Sample Tag] text field.",

            NAME + " Config");
        return false;
      }

      for (Path path : groupAnnotationMap.values()) {
        List<QuantLabelAnnotation> annotations = TmtiPanel.parseTmtAnnotationFile(path.toFile());
        for (QuantLabelAnnotation a : annotations) {
          if (a.getSample().trim().isEmpty()) {
            SwingUtils.showErrorDialog(panel, "Empty sample name found in annotation file: " + path.toAbsolutePath() + ".<br>To exclude a channel, use <b>NA</b> as the sample name.", "ERROR: empty sample name");
            return false;
          }
        }
      }

      // TMT-Integrator can crash if a mod site probability is >= 0 when no mods are provided
      if (panel.getMinSiteProb() >= 0 && panel.getModTag().isEmpty()) {
        if (Fragpipe.headless) {
          log.error("TMT Integrator Min mod site probability is specified, but no mods are provided. Please set Min site prob to -1 or specify mods.");
        } else {
          JOptionPane.showMessageDialog(panel, "TMT Integrator Min mod site probability is specified, but no mods are provided. Please set Min site prob to -1 or specify mods.", NAME + " error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }


      try (BufferedWriter bw = Files.newBufferedWriter(pathConf, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
        panel.writeConfig(bw, conf);
      }
      List<String> cmdCheck = new ArrayList<>();
      cmdCheck.add(Fragpipe.getBinJava());
      cmdCheck.add("-jar");
      cmdCheck.add(constructClasspathString(classpathJars));
      cmdCheck.add(pathConf.toString());
      cmdCheck.add("--ValParam");
      ProcessBuilder pb = new ProcessBuilder(cmdCheck);
      pb.directory(wd.toFile());
      log.debug("Running TMT-I config file check: {}", String.join(", ", pb.command()));
      Process p = pb.start();
      int checkExitCode = p.waitFor();
      if (checkExitCode != 0) {
        if (Fragpipe.headless) {
          log.error("Invalid TMT Integrator config file, exit code " + checkExitCode);
        } else {
          JOptionPane.showMessageDialog(panel, "Invalid TMT Integrator config file, exit code " + checkExitCode, "TMT Integrator configuration", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
    } catch (IOException e) {
      log.error("Error while configuring TMT-Integrator", e);
      return false;
    } catch (InterruptedException e) {
      log.error("Error checking TMT Integrator written config file validity", e);
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-Xmx" + ramGb + "G");
    cmd.add("-jar");
    cmd.add(constructClasspathString(classpathJars));
    cmd.add(pathConf.toString());
    mapGroupsToProtxml.keySet().forEach(g -> {
      cmd.add(g.outputDir(wd).resolve("psm.tsv").toString());
    });

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }
}
