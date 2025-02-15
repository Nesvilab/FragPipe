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
import static org.nesvilab.fragpipe.cmd.ToolingUtils.JFREECHART_JAR;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.generateLFQExperimentAnnotation;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.writeIsobaricQuantExperimentAnnotation;
import static org.nesvilab.utils.SwingUtils.showErrorDialogWithStacktrace;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tabs.TabWorkflow.InputDataType;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdIonquant extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdIonquant.class);

  public static final String NAME = "IonQuant";
  public static final String JAR_IONQUANT_MAIN_CLASS = "ionquant.IonQuant";
  public static final String[] JAR_DEPS = {JFREECHART_JAR, BATMASS_IO_JAR};
  private static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");

  public CmdIonquant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }


  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp,
      Path extLibsThermo,
      Path extLibsBruker,
      Path binIonQuant,
      int ramGb,
      Map<String, String> uiCompsRepresentation,
      InputDataType dataType,
      Map<InputLcmsFile, List<Path>> lcmsToFraggerPepxml,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml,
      int nThreads,
      Set<Float> modMassSet,
      boolean isDryRun,
      boolean generateSiteReport,
      boolean generateMSstatsInput,
      boolean writeExperimentAnnotation) {
    return configure(comp,
        extLibsThermo,
        extLibsBruker,
        binIonQuant,
        ramGb,
        uiCompsRepresentation,
        dataType,
        lcmsToFraggerPepxml,
        mapGroupsToProtxml,
        nThreads,
        modMassSet,
        isDryRun,
        true,
        false,
        generateSiteReport,
        generateMSstatsInput,
        20,
        2,
        "tmt10",
        null,
        writeExperimentAnnotation);
  }

  public boolean configure(Component comp,
      Path extLibsThermo,
      Path extLibsBruker,
      Path binIonQuant,
      int ramGb,
      Map<String, String> uiCompsRepresentation,
      InputDataType dataType,
      Map<InputLcmsFile, List<Path>> lcmsToFraggerPepxml,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml,
      int nThreads, Set<Float> modMassSet,
      boolean isDryRun,
      boolean performMS1Quant,
      boolean performIsobaricQuant,
      boolean generateSiteReport,
      boolean generateMSstatsInput,
      float isoTol,
      int isoLevel,
      String isoType,
      Map<LcmsFileGroup, Path> annotationMap,
      boolean writeExperimentAnnotation) {

    initPreConfig();

    List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
    if (extLibsBruker != null) {
      sup.add("d");
    }
    if (extLibsThermo != null) {
      sup.add("raw");
    }
    if (!checkCompatibleFormats(comp, lcmsToFraggerPepxml, sup)) {
      return false;
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());

    if (Fragpipe.headless) {
      cmd.add("-Djava.awt.headless=true"); // In some rare case, the server does not have X11 but DISPLAY env var is set, which crashes the headless mode. Setting the headless env to true to prevent the crash.
    }

    cmd.add("-Xmx" + ramGb + "G");

    if (extLibsBruker != null) {
      cmd.add(createJavaDParamString("libs.bruker.dir", extLibsBruker.toString()));
    } else {
      if (lcmsToFraggerPepxml.keySet().stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".d"))) {
        if (Fragpipe.headless) {
          log.error("When processing .d files IonQuant requires native Bruker libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.");
        } else {
          JOptionPane.showMessageDialog(comp,
              "<html>When processing .d files IonQuant requires native Bruker libraries.<br/>\n"
                  + "Native libraries come with MSFragger zip download, contained in <i>ext</i><br/>\n"
                  + "sub-directory. If you don't have an <i>ext</i> directory next to MSFragger.jar<br/>\n"
                  + "please go to Config tab and Update MSFragger.",
              NAME + " error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }
    }

    if (extLibsThermo != null) {
      cmd.add(createJavaDParamString("libs.thermo.dir", extLibsThermo.toString()));
    } else {
      if (lcmsToFraggerPepxml.keySet().stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".raw"))) {
        if (Fragpipe.headless) {
          log.error("When processing .RAW files IonQuant requires native Thermo libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.");
        } else {
          JOptionPane.showMessageDialog(comp,
              "<html>When processing .RAW files IonQuant requires native Thermo libraries.<br/>\n"
                  + "Native libraries come with MSFragger zip download, contained in <i>ext</i><br/>\n"
                  + "sub-directory. If you don't have an <i>ext</i> directory next to MSFragger.jar<br/>\n"
                  + "please go to Config tab and Update MSFragger.",
              NAME + " error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }
    }

    cmd.add("-cp");
    cmd.add(constructClasspathString(classpathJars, binIonQuant));
    cmd.add(JAR_IONQUANT_MAIN_CLASS);
    cmd.add("--threads");
    cmd.add(String.valueOf(nThreads));
    cmd.add("--perform-ms1quant");
    cmd.add(performMS1Quant ? "1" : "0");
    cmd.add("--perform-isoquant");
    cmd.add(performIsobaricQuant ? "1" : "0");
    cmd.add("--isotol");
    cmd.add(String.valueOf(isoTol));
    cmd.add("--isolevel");
    cmd.add(String.valueOf(isoLevel));
    cmd.add("--isotype");
    cmd.add(isoType);
    cmd.add("--ionmobility");
    cmd.add(dataType == InputDataType.ImMsTimsTof ? "1" : "0");
    cmd.add("--site-reports");
    cmd.add(generateSiteReport ? "1" : "0");
    cmd.add("--msstats");
    cmd.add(generateMSstatsInput ? "1" : "0");

    if (annotationMap != null && !annotationMap.isEmpty()) {
      for (Map.Entry<LcmsFileGroup, Path> annotation : annotationMap.entrySet()) {
        String a = wd.resolve(annotation.getKey().name).resolve("psm.tsv").toString();
        String b = annotation.getValue().toAbsolutePath().toString();
        if (a.contains("=") || b.contains("=")) {
          SwingUtils.showErrorDialog(comp, "There are '=' sign in " + a + " or " + b + ". Please rename the file or directory.", "Error");
          return false;
        }
        cmd.add("--annotation");
        cmd.add(a + "=" + b);
      }
    }

    // always set min exp to 1
    cmd.add("--minexps");
    cmd.add("1");

    final long namedExpCount = mapGroupsToProtxml.keySet().stream().map(group -> group.name).filter(StringUtils::isNotBlank).distinct().count();
    final boolean isMultidir = namedExpCount > 0;

    if (uiCompsRepresentation == null || uiCompsRepresentation.isEmpty()) { // Most sensitivity options without MBR or MaxLFQ for the Isobaric quantification
      cmd.add("--mbr");
      cmd.add("0");
      cmd.add("--maxlfq");
      cmd.add("0");
      cmd.add("--requantify");
      cmd.add("0");
      cmd.add("--mztol");
      cmd.add("10");
      cmd.add("--imtol");
      cmd.add("0.05");
      cmd.add("--rttol");
      cmd.add("1");
      cmd.add("--normalization");
      cmd.add("0");
      cmd.add("--minisotopes");
      cmd.add("1");
      cmd.add("--minscans");
      cmd.add("1");
      cmd.add("--writeindex");
      cmd.add("0");
      cmd.add("--tp");
      cmd.add("0");
      cmd.add("--minfreq");
      cmd.add("0");
      cmd.add("--minions");
      cmd.add("1");
      cmd.add("--locprob");
      cmd.add("0");
      cmd.add("--uniqueness");
      cmd.add("0");
    } else {
      List<String> dynamicParams = Arrays.asList(
          "mbr",
          "maxlfq",
          "requantify",
          "mztol",
          "imtol",
          "rttol",
          "mbrmincorr",
          "mbrrttol",
          "mbrimtol",
          "mbrtoprun",
          "ionfdr",
          "proteinfdr",
          "peptidefdr",
          "normalization",
          "minisotopes",
          "minscans",
          "writeindex",
          "light",
          "medium",
          "heavy",
          "tp",
          "minfreq",
          "minions",
          "excludemods",
          "locprob",
          "uniqueness"
      );

      for (String dynamicParam : dynamicParams) {
        String v = getOrThrow(uiCompsRepresentation, StringUtils.prependOnce(dynamicParam, "ionquant."));
        if ("mbr".equalsIgnoreCase(dynamicParam) && "1".equals(v)) {
          // it's mbr
          if (!isMultidir) {
            // it's not multi exp
            if (Fragpipe.headless) {
              log.error("IonQuant with MBR requires designating LCMS runs to experiments. If in doubt how to resolve this error, just assign all LCMS runs to the same experiment name.");
            } else {
              JOptionPane.showMessageDialog(comp, SwingUtils.makeHtml(
                      "IonQuant with MBR requires designating LCMS runs to experiments.\n"
                          + "See Workflow tab.\n"
                          + "If in doubt how to resolve this error, just assign all LCMS runs to the same experiment name."),
                  NAME + " error", JOptionPane.WARNING_MESSAGE);
            }
            return false;
          }
        }
        if (StringUtils.isNotBlank(v)) {
          if ((dynamicParam.contentEquals("light") || dynamicParam.contentEquals("medium") || dynamicParam.contentEquals("heavy"))) {
            if (getOrThrow(uiCompsRepresentation, "ionquant.use-labeling").contentEquals("false")) { // IonQuant does not have use-labeling parameter. If use-labeling = false, do not write light, medium, or heavy so that IonQuant won't run in label quant model.
              continue;
            }
          }

          cmd.add("--" + dynamicParam);
          cmd.add(v);
        }
      }
    }

    try {
      final Path filelist = wd.resolve("filelist_ionquant.txt");

      if (Files.exists(filelist.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
        BufferedWriter bufferedWriter = Files.newBufferedWriter(filelist);
        bufferedWriter.write("flag\tvalue\n");

        for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
          LcmsFileGroup group = e.getKey();
          Path psmTsv = group.outputDir(wd).resolve("psm.tsv");
          bufferedWriter.write("--psm");
          bufferedWriter.write("\t");
          bufferedWriter.write(wd.relativize(psmTsv).toString());
          bufferedWriter.write("\n");
        }

        if (isMultidir) {
          cmd.add("--multidir");
          cmd.add(".");
        }

        // compute unique lcms file directories
        Set<Path> lcmsDirsUnique = Seq.seq(lcmsToFraggerPepxml.keySet()).map(lcms -> lcms.getPath().toAbsolutePath().getParent())
            .toSet();
        for (Path path : lcmsDirsUnique) {
          bufferedWriter.write("--specdir");
          bufferedWriter.write("\t");
          bufferedWriter.write(StringUtils.appendPrependOnce(path.toString(), null));
          bufferedWriter.write("\n");
        }

        bufferedWriter.close();
      }

      cmd.add("--filelist");
      cmd.add(filelist.toAbsolutePath().toString());

      if (modMassSet != null) {
        Path modMassListPath = wd.resolve("modmasses_ionquant.txt");
        if (Files.exists(modMassListPath.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
          BufferedWriter bufferedWriter = Files.newBufferedWriter(modMassListPath);
          for (float modMass : modMassSet) {
            bufferedWriter.write(modMass + "\n");
          }
          bufferedWriter.close();
        }

        cmd.add("--modlist");
        cmd.add(modMassListPath.toAbsolutePath().toString());
      }
    } catch (IOException ex) {
      throw new UncheckedIOException(ex);
    }

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    if (!isDryRun && writeExperimentAnnotation) {
      try {
        if (performIsobaricQuant && annotationMap != null) {
          writeIsobaricQuantExperimentAnnotation(wd, annotationMap);
        } else if (performMS1Quant) {
          generateLFQExperimentAnnotation(wd, 0);
        }
      } catch (Exception ex) {
        showErrorDialogWithStacktrace(ex, comp);
        return false;
      }
    }

    isConfigured = true;
    return true;
  }

  private String getOrThrow(Map<String, String> m, String key) {
    String s = m.get(key);
    if (s == null)
      throw new IllegalStateException("Could not get key: " + key);
    return s;
  }

  private boolean checkCompatibleFormats(Component comp, Map<InputLcmsFile, List<Path>> lcmsToPepxml, List<String> supportedFormats) {
    List<String> notSupportedExts = getNotSupportedExts1(lcmsToPepxml, supportedFormats);
    if (!notSupportedExts.isEmpty()) {
      if (Fragpipe.headless) {
        log.error(String.format("%s can't work with '.%s' files. You can convert files using msconvert from ProteoWizard.", NAME, String.join(", ", notSupportedExts)));
      } else {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("<html>%s can't work with '.%s' files.<br/>", NAME, String.join(", ", notSupportedExts)));
        if (notSupportedExts.contains(".d") || notSupportedExts.contains("d")) {
          sb.append("Support for Bruker files requires 'ext' folder with 'bruker' sub-folder<br/>\n")
              .append("to be next to your MSFragger.jar. It is shipped with MSFragger.zip distribution.<br/>\n");
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
