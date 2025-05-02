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

import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.tools.crystalc.CrystalcPanel;
import org.nesvilab.fragpipe.tools.crystalc.CrystalcParams;
import org.nesvilab.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdCrystalc extends CmdBase {

  public static final String CRYSTALC_VERSION = "1.5.8";
  public static final String JAR_CRYSTALC_NAME = "original-crystalc-" + CRYSTALC_VERSION + ".jar";
  /** Fully qualified name, such as one you'd use for `java -cp my.jar com.example.MyClass`. */
  public static final String JAR_CRYSTALC_MAIN_CLASS = "crystalc.Run";
  private static final Logger log = LoggerFactory.getLogger(CmdCrystalc.class);

  public static final String NAME = "Crystal-C";
  public static final String JAR_GRPPR_NAME = "grppr-0.3.23.jar";
  public static final String JAR_MSFTBX_NAME = BATMASS_IO_JAR;
  private static String[] JAR_DEPS = {JAR_MSFTBX_NAME, JAR_GRPPR_NAME};
  private static final String THERMO_RAW_EXT = "RAW";
  private static final String BRUKER_RAW_EXT = "d";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");

  public CmdCrystalc(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  /**
   * @param pepxmlExtFragger Need to provide the extension, because there can be multiple dots
   * in the extension, so can't guess what the extension is.
   */
  private String getModifiedPepxmlFn(String pepxmlFn, String pepxmlExtFragger) {
    int lastIndexOf = pepxmlFn.toLowerCase().lastIndexOf(pepxmlExtFragger.toLowerCase());
    if (lastIndexOf < 0) {
      throw new IllegalArgumentException("Pepxml file name must end with the extension from Fragger config");
    }

    return pepxmlFn.substring(0, lastIndexOf - 1) + "_c." + StringUtils.afterLastDot(pepxmlFn);
  }

  /**
   * @param inputs Pepxml files after search engine, but before PeptideProphet.
   */
  public Map<InputLcmsFile, List<Path>> outputs(Map<InputLcmsFile, List<Path>> inputs, String pepxmlExtFragger) {
    Map<InputLcmsFile, List<Path>> m = new HashMap<>();
    for (Entry<InputLcmsFile, List<Path>> e : inputs.entrySet()) {
      for (Path p : e.getValue()) {
        Path dir = p.toAbsolutePath().getParent();
        String pepxmlFn = p.getFileName().toString();
        List<Path> t = m.get(e.getKey());
        if (t == null) {
          t = new ArrayList<>(e.getValue().size());
          t.add(dir.resolve(getModifiedPepxmlFn(pepxmlFn, pepxmlExtFragger)));
          m.put(e.getKey(), t);
        } else {
          t.add(dir.resolve(getModifiedPepxmlFn(pepxmlFn, pepxmlExtFragger)));
        }
      }
    }
    return m;
  }

  private boolean checkCompatibleFormats(Component comp, Map<InputLcmsFile, List<Path>> pepxmlFiles, List<String> supportedFormats) {
    List<String> notSupportedExts = getNotSupportedExts1(pepxmlFiles, supportedFormats);
    if (!notSupportedExts.isEmpty()) {
      if (Fragpipe.headless) {
        log.error(String.format("%s can't work with '.%s' files. You can convert files using msconvert from ProteoWizard.", NAME, String.join(", ", notSupportedExts)));
      } else {
        JOptionPane.showMessageDialog(comp, String.format(
                "<html>%s can't work with '.%s' files.<br/>"
                    + "Compatible formats are: %s<br/>"
                    + "Either remove files from input or disable %s<br/>"
                    + "You can also convert files using <i>msconvert</i> from ProteoWizard.",
                NAME, String.join(", ", notSupportedExts), String.join(", ", supportedFormats), NAME),
            NAME + " error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }
    return true;
  }

  /**
   * @param ccParams Get these by calling {@link CrystalcPanel#toParams()}.
   */
  public boolean configure(Component comp,
      Path jarFragpipe,
      boolean isDryRun,
      Path extLibsThermo,
      String msfraggerOutputExt,
      int ramGb,
      CrystalcParams ccParams,
      String fastaPath,
      Map<InputLcmsFile, List<Path>> pepxmlFiles) {
    initPreConfig();

    final List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
    if (extLibsThermo != null) {
      sup.add(THERMO_RAW_EXT);
    }

    if (!checkCompatibleFormats(comp, pepxmlFiles, sup)) {
      return false;
    }

    if (StringUtils.isNullOrWhitespace(fastaPath)) {
      if (Fragpipe.headless) {
        log.error("Fasta file [Crystal-C] path can't be empty.");
      } else {
        JOptionPane.showMessageDialog(comp, "Fasta file [Crystal-C] path can't be empty.", "Warning", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_CRYSTALC_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    if (!"pepxml".equalsIgnoreCase(msfraggerOutputExt)) {
      if (Fragpipe.headless) {
        log.error("Crystal-C only accepts pepXML file extension. Switch to pepXML in MSFragger options or disable Crystal-C.");
      } else {
        JOptionPane.showMessageDialog(comp, "Crystal-C only accepts pepXML file extension. Switch to pepXML in MSFragger options or disable Crystal-C.", "Not supported by Crystal-C", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }

    final String ccParamsFilePrefix = "crystalc";
    final String ccParamsFileSuffix = ".params";

    // multiple raw file extensions or multiple lcms file locaitons
    // issue a separate command for each pepxml file
    List<Path> filesToDelete = new ArrayList<>();
    int index = -1;
    for (Map.Entry<InputLcmsFile, List<Path>> kv : pepxmlFiles.entrySet()) {
      for (Path pepxml : kv.getValue()) {
        final InputLcmsFile lcms = kv.getKey();
        final String lcmsFn = lcms.getPath().getFileName().toString();
        final String pepxmlFn = pepxml.getFileName().toString();
        final Path outDir = lcms.outputDir(wd);

        CrystalcParams ccp;
        Path ccParamsPath = lcms.outputDir(wd).resolve(ccParamsFilePrefix + "-" + (++index) + "-" + pepxmlFn + ccParamsFileSuffix);

        filesToDelete.add(ccParamsPath);

        try {
          ccp = ccParams;
          String ext = StringUtils.afterLastDot(lcmsFn);
          ccp.setRawFileLocation(lcms.getPath().toAbsolutePath().getParent().toString());
          ccp.setRawFileExt(ext);
          ccp.setOutputLocation(outDir.toString());
          ccp.setFasta(fastaPath);
          if (!isDryRun) {
            Files.deleteIfExists(ccParamsPath);
            ccp.save(Files.newOutputStream(ccParamsPath, StandardOpenOption.CREATE));
          }
        } catch (IOException e) {
          if (Fragpipe.headless) {
            log.error("Could not create Crystal-C parameter file. " + e.getMessage());
          } else {
            JOptionPane.showMessageDialog(comp, "Could not create Crystal-C parameter file.\n" + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
          }
          return false;
        }

        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        if (extLibsThermo != null) {
          cmd.add(createJavaDParamString("libs.thermo.dir", extLibsThermo.toString()));
        }
        cmd.add("-Xmx" + ramGb + "G");
        cmd.add("-cp");
        cmd.add(constructClasspathString(classpathJars));
        cmd.add(JAR_CRYSTALC_MAIN_CLASS);
        cmd.add(ccParamsPath.toString());
        cmd.add(pepxml.toString());
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(outDir.toFile());
        pbis.add(PbiBuilder.from(pb));
      }
    }

    // delete temp files
    List<ProcessBuilder> pbsDeleteTemp = ToolingUtils.pbsDeleteFiles(jarFragpipe, filesToDelete);
    LinkedList<ProcessBuilderInfo> pbisPostParallel = pbsDeleteTemp.stream().map(pb -> new PbiBuilder().setPb(pb).setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL).setName(getCmdName() + " delete temp").create()).collect(Collectors.toCollection(LinkedList::new));
    pbis.addAll(pbisPostParallel);

    isConfigured = true;
    return true;
  }
}
