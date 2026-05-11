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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tools.ptmshepherd.PtmshepherdParams;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.nesvilab.fragpipe.cmd.CmdPairScans.checkCompatibleFormats;


public class CmdPtmshepherd extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPtmshepherd.class);
  public static final String NAME = "PTMShepherd";
  public static final String CONFIG_FN = "shepherd.config";
  public static final String SHEPHERD_VERSION = "3.0.14";
  public static final String JAR_SHEPHERD_NAME = "ptmshepherd-" + SHEPHERD_VERSION + ".jar";
  /** Fully qualified name, such as one you'd use for `java -cp my.jar com.example.MyClass`. */
  public static final String JAR_SHEPHERD_MAIN_CLASS = "edu.umich.andykong.ptmshepherd.PTMShepherd";
  public static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR, ToolingUtils.JFREECHART_JAR, "commons-math3-3.6.1.jar", "hipparchus-1.8/hipparchus-core-1.8.jar", "hipparchus-1.8/hipparchus-stat-1.8.jar"};
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");
  private static final String THERMO_RAW_EXT = "RAW";
  private static final String BRUKER_RAW_EXT = "d";

  public CmdPtmshepherd(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp,
                           boolean isDryRun,
                           Path extLibsThermo,
                           Path extLibsBruker,
                           int ramGb,
                           Path db,
                           Map<LcmsFileGroup, Path> mapGroupsToProtxml,
                           Map<String, String> additionalProps,
                           Path binIonQuant,
                           List<InputLcmsFile> lcmsFiles
                           ) {

    initPreConfig();

    // check vendor format input files vs provided reader libraries
    ArrayList<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
    if (extLibsThermo != null) {
      sup.add(THERMO_RAW_EXT);
    }
    if (extLibsBruker != null) {
      sup.add(BRUKER_RAW_EXT);
    }
    if (!checkCompatibleFormats(comp, lcmsFiles, sup)) {
      return false;
    }
    String thermoLib = "";
    if (extLibsThermo != null) {
      thermoLib = createJavaDParamString("libs.thermo.dir", extLibsThermo.toString());
    } else {
      if (lcmsFiles.stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".raw"))) {
        if (Fragpipe.headless) {
          log.error("When processing .RAW files, PTM-Shepherd requires native Thermo libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.");
        } else {
          SwingUtils.showErrorDialog(comp, "When processing .RAW files, PTM-Shepherd requires native Thermo libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.", NAME + " error");
        }
        return false;
      }
    }
    String brukerLib = "";
    if (extLibsBruker != null) {
      brukerLib = createJavaDParamString("libs.bruker.dir", extLibsBruker.toString());
    } else {
      if (lcmsFiles.stream().anyMatch(f -> f.getPath().getFileName().toString().toLowerCase().endsWith(".d"))) {
        if (Fragpipe.headless) {
          log.error("When processing .d files, PTM-Shepherd requires native Bruker libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.");
        } else {
          SwingUtils.showErrorDialog(comp, "When processing .d files, PTM-Shepherd requires native Bruker libraries. Native libraries come with MSFragger zip download, contained in ext sub-directory.", NAME + " error");
        }
        return false;
      }
    }

    // check that each group only has lcms files in one directory
    for (LcmsFileGroup g : mapGroupsToProtxml.keySet()) {
      List<Path> lcmsPathsForGroup = g.lcmsFiles.stream().map(inputLcmsFile -> inputLcmsFile.getPath().toAbsolutePath().getParent()).distinct().collect(Collectors.toList());
      if (lcmsPathsForGroup.size() != 1) {
        if (Fragpipe.headless) {
          log.error("PTM Shepherd requires all LCMS files in a group/experiment to be in one directory.");
        } else {
          String msg = "PTM Shepherd requires all LCMS files in a group/experiment to be in one directory.\n<br/><br/>"
              + "<b>Check 'Workflows' tab, 'Input LCMS files' section.</b>";
          SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
          log.error(msg);
        }
        return false;
      }
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_SHEPHERD_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    PtmshepherdParams params = new PtmshepherdParams(wd, db, mapGroupsToProtxml, additionalProps);

    if (params.getProp("use_external_library").equalsIgnoreCase("true")) {
      if (params.getProp("glyco_lib_path") == null || params.getProp("glyco_lib_path").isEmpty()) {
        // fall back to default path if not specified
        params.getProps().put("glyco_lib_path", FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve("default.glycolib").toString());
      }
      // ensure final path exists before starting run
      if (!Files.exists(Path.of(params.getProp("glyco_lib_path")))) {
        if (Fragpipe.headless) {
            log.error("Glycan library mode requested, but library file not found at specified path: {}", params.getProp("glyco_lib_path"));
        } else {
          String msg = "Glycan library mode requested, but library file not found at specified path:<br/>\n" + params.getProp("glyco_lib_path") + "<br/><br/>Please check the path and try again.";
          SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }
    }

    String config;
    try {
      config = params.createConfig();
    } catch (Exception e) {
      if (Fragpipe.headless) {
        log.error("Could not configure PTM Shepherd. Error message:" + e.getMessage());
      } else {
        String msg = "Could not configure PTM Shepherd.\n<br/><br/>Error message:" + e.getMessage();
        SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }

    // write config file
    Path pathConfig = wd.resolve(CONFIG_FN);

    if (!isDryRun) {
      log.debug("Writing {} config to file: {}", NAME, pathConfig);
      try {
        Files.deleteIfExists(pathConfig);
      } catch (IOException e) {
        if (Fragpipe.headless) {
          log.error("Could not delete existing config file: " + pathConfig);
        } else {
          SwingUtils.showDialog(comp, SwingUtils.createClickableHtml("Could not delete existing config file:<br/>\n" + pathConfig), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }
      try (BufferedWriter bw = Files.newBufferedWriter(pathConfig, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
        bw.write(config);
        bw.flush();
      } catch (IOException e) {
        if (Fragpipe.headless) {
          log.error("Error writing Shepherd config to file. Error message: " + e.getMessage());
        } else {
          String msg = "Error writing Shepherd config to file.\n<br/><br/>Error message: " + e.getMessage();
          SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }
    }

    // builders
    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-Xmx" + ramGb + "G");
    if (extLibsThermo != null) {
      cmd.add(thermoLib);
    }
    if (extLibsBruker != null) {
      cmd.add(brukerLib);
    }
    cmd.add("-cp");
    if (params.needsIonQuant()) {
      cmd.add(constructClasspathString(classpathJars, binIonQuant));
    } else {
      cmd.add(constructClasspathString(classpathJars));
    }
    cmd.add(JAR_SHEPHERD_MAIN_CLASS);
    cmd.add("\"" + pathConfig.toAbsolutePath() + "\"");
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }
}
