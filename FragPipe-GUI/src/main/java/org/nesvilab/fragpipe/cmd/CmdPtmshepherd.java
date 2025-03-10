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
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tools.ptmshepherd.PtmshepherdParams;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CmdPtmshepherd extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPtmshepherd.class);
  public static final String NAME = "PTMShepherd";
  public static final String CONFIG_FN = "shepherd.config";
  public static final String JAR_SHEPHERD_NAME = "ptmshepherd-3.0.1-rc6.jar";
  /** Fully qualified name, such as one you'd use for `java -cp my.jar com.example.MyClass`. */
  public static final String JAR_SHEPHERD_MAIN_CLASS = "edu.umich.andykong.ptmshepherd.PTMShepherd";
  public static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR, "commons-math3-3.6.1.jar", "hipparchus-1.8/hipparchus-core-1.8.jar", "hipparchus-1.8/hipparchus-stat-1.8.jar"};
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");
  private static final String THERMO_RAW_EXT = "RAW";

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
      int ramGb,
      Path db,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml,
      Map<String, String> additionalProps,
      Path jarFragpipe) {

    initPreConfig();

    ArrayList<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
    if (extLibsThermo != null) {
      sup.add(THERMO_RAW_EXT);
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

    Path root = FragpipeLocations.get().getDirFragpipeRoot();
    Path libsDir = root.resolve("lib");
    if (Files.isDirectory(jarFragpipe)) {
      libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib");
      log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
    }

    Set<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toSet());
    try {
      toJoin.addAll(Files.walk(libsDir).filter(p -> p.getFileName().toString().endsWith(".jar")).
          filter(p -> p.getFileName().toString().startsWith("commons-lang3")).
          map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList())
      );
    } catch (IOException ex) {
      ex.printStackTrace();
      return false;
    }

    toJoin.add(jarFragpipe.toAbsolutePath().normalize().toString());
    final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));

    PtmshepherdParams params = new PtmshepherdParams(wd, db, mapGroupsToProtxml, additionalProps);
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
      cmd.add(createJavaDParamString("libs.thermo.dir", extLibsThermo.toString()));
    }
    cmd.add("-cp");
    cmd.add(classpath);
    cmd.add(JAR_SHEPHERD_MAIN_CLASS);
    cmd.add("\"" + pathConfig.toAbsolutePath() + "\"");
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }
}
