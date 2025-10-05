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
import static org.nesvilab.fragpipe.tabs.TabWorkflow.manifestExt;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.generateLFQExperimentAnnotation;
import static org.nesvilab.utils.SwingUtils.showErrorDialogWithStacktrace;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.tools.skyline.Skyline;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.SwingUtils;

import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdSkyline extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdSkyline.class);
  public static String NAME = "Skyline";


  public CmdSkyline(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp,
      String skylinePath,
      String skylineVersion,
      Path jarFragpipe,
      int ramGb,
      int modsMode,
      boolean useSsl,
      int precursorTolerance,
      int fragmentTolerance,
      boolean runSkylineQuant,
      boolean skipSkylineDocumentGeneration,
      String modTag,
      float siteProb,
      boolean isDryRun) {
    initPreConfig();

    if (skylinePath == null) {
      if (Fragpipe.headless) {
        log.error("Skyline was requested but no installed version could be found! Please verify that it is installed or located at the specified path.");
      } else {
        JOptionPane.showMessageDialog(comp,
                "Skyline was requested but no installed version could be found! Please verify that it is installed or located at the specified path.",
                "Error", JOptionPane.ERROR_MESSAGE);
      }
      return false;
    }

    if (!skipSkylineDocumentGeneration) {
      final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(BATMASS_IO_JAR));
      if (classpathJars == null) {
        return false;
      }
  
      Path root = FragpipeLocations.get().getDirFragpipeRoot();
      Path libsDir = root.resolve("lib");
      if (Files.isDirectory(jarFragpipe)) {
        libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib");
      }
  
      Set<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toSet());
      try {
        toJoin.addAll(Files.walk(libsDir).filter(p -> p.getFileName().toString().endsWith(".jar")).
                filter(p -> p.getFileName().toString().startsWith("maven-artifact") ||
                        p.getFileName().toString().startsWith("commons-lang3") ||
                        p.getFileName().toString().startsWith("fragpipe-")).
                map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList())
        );
      } catch (IOException ex) {
        ex.printStackTrace();
        return false;
      }
  
      toJoin.add(jarFragpipe.toAbsolutePath().normalize().toString());
      final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));
  
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmd.add("-Djava.awt.headless=true");
      }
      cmd.add("-Xmx" + ramGb + "G");
      cmd.add("-cp");
      cmd.add(classpath);
      cmd.add(Skyline.class.getCanonicalName());
      cmd.add(skylinePath);
      cmd.add(wd.toAbsolutePath().normalize().toString());
      cmd.add(skylineVersion);
      cmd.add(String.valueOf(modsMode));
      cmd.add(String.valueOf(useSsl));
      cmd.add(String.valueOf(precursorTolerance));
      cmd.add(String.valueOf(fragmentTolerance));
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
      pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " create Skyline document").create());
    }

    if (runSkylineQuant) {
      List<Path> tt = FragpipeLocations.checkToolsMissing(Seq.of("Skyline/fragpipe_report.skyr"));
      if (tt == null || tt.isEmpty()) {
        SwingUtils.showErrorDialog(comp, "Could not find Skyline quant report tempolate file.", "Error");
        return false;
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(skylinePath);
      cmd.add("--in=\"" + wd.resolve("skyline_files").resolve("fragpipe.sky").toAbsolutePath().normalize() + "\"");
      cmd.add("--report-add=\"" + tt.get(0).toAbsolutePath().normalize() + "\"");
      cmd.add("--report-conflict-resolution=overwrite");
      cmd.add("--report-name=\"FragPipe_Skyline_quant\"");
      cmd.add("--report-file=\"" + wd.resolve("skyline_files").resolve("fragpipe_skyline_quant.csv").toAbsolutePath().normalize() + "\"");
      ProcessBuilder pb2 = new ProcessBuilder(cmd);
      pb2.directory(wd.resolve("skyline_files").toFile());
      pbis.add(new PbiBuilder().setPb(pb2).setName(getCmdName() + " run Skyline quant").create());

      List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Stream.of(CmdDiann.SITE_REPORTER));
      if (classpathJars == null) {
        System.err.println("Could not find " + CmdDiann.SITE_REPORTER);
      } else {
        List<String> cmd2 = new ArrayList<>();
        cmd2.add(Fragpipe.getBinJava());
        cmd2.add("-Xmx" + ramGb + "G");
        cmd2.add("-jar");
        cmd2.add(constructClasspathString(classpathJars));
        cmd2.add("--pr");
        cmd2.add(wd.resolve("skyline_files").resolve("fragpipe_skyline_quant.csv").toAbsolutePath().normalize().toString());
        cmd2.add("--exp-ann");
        cmd2.add(wd.resolve("fragpipe-files" + manifestExt).toAbsolutePath().normalize().toString());
        cmd2.add("--out-dir");
        cmd2.add(wd.resolve("skyline_files").toAbsolutePath().normalize().toString());
        cmd2.add("--mod-tag");
        cmd2.add(modTag);
        cmd2.add("--min-site-prob");
        cmd2.add(String.valueOf(siteProb));
        ProcessBuilder pb = new ProcessBuilder(cmd2);
        pb.directory(wd.resolve("skyline_files").toFile());
        pbis.add(new PbiBuilder().setPb(pb).setName(getCmdName() + " generate site reports").create());
      }
    }

    if (!isDryRun) {
      try {
        generateLFQExperimentAnnotation(wd, 1);
      } catch (Exception ex) {
        showErrorDialogWithStacktrace(ex, comp);
        return false;
      }
    }

    isConfigured = true;
    return true;
  }

}

