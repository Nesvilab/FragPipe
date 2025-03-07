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

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.tools.skyline.Skyline;
import org.nesvilab.utils.OsUtils;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
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

  public boolean configure(Component comp, String skylinePath, String skylineVersion, Path jarFragpipe, int ramGb, int modsMode, boolean useSsl) {
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

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(BATMASS_IO_JAR));
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

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

}

