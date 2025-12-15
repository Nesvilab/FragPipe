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
import org.nesvilab.fragpipe.tabs.TabRun;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class CmdExportMatchedFragments extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdExportMatchedFragments.class);
  public static final String NAME = "ExportMatchedFragments";
  private static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR};

  public CmdExportMatchedFragments(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Path workDir, int nThreads) {
    initPreConfig();

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(TabRun.FP_FRAGMENTS).concat(JAR_DEPS));
    if (classpathJars == null || classpathJars.isEmpty()) {
      log.error("Cannot find the matched fragments executable file.");
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-cp");
    cmd.add(constructClasspathString(classpathJars));
    cmd.add("FragmentsExportMainClass");
    cmd.add(workDir.toAbsolutePath().normalize().toString());
    cmd.add(nThreads + "");
    cmd.add("r");

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(workDir.toFile());
    pbis.add(new PbiBuilder().setPb(pb).setName(NAME).create());

    isConfigured = true;
    return true;
  }
}

