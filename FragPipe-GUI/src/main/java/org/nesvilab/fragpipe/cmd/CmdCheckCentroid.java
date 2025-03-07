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

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.util.CheckCentroid;

public class CmdCheckCentroid extends CmdBase {


  public static final String NAME = "CheckCentroid";
  public static final String JAR_MSFTBX_NAME = ToolingUtils.BATMASS_IO_JAR;
  private static String[] JAR_DEPS = {JAR_MSFTBX_NAME};

  public CmdCheckCentroid(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Path jarFragpipe, int ramGb, int nThreads, List<InputLcmsFile> lcmsFiles) {
    initPreConfig();

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(jarFragpipe.toAbsolutePath().normalize().toString()).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    for (InputLcmsFile lcms : lcmsFiles) {
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-Xmx" + ramGb + "G");
      cmd.add("-cp");
      cmd.add(constructClasspathString(classpathJars));
      cmd.add(CheckCentroid.class.getCanonicalName());
      cmd.add(lcms.getPath().toAbsolutePath().normalize().toString());
      cmd.add(nThreads + "");
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pbis.add(PbiBuilder.from(pb));
      break; // Only check one file to save some time.
    }

    isConfigured = true;
    return true;
  }
}
