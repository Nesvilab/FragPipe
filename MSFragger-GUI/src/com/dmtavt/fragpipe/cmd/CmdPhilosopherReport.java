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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.github.chhh.utils.UsageTrigger;

public class CmdPhilosopherReport extends CmdBase {

  public static final String NAME = "PhilosopherReport";

  public CmdPhilosopherReport(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      boolean doPrintDecoys, boolean doMsstats, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

    initPreConfig();

    Set<Path> groupWds = mapGroupsToProtxml.keySet().stream().map(g -> g.outputDir(wd))
        .collect(Collectors.toSet());
    for (Path groupWd : groupWds) {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_REPORT);
      if (doPrintDecoys) {
        cmd.add("--decoys");
      }
      if (doMsstats) {
        cmd.add("--msstats");
      }
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
