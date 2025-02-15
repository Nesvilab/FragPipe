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
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.tools.philosopher.PhilosopherProps;
import org.nesvilab.utils.UsageTrigger;

public class CmdIprophet extends CmdBase {

  private static final String NAME = "iProphet";

  public CmdIprophet(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String decoyTag, int nThreads, Map<InputLcmsFile, List<Path>> pepxmlFiles) {

    initPreConfig();

    final List<String> cmd = new ArrayList<>();
    cmd.add(usePhilosopher.useBin());
    cmd.add(PhilosopherProps.CMD_IPROPHET);
    cmd.add("--decoy");
    cmd.add(decoyTag);
    cmd.add("--nonsp");
    cmd.add("--output");
    cmd.add("combined");
    cmd.add("--threads");
    cmd.add(Integer.toString(nThreads));
    pepxmlFiles.values().stream().flatMap(List::stream).distinct().forEach(pepxml -> cmd.add(pepxml.toString()));

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
