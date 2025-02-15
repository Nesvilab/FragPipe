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

import org.nesvilab.fragpipe.tools.philosopher.PhilosopherProps;
import org.nesvilab.utils.UsageTrigger;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class CmdDatabaseUpdate extends CmdBase {

  public static final String NAME = "DbUpdate";

  public CmdDatabaseUpdate(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger binPhilosopher,
      Path fastaPath, boolean isAddContaminants) {

    initPreConfig();

    List<String> cmd = new ArrayList<>();
    cmd.add(binPhilosopher.useBin(this.wd));
    cmd.add(PhilosopherProps.CMD_DATABASE);
    cmd.add("--custom");
    cmd.add(fastaPath.toString());

    if (isAddContaminants) {
      cmd.add("--contam");
      cmd.add("--contamprefix");
    }

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(this.wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
