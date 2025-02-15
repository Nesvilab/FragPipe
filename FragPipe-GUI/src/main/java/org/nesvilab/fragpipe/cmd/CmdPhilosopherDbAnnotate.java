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
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.tools.philosopher.PhilosopherProps;
import org.nesvilab.utils.UsageTrigger;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdPhilosopherDbAnnotate extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdPhilosopherDbAnnotate.class);
  public static final String NAME = "PhilosopherDbAnnotate";

  public CmdPhilosopherDbAnnotate(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, int ramGb, int threads, UsageTrigger binPhilosopher,
      String dbPath, String decoyTag, InputLcmsFile firstInputLcmsFile) {

    initPreConfig();

    if (dbPath == null) {
      if (Fragpipe.headless) {
        log.error("Fasta file path can't be empty");
      } else {
        JOptionPane.showMessageDialog(comp, "Fasta file path can't be empty (Report)", "Warning", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(binPhilosopher.useBin(wd.resolve(firstInputLcmsFile.getGroup())));
    cmd.add(PhilosopherProps.CMD_DATABASE);
    cmd.add("--annotate");
    cmd.add(dbPath);
    cmd.add("--prefix");
    cmd.add(decoyTag);
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.environment().put("GOMEMLIMIT", ramGb + "GiB");
    pb.environment().put("GOGC", "100");
    pb.environment().put("GOMAXPROCS", String.valueOf(threads));
    pb.directory(wd.resolve(firstInputLcmsFile.getGroup()).toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
