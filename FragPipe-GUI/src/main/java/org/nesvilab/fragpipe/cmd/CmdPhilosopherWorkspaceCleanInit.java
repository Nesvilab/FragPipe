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

import org.nesvilab.utils.UsageTrigger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdPhilosopherWorkspaceCleanInit extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdPhilosopherWorkspaceCleanInit.class);

  public static final String NAME = "WorkspaceCleanInit";

  public CmdPhilosopherWorkspaceCleanInit(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  public CmdPhilosopherWorkspaceCleanInit(boolean isRun, String title, Path workDir) {
    super(isRun, title, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(UsageTrigger usePhilosopher) {
    return configure(usePhilosopher, true);
  }

  public boolean configure(UsageTrigger usePhilosopher, boolean doClean) {
    initPreConfig();

    if (doClean) {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(wd));
      cmd.addAll(asParts("workspace --clean --nocheck"));
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(wd));
      cmd.addAll(asParts("workspace --init --nocheck --temp"));
      final Path phiTempDir = Paths.get(System.getProperty("java.io.tmpdir"), UUID.randomUUID().toString());
      try {
        Files.createDirectories(phiTempDir);
      } catch (IOException ex) {
        throw new RuntimeException("Could not generate temporary directory " + phiTempDir + " for Philosopher", ex);
      }
      cmd.add(phiTempDir.toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
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
