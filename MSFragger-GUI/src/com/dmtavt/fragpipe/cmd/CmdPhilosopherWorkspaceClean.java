package com.dmtavt.fragpipe.cmd;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import com.github.chhh.utils.UsageTrigger;

public class CmdPhilosopherWorkspaceClean extends CmdBase {
  public static final String NAME = "Workspace";

  public CmdPhilosopherWorkspaceClean(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(UsageTrigger usePhilosopher) {
    pbis.clear();
    {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(wd));
      cmd.addAll(asParts("workspace --clean --nocheck"));
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return Integer.MAX_VALUE - 100;
  }

}
