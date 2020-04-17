package com.dmtavt.fragpipe.cmd;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import com.github.chhh.utils.UsageTrigger;

public class CmdPhilosopherWorkspaceCleanInit extends CmdBase {

  public static final String NAME = "Workspace";
  private int priority = 10;

  public CmdPhilosopherWorkspaceCleanInit(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(UsageTrigger usePhilosopher) {
    return configure(usePhilosopher, true);
  }

  public boolean configure(UsageTrigger usePhilosopher, boolean doClean) {
    pbis.clear();
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
      cmd.addAll(asParts("workspace --init --nocheck"));
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return priority;
  }

  public void setPriority(int priority) {
    this.priority = priority;
  }
}
