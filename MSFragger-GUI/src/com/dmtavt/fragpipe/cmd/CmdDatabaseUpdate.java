package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.github.chhh.utils.UsageTrigger;
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
