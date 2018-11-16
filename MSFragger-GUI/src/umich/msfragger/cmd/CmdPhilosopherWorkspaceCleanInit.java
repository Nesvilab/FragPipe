package umich.msfragger.cmd;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import umich.msfragger.util.UsageTrigger;

public class CmdPhilosopherWorkspaceCleanInit extends CmdBase {

  public static final String NAME = "Workspace";

  public CmdPhilosopherWorkspaceCleanInit(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(UsageTrigger usePhilosopher) {
    pbs.clear();
    {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(wd));
      cmd.add("workspace");
      cmd.add("--clean");
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
      pbs.add(pb);
    }

    {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(wd));
      cmd.add("workspace");
      cmd.add("--init");
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(wd.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 10;
  }
}
