package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.UsageTrigger;

public class CmdDatabaseDownload extends CmdBase {

  public static final String NAME = "DbDownload";

  public CmdDatabaseDownload(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger binPhilosopher, String uniprotId) {

    pbs.clear();
    List<String> cmd = new ArrayList<>();
    cmd.add(binPhilosopher.useBin(this.wd));
    cmd.add(PhilosopherProps.CMD_DATABASE);
    cmd.add("--reviewed");
    cmd.add("--contam");
    cmd.add("--id");
    cmd.add(uniprotId);
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(this.wd.toFile());
    pbs.add(pb);

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 90;
  }


}
