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

  public boolean configure(Component comp, UsageTrigger binPhilosopher, String uniprotId,
      boolean isReviewed, boolean isAddContaminants, boolean isAddIsoforms, boolean isAddDecoys,
                           Path addFastaPath) {

    pbis.clear();
    List<String> cmd = new ArrayList<>();
    cmd.add(binPhilosopher.useBin(this.wd));
    cmd.add(PhilosopherProps.CMD_DATABASE);
    if (isReviewed) {
      cmd.add("--reviewed");
    }
    if (isAddContaminants) {
      cmd.add("--contam");
    }
    if (isAddIsoforms) {
      cmd.add("--isoform");
    }
    if (!isAddDecoys) {
      cmd.add("--nodecoys");
    }
    cmd.add("--id");
    cmd.add(uniprotId);
    if (addFastaPath != null) {
      cmd.add("--add");
      cmd.add(addFastaPath.toAbsolutePath().normalize().toString());
    }
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(this.wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 90;
  }


}
