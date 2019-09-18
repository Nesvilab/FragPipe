package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.UsageTrigger;

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
      String decoyTag, int nThreads, Map<InputLcmsFile, Path> pepxmlFiles) {

    pbis.clear();

    final List<String> cmd = new ArrayList<>();
    cmd.add(usePhilosopher.useBin());
    cmd.add(PhilosopherProps.CMD_IPROPHET);
    cmd.add("--decoy");
    cmd.add(decoyTag);
    cmd.add("--nonsp");
    cmd.add("--output");
    cmd.add("combined");
    cmd.add("--threads");
    final int threads;
    if (nThreads > 0) {
      threads = nThreads;
    } else {
      threads = Math.max(1, Runtime.getRuntime().availableProcessors() - 1);
    }
    cmd.add(Integer.toString(threads));
    pepxmlFiles.values().stream().distinct().forEach(pepxml -> cmd.add(pepxml.toString()));

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 94;
  }
}
