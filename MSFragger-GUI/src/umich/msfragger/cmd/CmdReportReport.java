package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.UsageTrigger;

public class CmdReportReport extends CmdBase {

  public static final String NAME = "ReportReport";

  public CmdReportReport(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

    pbs.clear();
    Set<Path> groupWds = mapGroupsToProtxml.keySet().stream().map(g -> g.outputDir(wd))
        .collect(Collectors.toSet());
    for (Path groupWd : groupWds) {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_REPORT);
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
