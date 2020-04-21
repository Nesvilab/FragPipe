package com.dmtavt.fragpipe.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.github.chhh.utils.UsageTrigger;

public class CmdPhilosopherReport extends CmdBase {

  public static final String NAME = "PhilosopherReport";

  public CmdPhilosopherReport(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  @Override
  public int getPriority() {
    return 110;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      boolean doPrintDecoys, boolean doMzId, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

    pbis.clear();
    Set<Path> groupWds = mapGroupsToProtxml.keySet().stream().map(g -> g.outputDir(wd))
        .collect(Collectors.toSet());
    for (Path groupWd : groupWds) {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_REPORT);
      if (doPrintDecoys) {
        cmd.add("--decoys");
      }
      if (doMzId) {
        cmd.add("--mzid");
      }
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }
}
