package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdReportAbacus extends CmdBase {

  private static final String NAME = "ReportAbacus";

  private final List<String> KNOWN_OPTS = Arrays.asList(
      "comb",
      "labels",
      "pepProb",
      "picked",
      "prtProb",
      "razor",
      "tag",
      "uniqueonly");

  public CmdReportAbacus(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String textAbacusCmdOpts, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path protxml = e.getValue();
      final Path groupWd = group.outputDir(wd);

      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add("abacus");
      if (!StringUtils.isNullOrWhitespace(textAbacusCmdOpts)) {
        cmd.addAll(Arrays.asList(textAbacusCmdOpts.trim().split("[\\s]+")));
      }
      cmd.add("--comb");
      cmd.add(protxml.toString());

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
