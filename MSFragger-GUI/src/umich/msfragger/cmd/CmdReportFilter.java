package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdReportFilter extends CmdBase {

  public static final String NAME = "ReportFilter";
  public static final String FN_CAPTURE_STDOUT = "filter.log";
  public static final String FN_CAPTURE_STDERR = "filter.log";

  public CmdReportFilter(boolean isRun, Path workDir) {
    super(isRun, workDir, FN_CAPTURE_STDOUT, FN_CAPTURE_STDERR);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String decoyTag, String textReportFilter, boolean dontUseFilterProtxml,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

    pbs.clear();
    for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path protxml = e.getValue();
      if (group.lcmsFiles.isEmpty())
        throw new IllegalStateException("CmdReportFilter - LCMS file group is empty. "
            + "This is a bug, report to developers.");
      Path groupWd = group.outputDir(wd);

      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(wd));
      cmd.add(PhilosopherProps.CMD_FILTER);

      // check for extra arguments
      if (!StringUtils.isNullOrWhitespace(textReportFilter)) {
        cmd.addAll(StringUtils.splitCommandLine(textReportFilter));
      }
      cmd.add("--tag");
      cmd.add(decoyTag);
      cmd.add("--pepxml");
      cmd.add(groupWd.toString());
      if (!dontUseFilterProtxml) {
        cmd.add("--protxml");
        cmd.add(protxml.toString());
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
