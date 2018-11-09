package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
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

    Map<Path, List<Entry<LcmsFileGroup, Path>>> protxmlToGroups = mapGroupsToProtxml.entrySet().stream()
        .collect(Collectors.groupingBy(Entry::getValue));

    for (Entry<Path, List<Entry<LcmsFileGroup, Path>>> e : protxmlToGroups.entrySet()) {
      Path protxml = e.getKey();
      Set<Path> foldersWithPepxmls = e.getValue().stream().map(g -> g.getKey().outputDir(wd))
          .collect(Collectors.toSet());

      if (foldersWithPepxmls.size() < 2) {
        JOptionPane.showMessageDialog(comp,
            "Multi-experiment report requires more than one\n"
            + "experiment/group being processed together.\n\n"
                + "Turn off separate processing of groups on LCMS\n"
                + "tab or add another experiment/group.",
            "Multi-experiment report error", JOptionPane.WARNING_MESSAGE);
        return false;
      }

      List<String> cmd = new ArrayList<>();
      final Path executeInDir = protxml.getParent();
      cmd.add(usePhilosopher.useBin(executeInDir));
      cmd.add("abacus");
      if (!StringUtils.isNullOrWhitespace(textAbacusCmdOpts)) {
        cmd.addAll(Arrays.asList(textAbacusCmdOpts.trim().split("[\\s]+")));
      }
      cmd.add("--comb");
      cmd.add(protxml.toString());

      // list locations with pepxml files
      for (Path pepxmlDir : foldersWithPepxmls) {
        cmd.add(pepxmlDir.toString());
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(executeInDir.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
