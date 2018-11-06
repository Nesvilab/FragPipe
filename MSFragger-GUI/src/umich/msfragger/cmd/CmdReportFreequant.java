package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.UsageTrigger;

public class CmdReportFreequant extends CmdBase {

  public static final String NAME = "ReportFreequant";

  public CmdReportFreequant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String textReportLabelfree, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

    for (Map.Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path protxml = e.getValue();

      final Set<Path> lcmsDirsForProtxml = group.lcmsFiles.stream()
          .map(f -> f.path.getParent())
          .collect(Collectors.toSet());
      if (lcmsDirsForProtxml.size() > 1) {
        String msg= "All LCMS input files for an experiment/group must be\n"
            + "located in the same directory for Freequant to work.";
        JOptionPane.showMessageDialog(comp, msg, "Freequant Error", JOptionPane.WARNING_MESSAGE);
        return false;
      }
      final Path lcmsDir = lcmsDirsForProtxml.iterator().next().toAbsolutePath();
      final Path groupWd = group.outputDir(wd);

      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_LABELFREE);

      List<String> allowed = new ArrayList<>();
      allowed.add("ptw");
      allowed.add("tol");
      for (String paramName : allowed) {
        Pattern reFullParam = Pattern
            .compile(String.format("--%s\\s+(\\d+(?:\\.\\d+)?)", paramName));
        Matcher m = reFullParam.matcher(textReportLabelfree);
        if (m.find()) {
          cmd.add("--" + paramName);
          cmd.add(m.group(1));
        }
      }

      // we have checked that all lcms files are in the same folder, so
      cmd.add("--dir");
      cmd.add(lcmsDir.toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());

      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
