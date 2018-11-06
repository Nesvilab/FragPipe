package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.JOptionPane;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdReportDbAnnotate extends CmdBase {

  public static final String NAME = "ReportDbAnnotate";

  public CmdReportDbAnnotate(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger binPhilosopher,
      String textReportAnnotate, String dbPath) {

    if (dbPath == null) {
      JOptionPane.showMessageDialog(comp, "Fasta file path can't be empty (Report)",
          "Warning", JOptionPane.WARNING_MESSAGE);
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(binPhilosopher.useBin(wd));
    cmd.add(PhilosopherProps.CMD_DATABASE);
    cmd.add("--annotate");

    cmd.add(dbPath);
    if (!StringUtils.isNullOrWhitespace(textReportAnnotate)) {
      String[] params = textReportAnnotate.split("[\\s]+");
      cmd.addAll(Arrays.asList(params));
    }
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbs.add(pb);

    isConfigured = true;
    return true;
  }
}
