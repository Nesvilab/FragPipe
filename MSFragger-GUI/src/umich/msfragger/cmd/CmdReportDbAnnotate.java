package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JOptionPane;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.philosopher.PhilosopherProps;
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
      String dbPath, String decoyTag,
      Map<InputLcmsFile, Path> pepxmlFiles, Map<LcmsFileGroup, Path> protxmlFiles) {

    pbis.clear();
    if (dbPath == null) {
      JOptionPane.showMessageDialog(comp, "Fasta file path can't be empty (Report)",
          "Warning", JOptionPane.WARNING_MESSAGE);
      return false;
    }

    Set<Path> pepProtDirs = Stream
        .concat(pepxmlFiles.values().stream(), protxmlFiles.values().stream())
        .map(Path::getParent)
        .collect(Collectors.toSet());

    for (Path pepxmlDir : pepProtDirs) {
      List<String> cmd = new ArrayList<>();
      cmd.add(binPhilosopher.useBin(pepxmlDir));
      cmd.add(PhilosopherProps.CMD_DATABASE);
      cmd.add("--annotate");
      cmd.add(dbPath);
      cmd.add("--prefix");
      cmd.add(decoyTag);
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(pepxmlDir.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 90;
  }
}
