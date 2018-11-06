package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.swing.JOptionPane;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.speclib.SpecLibGen;
import umich.msfragger.util.UsageTrigger;

public class CmdSpecLibGen extends CmdBase {

  public static final String NAME = "SpecLibGen";

  public CmdSpecLibGen(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml, String fastaPath, boolean isRunProteinProphet) {

    final SpecLibGen slg = SpecLibGen.get();
    if (!slg.isInitialized()) {
      JOptionPane.showMessageDialog(comp,
          "Spectral Library Generation scripts did not initialize correctly.",
          "Spectral Library Generation Error", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path protxml = e.getValue();
      final Path groupWd = group.outputDir(wd);

      if (!isRunProteinProphet && !Files.exists(protxml)) {
        JOptionPane.showMessageDialog(comp,
            "Protein Prophet not selected and the output directory:\n"
                + "    " + groupWd.toString() + "\n"
                + "does not contain a '" + protxml.getFileName().toString() + "' file.\n\n"
                + "Either uncheck Spectral Library Generation checkbox or enable Protein Prophet.",
            "Spec Lib Gen configuration Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(slg.getPi().getCommand());
      cmd.add(slg.getScriptSpecLibGenPath().toString());
      cmd.add(fastaPath);
      cmd.add(groupWd.toString()); // this is "Pep xml directory"
      cmd.add(protxml.toString()); // protxml file
      cmd.add(groupWd.toString()); // output directory
      cmd.add("True"); // overwrite (true/false), optional arg
      cmd.add(usePhilosopher.useBin()); // philosopher binary path (optional)

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(groupWd.toFile());

      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
