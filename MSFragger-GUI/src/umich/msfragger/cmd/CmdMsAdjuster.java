package umich.msfragger.cmd;

import java.awt.Component;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.JOptionPane;
import umich.msfragger.gui.FraggerPanel;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcProps;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.StringUtils;

public class CmdMsAdjuster extends CmdBase {

  public static final String NAME = "MsAdjuster";
  private int priority;

  public CmdMsAdjuster(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Path jarFragpipe, FraggerPanel fp,
      List<InputLcmsFile> lcmsFiles, boolean doCleanup, int priority) {
    pbs.clear();
    Path jarMsadjusterPath;
    Path jarDepsPath;
    try {
      // common deps
      jarDepsPath = JarUtils
          .unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_COMMON_DEPS,
              ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);
      // msadjuster jar
      jarMsadjusterPath = JarUtils
          .unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_MSADJUSTER_NAME,
              ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);

    } catch (IOException | NullPointerException ex) {
      JOptionPane.showMessageDialog(comp,
          "Could not unpack tools to a temporary directory.\n"
              + "Disable precursor mass adjustment in MSFragger tab.",
          "Can't unpack", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    this.priority = priority;
    int ramGb = fp.getRamGb();

    for (InputLcmsFile f : lcmsFiles) {

      if (!doCleanup) {
        // run MsAdjuster
        ArrayList<String> cmd = new ArrayList<>();
        cmd.add("java");
        if (ramGb > 0) {
          cmd.add("-Xmx" + ramGb + "G");
        }
        if (jarDepsPath != null) {
          cmd.add("-cp");
          List<String> toJoin = new ArrayList<>();
          toJoin.add(jarDepsPath.toString());
          toJoin.add(jarMsadjusterPath.toString());
          final String sep = System.getProperties().getProperty("path.separator");
          cmd.add("\"" + org.apache.commons.lang3.StringUtils.join(toJoin, sep) + "\"");
          cmd.add(CrystalcProps.JAR_MSADJUSTER_MAIN_CLASS);
        } else {
          cmd.add("-jar");
          cmd.add(jarMsadjusterPath.toAbsolutePath().normalize().toString());
        }
        cmd.add("20");
        cmd.add(f.path.toString());
        pbs.add(new ProcessBuilder(cmd));

      } else {
        // run MsAdjuster cleanup

        // MsAdjuster creates these files
        Path origin = Paths.get(StringUtils.upToLastDot(f.path.toString()) + ".ma");
        Path destination = f.outputDir(wd);
        pbs.addAll(ToolingUtils.pbsMoveFiles(jarFragpipe, destination, Collections.singletonList(origin)));
      }
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return priority;
  }
}
