package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.github.chhh.utils.StringUtils;
import java.awt.Component;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.jooq.lambda.Seq;
import com.dmtavt.fragpipe.api.InputLcmsFile;

public class CmdMsAdjuster extends CmdBase {

  public static final String NAME = "MsAdjuster";
  public static final String JAR_MSADJUSTER_NAME = "original-msadjuster-1.0.3.jar";
  /** Fully qualified name, such as one you'd use for `java -cp my.jar com.example.MyClass`. */
  public static final String JAR_MSADJUSTER_MAIN_CLASS = "Main";
  private int priority;
  private boolean isCleanup;
  private static String[] JAR_DEPS = {CmdCrystalc.JAR_MSFTBX_NAME, CmdCrystalc.JAR_GRPPR_NAME};

  public CmdMsAdjuster(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return !isCleanup ? NAME : NAME + " (Cleanup)";
  }

  public boolean configure(Component comp, Path jarFragpipe, int ramGb,
      List<InputLcmsFile> lcmsFiles, boolean doCleanup, int priority) {

    initPreConfig();

    isCleanup = doCleanup;
    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_MSADJUSTER_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    this.priority = priority;

    for (InputLcmsFile f : lcmsFiles) {

      if (!doCleanup) {
        // run MsAdjuster
        ArrayList<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        if (ramGb > 0) {
          cmd.add("-Xmx" + ramGb + "G");
        }
        cmd.add("-cp");
        cmd.add(constructClasspathString(classpathJars));
        cmd.add(JAR_MSADJUSTER_MAIN_CLASS);
        cmd.add("20");
        cmd.add(f.getPath().toString());

        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(f.outputDir(wd).toFile());
        pbis.add(PbiBuilder.from(pb));

      } else {
        // run MsAdjuster cleanup

        // MsAdjuster creates these files
        Path origin = Paths.get(StringUtils.upToLastDot(f.getPath().toString()) + ".ma");
        Path destination = f.outputDir(wd);
        if (!destination.equals(origin.getParent())) {
          List<ProcessBuilder> pbsMove = ToolingUtils
              .pbsMoveFiles(jarFragpipe, destination, Collections.singletonList(origin));
          pbis.addAll(PbiBuilder.from(pbsMove));
        }
      }
    }

    isConfigured = true;
    return true;
  }
}
