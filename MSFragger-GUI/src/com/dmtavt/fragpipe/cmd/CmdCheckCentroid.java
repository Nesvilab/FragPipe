package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.util.CheckCentroid;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.jooq.lambda.Seq;

public class CmdCheckCentroid extends CmdBase {


  public static final String NAME = "CheckCentroid";
  public static final String JAR_MSFTBX_NAME = ToolingUtils.BATMASS_IO_JAR;
  private static String[] JAR_DEPS = {JAR_MSFTBX_NAME};

  public CmdCheckCentroid(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Path jarFragpipe, int ramGb, int nThreads, List<InputLcmsFile> lcmsFiles) {
    initPreConfig();

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(jarFragpipe.toAbsolutePath().toString()).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    for (InputLcmsFile lcms : lcmsFiles) {
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      if (ramGb > 0) {
        cmd.add("-Xmx" + ramGb + "G");
      }
      cmd.add("-cp");
      cmd.add(constructClasspathString(classpathJars));
      cmd.add(CheckCentroid.class.getCanonicalName());
      cmd.add(lcms.getPath().toAbsolutePath().toString());
      cmd.add(nThreads + "");
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }
}
