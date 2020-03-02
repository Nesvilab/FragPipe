package umich.msfragger.cmd;

import java.nio.file.Path;

public class CmdTmtIntegrator extends CmdBase {
  public static final String NAME = "TmtIntegrator";

  public CmdTmtIntegrator(boolean isRun, Path workDir, String fileCaptureStdout,
      String fileCaptureStderr) {
    super(isRun, workDir, fileCaptureStdout, fileCaptureStderr);
  }

  public CmdTmtIntegrator(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }


}
