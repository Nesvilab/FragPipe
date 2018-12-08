package umich.msfragger.cmd;

import java.nio.file.Path;
import java.util.LinkedList;

public abstract class CmdBase {

  final boolean isRun;
  final Path wd;
  final LinkedList<ProcessBuilder> pbs;
  final String fileCaptureStdout;
  final String fileCaptureStderr;
  boolean isConfigured;

  public CmdBase(
      boolean isRun, Path workDir, String fileCaptureStdout, String fileCaptureStderr) {
    this.isRun = isRun;
    this.wd = workDir;
    this.fileCaptureStdout = fileCaptureStdout;
    this.fileCaptureStderr = fileCaptureStderr;
    this.pbs = new LinkedList<>();
  }

  public CmdBase(
      boolean isRun, Path workDir) {
    this.isRun = isRun;
    this.wd = workDir;
    this.fileCaptureStdout = "";
    this.fileCaptureStderr = "";
    this.pbs = new LinkedList<>();
  }

  public boolean isRun() {
    return isRun;
  }

  public Path getWd() {
    return wd;
  }

  /**
   * Extending classes can override this to modify the priority level.
   */
  public int getPriority() {
    return 100;
  }

  public abstract String getCmdName();

  public ProcessBuildersDescriptor builders() {
    if (!isConfigured)
      throw new IllegalStateException("Call to #processBuilders() before calling #configure()");
    return new ProcessBuildersDescriptor(getCmdName(), getPriority()).addAll(pbs);
  }
}
