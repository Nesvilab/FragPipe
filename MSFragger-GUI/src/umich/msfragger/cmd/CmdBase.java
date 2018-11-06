package umich.msfragger.cmd;

import java.nio.file.Path;
import java.util.LinkedList;

public abstract class CmdBase {

  final boolean isRun;
  final Path wd;
  final LinkedList<ProcessBuilder> pbs;
  boolean isConfigured;

  public CmdBase(
      boolean isRun, Path workDir) {
    this.isRun = isRun;
    this.wd = workDir;
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

  public ProcessBuilderDescriptor builders() {
    if (!isConfigured)
      throw new IllegalStateException("Call to #processBuilders() before calling #configure()");
    return new ProcessBuilderDescriptor(getCmdName(), getPriority()).addAll(pbs);
  }
}
