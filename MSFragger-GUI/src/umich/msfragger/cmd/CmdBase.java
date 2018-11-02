package umich.msfragger.cmd;

import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

public class CmdBase {

  final boolean isRun;
  final Path wd;
  final Deque<ProcessBuilder> pbs;
  boolean isConfigured;

  public CmdBase(
      boolean isRun, Path workDir) {
    this.isRun = isRun;
    this.wd = workDir;
    this.pbs = new ArrayDeque<>();
  }

  public boolean isRun() {
    return isRun;
  }

  public Path getWd() {
    return wd;
  }

  public List<ProcessBuilder> processBuilders() {
    if (!isConfigured)
      throw new IllegalStateException("Call to #processBuilders() before calling #configure()");
    return new ArrayList<>(pbs);
  }
}
