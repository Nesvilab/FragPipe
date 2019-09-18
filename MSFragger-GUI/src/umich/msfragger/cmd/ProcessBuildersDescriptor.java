package umich.msfragger.cmd;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ProcessBuildersDescriptor {
  public final List<ProcessBuilder> pbs;
  public final String name;
  public final String fileCaptureStdout;
  public final String fileCaptureStderr;
  public final int priority;
  private String parallelGroup = null;

  public ProcessBuildersDescriptor(String name, int priority,
      String fileCaptureStdout, String fileCaptureStderr) {
    this.name = name;
    this.priority = priority;
    this.pbs = new ArrayList<>();
    this.fileCaptureStdout = fileCaptureStdout;
    this.fileCaptureStderr = fileCaptureStderr;
  }

  public String getParallelGroup() {
    return parallelGroup;
  }

  public void setParallelGroup(String parallelGroup) {
    this.parallelGroup = parallelGroup;
  }

  public int size() {
    return pbs.size();
  }

  public boolean isEmpty() {
    return pbs.isEmpty();
  }

  public ProcessBuildersDescriptor add(ProcessBuilder processBuilder) {
    pbs.add(processBuilder);
    return this;
  }

  public ProcessBuildersDescriptor addAll(Collection<? extends ProcessBuilder> c) {
    pbs.addAll(c);
    return this;
  }


}
