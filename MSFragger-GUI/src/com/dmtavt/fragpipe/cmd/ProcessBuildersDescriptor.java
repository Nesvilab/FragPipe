package com.dmtavt.fragpipe.cmd;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ProcessBuildersDescriptor {
  public final List<ProcessBuilderInfo> pbis;
  public final String name;
  public final String fnStdout;
  public final String fnStderr;
  private String parallelGroup = null;

  public ProcessBuildersDescriptor(String name, String fnStdout, String fnStderr) {
    this.name = name;
    this.pbis = new ArrayList<>();
    this.fnStdout = fnStdout;
    this.fnStderr = fnStderr;
  }

  public String getParallelGroup() {
    return parallelGroup;
  }

  public void setParallelGroup(String parallelGroup) {
    this.parallelGroup = parallelGroup;
  }

  public int size() {
    return pbis.size();
  }

  public boolean isEmpty() {
    return pbis.isEmpty();
  }

  public ProcessBuildersDescriptor add(ProcessBuilderInfo pbi) {
    pbis.add(pbi);
    return this;
  }

  public ProcessBuildersDescriptor addAll(Collection<? extends ProcessBuilderInfo> c) {
    pbis.addAll(c);
    return this;
  }


}
