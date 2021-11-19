package com.dmtavt.fragpipe.process;

import com.dmtavt.fragpipe.cmd.ProcessBuilderInfo;

public class RunnableDescription {

  public final ProcessDescription description;
  public final Runnable runnable;
  public final String parallelGroup;
  public final ProcessBuilderInfo pbi;

  public RunnableDescription(ProcessDescription description,
      Runnable runnable) {
    this(description, runnable, null, null);
  }

  public RunnableDescription(
          ProcessDescription description,
          Runnable runnable, String parallelGroup, ProcessBuilderInfo pbi) {
    this.description = description;
    this.runnable = runnable;
    this.parallelGroup = parallelGroup;
    this.pbi = pbi;
  }
}
