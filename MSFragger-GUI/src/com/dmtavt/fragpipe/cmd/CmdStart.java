package com.dmtavt.fragpipe.cmd;

import java.nio.file.Path;

/**
 * Fake command used in graph building as a starting point.
 */
public class CmdStart extends CmdBase {

  public static final String NAME = "START";

  public CmdStart(boolean isRun, Path workDir) {
    super(isRun, workDir);
    isConfigured = true;
    isRun = false;
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  /**
   * Extending classes can override this to modify the priority level.
   */
  @Override
  public int getPriority() {
    return Integer.MIN_VALUE;
  }
}
