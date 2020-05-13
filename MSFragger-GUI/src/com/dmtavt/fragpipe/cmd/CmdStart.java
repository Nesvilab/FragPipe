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
    isRun = true;
  }

  @Override
  public String getCmdName() {
    return NAME;
  }
}
