package com.dmtavt.fragpipe.cmd;

import java.nio.file.Path;
import org.apache.commons.lang3.NotImplementedException;

public class CmdPtmProphet extends CmdBase {
  public static String NAME = "PtmProphet";

  public CmdPtmProphet(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure() {

    initPreConfig();

    if (!isConfigured) {
      throw new NotImplementedException();
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 93; // after peptide prophet
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
