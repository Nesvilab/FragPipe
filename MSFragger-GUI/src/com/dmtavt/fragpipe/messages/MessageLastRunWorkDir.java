package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;

public class MessageLastRunWorkDir {
  public final Path workDir;

  public MessageLastRunWorkDir(Path workDir) {
    this.workDir = workDir;
  }
}
