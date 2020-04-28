package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;

public class MessageOpenInExplorer {

  public final Path path;

  public MessageOpenInExplorer(Path path) {
    this.path = path;
  }
}
