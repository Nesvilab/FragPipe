package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

public class MessageLcmsAddFolder {
  public final List<Path> dirs;

  public MessageLcmsAddFolder() {
    dirs = Collections.emptyList();
  }

  public MessageLcmsAddFolder(List<Path> dirs) {
    this.dirs = dirs;
  }
}
