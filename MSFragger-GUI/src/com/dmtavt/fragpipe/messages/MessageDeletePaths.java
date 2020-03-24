package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;
import java.util.Set;

public class MessageDeletePaths {
  public final Set<Path> toDelete;

  public MessageDeletePaths(Set<Path> toDelete) {
    this.toDelete = toDelete;
  }
}
