package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

public class MessageKillAll {

  public MessageKillAll(REASON reason, List<Path> pathsToDelete) {
    this.reason = reason;
    this.pathsToDelete = pathsToDelete;
  }

  public MessageKillAll(REASON reason) {
    this.reason = reason;
    this.pathsToDelete = Collections.emptyList();
  }

  public enum REASON {NO_REASON, CANT_START_PROCESS, NON_ZERO_RETURN_FROM_PROCESS, USER_ACTION}

  public final REASON reason;
  public final List<Path> pathsToDelete;
}
