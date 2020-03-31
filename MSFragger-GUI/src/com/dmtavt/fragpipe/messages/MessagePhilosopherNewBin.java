package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class MessagePhilosopherNewBin {
  public final String path;

  public MessagePhilosopherNewBin(String path) {
    this.path = path;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessagePhilosopherNewBin.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .toString();
  }
}
