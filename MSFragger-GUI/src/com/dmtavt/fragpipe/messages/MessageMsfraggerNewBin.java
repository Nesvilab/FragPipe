package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class MessageMsfraggerNewBin {
  public final String binPath;

  public MessageMsfraggerNewBin(String binPath) {
    this.binPath = binPath;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageMsfraggerNewBin.class.getSimpleName() + "[", "]")
        .add("binPath='" + binPath + "'")
        .toString();
  }
}
