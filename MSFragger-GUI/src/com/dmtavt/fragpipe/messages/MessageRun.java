package com.dmtavt.fragpipe.messages;

/**
 * This message is sent when the Run button is clicked.
 */
public class MessageRun {
  public final boolean isDryRun;

  public MessageRun(boolean isDryRun) {
    this.isDryRun = isDryRun;
  }
}
