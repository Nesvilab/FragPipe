package com.dmtavt.fragpipe.messages;

public class MessageSaveAsWorkflow {
  public final boolean toCustomDir;
  public final boolean saveWithFieldTypes;

  public MessageSaveAsWorkflow(boolean toCustomDir, boolean saveWithFieldTypes) {
    this.toCustomDir = toCustomDir;
    this.saveWithFieldTypes = saveWithFieldTypes;
  }

  public MessageSaveAsWorkflow(boolean toCustomDir) {
    this(toCustomDir, false);
  }
}
