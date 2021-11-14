package com.dmtavt.fragpipe.messages;

public class MessageSaveAsWorkflow {
  public final boolean toCustomDir;
  public final boolean saveWithFieldTypes;
  public final boolean saveAll;

  public MessageSaveAsWorkflow(boolean toCustomDir, boolean saveWithFieldTypes, boolean saveAll) {
    this.toCustomDir = toCustomDir;
    this.saveWithFieldTypes = saveWithFieldTypes;
    this.saveAll = saveAll;
  }

  public MessageSaveAsWorkflow(boolean toCustomDir, boolean saveWithFieldTypes) {
    this(toCustomDir, saveWithFieldTypes, false);
  }

  public MessageSaveAsWorkflow(boolean toCustomDir) {
    this(toCustomDir, false);
  }
}
