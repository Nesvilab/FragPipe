package com.dmtavt.fragpipe.messages;

public class NoteConfigTmtI implements INoteConfig {

  private final boolean isValid;

  public NoteConfigTmtI(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
