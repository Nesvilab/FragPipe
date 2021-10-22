package com.dmtavt.fragpipe.messages;

public class NoteConfigDiann implements INoteConfig {

  private boolean isValid;

  public NoteConfigDiann(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
