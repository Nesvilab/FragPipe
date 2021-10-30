package com.dmtavt.fragpipe.messages;

public class NoteConfigUmpire implements INoteConfig {

  private final boolean isValid;

  public NoteConfigUmpire(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
