package com.dmtavt.fragpipe.messages;

public class NoteConfigCrystalC implements INoteConfig {

  private final boolean isValid;

  public NoteConfigCrystalC(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
