package com.dmtavt.fragpipe.messages;

public class NoteConfigIonQuant implements INoteConfig {

  private final boolean isValid;

  public NoteConfigIonQuant(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
