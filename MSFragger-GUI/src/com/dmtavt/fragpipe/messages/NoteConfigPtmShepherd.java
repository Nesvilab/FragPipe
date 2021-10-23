package com.dmtavt.fragpipe.messages;

public class NoteConfigPtmShepherd implements INoteConfig {

  private final boolean isValid;

  public NoteConfigPtmShepherd(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
