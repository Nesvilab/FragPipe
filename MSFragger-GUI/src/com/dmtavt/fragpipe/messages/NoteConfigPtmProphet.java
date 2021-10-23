package com.dmtavt.fragpipe.messages;

public class NoteConfigPtmProphet implements INoteConfig {

  private final boolean isValid;

  public NoteConfigPtmProphet(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
