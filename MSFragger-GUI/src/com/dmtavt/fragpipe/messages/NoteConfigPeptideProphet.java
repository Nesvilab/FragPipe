package com.dmtavt.fragpipe.messages;

public class NoteConfigPeptideProphet implements INoteConfig {

  private final boolean isValid;

  public NoteConfigPeptideProphet(boolean isValid) {
    this.isValid = isValid;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
