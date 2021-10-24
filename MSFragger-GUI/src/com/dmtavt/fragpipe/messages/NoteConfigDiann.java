package com.dmtavt.fragpipe.messages;

public class NoteConfigDiann implements INoteConfig {

  private final boolean isValid;
  private final boolean isChecked;

  public NoteConfigDiann(boolean isValid, boolean isChecked) {
    this.isValid = isValid;
    this.isChecked = isChecked;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }

  public boolean isChecked() {
    return isChecked;
  }
}
