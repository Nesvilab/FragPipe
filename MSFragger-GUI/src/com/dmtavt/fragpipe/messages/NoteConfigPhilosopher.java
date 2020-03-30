package com.dmtavt.fragpipe.messages;

public class NoteConfigPhilosopher implements INoteConfig {
  public final String path;
  public final String version;
  public final boolean isTooOld;
  public final boolean isPathValid;
  public final boolean isBinValid;

  public NoteConfigPhilosopher(String path, String version, boolean isPathValid,
      boolean isTooOld, boolean isBinValid) {
    this.path = path;
    this.version = version;
    this.isPathValid = isPathValid;
    this.isTooOld = isTooOld;
    this.isBinValid = isBinValid;
  }

  @Override
  public boolean isValid() {
    return isPathValid && isBinValid;
  }
}
