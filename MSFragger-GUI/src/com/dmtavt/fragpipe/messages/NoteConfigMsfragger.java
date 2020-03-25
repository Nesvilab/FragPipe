package com.dmtavt.fragpipe.messages;

public class NoteConfigMsfragger {
  public final String jarPath;
  public final String version;
  public final boolean isTooOld;
  public NoteConfigMsfragger(String jarPath, String version, boolean isTooOld) {
    this.jarPath = jarPath;
    this.version = version;
    this.isTooOld = isTooOld;
  }
}
