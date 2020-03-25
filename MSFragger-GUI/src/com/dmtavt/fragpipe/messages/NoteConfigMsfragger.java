package com.dmtavt.fragpipe.messages;

public class NoteConfigMsfragger {
  public final String jarPath;
  public final String version;
  public NoteConfigMsfragger(String jarPath, String version) {
    this.jarPath = jarPath;
    this.version = version;
  }
}
