package com.dmtavt.fragpipe.messages;

public class NotePhilosopherConfig implements INoteConfig {
  public final String path;
  public final String version;
  public final Throwable validation;

  public NotePhilosopherConfig(String path, String version) {
    this(path, version, null);
  }

  public NotePhilosopherConfig(String path, String version, Throwable validation) {
    this.path = path;
    this.version = version;
    this.validation = validation;
  }

  @Override
  public boolean isValid() {
    return validation == null && version != null;
  }
}
