package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

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

  @Override
  public String toString() {
    return new StringJoiner(", ", NotePhilosopherConfig.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("validation=" + (validation == null ? "null" : validation.getMessage()))
        .toString();
  }
}
