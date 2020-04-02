package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NotePhilosopherConfig implements INoteConfig {
  public final String path;
  public final String version;
  public final Throwable ex;

  public NotePhilosopherConfig(String path, String version) {
    this(path, version, null);
  }

  public NotePhilosopherConfig(String path, String version, Throwable ex) {
    this.path = path;
    this.version = version;
    this.ex = ex;
  }

  @Override
  public boolean isValid() {
    return ex == null && version != null && path != null;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NotePhilosopherConfig.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
