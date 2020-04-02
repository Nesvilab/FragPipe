package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteConfigPhilosopher implements INoteConfig {
  public final String path;
  public final String version;
  public final Throwable ex;

  public NoteConfigPhilosopher(String path, String version) {
    this(path, version, null);
  }

  public NoteConfigPhilosopher(String path, String version, Throwable ex) {
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
    return new StringJoiner(", ", NoteConfigPhilosopher.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
