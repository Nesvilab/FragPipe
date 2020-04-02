package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteConfigMsfragger implements INoteConfig {

  public final String path;
  public final String version;
  public final boolean isTooOld;
  public final Throwable ex;

  public NoteConfigMsfragger(String path, String version) {
    this(path, version, null);
  }

  public NoteConfigMsfragger(String path, String version, Throwable ex) {
    this(path, version, false, ex);
  }

  public NoteConfigMsfragger(String path, String version, boolean isTooOld, Throwable ex) {
    this.path = path;
    this.version = version;
    this.isTooOld = isTooOld;
    this.ex = ex;
  }

  @Override
  public boolean isValid() {
    return ex == null;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigMsfragger.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("isTooOld=" + isTooOld)
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
