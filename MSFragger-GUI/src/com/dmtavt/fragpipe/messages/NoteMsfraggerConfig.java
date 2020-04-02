package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteMsfraggerConfig implements INoteConfig {

  public final String path;
  public final String version;
  public final boolean isTooOld;
  public final Throwable ex;

  public NoteMsfraggerConfig(String path, String version) {
    this(path, version, null);
  }

  public NoteMsfraggerConfig(String path, String version, Throwable ex) {
    this(path, version, false, ex);
  }

  public NoteMsfraggerConfig(String path, String version, boolean isTooOld, Throwable ex) {
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
    return new StringJoiner(", ", NoteMsfraggerConfig.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("isTooOld=" + isTooOld)
        .add("validation=" + (ex == null ? "null" : ex.getMessage()))
        .toString();
  }
}
