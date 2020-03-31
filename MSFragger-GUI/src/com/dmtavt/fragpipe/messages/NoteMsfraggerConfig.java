package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteMsfraggerConfig implements INoteConfig {

  public final String path;
  public final String version;
  public final boolean isTooOld;
  public final Throwable validation;

  public NoteMsfraggerConfig(String path, String version) {
    this(path, version, null);
  }

  public NoteMsfraggerConfig(String path, String version, Throwable validation) {
    this(path, version, false, validation);
  }

  public NoteMsfraggerConfig(String path, String version, boolean isTooOld, Throwable validation) {
    this.path = path;
    this.version = version;
    this.isTooOld = isTooOld;
    this.validation = validation;
  }

  @Override
  public boolean isValid() {
    return validation == null;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteMsfraggerConfig.class.getSimpleName() + "[", "]")
        .add("path='" + path + "'")
        .add("version='" + version + "'")
        .add("isTooOld=" + isTooOld)
        .add("validation=" + (validation == null ? "null" : validation.getMessage()))
        .toString();
  }
}
