package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteConfigMsfragger implements INoteConfig {

  public final String path;
  public final String version;
  public final boolean isTooOld;
  public final Throwable validation;

  public NoteConfigMsfragger(String path, String version) {
    this(path, version, null);
  }

  public NoteConfigMsfragger(String path, String version, Throwable validation) {
    this(path, version, false, validation);
  }

  public NoteConfigMsfragger(String path, String version, boolean isTooOld, Throwable validation) {
    this.path = path;
    this.version = version;
    this.isTooOld = isTooOld;
    this.validation = validation;
  }

  @Override
  public boolean isValid() {
    return validation == null;
  }
}
