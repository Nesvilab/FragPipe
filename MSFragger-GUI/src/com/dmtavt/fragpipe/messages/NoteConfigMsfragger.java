package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteConfigMsfragger {
  public final String jarPath;
  public final String version;
  public final boolean isTooOld;
  public final boolean isValid;
  public NoteConfigMsfragger(String jarPath, String version, boolean isValid,
      boolean isTooOld) {
    this.jarPath = jarPath;
    this.version = version;
    this.isValid = isValid;
    this.isTooOld = isTooOld;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteConfigMsfragger.class.getSimpleName() + "[", "]")
        .add("jarPath='" + jarPath + "'")
        .add("version='" + version + "'")
        .add("isTooOld=" + isTooOld)
        .add("isValid=" + isValid)
        .toString();
  }
}
