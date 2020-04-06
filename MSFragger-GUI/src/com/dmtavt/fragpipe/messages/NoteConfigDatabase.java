package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;

public class NoteConfigDatabase implements INoteConfig {
  public final Path path;
  public final int numEntries;
  public final int decoysCnt;
  public final boolean isValid;

  public NoteConfigDatabase(Path path, int numEntries, int decoysCnt, boolean isValid) {
    this.path = path;
    this.numEntries = numEntries;
    this.decoysCnt = decoysCnt;
    this.isValid = isValid;
  }

  public NoteConfigDatabase() {
    path = null;
    numEntries = -1;
    decoysCnt = -1;
    this.isValid = false;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
