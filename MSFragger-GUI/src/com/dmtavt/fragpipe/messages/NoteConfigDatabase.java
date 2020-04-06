package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;

public class NoteConfigDatabase implements INoteConfig {
  public final Path path;
  public final int numEntries;
  public final double pctDecoys;
  public final boolean isValid;

  public NoteConfigDatabase(Path path, int numEntries, double pctDecoys, boolean isValid) {
    this.path = path;
    this.numEntries = numEntries;
    this.pctDecoys = pctDecoys;
    this.isValid = isValid;
  }

  public NoteConfigDatabase() {
    path = null;
    numEntries = -1;
    pctDecoys = Double.NaN;
    this.isValid = false;
  }

  @Override
  public boolean isValid() {
    return isValid;
  }
}
