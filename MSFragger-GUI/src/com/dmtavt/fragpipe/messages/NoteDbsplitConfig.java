package com.dmtavt.fragpipe.messages;

import umich.msfragger.params.dbslice.DbSplit2;

public class NoteDbsplitConfig {

  public final DbSplit2 dbSplit2;
  public final Throwable ex;

  public NoteDbsplitConfig(DbSplit2 dbSplit2, Throwable ex) {
    this.dbSplit2 = dbSplit2;
    this.ex = ex;
  }
}
