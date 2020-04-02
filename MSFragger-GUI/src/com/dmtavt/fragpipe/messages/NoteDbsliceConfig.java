package com.dmtavt.fragpipe.messages;

import umich.msfragger.params.dbslice.DbSlice2;

public class NoteDbsliceConfig {

  public final DbSlice2 dbSlice2;
  public final Throwable ex;

  public NoteDbsliceConfig(DbSlice2 dbSlice2, Throwable ex) {
    this.dbSlice2 = dbSlice2;
    this.ex = ex;
  }
}
