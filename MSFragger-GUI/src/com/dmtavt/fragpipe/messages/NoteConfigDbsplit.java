package com.dmtavt.fragpipe.messages;

import umich.msfragger.params.dbslice.DbSplit2;

public class NoteConfigDbsplit {

  public final DbSplit2 instance;
  public final Throwable ex;

  public NoteConfigDbsplit(DbSplit2 instance, Throwable ex) {
    this.instance = instance;
    this.ex = ex;
  }
}
