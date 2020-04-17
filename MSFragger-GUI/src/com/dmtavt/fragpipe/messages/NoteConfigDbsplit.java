package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.params.dbslice.DbSplit2;

public class NoteConfigDbsplit implements INoteConfig {

  public final DbSplit2 instance;
  public final Throwable ex;

  public NoteConfigDbsplit(DbSplit2 instance, Throwable ex) {
    this.instance = instance;
    this.ex = ex;
  }

  @Override
  public boolean isValid() {
    return instance != null && ex == null;
  }
}
