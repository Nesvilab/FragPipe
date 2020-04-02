package com.dmtavt.fragpipe.messages;

import umich.msfragger.params.speclib.SpecLibGen2;

public class NoteConfigSpeclibgen implements INoteConfig {
  public final SpecLibGen2 instance;
  public final Throwable ex;

  public NoteConfigSpeclibgen(SpecLibGen2 instance, Throwable ex) {
    this.instance = instance;
    this.ex = ex;
  }

  @Override
  public boolean isValid() {
    return ex == null && instance != null;
  }
}
