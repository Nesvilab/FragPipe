package com.dmtavt.fragpipe.messages;

import java.util.Properties;

public class NoteFragpipeProperties {
  public final Properties propsFix;
  public final boolean includesRemote;

  public NoteFragpipeProperties(Properties propsFix, boolean includesRemote) {
    this.propsFix = propsFix;
    this.includesRemote = includesRemote;
  }
}
