package com.dmtavt.fragpipe.messages;

import java.util.Properties;

public class NoteFragpipeProperties {
  public final Properties props;
  public final boolean includesRemote;

  public NoteFragpipeProperties(Properties props, boolean includesRemote) {
    this.props = props;
    this.includesRemote = includesRemote;
  }
}
