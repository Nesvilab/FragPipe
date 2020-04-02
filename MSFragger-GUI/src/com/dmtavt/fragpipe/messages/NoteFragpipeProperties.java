package com.dmtavt.fragpipe.messages;

import java.util.Properties;

public class NoteFragpipeProperties {
  public final Properties props;
  public final boolean isAttemptedToGetRemote;

  public NoteFragpipeProperties(Properties props, boolean isAttemptedToGetRemote) {
    this.props = props;
    this.isAttemptedToGetRemote = isAttemptedToGetRemote;
  }
}
