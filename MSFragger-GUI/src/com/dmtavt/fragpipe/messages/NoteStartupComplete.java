package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.Bus;
import java.util.Properties;

public class NoteStartupComplete {
  public final NoteFragpipeProperties props;
  public final NoteFragpipeCache cache;

  public NoteStartupComplete(NoteFragpipeProperties props, NoteFragpipeCache cache) {
    this.props = props;
    this.cache = cache;
  }
}
