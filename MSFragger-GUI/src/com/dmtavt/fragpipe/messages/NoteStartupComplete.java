package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.Bus;
import java.util.Properties;

public class NoteStartupComplete {
  public final NoteFragpipeProperties props;
  public final NotePreviousUiState uiState;

  public NoteStartupComplete(NoteFragpipeProperties props,
      NotePreviousUiState uiState) {
    this.props = props;
    this.uiState = uiState;
  }
}
