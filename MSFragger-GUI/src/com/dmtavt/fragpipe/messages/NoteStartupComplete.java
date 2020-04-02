package com.dmtavt.fragpipe.messages;

import java.util.Properties;

public class NoteStartupComplete {
  public final Properties lastUiState;
  public final Properties fragpipeProperties;


  public NoteStartupComplete(Properties lastUiState, Properties fragpipeProperties) {
    this.lastUiState = lastUiState;
    this.fragpipeProperties = fragpipeProperties;
  }
}
