package com.dmtavt.fragpipe.messages;

import java.util.Properties;

public class NoteFragpipeCache implements INoteConfig {
  public final Properties propsRuntime;
  public final Properties propsUiState;

  public NoteFragpipeCache(Properties propsRuntime, Properties propsUiState) {
    this.propsRuntime = propsRuntime;
    this.propsUiState = propsUiState;
  }

  @Override
  public boolean isValid() {
    return propsRuntime != null && propsUiState != null;
  }
}
