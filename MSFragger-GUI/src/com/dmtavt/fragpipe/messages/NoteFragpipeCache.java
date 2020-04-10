package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.api.PropsFile;
import java.util.Properties;

public class NoteFragpipeCache implements INoteConfig {
  public final PropsFile propsRuntime;
  public final PropsFile propsUiState;

  public NoteFragpipeCache(PropsFile propsRuntime, PropsFile propsUiState) {
    this.propsRuntime = propsRuntime;
    this.propsUiState = propsUiState;
  }

  @Override
  public boolean isValid() {
    return propsRuntime != null && propsUiState != null;
  }
}
