package com.dmtavt.fragpipe.messages;

import java.util.Properties;

public class NotePreviousUiState implements INoteConfig {
  public final Properties props;

  public NotePreviousUiState(Properties props) {
    this.props = props;
  }

  @Override
  public boolean isValid() {
    return props != null;
  }
}
