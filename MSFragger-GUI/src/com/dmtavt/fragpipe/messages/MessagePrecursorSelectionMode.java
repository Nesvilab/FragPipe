package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.params.enums.FraggerPrecursorMassMode;

public class MessagePrecursorSelectionMode {
  public final FraggerPrecursorMassMode mode;

  public MessagePrecursorSelectionMode(FraggerPrecursorMassMode mode) {
    this.mode = mode;
  }
}
