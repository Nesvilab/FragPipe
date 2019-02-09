package umich.msfragger.messages;

import umich.msfragger.params.enums.FraggerPrecursorMassMode;

public class MessagePrecursorSelectionMode {
  public final FraggerPrecursorMassMode mode;

  public MessagePrecursorSelectionMode(FraggerPrecursorMassMode mode) {
    this.mode = mode;
  }
}
