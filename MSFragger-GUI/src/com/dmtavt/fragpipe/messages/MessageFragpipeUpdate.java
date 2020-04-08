package com.dmtavt.fragpipe.messages;

public class MessageFragpipeUpdate {
  public final String key;
  public final String text;

  public MessageFragpipeUpdate(String key, String text) {
    this.key = key;
    this.text = text;
  }
}
