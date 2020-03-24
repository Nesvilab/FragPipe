package com.dmtavt.fragpipe.messages;

public abstract class MessageToolInit {
  public final boolean append;
  public final boolean isError;
  public final String text;

  public MessageToolInit(boolean append, boolean isError, String text) {
    this.append = append;
    this.isError = isError;
    this.text = text;
  }

  @Override
  public String toString() {
    return "Message{" +
        "append=" + append +
        ", isError=" + isError +
        ", text='" + text + '\'' +
        '}';
  }
}
