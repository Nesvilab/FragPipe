package com.dmtavt.fragpipe.messages;

public class MessageSpeclibGenSpectrastInit {

  public final boolean isSuccess;
  public final String message;

  public MessageSpeclibGenSpectrastInit(boolean isSuccess, String message) {
    this.isSuccess = isSuccess;
    this.message = message;
  }
}
