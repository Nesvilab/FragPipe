package com.dmtavt.fragpipe.messages;

public class MessageClearCache {
  public final boolean doClose;

  public MessageClearCache(boolean doClose) {
    this.doClose = doClose;
  }

  public MessageClearCache() {
    this(false);
  }
}
