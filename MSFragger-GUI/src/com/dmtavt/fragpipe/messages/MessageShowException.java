package com.dmtavt.fragpipe.messages;

import javax.swing.JComponent;

public class MessageShowException {
  public final String topic;
  public final JComponent comp;
  public final Throwable ex;
  public final boolean showStacktrace;

  public MessageShowException(String topic, JComponent comp, Throwable ex, boolean showStacktrace) {
    this.topic = topic;
    this.comp = comp;
    this.ex = ex;
    this.showStacktrace = showStacktrace;
  }
}
