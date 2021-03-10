package com.dmtavt.fragpipe.messages;

import java.util.Objects;
import java.util.StringJoiner;
import javax.swing.JComponent;
import net.java.balloontip.BalloonTip;

public class MessageBalloon {
  public final String topic;
  public final BalloonTip tip;
  public final JComponent parent;
  public final String html;

  /** Closes balloons for a topic. */
  public MessageBalloon(String topic) {
    Objects.requireNonNull(topic);
    this.topic = topic;
    this.tip = null;
    parent = null;
    html = null;
  }

  public MessageBalloon(String topic, BalloonTip tip) {
    Objects.requireNonNull(topic);
    Objects.requireNonNull(tip);
    this.topic = topic;
    this.tip = tip;
    html = null;
    parent = null;
  }

  public MessageBalloon(String topic, JComponent parent, String html) {
    Objects.requireNonNull(topic);
    Objects.requireNonNull(parent);
    Objects.requireNonNull(html);
    this.topic = topic;
    this.parent = parent;
    this.html = html;
    tip = null;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageBalloon.class.getSimpleName() + "[", "]")
        .add("topic='" + topic + "'")
        .add("tip=" + tip)
        .add("parent=" + parent)
        .add("html='" + html + "'")
        .toString();
  }
}
