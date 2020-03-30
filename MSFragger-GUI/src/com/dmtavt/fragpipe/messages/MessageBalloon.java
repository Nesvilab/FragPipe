package com.dmtavt.fragpipe.messages;

import java.util.Objects;
import java.util.StringJoiner;
import javax.swing.JComponent;
import net.java.balloontip.BalloonTip;

public class MessageBalloon {
  public final String topic;
  public final BalloonTip tip;
  public final JComponent parent;
  public final JComponent body;
  public final String html;

  /** Closes balloons for a topic. */
  public MessageBalloon(String topic) {
    Objects.requireNonNull(topic);
    this.topic = topic;
    this.tip = null;
    parent = null;
    html = null;
    body = null;
  }

  public MessageBalloon(String topic, BalloonTip tip) {
    Objects.requireNonNull(topic);
    Objects.requireNonNull(tip);
    this.topic = topic;
    this.tip = tip;
    html = null;
    parent = null;
    body = null;
  }

  public MessageBalloon(String topic, JComponent parent, JComponent body) {
    Objects.requireNonNull(topic);
    Objects.requireNonNull(parent);
    Objects.requireNonNull(body);
    this.topic = topic;
    this.parent = parent;
    this.body = body;
    tip = null;
    html = null;
  }

  public MessageBalloon(String topic, JComponent parent, String html) {
    Objects.requireNonNull(topic);
    Objects.requireNonNull(parent);
    Objects.requireNonNull(html);
    this.topic = topic;
    this.parent = parent;
    this.html = html;
    tip = null;
    body = null;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageBalloon.class.getSimpleName() + "[", "]")
        .add("topic='" + topic + "'")
        .add("tip=" + tip)
        .add("parent=" + parent)
        .add("body=" + body)
        .add("html='" + html + "'")
        .toString();
  }
}
