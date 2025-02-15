/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.messages;

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
  public final boolean exitHeadless;

  public MessageBalloon(String topic) {
    Objects.requireNonNull(topic);
    this.topic = topic;
    this.tip = null;
    parent = null;
    html = null;
    body = null;
    this.exitHeadless = false;
  }

  public MessageBalloon(String topic, BalloonTip tip) {
    Objects.requireNonNull(topic);
    Objects.requireNonNull(tip);
    this.topic = topic;
    this.tip = tip;
    html = null;
    parent = null;
    body = null;
    this.exitHeadless = false;
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
    this.exitHeadless = false;
  }

  public MessageBalloon(String topic, JComponent parent, String html, boolean exitHeadless) {
    Objects.requireNonNull(topic);
    Objects.requireNonNull(parent);
    Objects.requireNonNull(html);
    this.topic = topic;
    this.parent = parent;
    this.html = html;
    tip = null;
    body = null;
    this.exitHeadless = exitHeadless;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageBalloon.class.getSimpleName() + "[", "]")
        .add("topic='" + topic + "'")
        .add("tip=" + tip)
        .add("parent=" + parent)
        .add("body=" + body)
        .add("html='" + html + "'")
        .add("exitFragPipe='" + exitHeadless + "'")
        .toString();
  }
}
