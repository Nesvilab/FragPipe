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
