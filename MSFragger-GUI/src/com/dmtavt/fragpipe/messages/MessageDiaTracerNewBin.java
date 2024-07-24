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

package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class MessageDiaTracerNewBin {
  public final String binPath;

  public MessageDiaTracerNewBin(String binPath) {
    this.binPath = binPath;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageDiaTracerNewBin.class.getSimpleName() + "[", "]").add("binPath='" + binPath + "'").toString();
  }
}
