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

package org.nesvilab.fragpipe.tools.fragger;

import java.util.StringJoiner;

public class MsfraggerEnzyme {
  public final String name;
  public final String cut;
  public final String nocuts;
  public final String sense;

  public MsfraggerEnzyme(String name, String cut, String nocuts, String sense) {
    this.name = name;
    this.cut = cut;
    this.nocuts = nocuts;
    this.sense = sense;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MsfraggerEnzyme.class.getSimpleName() + "[", "]")
        .add("name='" + name + "'")
        .add("cut='" + cut + "'")
        .add("nocuts='" + nocuts + "'")
        .add("sense='" + sense + "'")
        .toString();
  }
}
