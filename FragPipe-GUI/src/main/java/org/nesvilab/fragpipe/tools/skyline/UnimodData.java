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

package org.nesvilab.fragpipe.tools.skyline;

import com.google.common.collect.ComparisonChain;
import java.util.Set;

public class UnimodData implements Comparable<UnimodData> {

  public final String name;
  public final Set<Character> aas;
  public final char terminus;
  public final int id;
  public final float monoMass;
  public final boolean structural;
  public final boolean hidden;

  public UnimodData(String name, Set<Character> aas, char terminus, Integer id, float monoMass, boolean structural, boolean hidden) {
    this.name = name;
    this.aas = aas;
    this.terminus = terminus;
    this.id = id;
    this.monoMass = monoMass;
    this.structural = structural;
    this.hidden = hidden;
  }

  @Override
  public int compareTo(UnimodData o) {
    return ComparisonChain.start()
        .compareTrueFirst(structural, o.structural)
        .compareFalseFirst(hidden, o.hidden)
        .compare(id, o.id)
        .compare(name, o.name)
        .compare(monoMass, o.monoMass)
        .result();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof UnimodData) {
      return compareTo((UnimodData) o) == 0;
    } else {
      return false;
    }
  }

  @Override
  public String toString() {
    return name + " " + aas + " " + terminus + " " + id + " " + monoMass + " " + structural + " " + hidden;
  }
}

