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

package com.dmtavt.fragpipe.tools.diann;

import com.google.common.collect.ComparisonChain;

class Fragment implements Comparable<Fragment> {

  final float mz;
  final float intensity;
  final char type;
  final byte charge;
  final int ordinal;
  final String lossType;

  Fragment(float mz, float intensity, char type, byte charge, int ordinal, String lossType) {
    this.mz = mz;
    this.intensity = intensity;
    this.type = type;
    this.charge = charge;
    this.ordinal = ordinal;
    this.lossType = lossType;
  }

  @Override
  public int compareTo(Fragment o) {
    return ComparisonChain.start()
        .compare(mz, o.mz)
        .compare(type, o.type)
        .compare(ordinal, o.ordinal)
        .compare(lossType, o.lossType)
        .compare(charge, o.charge)
        .result();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof Fragment) {
      return compareTo((Fragment) o) == 0;
    } else {
      return false;
    }
  }

  @Override
  public String toString() {
    if (lossType.isEmpty()) {
      return String.valueOf(type) + ordinal + "^" + charge;
    } else {
      return String.valueOf(type) + ordinal + "-" + lossType + "^" + charge;
    }
  }

  @Override
  public int hashCode() {
    return toString().hashCode();
  }
}
