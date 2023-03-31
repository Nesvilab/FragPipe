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

package com.dmtavt.fragpipe.util;

import com.google.common.primitives.Floats;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class Utils {

  public static final double threshold = 0.005;
  public static final float[] AAMasses = {
      (float) 71.03711, // A
      (float) 0, // B
      (float) 103.00919, // C
      (float) 115.02694, // D
      (float) 129.04259, // E
      (float) 147.06841, // F
      (float) 57.02146, // G
      (float) 137.05891, // H
      (float) 113.08406, // I
      (float) 0, // J
      (float) 128.09496, // K
      (float) 113.08406, // L
      (float) 131.04049, // M
      (float) 114.04293, // N
      (float) 237.14773, // O
      (float) 97.05276, // P
      (float) 128.05858, // Q
      (float) 156.10111, // R
      (float) 87.03203, // S
      (float) 101.04768, // T
      (float) 150.95363, // U
      (float) 99.06841, // V
      (float) 186.07931, // W
      (float) 0, // X
      (float) 163.06333, //  Y
      (float) 0, // Z
  };

  public static float[] removeClosedModifications(Set<Float> inputList) {
    List<Float> tt = new LinkedList<>(inputList);
    tt.sort(Comparator.naturalOrder());
    while (true) {
      boolean ok = true;
      for (int i = 0; i < tt.size() - 1; ++i) {
        if (tt.get(i + 1) - tt.get(i) < threshold) { // inputArray is sorted.
          String a = tt.get(i).toString();
          String b = tt.get(i + 1).toString();
          if (a.length() - a.indexOf(".") < b.length() - b.indexOf(".")) { // Keep the one with fewer decimal digits.
            System.err.println("There are closed modifications: " + a + " and " + b + ". Will keep " + a);
            tt.remove(i + 1);
          } else { // If two have the same decimal digits, keep the one with bigger value because this one is more likely to be from rounding not truncating.
            System.err.println("There are closed modifications: " + a + " and " + b + ". Will keep " + b);
            tt.remove(i);
          }
          ok = false;
          break;
        }
      }
      if (ok) {
        return Floats.toArray(tt);
      }
    }
  }

  public static float correctModMass(double mass, float[] theoModMasses) {
    if (Math.abs(mass) < threshold) {
      return 0;
    }

    double gap = Double.MAX_VALUE;
    float roundedMass = 0;
    for (float modMass : theoModMasses) {
      if (Math.abs(mass - modMass) < gap) {
        gap = Math.abs(mass - modMass);
        roundedMass = modMass;
      }
    }
    if (gap > threshold) {
      throw new RuntimeException("Failed to correct the modification mass " + mass + ". The closest theoretical mass is " + roundedMass + ".");
    }
    return roundedMass;
  }
}
