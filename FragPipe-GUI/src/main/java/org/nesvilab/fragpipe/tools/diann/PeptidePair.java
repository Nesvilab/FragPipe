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

package org.nesvilab.fragpipe.tools.diann;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class PeptidePair {

  final String lightModifiedSequence;
  final String mediumModifiedSequence;
  final String heavyModifiedSequence;
  final List<Float> logRatioMLList = new ArrayList<>(1);
  final List<Float> logRatioHMList = new ArrayList<>(1);
  final List<Float> logRatioHLList = new ArrayList<>(1);
  Set<Integer> chargeSet = new TreeSet<>();

  public PeptidePair(String lightModifiedSequence, String mediumModifiedSequence, String heavyModifiedSequence) {
    this.lightModifiedSequence = lightModifiedSequence;
    this.mediumModifiedSequence = mediumModifiedSequence;
    this.heavyModifiedSequence = heavyModifiedSequence;
  }
}
