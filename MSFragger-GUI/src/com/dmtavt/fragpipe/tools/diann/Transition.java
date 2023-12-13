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

import com.dmtavt.fragpipe.tools.diann.PlexDiaHelper.Peptide;
import com.google.common.collect.ComparisonChain;

class Transition implements Comparable<Transition> {

  final float precursorMz;
  final Fragment[] fragments;
  final String proteinId;
  final String geneName;
  final Peptide peptide;
  final byte peptideCharge;
  final float normalizedRetentionTime;
  final float precursorIonMobility;
  final float averageExperimentRetentionTime;

  Transition(float precursorMz, Fragment[] fragments, String proteinId, String geneName, Peptide peptide, byte peptideCharge, float normalizedRetentionTime, float precursorIonMobility, float averageExperimentRetentionTime) {
    this.precursorMz = precursorMz;
    this.fragments = fragments;
    this.proteinId = proteinId;
    this.geneName = geneName;
    this.peptide = peptide;
    this.peptideCharge = peptideCharge;
    this.normalizedRetentionTime = normalizedRetentionTime;
    this.precursorIonMobility = precursorIonMobility;
    this.averageExperimentRetentionTime = averageExperimentRetentionTime;
  }

  @Override
  public int compareTo(Transition o) {
    return ComparisonChain.start()
        .compare(precursorMz, o.precursorMz)
        .compare(peptide, o.peptide)
        .compare(peptideCharge, o.peptideCharge)
        .compare(normalizedRetentionTime, o.normalizedRetentionTime)
        .compare(precursorIonMobility, o.precursorIonMobility)
        .result();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof Transition) {
      return compareTo((Transition) o) == 0;
    } else {
      return false;
    }
  }

  @Override
  public String toString() {
    return peptide + "-" + peptideCharge + "-" + normalizedRetentionTime + "-" + precursorIonMobility + "-" + precursorMz;
  }

  @Override
  public int hashCode() {
    return toString().hashCode();
  }
}
