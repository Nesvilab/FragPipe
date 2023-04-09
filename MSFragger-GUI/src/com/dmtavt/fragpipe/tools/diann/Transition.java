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
