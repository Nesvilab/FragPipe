package com.dmtavt.fragpipe.tools.diann;

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
