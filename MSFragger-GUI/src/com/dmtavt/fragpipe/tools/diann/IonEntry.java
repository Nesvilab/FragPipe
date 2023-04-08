package com.dmtavt.fragpipe.tools.diann;

import com.dmtavt.fragpipe.tools.diann.PlexDiaHelper.Peptide;
import java.util.regex.Pattern;

class IonEntry {

  private static final Pattern pattern = Pattern.compile("@");

  final String run;
  final String ion;
  final Peptide peptide;
  final String labelFreeIonID;
  final int labelType;
  final float apexRt;
  final float intensity;

  IonEntry(String run, Peptide peptide, byte charge, float apexRt, float intensity) {
    this.run = run;
    this.peptide = peptide;
    this.apexRt = apexRt;
    this.intensity = intensity;

    ion = peptide.modifiedPeptide + charge;
    labelType = peptide.detectLabelTypes();
    labelFreeIonID = peptide.getLabelFreePeptide() + "@" + peptide.getLabelCount() + "@" + charge;
  }

  static String getFirstPart(String labelFreeIonID) {
    String[] parts = pattern.split(labelFreeIonID, -1);
    return parts[0];
  }

  static int getSecondPart(String labelFreeIonID) {
    String[] parts = pattern.split(labelFreeIonID, -1);
    return Integer.parseInt(parts[1]);
  }

  static int getThirdPart(String labelFreeIonID) {
    String[] parts = pattern.split(labelFreeIonID, -1);
    return Integer.parseInt(parts[2]);
  }
}
