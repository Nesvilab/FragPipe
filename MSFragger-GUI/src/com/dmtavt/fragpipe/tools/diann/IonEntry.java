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

import java.util.Map;
import java.util.regex.Pattern;
import umich.ms.fileio.filetypes.library.Peptide;

class IonEntry {

  private static final Pattern pattern = Pattern.compile("@");

  final String run;
  final String ion;
  final Peptide peptide;
  final String labelFreeIonID;
  final int labelType;
  final float apexRt;
  final float intensity;

  IonEntry(String run, Peptide peptide, byte charge, float apexRt, float intensity, Map<Character, Float> lightAaMassMap, Map<Character, Float> mediumAaMassMap, Map<Character, Float> heavyAaMassMap) {
    this.run = run;
    this.peptide = peptide;
    this.apexRt = apexRt;
    this.intensity = intensity;

    ion = peptide.modifiedPeptide + charge;
    labelType = peptide.detectLabelTypes(lightAaMassMap, mediumAaMassMap, heavyAaMassMap);
    labelFreeIonID = peptide.getLabelFreePeptide(lightAaMassMap, mediumAaMassMap, heavyAaMassMap) + "@" + peptide.getLabelCount(lightAaMassMap, mediumAaMassMap, heavyAaMassMap) + "@" + charge;
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
