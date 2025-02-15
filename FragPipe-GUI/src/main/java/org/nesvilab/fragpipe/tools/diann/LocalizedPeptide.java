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

import com.google.common.collect.ComparisonChain;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LocalizedPeptide implements Comparable<LocalizedPeptide> {

  private static final Pattern pattern = Pattern.compile("[cnA-Z](\\(([0-9.]+)\\))?");
  private static final Pattern pattern2 = Pattern.compile("[cn()0-9.]+");

  final String localizedPeptide;
  final String scanName;
  final List<Float> probabilities;
  final int[] bestSites;
  final int localizedSites;
  final float sumOfBestProbabilities;



  LocalizedPeptide(String localizedPeptide, String scanName, float localizationProbT) {
    this.localizedPeptide = localizedPeptide;
    this.scanName = scanName;

    probabilities = new ArrayList<>();

    int length = 0;
    long[] probSiteMap = new long[localizedPeptide.length()];
    Matcher matcher = pattern.matcher(localizedPeptide);
    while (matcher.find()) {
      if (matcher.group(1) == null) {
        probabilities.add(0f);
      } else {
        float probability = Float.parseFloat(matcher.group(2));
        probabilities.add(probability);
        probSiteMap[length] = ((long) Float.floatToRawIntBits(probability)) << 32 | length;
      }
      ++length;
    }

    float ttt = 0;
    for (float probability : probabilities) {
      ttt += probability;
    }
    int mods = Math.round(ttt);

    bestSites = new int[mods];

    Arrays.sort(probSiteMap);
    int t1 = 0;
    float t2 = 0;
    for (int i = probSiteMap.length - 1; i > probSiteMap.length - 1 - mods; --i) {
      float tt = Float.intBitsToFloat((int) (probSiteMap[i] >>> 32));
      bestSites[probSiteMap.length - 1 - i] = (int) probSiteMap[i];
      if (tt > localizationProbT) {
        ++t1;
      }
      t2 += tt;
    }
    localizedSites = t1;
    sumOfBestProbabilities = t2;
  }

  public String getSequence() {
    return pattern2.matcher(localizedPeptide).replaceAll("");
  }

  public float getBestLocalization() {
    return probabilities.get(bestSites[0]);
  }

  public int compareTo(LocalizedPeptide other) {
    return ComparisonChain.start().compare(this.localizedSites, other.localizedSites).compare(this.sumOfBestProbabilities, other.sumOfBestProbabilities).compare(this.localizedPeptide, other.localizedPeptide).result();
  }

  public boolean equals(Object other) {
    if (other instanceof LocalizedPeptide) {
      return compareTo((LocalizedPeptide) other) == 0;
    } else {
      return false;
    }
  }

  public int hashCode() {
    return localizedPeptide.hashCode();
  }

  public String toString() {
    return localizedPeptide;
  }
}
