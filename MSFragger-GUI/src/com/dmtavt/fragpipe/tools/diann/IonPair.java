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

class IonPair {

  final IonEntry lightIonEntry;
  final IonEntry mediumIonEntry;
  final IonEntry heavyIonEntry;

  IonPair(IonEntry lightIonEntry, IonEntry mediumIonEntry, IonEntry heavyIonEntry) {
    this.lightIonEntry = lightIonEntry;
    this.mediumIonEntry = mediumIonEntry;
    this.heavyIonEntry = heavyIonEntry;
  }

  float getTotalIntensity() {
    float totalIntensity = 0;
    if (lightIonEntry != null) {
      totalIntensity += lightIonEntry.intensity;
    }
    if (mediumIonEntry != null) {
      totalIntensity += mediumIonEntry.intensity;
    }
    if (heavyIonEntry != null) {
      totalIntensity += heavyIonEntry.intensity;
    }
    return totalIntensity;
  }
}
