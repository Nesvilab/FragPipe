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
package org.nesvilab.fragpipe.tools.enums;

import static org.nesvilab.fragpipe.tools.fragger.MsfraggerParams.PROP_fragment_mass_tolerance;

/**
 *
 * @author Dmitry Avtonomov
 */
public enum MassTolUnits {
  PPM (1),
  Da(0);

  private final int val;

  private MassTolUnits(int val) {
    this.val = val;
  }

  public int valueInParamsFile() {
    return val;
  }

  public static MassTolUnits fromFileToUi(String fileRepresentation) {
    int v = Integer.parseInt(fileRepresentation);
    for (int i = 0; i < MassTolUnits.values().length; i++) {
      MassTolUnits u = MassTolUnits.values()[i];
      if (u.valueInParamsFile() == v)
        return u;
    }
    throw new IllegalStateException("Value for MassTolUnits stored in params file for property " + PROP_fragment_mass_tolerance +
        " does not correspond to enum values of MassTolUnits.");
  }
}
