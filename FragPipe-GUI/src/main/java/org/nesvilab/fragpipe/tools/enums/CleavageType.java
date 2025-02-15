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

/**
 * @author Dmitry Avtonomov
 */
public enum CleavageType {
  ENZYMATIC(2),
  SEMI(1),
  SEMI_N_TERM(3),
  NONSPECIFIC(0);

  private final int numEnzymeTermini;

  CleavageType(int num_enzyme_termini) {
    this.numEnzymeTermini = num_enzyme_termini;
  }

  public int valueInParamsFile() {
    return numEnzymeTermini;
  }

  public static CleavageType fromValueInParamsFile(String paramsFileRepresentation) {
    for (CleavageType ct : CleavageType.values()) {
      if (Integer.toString(ct.valueInParamsFile()).equals(paramsFileRepresentation)) {
        return ct;
      }
    }
    throw new IllegalArgumentException(
        "Enum CleavageType does not contain a mapping for params file value of '"
            + paramsFileRepresentation + "'");
  }
}
