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
package org.nesvilab.fragpipe.tools.fragger;

import org.nesvilab.fragpipe.exceptions.ValidationException;

/**
 * @author Dmitry Avtonomov
 */
public class Mod {

  public final float massDelta;
  public final String sites;
  public final boolean isEnabled;
  public final int maxOccurrences;

  public Mod(float massDelta, String sites, boolean isEnabled, int maxOccurrences) {
    this.massDelta = massDelta;
    this.sites = sites;
    this.isEnabled = isEnabled;
    this.maxOccurrences = maxOccurrences;
  }

  public static String asString(Mod m) {
    return m.massDelta + "," + m.sites + "," + m.isEnabled + "," + m.maxOccurrences;
  }

  public static Mod fromString(String s) throws ValidationException {
      String[] split = s.split(",");
      if (split.length != 4)
          throw new ValidationException("Not a valid mod string");
      return new Mod(Float.parseFloat(split[0]), split[1], Boolean.parseBoolean(split[2]), Integer.parseInt(split[3]));
  }
}
