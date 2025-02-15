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
package org.nesvilab.fragpipe.tools.msbooster;

import org.nesvilab.fragpipe.exceptions.ValidationException;

public class Model {

  public String name;
  public boolean isEnabled;

  public Model(String name, boolean isEnabled) {
    this.name = name;
    this.isEnabled = isEnabled;
  }

  public static String asString(Model m) {
    return m.name + "," + m.isEnabled;
  }

  public static Model fromString(String s) throws ValidationException {
    String[] split = s.split(",");
    if (split.length != 2) {
      throw new ValidationException("Not a valid model string");
    }
    return new Model(split[0], Boolean.parseBoolean(split[1]));
  }
}
