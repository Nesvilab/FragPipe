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

package org.nesvilab.fragpipe.tools.tmtintegrator;

public class ComboValue implements UiRepresentableValue {
  public final String valInConfig;
  public final String valInUi;
  public final String description;

  public ComboValue(String valInConfig, String valInUi, String description) {
    this.valInConfig = valInConfig;
    this.valInUi = valInUi;
    this.description = description;
  }

  public ComboValue(String valueInFile, String valInUi) {
    this(valueInFile, valInUi, "");
  }

  @Override
  public String getValInConfig() {
    return valInConfig;
  }

  @Override
  public String getValInUi() {
    return valInUi;
  }

  public String getDescription() {
    return description;
  }
}
