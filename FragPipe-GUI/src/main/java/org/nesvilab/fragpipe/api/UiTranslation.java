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

package org.nesvilab.fragpipe.api;

public class UiTranslation {
  final String inUi;
  final String inConf;

  public UiTranslation(String inUi, String inConf) {
    this.inUi = inUi;
    this.inConf = inConf;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    UiTranslation that = (UiTranslation) o;

    if (inUi != null ? !inUi.equals(that.inUi) : that.inUi != null) {
      return false;
    }
    return inConf != null ? inConf.equals(that.inConf) : that.inConf == null;
  }

  @Override
  public int hashCode() {
    int result = inUi != null ? inUi.hashCode() : 0;
    result = 31 * result + (inConf != null ? inConf.hashCode() : 0);
    return result;
  }
}
