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

package test.org.nesvilab.fragpipe.tools.crystalc;

import org.nesvilab.fragpipe.tools.crystalc.CrystalcParams;
import org.junit.Test;

public class CrystalcParamsTest {

  @Test
  public void loadDefault() {
    CrystalcParams p = new CrystalcParams();
    p.loadDefault();
    int a = 1;
  }
}