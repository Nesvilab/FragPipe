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

package com.dmtavt.fragpipe.util;

import static com.dmtavt.fragpipe.util.Utils.threshold;
import static org.junit.Assert.assertArrayEquals;

import com.google.common.primitives.Floats;
import java.util.HashSet;
import org.junit.Test;

public class UtilsTest {

  @Test
  public void removeClosedModifications() {
    float[] inputArray = new float[]{0.1f, 0.2f, 0.2001f, 0.3f, 0.4f};
    assertArrayEquals(new float[]{0.1f, 0.2f, 0.3f, 0.4f}, Utils.removeClosedModifications(new HashSet<>(Floats.asList(inputArray))), (float) threshold);

    inputArray = new float[]{0.4f, 0.1f, 0.2f, 0.2001f, 0.2998f, 0.2999f, 0.3f};
    assertArrayEquals(new float[]{0.1f, 0.2f, 0.3f, 0.4f}, Utils.removeClosedModifications(new HashSet<>(Floats.asList(inputArray))), (float) threshold);

    inputArray = new float[]{0.1999f, 0.2f, 0.2001f, 0.2998f, 0.2999f, 0.3f, 0.3001f};
    assertArrayEquals(new float[]{0.2f, 0.3f}, Utils.removeClosedModifications(new HashSet<>(Floats.asList(inputArray))), (float) threshold);

    inputArray = new float[]{0.1999f, 0.2f, 0.2001f, 0.2998f, 0.2999f, 0.3001f};
    assertArrayEquals(new float[]{0.2f, 0.3001f}, Utils.removeClosedModifications(new HashSet<>(Floats.asList(inputArray))), (float) threshold);
  }
}