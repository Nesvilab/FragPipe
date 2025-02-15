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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import org.junit.Test;

public class DiannToMsstatsTest {

  @Test
  public void test() throws Exception {
    Path diannPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/report.tsv")).toURI()).toAbsolutePath();
    if (!Files.exists(diannPath) || !Files.isRegularFile(diannPath) || !Files.isReadable(diannPath)) {
      throw new IllegalStateException("Test DIA-NN file not found: " + diannPath);
    }

    Map<String, String[]> t = new HashMap<>();
    t.put("run1", new String[]{"a", "1"});
    t.put("run2", new String[]{"b", "2"});

    new DiannToMsstats(diannPath.toAbsolutePath().toString(), diannPath.getParent().toString(), diannPath.getParent().resolve("psm.tsv").toString(), 0.01f, 0.01f, 0.01f, 0.01f, t);

    BufferedReader brExpected = new BufferedReader(Files.newBufferedReader(diannPath.getParent().resolve("msstats_expected.csv")));
    BufferedReader brActual = new BufferedReader(Files.newBufferedReader(diannPath.getParent().resolve("msstats.csv")));
    String lineExpected;
    String lineActual;
    while ((lineExpected = brExpected.readLine()) != null && (lineActual = brActual.readLine()) != null) {
      String[] a = lineExpected.split(",");
      String[] b = lineActual.split(",");
      assertEquals(a.length, b.length);

      for (int i = 0; i < a.length; ++i) {
        if (i == 11 && !a[i].equalsIgnoreCase("intensity") && !b[i].equalsIgnoreCase("intensity")) {
          assertEquals(Float.parseFloat(a[i]), Float.parseFloat(b[i]), 0.1f);
        } else {
          assertEquals(a[i], b[i]);
        }
      }
    }

    assertNull(brExpected.readLine());
    assertNull(brActual.readLine());

    brActual.close();
    brExpected.close();
  }
}
