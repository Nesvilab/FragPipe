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

import static com.dmtavt.fragpipe.cmd.ToolingUtils.UNIMOD_OBO;
import static com.dmtavt.fragpipe.cmd.ToolingUtils.getUnimodOboPath;

import com.dmtavt.fragpipe.FragpipeLocations;
import com.google.common.collect.Table;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.jooq.lambda.Seq;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class UnimodOboReaderTest {

  private Path unimodOboPath;

  @Before
  public void setUp() throws Exception {
    unimodOboPath = getUnimodOboPath(UNIMOD_OBO);
  }

  @Test
  public void readUnimodObo() throws Exception {
    UnimodOboReader unimodOboReader = new UnimodOboReader(unimodOboPath);

    Map<String, Float> unimodMassMap = unimodOboReader.unimodMassMap;
    Assert.assertEquals(1514, unimodMassMap.size());
    Assert.assertEquals(Float.NaN, unimodMassMap.get("unimod:0"), 0.0001f);
    Assert.assertEquals(42.010565f, unimodMassMap.get("unimod:1"), 0.0001f);
    Assert.assertEquals(1497.4434f, unimodMassMap.get("unimod:1716"), 0.0001f);
    Assert.assertEquals(88.016044f, unimodMassMap.get("unimod:2072"), 0.0001f);

    Table<Float, Character, Integer> massSiteUnimodTable = unimodOboReader.massSiteUnimodTable;
    Assert.assertEquals(Integer.valueOf(1), massSiteUnimodTable.get(42.010565f, 'n'));
    Assert.assertEquals(Integer.valueOf(35), massSiteUnimodTable.get(15.994915f, 'M'));
    Assert.assertEquals(Integer.valueOf(21), massSiteUnimodTable.get(79.966331f, 'S'));
    Assert.assertEquals(Integer.valueOf(21), massSiteUnimodTable.get(79.966331f, 'T'));
    Assert.assertEquals(Integer.valueOf(21), massSiteUnimodTable.get(79.966331f, 'Y'));

    Assert.assertEquals(Integer.valueOf(481), massSiteUnimodTable.get(4.025107f, 'K'));
    Assert.assertEquals(Integer.valueOf(188), massSiteUnimodTable.get(6.020129f, 'R'));
    Assert.assertEquals(Integer.valueOf(259), massSiteUnimodTable.get(8.014199f, 'K'));
    Assert.assertEquals(Integer.valueOf(267), massSiteUnimodTable.get(10.008269f, 'R'));
  }
}