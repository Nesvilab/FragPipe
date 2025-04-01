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

package org.nesvilab.fragpipe.cmd;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.Method;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;

public class CmdDiannTest {

  @Test
  @SuppressWarnings("unchecked")
  public void testCollectAllMods() throws Exception {
    CmdDiann cmdDiann = new CmdDiann(true, Paths.get("."));
    Method getPlexDiannFlagsMethod = CmdDiann.class.getDeclaredMethod("getPlexDiannFlags", String.class, String.class, String.class);
    getPlexDiannFlagsMethod.setAccessible(true);

    List<String> expectedFlags = Arrays.asList(
        "--fixed-mod",
        "label,618.36036,C,label",
        "--channels",
        "label,L,C,0.0;label,M,C,2.6172485;label,H,C,6.006897",
        "--peak-translation",
        "--original-mods");
    List<String> actualFlags = (List<String>) getPlexDiannFlagsMethod.invoke(cmdDiann, "C618.36036", "C620.9776", "C624.36722");
    assertEquals(expectedFlags, actualFlags);

    expectedFlags = Arrays.asList(
        "--fixed-mod",
        "label,0,K,label",
        "--fixed-mod",
        "label,0,R,label",
        "--channels",
        "label,L,KR,0.0:0.0;label,M,KR,4.025107:6.020129;label,H,KR,8.014199:10.008269",
        "--peak-translation",
        "--original-mods");
    actualFlags = (List<String>) getPlexDiannFlagsMethod.invoke(cmdDiann, "K0;R0", "K4.025107;R6.020129", "K8.014199;R10.008269");
    assertEquals(expectedFlags, actualFlags);

    expectedFlags = Arrays.asList(
        "--fixed-mod",
        "label,0,R,label",
        "--fixed-mod",
        "label,0,K,label",
        "--channels",
        "label,L,KR,0.0:0.0;label,M,KR,4.025107:6.020129;label,H,KR,8.014199:10.008269",
        "--peak-translation",
        "--original-mods");
    actualFlags = (List<String>) getPlexDiannFlagsMethod.invoke(cmdDiann, "R0;K0", "K4.025107;R6.020129;", "R10.008269;K8.014199");
    assertEquals(expectedFlags, actualFlags);

    expectedFlags = Arrays.asList(
        "--fixed-mod",
        "label,618.36036,C,label",
        "--fixed-mod",
        "label,0,nKR,label",
        "--channels",
        "label,L,CKRn,0.0:0.0:0.0:0.0;label,M,CKRn,2.6172485:4.322:4.322:4.322;label,H,CKRn,6.006897:5.4:5.4:5.4",
        "--peak-translation",
        "--original-mods");
    actualFlags = (List<String>) getPlexDiannFlagsMethod.invoke(cmdDiann, "C618.36036;nKR0", "C620.9776;nKR4.322", "C624.36722;nKR5.4");
    assertEquals(expectedFlags, actualFlags);
  }

  @Test(expected = Exception.class)
  @SuppressWarnings("unchecked")
  public void testCollectAllMods2() throws Exception {
    CmdDiann cmdDiann = new CmdDiann(true, Paths.get("."));
    Method getPlexDiannFlagsMethod = CmdDiann.class.getDeclaredMethod("getPlexDiannFlags", String.class, String.class, String.class);
    getPlexDiannFlagsMethod.setAccessible(true);

    List<String> expectedFlags = Arrays.asList(
        "--fixed-mod",
        "label,618.36036,C,label",
        "--fixed-mod",
        "label,0,nKR,label",
        "--channels",
        "label,L,CKRn,0.0:0.0:0.0:0.0;label,M,CKRn,2.6172485:4.322:4.322:4.322;label,H,CKRn,6.006897:5.4:5.4:5.4",
        "--peak-translation",
        "--original-mods");
    List<String> actualFlags = (List<String>) getPlexDiannFlagsMethod.invoke(cmdDiann, "C618.36036;nKR0", "C620.9776;", "C624.36722;nKR5.4");
    assertEquals(expectedFlags, actualFlags);
  }

  @Test(expected = Exception.class)
  @SuppressWarnings("unchecked")
  public void testCollectAllMods3() throws Exception {
    CmdDiann cmdDiann = new CmdDiann(true, Paths.get("."));
    Method getPlexDiannFlagsMethod = CmdDiann.class.getDeclaredMethod("getPlexDiannFlags", String.class, String.class, String.class);
    getPlexDiannFlagsMethod.setAccessible(true);

    List<String> expectedFlags = Arrays.asList(
        "--fixed-mod",
        "label,618.36036,C,label",
        "--fixed-mod",
        "label,0,nKR,label",
        "--channels",
        "label,L,CKRn,0.0:0.0:0.0:0.0;label,M,CKRn,2.6172485:4.322:4.322:4.322;label,H,CKRn,6.006897:5.4:5.4:5.4",
        "--peak-translation",
        "--original-mods");
    List<String> actualFlags = (List<String>) getPlexDiannFlagsMethod.invoke(cmdDiann, "", "C620.9776;", "C624.36722;nKR5.4");
    assertEquals(expectedFlags, actualFlags);
  }
}
