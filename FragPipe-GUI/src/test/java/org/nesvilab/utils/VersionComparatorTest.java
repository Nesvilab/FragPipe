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

package org.nesvilab.utils;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class VersionComparatorTest {
  VersionComparator vc;

  @Before
  public void setup() {
    vc = new VersionComparator();
  }


  @Test
  public void versionRcTests() {
    int compare = vc.compare("13.0-RC11", "13.0-RC2");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("13.0-RC11", "13.0.0-RC11");
    Assert.assertTrue(compare == 0);

    compare = vc.compare("13.0-RC11", "13.0.0-RC2");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("13.1-RC11", "13.1.0-RC2");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("13.1", "13.1.0-RC2");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("13.1.0", "13.1.0-RC2");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("13.1", "13.1-RC2");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("13.1.1-rc1", "13.1.0");
    Assert.assertTrue(compare > 0);

//    compare = vc.compare("13.1-rc1", "13.1.0"); // The DefaultArtifactVersion can not handle this case
//    Assert.assertTrue(compare > 0);
  }


  @Test
  public void versionIsGreater() {
    int compare = vc.compare("9.0", "9.0-RC1");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("9.0", "9.0-b01");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("9.0.0", "9.0.RC1");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("9.1.1", "9.1");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("9.1.1.0", "9.1");
    Assert.assertTrue(compare > 0);

    compare = vc.compare("9.1.0.1", "9.1");
    Assert.assertTrue(compare > 0);
  }

  @Test
  public void versionIsSmaller() {
    int compare = vc.compare("8.9", "9.0.RC1");
    Assert.assertTrue(compare < 0);

    compare = vc.compare("8.9.1", "9.0.b01");
    Assert.assertTrue(compare < 0);
  }

  @Test
  public void versionsEqual() {
    int compare = vc.compare("9.0", "9.0.0");
    Assert.assertEquals(0, compare);

    compare = vc.compare("9.0", "9.0.0.0.0.0.0.0");
    Assert.assertEquals(0, compare);

    compare = vc.compare("9.1.0", "9.1");
    Assert.assertEquals(0, compare);
  }
}
