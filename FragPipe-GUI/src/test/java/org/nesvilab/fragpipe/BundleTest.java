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

package org.nesvilab.fragpipe;

import org.nesvilab.fragpipe.params.ThisAppProps;
import org.jsoup.internal.StringUtil;
import org.junit.Assert;
import org.junit.Test;

public class BundleTest {

  @Test
  public void checkAnnouncementIsEmptyForRelease() {
    if (!Version.isDevBuild()) {
      String announce = ThisAppProps.getLocalProperties().getProperty(Version.PROP_ANNOUNCE);
      Assert.assertTrue(
          String.format("Bundle property [%s] must be empty for release versions, but had value [%s]", Version.PROP_ANNOUNCE, announce),
          StringUtil.isBlank(announce));
    }
  }
}
