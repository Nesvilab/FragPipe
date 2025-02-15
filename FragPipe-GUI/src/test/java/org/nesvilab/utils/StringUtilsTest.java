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

import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class StringUtilsTest {

  @Test
  public void testSimple() {
    String cmd = "command param1 param2";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(3, parts.size());
  }

  @Test
  public void testFlagsAndParams() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(5, parts.size());
  }

  @Test
  public void testPathWithDoubleQuotes() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i \"c:/some/path/to/file\"";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(7, parts.size());
  }

  @Test
  public void testDoubleQuotesAndParamAfter() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i \"c:/some/path/to/file\" --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }

  @Test
  public void testSingleQuotesAndParamAfter() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i 'c:/some/path/to/file' --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }

  @Test
  public void testDoubleQuotePathWithSpaces() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i \"c:/some/path with spaces/to/file\" --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }

  @Test
  public void testSingleQuotePathWithSpaces() {
    String cmd = "command --flag-1 --flag-2 --param-3 param-3-value -i 'c:/some/path with spaces/to/file' --help";
    List<String> parts = StringUtils.splitCommandLine(cmd);
    Assert.assertEquals(8, parts.size());
  }
}
