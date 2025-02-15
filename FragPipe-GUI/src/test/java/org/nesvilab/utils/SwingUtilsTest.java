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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SwingUtilsTest {
  private static final Logger log = LoggerFactory.getLogger(SwingUtilsTest.class);

  @Test
  public void makeHtml() {
    String s = "Not starts with html\nAnd has \n"
        + "some newlines <br/>\n"
        + "and a doble newline\n\n"
        + " that have non breaking spaces <br/>\n ";
    String s1 = SwingUtils.makeHtml(s);
    Assert.assertTrue(s1.startsWith("<html>"));
    Pattern reHtmlNewline = Pattern.compile("(.*?<br/>\\s*)");
    Matcher m = reHtmlNewline.matcher(s1);
    long count = 0;
    while (m.find()) {
      log.debug("Found match #{}: {}", count+1, m.group(1));
      count += 1;
    }
    Assert.assertEquals("Number of newlines", 6, count);
  }

  @Test @Ignore("Only used to test UI dialog popup manually")
  public void showHtmlMadeDialog() {
    Path saveDir = Paths.get("C:\\tmp\\hoho.txt");
    String msg = "<html>files need to be saved<br/>\n"
        + "in the same directory as LCMS files for that plex. Please save the<br/>\n"
        + "file in:<br/>\n<br/>\n" + saveDir.toString();
    String htmlMsg = SwingUtils.makeHtml(msg);

    SwingUtils.showWarningDialog(null,
        htmlMsg,
        "Select different location");
  }
}