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

package org.nesvilab.fragpipe.util;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.Ignore;
import org.junit.Test;

public class RewritePepxmlTest {

  @Test @Ignore
  public void testPepxmlRewrite() throws IOException {
    Path dir = Paths.get("G:\\dev\\msfragger\\dev1\\exp_1");
    String fn = "interact-b1906_293T_proteinID_01A_QE3_122212.pep.xml";
    Path path = dir.resolve(fn);

    RewritePepxml.rewriteRawPath(path, false, "I:\\data\\PXD001468\\b1906_293T_proteinID_01A_QE3_122212.d");
  }

}