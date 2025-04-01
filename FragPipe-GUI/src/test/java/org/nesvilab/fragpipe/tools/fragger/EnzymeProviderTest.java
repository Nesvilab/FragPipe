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

package org.nesvilab.fragpipe.tools.fragger;

import static org.junit.Assert.assertFalse;

import java.util.List;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EnzymeProviderTest {
  private static final Logger log = LoggerFactory.getLogger(EnzymeProviderTest.class);

  @Test
  public void get() {
    List<MsfraggerEnzyme> enzymes = new EnzymeProvider().get();
    log.info("Parsed enzymes: {}", enzymes);
    assertFalse("No enzymes parsed", enzymes.isEmpty());
  }
}