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

import java.nio.file.Files;
import java.nio.file.Paths;
import org.junit.Assume;
import org.junit.Test;

public class PairScansTest {

    @Test
    public void testPairScans() {
        String s = "E:\\_Software_Tests\\DIA\\glycoDIA_HCD-ETD\\181217_Fusion_(LC2)_NewObj_Serum_deSA_Jacalin_HRM_4h_ETD_HCD_DDA_mz(400_1200).mzML";
        Assume.assumeTrue(Files.exists(Paths.get(s)));
        String[] args = new String[] {
                s,
                "11",
                "HCD",
                "ETD",
                "true",
                "false"};
        PairScans.main(args);
    }

}
