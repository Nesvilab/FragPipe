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

import java.io.*;

public class AppendToFile {

    public AppendToFile() {

    }

    public static void main(String[] args) {
        try {
            /* Args:
             * - file to append to
             * - file to append from
             */
            appendFileContents(args[0].trim(), args[1].trim());
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Append the contents of "fileAppendFrom" to the end of "fileAppendTo"
     * @param fileAppendTo destination file
     * @param fileAppendFrom source file
     */
    private static void appendFileContents(String fileAppendTo, String fileAppendFrom) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(fileAppendFrom));
        BufferedWriter writer = new BufferedWriter(new FileWriter(fileAppendTo, true));

        String readline;
        while ((readline = reader.readLine()) != null) {
            writer.write(readline + "\n");
        }
        writer.close();
    }

}
