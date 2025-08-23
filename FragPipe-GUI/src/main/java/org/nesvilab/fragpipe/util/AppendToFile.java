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

import java.io.*;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

public class AppendToFile {

    public AppendToFile() {

    }

    public static void main(String[] args) {
        Locale.setDefault(Locale.US);
        try {
            /* Args:
             * - file to append to
             * - file to append from
             */
            appendMassesFileContents(args[0].trim(), args[1].trim());
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Append the contents of "fileAppendFrom" to the end of "fileAppendTo", removing duplicate masses if present.
     * @param fileAppendTo destination file
     * @param fileAppendFrom source file
     */
    private static void appendMassesFileContents(String fileAppendTo, String fileAppendFrom) throws IOException {
        BufferedReader appendFromReader = new BufferedReader(new FileReader(fileAppendFrom));
        BufferedReader appendToReader = new BufferedReader(new FileReader(fileAppendTo));

        BufferedWriter writer = new BufferedWriter(new FileWriter(fileAppendTo, true));

        Set<Long> masses = new HashSet<>();
        // read all masses in the file to append to so we can avoid duplicates while appending
        String readline;
        while ((readline = appendToReader.readLine()) != null) {
            long mass = Math.round(Double.parseDouble(readline.trim()) * 100);
            masses.add(mass);
        }

        // read new masses and append, checking for duplicates
        while ((readline = appendFromReader.readLine()) != null) {
            long mass = Math.round(Double.parseDouble(readline.trim()) * 100);
            if (!masses.contains(mass)) {
                writer.write(readline + "\n");
            }
        }
        writer.close();
    }

}
