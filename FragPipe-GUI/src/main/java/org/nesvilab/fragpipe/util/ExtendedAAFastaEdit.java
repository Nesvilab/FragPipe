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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Command-line utility that creates a copy of a FASTA file with extended (non-canonical)
 * amino acid names in parentheses replaced by "X". For example, "PEP(special-name)TIDE"
 * becomes "PEPXTIDE". Only protein sequence lines are edited; header lines (starting with ">")
 * are written unchanged.
 *
 * <p>The output file is written to the same directory as the input file, with "_toX" appended
 * to the filename before the extension (e.g., "database.fasta" -> "database_toX.fasta").
 *
 * <p>Usage: ExtendedAAFastaEdit input_fasta;
 */
public class ExtendedAAFastaEdit {

    private static final Pattern EXTENDED_AA_PATTERN = Pattern.compile("\\([^)]+\\)");

    public ExtendedAAFastaEdit() {
    }

    public static void main(String[] args) {
        Locale.setDefault(Locale.US);
        if (args.length < 1) {
            System.err.println("Usage: ExtendedAAFastaEdit <input_fasta>");
            System.exit(1);
        }
        try {
            editFasta(args[0].trim());
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Returns the output path for the edited FASTA file, with "_toX" inserted before the
     * file extension (or appended if there is no extension).
     */
    public static String getOutputPath(String inputPath) {
        Path p = Paths.get(inputPath);
        String name = p.getFileName().toString();
        int dot = name.lastIndexOf('.');
        String newName;
        if (dot >= 0) {
            newName = name.substring(0, dot) + "_toX" + name.substring(dot);
        } else {
            newName = name + "_toX";
        }
        return p.resolveSibling(newName).toString();
    }

    /**
     * Reads the input FASTA file and writes a new file with all extended AA patterns
     * (sequences of the form "(name)") in sequence lines replaced with "X".
     */
    private static void editFasta(String inputPath) throws IOException {
        String outputPath = getOutputPath(inputPath);
        try (BufferedReader reader = new BufferedReader(new FileReader(inputPath));
             BufferedWriter writer = new BufferedWriter(new FileWriter(outputPath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.startsWith(">")) {
                    // Header line — write unchanged
                    writer.write(line);
                } else {
                    // Sequence line — replace (name) patterns with X
                    writer.write(EXTENDED_AA_PATTERN.matcher(line).replaceAll("X"));
                }
                writer.newLine();
            }
        }
        // Verify the output file was created successfully
        if (!Paths.get(outputPath).toFile().exists()) {
            throw new IOException("Output FASTA file was not created: " + outputPath);
        }
    }
}
