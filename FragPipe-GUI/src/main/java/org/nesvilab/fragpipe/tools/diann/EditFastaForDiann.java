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

package org.nesvilab.fragpipe.tools.diann;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.regex.Matcher;

public class EditFastaForDiann {

  public static void main(String[] args) {
    Locale.setDefault(Locale.US);

    if (args.length < 3) {
      System.err.println("Usage: EditFastaForDiann <input_fasta> <output_fasta> <decoy_tag>");
      System.exit(1);
    }

    Path inputPath = Paths.get(args[0].trim());
    Path outputPath = Paths.get(args[1].trim());
    String decoyTag = args[2].trim();

    long startTime = System.nanoTime();

    try {
      editFasta(inputPath, outputPath, decoyTag);
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
    }

    System.out.printf("Done in %.2f seconds.%n", (System.nanoTime() - startTime) * 1e-9);
  }

  private static void editFasta(Path inputPath, Path outputPath, String decoyTag) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(Files.newInputStream(inputPath), StandardCharsets.UTF_8), 1 << 24);
         BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(outputPath), StandardCharsets.UTF_8), 1 << 24)) {
      String line;
      while ((line = reader.readLine()) != null) {
        if (line.startsWith(">")) {
          line = transformDecoyHeader(line, decoyTag);
        }
        writer.write(line);
        writer.newLine();
      }
    }
  }

  private static String transformDecoyHeader(String headerLine, String decoyTag) {
    headerLine = headerLine.replaceFirst(">contam_", ">");

    String afterGt = headerLine.substring(1);

    if (!afterGt.startsWith(decoyTag)) {
      return headerLine;
    }

    String afterDecoy = afterGt.substring(decoyTag.length());
    String dbType;
    String rest;
    int spIdx = afterDecoy.indexOf("sp|");
    int trIdx = afterDecoy.indexOf("tr|");
    if (spIdx >= 0) {
      dbType = "sp";
      rest = afterDecoy.substring(spIdx + 3);
    } else if (trIdx >= 0) {
      dbType = "tr";
      rest = afterDecoy.substring(trIdx + 3);
    } else {
      return headerLine;
    }

    // Split into identifier part and description part at the first space
    int spaceIdx = rest.indexOf(' ');
    String identifierPart = spaceIdx < 0 ? rest : rest.substring(0, spaceIdx);
    String descriptionPart = spaceIdx < 0 ? "" : rest.substring(spaceIdx);

    // Edit the identifier part: prepend decoyTag to each non-empty field
    String[] fields = identifierPart.split("\\|", -1);
    for (int i = 0; i < fields.length; ++i) {
      if (!fields[i].isEmpty()) {
        fields[i] = decoyTag + fields[i];
      }
    }

    // Edit GN= in the description part
    if (!descriptionPart.isEmpty()) {
      descriptionPart = descriptionPart.replaceAll("GN=([^ ]+)", "GN=" + Matcher.quoteReplacement(decoyTag) + "$1");
    }

    return ">" + dbType + "|" + String.join("|", fields) + descriptionPart;
  }
}
