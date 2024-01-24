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

package com.dmtavt.fragpipe.tools.diann;

import static com.dmtavt.fragpipe.cmd.ToolingUtils.UNIMOD_OBO;
import static com.dmtavt.fragpipe.cmd.ToolingUtils.getUnimodOboPath;

import com.dmtavt.fragpipe.util.UnimodOboReader;
import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Localization {

  private static final Pattern pattern = Pattern.compile("([ncA-Z]+):([\\d.-]+)");

  private final UnimodOboReader unimodOboReader;


  public static void main(String[] args) {
    long startTime = System.nanoTime();

    Path wd = Paths.get(args[0].trim());
    Path psmPath = null;

    try {
      List<Path> tt = Files.walk(wd).filter(Files::isRegularFile).filter(path -> path.getFileName().toString().equals("psm.tsv")).collect(Collectors.toList());
      if (tt.isEmpty()) {
        System.out.println("Could not find psm.tsv in " + wd);
        System.exit(0);
      } else if (tt.size() > 1) {
        System.out.println("Found more than one psm.tsv in " + wd + " but the DIA workflow should only have one,");
        System.exit(0);
      } else {
        psmPath = tt.get(0);
      }

      Localization localization = new Localization();
      localization.propagate(psmPath, wd.resolve("diann-output"));
    } catch (Exception ex) {
      ex.printStackTrace();
      System.exit(1);
    }

    System.out.printf("Done in %.2f seconds.\n", (System.nanoTime() - startTime) * 1e-9);
  }

  public Localization() throws Exception {
    Path unimodPath = getUnimodOboPath(UNIMOD_OBO);
    unimodOboReader = new UnimodOboReader(unimodPath);
  }

  public void propagate(Path psm_path, Path diann_directory) throws Exception {
    TreeBasedTable<String, String, LocalizedPeptide> precursorModificationLocalizationTable = TreeBasedTable.create();

    int scanNameColumnIdx = -1;
    int peptideColumnIdx = -1;
    int assignedModificationsColumnIdx = -1;
    int chargeColumnIdx = -1;
    Map<String, Integer> modificationColumnIdxMap = new TreeMap<>();

    String line;
    BufferedReader reader = Files.newBufferedReader(psm_path);
    while ((line = reader.readLine()) != null) {
      line = line.trim();
      if (line.isEmpty()) {
        continue;
      }

      String[] parts = line.split("\t");
      if (line.startsWith("Spectrum\tSpectrum File\t")) {
        for (int i = 0; i < parts.length; ++i) {
          if (parts[i].trim().contentEquals("Spectrum")) {
            scanNameColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Peptide")) {
            peptideColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Assigned Modifications")) {
            assignedModificationsColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Charge")) {
            chargeColumnIdx = i;
          } else {
            Matcher matcher = pattern.matcher(parts[i].trim());
            if (matcher.matches()) {
              modificationColumnIdxMap.put(parts[i].trim(), i);
            }
          }
        }
      } else {
        if (modificationColumnIdxMap.isEmpty()) {
          System.out.println("No modification localization columns found in " + psm_path + ". Do not propagate localization information.");
          System.exit(0);
        }

        if (scanNameColumnIdx < 0 || peptideColumnIdx < 0 || assignedModificationsColumnIdx < 0 || chargeColumnIdx < 0) {
          System.err.printf("Missing %s, %s, %s, or %s in %s.", "Spectrum", "Peptide", "Assigned Modifications", "Charge", psm_path);
          System.exit(1);
        }

        String precursor = unimodOboReader.convertPeptide(parts[peptideColumnIdx].trim(), parts[assignedModificationsColumnIdx].trim()) + parts[chargeColumnIdx].trim();

        for (Map.Entry<String, Integer> e : modificationColumnIdxMap.entrySet()) {
          if (parts[e.getValue()].trim().isEmpty()) {
            continue;
          }
          LocalizedPeptide localizedPeptide1 = new LocalizedPeptide(parts[e.getValue()].trim(), parts[scanNameColumnIdx].trim(), 0.75f); // Use 0.75 to get the number of confidently localized sites
          LocalizedPeptide localizedPeptide2 = precursorModificationLocalizationTable.get(precursor, e.getKey());
          if (localizedPeptide2 == null || localizedPeptide2.compareTo(localizedPeptide1) < 0) {
            precursorModificationLocalizationTable.put(precursor, e.getKey(), localizedPeptide1);
          }
        }
      }
    }
    reader.close();

    editReport(diann_directory.resolve("report.tsv"), precursorModificationLocalizationTable, 1);
    editReport(diann_directory.resolve("report.pr_matrix.tsv"), precursorModificationLocalizationTable, 2);
  }

  private void editReport(Path p, Table<String, String, LocalizedPeptide> precursorModificationLocalizationTable, int type) throws Exception {
    String s = "";
    String firstLineMarker = "";
    if (type == 1) {
      s = "report2.tsv";
      firstLineMarker = "File.Name\tRun\t";
    } else if (type == 2) {
      s = "report.pr_matrix2.tsv";
      firstLineMarker = "Protein.Group\tProtein.Ids\t";
    }
    Path p2 = p.getParent().resolve(s);

    String[] modificationArray = precursorModificationLocalizationTable.columnKeySet().toArray(new String[0]);
    int precursorIdColumnIdx = -1;

    String line;
    String[] columnArray = null;
    BufferedWriter writer = Files.newBufferedWriter(p2);
    BufferedReader reader = Files.newBufferedReader(p);
    while ((line = reader.readLine()) != null) {
      line = line.trim();
      if (line.isEmpty()) {
        continue;
      }

      String[] parts = line.split("\t");
      if (line.startsWith(firstLineMarker)) {
        for (int i = 0; i < parts.length; ++i) {
          if (parts[i].trim().contentEquals("Precursor.Id")) {
            precursorIdColumnIdx = i;
            break;
          }
        }

        columnArray = new String[parts.length];
        System.arraycopy(parts, 0, columnArray, 0, parts.length);

        writer.write(line);
        for (String modification : modificationArray) {
          writer.write("\t");
          writer.write(modification);
          writer.write("\t");
          writer.write(modification + " Best Localization");
          writer.write("\t");
          writer.write(modification + " Best Scan");
        }
        writer.write("\n");
      } else {
        if (precursorIdColumnIdx < 0) {
          System.err.printf("Missing %s column in %s.", "Precursor.Id", p);
          System.exit(1);
        }

        // Make sure that all rows have the same number of columns.
        Arrays.fill(columnArray, "");
        System.arraycopy(line.split("\t"), 0, columnArray, 0, parts.length);
        writer.write(String.join("\t", columnArray));

        String precursor = parts[precursorIdColumnIdx].trim();
        Map<String, LocalizedPeptide> tt = precursorModificationLocalizationTable.row(precursor);
        if (tt.isEmpty()) {
          for (int i = 0; i < modificationArray.length; ++i) {
            writer.write("\t\t\t");
          }
        } else {
          for (String modification : modificationArray) {
            writer.write("\t");
            LocalizedPeptide localizedPeptide = tt.get(modification);
            if (localizedPeptide == null) {
              writer.write("\t\t");
            } else {
              writer.write(localizedPeptide.localizedPeptide);
              writer.write("\t");
              writer.write(String.valueOf(localizedPeptide.getBestLocalization()));
              writer.write("\t");
              writer.write(localizedPeptide.scanName);
            }
          }
        }

        writer.write("\n");
      }
    }

    reader.close();
    writer.close();

    Files.move(p2, p, StandardCopyOption.REPLACE_EXISTING);
  }
}
