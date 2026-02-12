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

import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;
import umich.ms.fileio.filetypes.unimod.UnimodOboReader;
import umich.ms.fileio.filetypes.unimod.UnimodOboReader.Precursor;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.nesvilab.fragpipe.cmd.CmdDiann.labelPattern;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.UNIMOD_OBO;
import static org.nesvilab.fragpipe.cmd.ToolingUtils.getUnimodOboPath;

public class Propagation {

  private static final Pattern pattern = Pattern.compile("([ncA-Z]+):([\\d.-]+)");

  private final UnimodOboReader unimodOboReader;
  private final Map<String, Float> diannLabelMassMap = new HashMap<>();


  public static void main(String[] args) {
    Locale.setDefault(Locale.US);
    long startTime = System.nanoTime();

    Path wd = Paths.get(args[0].trim());
    String lightString = args[1].trim();
    String mediumString = args[2].trim();
    String heavyString = args[3].trim();

    if (lightString.contentEquals("-")) {
      lightString = "";
    }
    if (mediumString.contentEquals("-")) {
      mediumString = "";
    }
    if (heavyString.contentEquals("-")) {
      heavyString = "";
    }

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

      Propagation propagation = new Propagation(lightString, mediumString, heavyString);
      propagation.propagate(psmPath, wd.resolve("dia-quant-output"));
    } catch (Exception ex) {
      ex.printStackTrace();
      System.exit(1);
    }

    System.out.printf("Done in %.2f seconds.\n", (System.nanoTime() - startTime) * 1e-9);
  }

  public Propagation(String lightString, String mediumString, String heavyString) throws Exception {
    Path unimodPath = getUnimodOboPath(UNIMOD_OBO);
    unimodOboReader = new UnimodOboReader(unimodPath);

    if (lightString != null && !lightString.isEmpty()) {
      convertLabelString("L", lightString, diannLabelMassMap);
    }
    if (mediumString != null && !mediumString.isEmpty()) {
      convertLabelString("M", mediumString, diannLabelMassMap);
    }
    if (heavyString != null && !heavyString.isEmpty()) {
      convertLabelString("H", heavyString, diannLabelMassMap);
    }
  }

  public void propagate(Path psm_path, Path diann_directory) throws Exception {
    TreeBasedTable<Precursor, String, LocalizedPeptide> precursorModificationLocalizationTable = TreeBasedTable.create();
    Map<String, String[]> precursorProteinGeneMap = new HashMap<>();
    Map<String, String[]> proteinIdEntryDescMap = new HashMap<>();

    int scanNameColumnIdx = -1;
    int peptideColumnIdx = -1;
    int assignedModificationsColumnIdx = -1;
    int chargeColumnIdx = -1;
    int proteinColumnIdx = -1;
    int geneColumnIdx = -1;
    int mappedProteinsColumnIdx = -1;
    int mappedGenesColumnIdx = -1;
    int entryNameColumnIdx = -1;
    int proteinDescriptionColumnIdx = -1;
    int proteinIdColumnIdx = -1;
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
          } else if (parts[i].trim().contentEquals("Charge") || parts[i].trim().contentEquals("Precursor.Charge")) {
            chargeColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Protein")) {
            proteinColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Gene")) {
            geneColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Mapped Proteins")) {
            mappedProteinsColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Mapped Genes")) {
            mappedGenesColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Entry Name")) {
            entryNameColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Protein Description")) {
            proteinDescriptionColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Protein ID")) {
            proteinIdColumnIdx = i;
          } else {
            Matcher matcher = pattern.matcher(parts[i].trim());
            if (matcher.matches()) {
              modificationColumnIdxMap.put(parts[i].trim(), i);
            }
          }
        }
      } else {
        if (scanNameColumnIdx < 0 ||
            peptideColumnIdx < 0 ||
            assignedModificationsColumnIdx < 0 ||
            chargeColumnIdx < 0 ||
            proteinColumnIdx < 0 ||
            geneColumnIdx < 0 ||
            mappedProteinsColumnIdx < 0 ||
            mappedGenesColumnIdx < 0) {
          System.err.println("Missing column in " + psm_path + ": " + line);
          System.exit(1);
        }

        Precursor precursor = unimodOboReader.convertPrecursor(parts[peptideColumnIdx].trim(), parts[assignedModificationsColumnIdx].trim(), Integer.parseInt(parts[chargeColumnIdx]));

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

        Set<String> allMappedProteins = new TreeSet<>();
        if (parts.length > proteinColumnIdx && !parts[proteinColumnIdx].trim().isEmpty()) {
          allMappedProteins.add(parts[proteinColumnIdx].trim());
        }
        if (parts.length > mappedProteinsColumnIdx && !parts[mappedProteinsColumnIdx].trim().isEmpty()) {
          for (String s : parts[mappedProteinsColumnIdx].trim().split(",")) {
            allMappedProteins.add(s.trim());
          }
        }
        String allMappedProteinsStr = String.join(",", allMappedProteins);

        Set<String> allMappedGenes = new TreeSet<>();
        if (parts.length > geneColumnIdx && !parts[geneColumnIdx].trim().isEmpty()) {
          allMappedGenes.add(parts[geneColumnIdx].trim());
        }
        if (parts.length > mappedGenesColumnIdx && !parts[mappedGenesColumnIdx].trim().isEmpty()) {
          for (String s : parts[mappedGenesColumnIdx].trim().split(",")) {
            allMappedGenes.add(s.trim());
          }
        }
        String allMappedGenesStr = String.join(",", allMappedGenes);

        String entryName = "";
        if (entryNameColumnIdx >= 0 && parts.length > entryNameColumnIdx && !parts[entryNameColumnIdx].trim().isEmpty()) {
          entryName = parts[entryNameColumnIdx].trim();
        }

        String proteinDescription = "";
        if (proteinDescriptionColumnIdx >= 0 && parts.length > proteinDescriptionColumnIdx && !parts[proteinDescriptionColumnIdx].trim().isEmpty()) {
          proteinDescription = parts[proteinDescriptionColumnIdx].trim();
        }

        if (proteinIdColumnIdx >= 0) {
          String proteinId = parts[proteinIdColumnIdx].trim();
          String[] existing = proteinIdEntryDescMap.get(proteinId);
          if (existing == null) {
            proteinIdEntryDescMap.put(proteinId, new String[]{entryName, proteinDescription});
          } else {
            if (!existing[0].contentEquals(entryName)) {
              System.err.println("Inconsistent Entry Name for protein " + proteinId + " in " + psm_path + ": " + existing[0] + " vs " + entryName);
            }
            if (!existing[1].contentEquals(proteinDescription)) {
              System.err.println("Inconsistent Protein Description for protein " + proteinId + " in " + psm_path + ": " + existing[1] + " vs " + proteinDescription);
            }
          }
        }

        String[] ss = precursorProteinGeneMap.get(precursor.getSequence());
        if (ss == null) {
          precursorProteinGeneMap.put(precursor.getSequence(), new String[]{allMappedProteinsStr, allMappedGenesStr});
        } else {
          if (!ss[0].contentEquals(allMappedProteinsStr) || !ss[1].contentEquals(allMappedGenesStr)) {
            System.err.println("Inconsistent protein or gene mapping for sequence " + precursor.getSequence() + " in " + psm_path + ": " +
                    ss[0] + " vs " + allMappedProteinsStr + ", " + ss[1] + " vs " + allMappedGenesStr);
          }
        }
      }
    }
    reader.close();

    String[] modificationArray = null;
    if (!precursorModificationLocalizationTable.isEmpty()) {
      modificationArray = precursorModificationLocalizationTable.columnKeySet().toArray(new String[0]);
    }

    Path p = diann_directory.resolve("report.tsv");
    if (Files.exists(p) && Files.isReadable(p)) {
      editReport(p, precursorModificationLocalizationTable, modificationArray, precursorProteinGeneMap, proteinIdEntryDescMap, 1);
    }

    p = diann_directory.resolve("report.pr_matrix.tsv");
    if (Files.exists(p) && Files.isReadable(p)) {
      editReport(p, precursorModificationLocalizationTable, modificationArray, precursorProteinGeneMap, proteinIdEntryDescMap, 2);
    }

    p = diann_directory.resolve("report.pg_matrix.tsv");
    if (Files.exists(p) && Files.isReadable(p)) {
      editPgMatrix(p, proteinIdEntryDescMap);
    }
  }

  private void editReport(Path p, Table<Precursor, String, LocalizedPeptide> precursorModificationLocalizationTable, String[] modificationArray, Map<String, String[]> precursorProteinGeneMap, Map<String, String[]> proteinIdEntryDescMap, int type) throws Exception {
    String s = "";
    if (type == 1) {
      s = "report2.tsv";
    } else if (type == 2) {
      s = "report.pr_matrix2.tsv";
    }
    Path p2 = p.toAbsolutePath().getParent().resolve(s);

    int strippedSequenceColumnIdx = -1;
    int modifiedSequenceColumnIdx = -1;
    int chargeColumnIdx = -1;
    int proteinIdsColumnIdx = -1;
    int proteinNamesColumnIdx = -1;
    int firstProteinDescriptionColumnIdx = -1;

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
      if (line.contains("Protein.Group") && line.contains("Protein.Ids")) {
        for (int i = 0; i < parts.length; ++i) {
          if (parts[i].trim().contentEquals("Stripped.Sequence")) {
            strippedSequenceColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Modified.Sequence")) {
            modifiedSequenceColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Precursor.Charge")) {
            chargeColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Protein.Ids")) {
            proteinIdsColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Protein.Names")) {
            proteinNamesColumnIdx = i;
          } else if (parts[i].trim().contentEquals("First.Protein.Description")) {
            firstProteinDescriptionColumnIdx = i;
          }
        }

        columnArray = new String[parts.length];
        System.arraycopy(parts, 0, columnArray, 0, parts.length);

        writer.write(line);
        writer.write("\tAll Mapped Proteins\tAll Mapped Genes");

        if (!precursorModificationLocalizationTable.isEmpty() && modificationArray != null) {
          for (String modification : modificationArray) {
            writer.write("\t");
            writer.write(modification);
            writer.write("\t");
            writer.write(modification + " Best Localization");
            writer.write("\t");
            writer.write(modification + " Best Scan");
          }
        }

        writer.write("\n");
      } else {
        if (strippedSequenceColumnIdx < 0 || modifiedSequenceColumnIdx < 0 || chargeColumnIdx < 0) {
          System.err.printf("Missing %s, %s, or %s in %s.", "Stripped.Sequence", "Modified.Sequence", "Charge", p);
          System.exit(1);
        }

        // Make sure that all rows have the same number of columns.
        Arrays.fill(columnArray, "");
        System.arraycopy(parts, 0, columnArray, 0, parts.length);

        Precursor precursor = null;
        try {
          precursor = new Precursor(parts[modifiedSequenceColumnIdx],
              parts[strippedSequenceColumnIdx].length(),
              Integer.parseInt(parts[chargeColumnIdx]),
              unimodOboReader.unimodMassMap,
              diannLabelMassMap);
        } catch (Exception e) {
          System.err.println("Something was wrong with the file " + p.toAbsolutePath() + "; line: " + line);
          e.printStackTrace();
          System.exit(1);
        }

        String[] ss = precursorProteinGeneMap.get(precursor.getSequence());

        if (proteinIdsColumnIdx >= 0) {
          String proteinId = columnArray[proteinIdsColumnIdx].trim();
          if (!proteinId.isEmpty()) {
            String[] ed = proteinIdEntryDescMap.get(proteinId);
            if (ed != null) {
              if (proteinNamesColumnIdx >= 0 && !ed[0].isEmpty()) {
                columnArray[proteinNamesColumnIdx] = ed[0];
              }
              if (firstProteinDescriptionColumnIdx >= 0 && !ed[1].isEmpty()) {
                columnArray[firstProteinDescriptionColumnIdx] = ed[1];
              }
            }
          }
        }

        writer.write(String.join("\t", columnArray));

        if (ss == null) {
          writer.write("\t\t");
        } else {
          writer.write("\t");
          writer.write(ss[0]);
          writer.write("\t");
          writer.write(ss[1]);
        }

        if (!precursorModificationLocalizationTable.isEmpty() && modificationArray != null) {
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
        }

        writer.write("\n");
      }
    }

    reader.close();
    writer.close();

    Files.move(p2, p, StandardCopyOption.REPLACE_EXISTING);
  }

  private void editPgMatrix(Path p, Map<String, String[]> proteinIdEntryDescMap) throws Exception {
    Path p2 = p.toAbsolutePath().getParent().resolve("report.pg_matrix2.tsv");

    int proteinIdsColumnIdx = -1;
    int proteinNamesColumnIdx = -1;
    int firstProteinDescriptionColumnIdx = -1;

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
      if (line.startsWith("Protein.Group\t")) {
        for (int i = 0; i < parts.length; ++i) {
          if (parts[i].trim().contentEquals("Protein.Ids")) {
            proteinIdsColumnIdx = i;
          } else if (parts[i].trim().contentEquals("Protein.Names")) {
            proteinNamesColumnIdx = i;
          } else if (parts[i].trim().contentEquals("First.Protein.Description")) {
            firstProteinDescriptionColumnIdx = i;
          }
        }

        columnArray = new String[parts.length];
        System.arraycopy(parts, 0, columnArray, 0, parts.length);

        writer.write(line);
        writer.write("\n");
      } else {
        if (proteinIdsColumnIdx < 0) {
          System.err.printf("Missing %s in %s.%n", "Protein.Ids", p);
          System.exit(1);
        }

        Arrays.fill(columnArray, "");
        System.arraycopy(parts, 0, columnArray, 0, parts.length);

        String proteinId = parts[proteinIdsColumnIdx].trim();
        String[] ed = proteinIdEntryDescMap.get(proteinId);
        if (ed != null) {
          if (proteinNamesColumnIdx >= 0 && !ed[0].isEmpty()) {
            columnArray[proteinNamesColumnIdx] = ed[0];
          }
          if (firstProteinDescriptionColumnIdx >= 0 && !ed[1].isEmpty()) {
            columnArray[firstProteinDescriptionColumnIdx] = ed[1];
          }
        }

        writer.write(String.join("\t", columnArray));
        writer.write("\n");
      }
    }

    reader.close();
    writer.close();

    Files.move(p2, p, StandardCopyOption.REPLACE_EXISTING);
  }

  private static void convertLabelString(String label, String inputStr, Map<String, Float> map) {
    Matcher matcher = labelPattern.matcher(inputStr.trim());
    while (matcher.find()) {
      char[] aas = matcher.group(1).toCharArray();
      float modMass = Float.parseFloat(matcher.group(2));
      for (char aa : aas) {
        if (aa == '*') {
          for (char c = 'A'; c < 'Z'; ++c) {
            map.put("label-" + c + "-" + label, modMass);
          }
          map.put("label-n-" + label, modMass);
          map.put("label-c-" + label, modMass);
        } else {
          map.put("label-" + aa + "-" + label, modMass);
        }
      }
    }
  }
}
