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
import java.io.FileReader;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.io.FilenameUtils;


public class DiannToMsstats {

  private static final Pattern pattern = Pattern.compile("([\\w-]+)\\^(\\d+)\\/([\\d.]+)");
  private static final Pattern pattern2 = Pattern.compile("([ncA-Z]+):([\\d.-]+)");

  public static void main(String[] args) {
    Locale.setDefault(Locale.US);
    long startTime = System.nanoTime();

    try {
      String manifestPath = args[7].trim();
      Map<String, String[]> runConditionBioreplicateMap = new HashMap<>();
      String line;
      BufferedReader bufferedReader = new BufferedReader(new FileReader(manifestPath));
      while ((line = bufferedReader.readLine()) != null) {
        line = line.trim();
        String[] split = line.split("\t");
        runConditionBioreplicateMap.put(FilenameUtils.getBaseName(split[0]), new String[]{split[1], split[2]});
      }
      bufferedReader.close();

      new DiannToMsstats(args[0], args[1], args[2], Float.parseFloat(args[3]), Float.parseFloat(args[4]), Float.parseFloat(args[5]), Float.parseFloat(args[6]), runConditionBioreplicateMap);
    } catch (Exception ex) {
      ex.printStackTrace();
      System.exit(1);
    }

    System.out.printf("Done in %.2f seconds.\n", (System.nanoTime() - startTime) * 1e-9);
  }

  public DiannToMsstats(String diannPath, String workdir, String psmPath, float globalProteinFdrT, float runProteinFdrT, float globalPrecursorFdrT, float runPrecursorFdrT, Map<String, String[]> runConditionBioreplicateMap) throws Exception {
    Map<String, int[]> peptideStartEntryMap = new HashMap<>();

    String line;

    int peptideColumn = -1;
    int startColumn = -1;
    int endColumn = -1;
    BufferedReader bufferedReader = new BufferedReader(new FileReader(psmPath));
    while ((line = bufferedReader.readLine()) != null) {
      line = line.trim();
      if (line.isEmpty()) {
        continue;
      }
      if (line.startsWith("Spectrum\tSpectrum File\t")) {
        String[] header = line.split("\t");
        for (int i = 0; i < header.length; ++i) {
          if (header[i].trim().equalsIgnoreCase("Peptide")) {
            peptideColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("Protein Start")) {
            startColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("Protein End")) {
            endColumn = i;
          }
        }
      } else {
        if (peptideColumn == -1 || startColumn == -1 || endColumn == -1) {
          throw new RuntimeException("Could not find all the required columns in the PSM file: " + psmPath);
        }

        String[] split = line.split("\t");
        String peptide = split[peptideColumn].trim();
        int start = Integer.parseInt(split[startColumn]);
        int end = Integer.parseInt(split[endColumn]);
        if (!peptideStartEntryMap.containsKey(peptide)) {
          peptideStartEntryMap.put(peptide, new int[]{start, end});
        }
      }
    }
    bufferedReader.close();

    write(workdir, diannPath, globalProteinFdrT, runProteinFdrT, globalPrecursorFdrT, runPrecursorFdrT, runConditionBioreplicateMap, peptideStartEntryMap, false);
    write(workdir, diannPath, globalProteinFdrT, runProteinFdrT, globalPrecursorFdrT, runPrecursorFdrT, runConditionBioreplicateMap, peptideStartEntryMap, true);
  }

  private void write(String workdir, String diannPath, float globalProteinFdrT, float runProteinFdrT, float globalPrecursorFdrT, float runPrecursorFdrT, Map<String, String[]> runConditionBioreplicateMap, Map<String, int[]> peptideStartEntryMap, boolean isMSstatsPTM) throws Exception {
    String line;
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(workdir + "/msstats" + (isMSstatsPTM ? "_ptm" : "") + ".csv"));
    bufferedWriter.write("ProteinName,PeptideSequence,Protein.Start,Protein.End,PrecursorCharge,FragmentIon,ProductCharge,IsotopeLabelType,Condition,BioReplicate,Run,Intensity");

    int runColumn = -1;
    int proteinGroupColumn = -1;
    int strippedSequenceColumn = -1;
    int modifiedSequenceColumn = -1;
    int precursorChargeColumn = -1;
    int qValueColumn = -1;
    int globalQValueColumn = -1;
    int pgQValueColumn = -1;
    int globalPgQValueColumn = -1;
    int fragmentQuantRawColumn = -1;
    int fragmentInfoColumn = -1;
    Map<String, Integer> modificationColumnIdxMap = new TreeMap<>();

    BufferedReader bufferedReader = new BufferedReader(new FileReader(diannPath));
    while ((line = bufferedReader.readLine()) != null) {
      line = line.trim();
      if (line.contains("Protein.Group") && line.contains("Protein.Ids")) {
        String[] header = line.split("\t");
        for (int i = 0; i < header.length; ++i) {
          if (header[i].trim().equalsIgnoreCase("run")) {
            runColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("protein.group")) {
            proteinGroupColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("stripped.sequence")) {
            strippedSequenceColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("modified.sequence")) {
            modifiedSequenceColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("precursor.charge")) {
            precursorChargeColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("q.value")) {
            qValueColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("global.q.value")) {
            globalQValueColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("pg.q.value")) {
            pgQValueColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("global.pg.q.value")) {
            globalPgQValueColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("fragment.quant.raw")) {
            fragmentQuantRawColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("fragment.info")) {
            fragmentInfoColumn = i;
          } else if (isMSstatsPTM) {
            Matcher matcher = pattern2.matcher(header[i].trim());
            if (matcher.matches()) {
              modificationColumnIdxMap.put(header[i].trim(), i);
            }
          }
        }

        if (!modificationColumnIdxMap.isEmpty()) {
          for (Map.Entry<String, Integer> entry : modificationColumnIdxMap.entrySet()) {
            bufferedWriter.write("," + entry.getKey());
          }
        }

        bufferedWriter.write("\n");
      } else {
        if (runColumn == -1 ||
            proteinGroupColumn == -1 ||
            strippedSequenceColumn == -1 ||
            modifiedSequenceColumn == -1 ||
            precursorChargeColumn == -1 ||
            qValueColumn == -1 ||
            globalQValueColumn == -1 ||
            pgQValueColumn == -1 ||
            globalPgQValueColumn == -1 ||
            fragmentQuantRawColumn == -1 ||
            fragmentInfoColumn == -1) {
          throw new RuntimeException("Could not find all the required columns in the DIA-NN output file: " + diannPath);
        }

        String[] row = line.split("\t");
        if (Float.parseFloat(row[qValueColumn]) < runPrecursorFdrT &&
            Float.parseFloat(row[globalQValueColumn]) < globalPrecursorFdrT &&
            Float.parseFloat(row[pgQValueColumn]) < runProteinFdrT &&
            Float.parseFloat(row[globalPgQValueColumn]) < globalProteinFdrT) {
          String run = row[runColumn].trim();

          String[] conditionBioreplicate = runConditionBioreplicateMap.get(run);
          if (conditionBioreplicate == null) {
            if (run.endsWith("_uncalibrated")) {
              run = run.substring(0, run.indexOf("_uncalibrated"));
              conditionBioreplicate = runConditionBioreplicateMap.get(run);
              if (conditionBioreplicate == null) {
                throw new RuntimeException("Could not find the run in the PSM file (tried with and without _uncalibrated): " + run);
              }
            } else {
              throw new RuntimeException("Could not find the run in the PSM file: " + run);
            }
          }

          String fragmentInfo = row[fragmentInfoColumn].trim();
          String[] fragmentInfoSplit = fragmentInfo.split(";");
          String fragmentIntensity = row[fragmentQuantRawColumn].trim();
          String[] fragmentIntensitySplit = fragmentIntensity.split(";");

          if (fragmentInfoSplit.length != fragmentIntensitySplit.length) {
            throw new RuntimeException("There are different number of fragment quant and fragment info: " + fragmentInfo + " vs " + fragmentIntensity);
          }

          int[] startEnd = peptideStartEntryMap.get(row[strippedSequenceColumn].trim());

          if (startEnd == null) {
            throw new RuntimeException("Could not find the peptide in the PSM file: " + row[strippedSequenceColumn].trim());
          }

          for (int i = 0; i < fragmentInfoSplit.length; ++i) {
            if (Math.abs(Float.parseFloat(fragmentIntensitySplit[i])) < 0.0001f) {
              continue;
            }
            Matcher matcher = pattern.matcher(fragmentInfoSplit[i].trim());
            if (matcher.matches()) {
              bufferedWriter.write(row[proteinGroupColumn].trim() + "," +
                  row[modifiedSequenceColumn].trim() + "," +
                  startEnd[0] + "," +
                  startEnd[1] + "," +
                  row[precursorChargeColumn].trim() + "," +
                  matcher.group(1).replace("-unknown", "") + "," +
                  matcher.group(2) + ",L," + // todo: support plexDIA
                  conditionBioreplicate[0] + "," +
                  conditionBioreplicate[1] + "," +
                  run + "," +
                  fragmentIntensitySplit[i]);

              if (!modificationColumnIdxMap.isEmpty()) {
                for (Map.Entry<String, Integer> entry : modificationColumnIdxMap.entrySet()) {
                  int idx = entry.getValue();
                  bufferedWriter.write(",");
                  if (idx < row.length && row[idx] != null && !row[idx].trim().isEmpty()) {
                    bufferedWriter.write(row[entry.getValue()].trim());
                  } else {
                    bufferedWriter.write("NA");
                  }
                }
              }

              bufferedWriter.write("\n");
            } else {
              throw new RuntimeException("Could not parse fragment info: " + fragmentInfoSplit[i]);
            }
          }
        }
      }
    }
    bufferedReader.close();
    bufferedWriter.close();
  }
}
