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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.io.FilenameUtils;


public class DiannToMsstats {

  private static final Pattern pattern = Pattern.compile("([\\w-]+)\\^(\\d+)\\/([\\d.]+)");

  public static void main(String[] args) {
    try {
      String manifestPath = args[6].trim();
      Map<String, String[]> runConditionBioreplicateMap = new HashMap<>();
      String line;
      BufferedReader bufferedReader = new BufferedReader(new FileReader(manifestPath));
      while ((line = bufferedReader.readLine()) != null) {
        line = line.trim();
        String[] split = line.split("\t");
        runConditionBioreplicateMap.put(FilenameUtils.getBaseName(split[0]), new String[]{split[1], split[2]});
      }
      bufferedReader.close();

      new DiannToMsstats(args[0], args[1], Float.parseFloat(args[2]), Float.parseFloat(args[3]), Float.parseFloat(args[4]), Float.parseFloat(args[5]), runConditionBioreplicateMap);
    } catch (Exception ex) {
      ex.printStackTrace();
      System.exit(1);
    }
  }

  public DiannToMsstats(String diannPath, String msstatsPath, float globalProteinFdrT, float runProteinFdrT, float globalPrecursorFdrT, float runPrecursorFdrT, Map<String, String[]> runConditionBioreplicateMap) throws Exception {
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(msstatsPath));
    bufferedWriter.write("ProteinName,PeptideSequence,PrecursorCharge,FragmentIon,ProductCharge,IsotopeLabelType,Condition,BioReplicate,Run,Intensity\n");

    String line;
    int runColumn = -1;
    int proteinGroupColumn = -1;
    int modifiedSequenceColumn = -1;
    int precursorChargeColumn = -1;
    int qValueColumn = -1;
    int globalQValueColumn = -1;
    int pgQValueColumn = -1;
    int globalPgQValueColumn = -1;
    int fragmentQuantCorrectedColumn = -1;
    int fragmentInfoColumn = -1;
    BufferedReader bufferedReader = new BufferedReader(new FileReader(diannPath));
    while ((line = bufferedReader.readLine()) != null) {
      line = line.trim();
      if (line.startsWith("File.Name\t")) {
        String[] header = line.split("\t");
        for (int i = 0; i < header.length; ++i) {
          if (header[i].trim().equalsIgnoreCase("run")) {
            runColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("protein.group")) {
            proteinGroupColumn = i;
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
          } else if (header[i].trim().equalsIgnoreCase("fragment.quant.corrected")) {
            fragmentQuantCorrectedColumn = i;
          } else if (header[i].trim().equalsIgnoreCase("fragment.info")) {
            fragmentInfoColumn = i;
          }
        }
      } else {
        if (runColumn == -1 ||
            proteinGroupColumn == -1 ||
            modifiedSequenceColumn == -1 ||
            precursorChargeColumn == -1 ||
            qValueColumn == -1 ||
            globalQValueColumn == -1 ||
            pgQValueColumn == -1 ||
            globalPgQValueColumn == -1 ||
            fragmentQuantCorrectedColumn == -1 ||
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
          String fragmentInfo = row[fragmentInfoColumn].trim();
          String[] fragmentInfoSplit = fragmentInfo.split(";");
          String fragmentIntensity = row[fragmentQuantCorrectedColumn].trim();
          String[] fragmentIntensitySplit = fragmentIntensity.split(";");

          if (fragmentInfoSplit.length != fragmentIntensitySplit.length) {
            throw new RuntimeException("There are different number of fragment quant and fragment info: " + fragmentInfo + " vs " + fragmentIntensity);
          }

          for (int i = 0; i < fragmentInfoSplit.length; ++i) {
            if (Math.abs(Float.parseFloat(fragmentIntensitySplit[i])) < 0.0001f) {
              continue;
            }
            Matcher matcher = pattern.matcher(fragmentInfoSplit[i].trim());
            if (matcher.matches()) {
              bufferedWriter.write(row[proteinGroupColumn].trim() + "," +
                  row[modifiedSequenceColumn].trim() + "," +
                  row[precursorChargeColumn].trim() + "," +
                  matcher.group(1) + "," +
                  matcher.group(2) + ",L," +
                  conditionBioreplicate[0] + "," +
                  conditionBioreplicate[1] + "," +
                  run + "," +
                  fragmentIntensitySplit[i] + "\n");
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
