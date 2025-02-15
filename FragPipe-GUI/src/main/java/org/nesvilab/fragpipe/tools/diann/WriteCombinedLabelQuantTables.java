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

import com.google.common.base.Joiner;
import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;
import com.google.common.primitives.Floats;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

class WriteCombinedLabelQuantTables {

  private static final Pattern pattern1 = Pattern.compile("[^A-Z]+");
  private static final double log2 = Math.log(2);

  private final Map<Character, Float> lightAaMassMap;
  private final Map<Character, Float> mediumAaMassMap;
  private final Map<Character, Float> heavyAaMassMap;
  private final Table<String, String, IonPair> ionRunPairTable;
  private final Map<String, String[]> sequenceProteinMap;

  private final Table<String, String, PeptidePair> peptideRunPairTable = HashBasedTable.create();
  private final Table<String, String, PeptidePair> sequenceRunPairTable = HashBasedTable.create();

  WriteCombinedLabelQuantTables(Map<Character, Float> lightAaMassMap, Map<Character, Float> mediumAaMassMap, Map<Character, Float> heavyAaMassMap, Table<String, String, IonPair> ionRunPairTable, Map<String, String[]> sequenceProteinMap) {
    this.lightAaMassMap = lightAaMassMap;
    this.mediumAaMassMap = mediumAaMassMap;
    this.heavyAaMassMap = heavyAaMassMap;
    this.ionRunPairTable = ionRunPairTable;
    this.sequenceProteinMap = sequenceProteinMap;
  }

  void writeCombinedIonLabelQuant(Path outputPath) throws Exception {
    BufferedWriter writer = Files.newBufferedWriter(outputPath);
    writer.write("Peptide Sequence"
        + "\tLabel Free Peptide"
        + "\tPeptide Length"
        + "\tCharge"
        + "\tLabel Count"
        + (lightAaMassMap != null ? "\tLight Modified Peptide" : "")
        + (mediumAaMassMap != null ? "\tMedium Modified Peptide" : "")
        + (heavyAaMassMap != null ? "\tHeavy Modified Peptide" : "")
    );

    if (lightAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Light Intensity");
      }
    }
    if (mediumAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Medium Intensity");
      }
    }
    if (heavyAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Heavy Intensity");
      }
    }

    if (lightAaMassMap != null && mediumAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio ML");
      }
    }
    if (lightAaMassMap != null && heavyAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio HL");
      }
    }
    if (mediumAaMassMap != null && heavyAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio HM");
      }
    }

    if (lightAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Light Apex Retention Time");
      }
    }
    if (mediumAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Medium Apex Retention Time");
      }
    }
    if (heavyAaMassMap != null) {
      for (String run : ionRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Heavy Apex Retention Time");
      }
    }

    writer.write("\tProtein Group\tProtein Ids\tProtein Names\tGenes\n");

    String[] runArray = ionRunPairTable.columnKeySet().toArray(new String[0]);
    for (Map.Entry<String, Map<String, IonPair>> e1 : ionRunPairTable.rowMap().entrySet()) {
      String labelFreeIonID = e1.getKey();
      String labelFreePeptide = IonEntry.getFirstPart(labelFreeIonID);
      int labelCount = IonEntry.getSecondPart(labelFreeIonID);
      int charge = IonEntry.getThirdPart(labelFreeIonID);
      String sequence = stripPeptide(labelFreePeptide);

      writer.write(sequence + "\t" + labelFreePeptide + "\t" + sequence.length() + "\t" + charge + "\t" + labelCount);

      // modified peptides
      if (lightAaMassMap != null) {
        Set<String> tt = new TreeSet<>();
        for (IonPair ionPair : e1.getValue().values()) {
          if (ionPair.lightIonEntry != null) {
            tt.add(ionPair.lightIonEntry.peptide.modifiedPeptide);
          }
        }
        writer.write("\t" + Joiner.on(";").join(tt));
      }
      if (mediumAaMassMap != null) {
        Set<String> tt = new TreeSet<>();
        for (IonPair ionPair : e1.getValue().values()) {
          if (ionPair.mediumIonEntry != null) {
            tt.add(ionPair.mediumIonEntry.peptide.modifiedPeptide);
          }
        }
        writer.write("\t" + Joiner.on(";").join(tt));
      }
      if (heavyAaMassMap != null) {
        Set<String> tt = new TreeSet<>();
        for (IonPair ionPair : e1.getValue().values()) {
          if (ionPair.heavyIonEntry != null) {
            tt.add(ionPair.heavyIonEntry.peptide.modifiedPeptide);
          }
        }
        writer.write("\t" + Joiner.on(";").join(tt));
      }

      // intensity
      if (lightAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.lightIonEntry != null && ionPair.lightIonEntry.intensity > 0) {
            writer.write("\t" + ionPair.lightIonEntry.intensity);
          } else {
            writer.write("\t");
          }
        }
      }
      if (mediumAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.mediumIonEntry != null && ionPair.mediumIonEntry.intensity > 0) {
            writer.write("\t" + ionPair.mediumIonEntry.intensity);
          } else {
            writer.write("\t");
          }
        }
      }
      if (heavyAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.heavyIonEntry != null && ionPair.heavyIonEntry.intensity > 0) {
            writer.write("\t" + ionPair.heavyIonEntry.intensity);
          } else {
            writer.write("\t");
          }
        }
      }

      // log-ratio
      if (lightAaMassMap != null && mediumAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.lightIonEntry != null && ionPair.lightIonEntry.intensity > 0 && ionPair.mediumIonEntry != null && ionPair.mediumIonEntry.intensity > 0) {
            writer.write("\t" + (float) (Math.log(ionPair.mediumIonEntry.intensity / ionPair.lightIonEntry.intensity) / log2));
          } else {
            writer.write("\t");
          }
        }
      }
      if (lightAaMassMap != null && heavyAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.lightIonEntry != null && ionPair.lightIonEntry.intensity > 0 && ionPair.heavyIonEntry != null && ionPair.heavyIonEntry.intensity > 0) {
            writer.write("\t" + (float) (Math.log(ionPair.heavyIonEntry.intensity / ionPair.lightIonEntry.intensity) / log2));
          } else {
            writer.write("\t");
          }
        }
      }
      if (mediumAaMassMap != null && heavyAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.mediumIonEntry != null && ionPair.mediumIonEntry.intensity > 0 && ionPair.heavyIonEntry != null && ionPair.heavyIonEntry.intensity > 0) {
            writer.write("\t" + (float) (Math.log(ionPair.heavyIonEntry.intensity / ionPair.mediumIonEntry.intensity) / log2));
          } else {
            writer.write("\t");
          }
        }
      }

      // apex retention time
      if (lightAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.lightIonEntry != null && ionPair.lightIonEntry.apexRt > 0) {
            writer.write("\t" + ionPair.lightIonEntry.apexRt);
          } else {
            writer.write("\t");
          }
        }
      }
      if (mediumAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.mediumIonEntry != null && ionPair.mediumIonEntry.apexRt > 0) {
            writer.write("\t" + ionPair.mediumIonEntry.apexRt);
          } else {
            writer.write("\t");
          }
        }
      }
      if (heavyAaMassMap != null) {
        for (String run : runArray) {
          IonPair ionPair = e1.getValue().get(run);
          if (ionPair != null && ionPair.heavyIonEntry != null && ionPair.heavyIonEntry.apexRt > 0) {
            writer.write("\t" + ionPair.heavyIonEntry.apexRt);
          } else {
            writer.write("\t");
          }
        }
      }

      String[] proteinArray = sequenceProteinMap.get(sequence);
      writer.write("\t" + proteinArray[0] + "\t" + proteinArray[1] + "\t" + proteinArray[2] + "\t" + proteinArray[3] + "\n");
    }

    writer.close();
  }

  void writeCombinedModifiedPeptideLabelQuant(Path outputPath) throws Exception {
    for (Map.Entry<String, Map<String, IonPair>> e1 : ionRunPairTable.columnMap().entrySet()) {
      String run = e1.getKey();
      for (Map.Entry<String, IonPair> e2 : e1.getValue().entrySet()) {
        String labelFreeIonID = e2.getKey();
        String labelFreePeptide = IonEntry.getFirstPart(labelFreeIonID);
        int labelCount = IonEntry.getSecondPart(labelFreeIonID);
        int charge = IonEntry.getThirdPart(labelFreeIonID);

        String labelFreePeptideID = labelFreePeptide + "@" + labelCount;

        IonPair ionPair = e2.getValue();

        PeptidePair peptidePair = peptideRunPairTable.get(labelFreePeptideID, run);
        if (peptidePair == null) {
          peptidePair = new PeptidePair(
              ionPair.lightIonEntry == null ? null : ionPair.lightIonEntry.peptide.peptideSequence,
              ionPair.mediumIonEntry == null ? null : ionPair.mediumIonEntry.peptide.peptideSequence,
              ionPair.heavyIonEntry == null ? null : ionPair.heavyIonEntry.peptide.peptideSequence
          );
          peptideRunPairTable.put(labelFreePeptideID, run, peptidePair);
        }

        peptidePair.chargeSet.add(charge);

        float lightIntensity = 0, mediumIntensity = 0, heavyIntensity = 0;
        if (ionPair.lightIonEntry != null) {
          lightIntensity = ionPair.lightIonEntry.intensity;
        }
        if (ionPair.mediumIonEntry != null) {
          mediumIntensity = ionPair.mediumIonEntry.intensity;
        }
        if (ionPair.heavyIonEntry != null) {
          heavyIntensity = ionPair.heavyIonEntry.intensity;
        }

        if (lightIntensity > 0 && mediumIntensity > 0) {
          peptidePair.logRatioMLList.add((float) (Math.log(mediumIntensity / lightIntensity) / log2));

        }
        if (lightIntensity > 0 && heavyIntensity > 0) {
          peptidePair.logRatioHLList.add((float) (Math.log(heavyIntensity / lightIntensity) / log2));
        }
        if (mediumIntensity > 0 && heavyIntensity > 0) {
          peptidePair.logRatioHMList.add((float) (Math.log(heavyIntensity / mediumIntensity) / log2));
        }
      }
    }

    BufferedWriter writer = Files.newBufferedWriter(outputPath);
    writer.write("Peptide Sequence"
        + "\tLabel Free Peptide"
        + "\tPeptide Length"
        + "\tCharges"
        + "\tLabel Count"
        + (lightAaMassMap != null ? "\tLight Modified Peptide" : "")
        + (mediumAaMassMap != null ? "\tMedium Modified Peptide" : "")
        + (heavyAaMassMap != null ? "\tHeavy Modified Peptide" : "")
    );

    if (lightAaMassMap != null && mediumAaMassMap != null) {
      for (String run : peptideRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio ML");
      }
    }
    if (lightAaMassMap != null && heavyAaMassMap != null) {
      for (String run : peptideRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio HL");
      }
    }
    if (mediumAaMassMap != null && heavyAaMassMap != null) {
      for (String run : peptideRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio HM");
      }
    }

    writer.write("\tProtein Group\tProtein Ids\tProtein Names\tGenes\n");

    String[] runArray = peptideRunPairTable.columnKeySet().toArray(new String[0]);
    for (Map.Entry<String, Map<String, PeptidePair>> e1 : peptideRunPairTable.rowMap().entrySet()) {
      String labelFreePeptideID = e1.getKey();
      String labelFreePeptide = IonEntry.getFirstPart(labelFreePeptideID);
      int labelCount = IonEntry.getSecondPart(labelFreePeptideID);
      String sequence = stripPeptide(labelFreePeptide);

      Set<Integer> tt = new TreeSet<>();
      for (PeptidePair e2 : e1.getValue().values()) {
        tt.addAll(e2.chargeSet);
      }

      writer.write(sequence + "\t" + labelFreePeptide + "\t" + sequence.length() + "\t" + Joiner.on(";").join(tt) + "\t" + labelCount);

      // modified peptide
      if (lightAaMassMap != null) {
        Set<String> ttt = new TreeSet<>();
        for (PeptidePair peptidePair : e1.getValue().values()) {
          if (peptidePair != null && peptidePair.lightModifiedSequence != null) {
            ttt.add(peptidePair.lightModifiedSequence);
          }
        }
        writer.write("\t" + Joiner.on(";").join(ttt));
      }
      if (mediumAaMassMap != null) {
        Set<String> ttt = new TreeSet<>();
        for (PeptidePair peptidePair : e1.getValue().values()) {
          if (peptidePair != null && peptidePair.mediumModifiedSequence != null) {
            ttt.add(peptidePair.mediumModifiedSequence);
          }
        }
        writer.write("\t" + Joiner.on(";").join(ttt));
      }
      if (heavyAaMassMap != null) {
        Set<String> ttt = new TreeSet<>();
        for (PeptidePair peptidePair : e1.getValue().values()) {
          if (peptidePair != null && peptidePair.heavyModifiedSequence != null) {
            ttt.add(peptidePair.heavyModifiedSequence);
          }
        }
        writer.write("\t" + Joiner.on(";").join(ttt));
      }

      // log2 ratio
      if (lightAaMassMap != null && mediumAaMassMap != null) {
        for (String run : runArray) {
          PeptidePair peptidePair = e1.getValue().get(run);
          if (peptidePair == null || peptidePair.logRatioMLList.isEmpty()) {
            writer.write("\t");
          } else {
            writer.write("\t" + calMedian(peptidePair.logRatioMLList));
          }
        }
      }
      if (lightAaMassMap != null && heavyAaMassMap != null) {
        for (String run : runArray) {
          PeptidePair peptidePair = e1.getValue().get(run);
          if (peptidePair == null || peptidePair.logRatioHLList.isEmpty()) {
            writer.write("\t");
          } else {
            writer.write("\t" + calMedian(peptidePair.logRatioHLList));
          }
        }
      }
      if (mediumAaMassMap != null && heavyAaMassMap != null) {
        for (String run : runArray) {
          PeptidePair peptidePair = e1.getValue().get(run);
          if (peptidePair == null || peptidePair.logRatioHMList.isEmpty()) {
            writer.write("\t");
          } else {
            writer.write("\t" + calMedian(peptidePair.logRatioHMList));
          }
        }
      }

      String[] proteinArray = sequenceProteinMap.get(sequence);
      writer.write("\t" + proteinArray[0] + "\t" + proteinArray[1] + "\t" + proteinArray[2] + "\t" + proteinArray[3] + "\n");
    }

    writer.close();
  }

  void writeCombinedSequenceLabelQuant(Path outputPath) throws Exception {
    for (Map.Entry<String, Map<String, IonPair>> e1 : ionRunPairTable.columnMap().entrySet()) {
      String run = e1.getKey();
      for (Map.Entry<String, IonPair> e2 : e1.getValue().entrySet()) {
        String labelFreeIonID = e2.getKey();
        String labelFreePeptide = IonEntry.getFirstPart(labelFreeIonID);
        int labelCount = IonEntry.getSecondPart(labelFreeIonID);
        int charge = IonEntry.getThirdPart(labelFreeIonID);
        String sequence = stripPeptide(labelFreePeptide);

        String sequenceID = sequence + "@" + labelCount;

        IonPair ionPair = e2.getValue();

        PeptidePair sequencePair = sequenceRunPairTable.get(sequenceID, run);
        if (sequencePair == null) {
          sequencePair = new PeptidePair(null, null, null);
          sequenceRunPairTable.put(sequenceID, run, sequencePair);
        }

        sequencePair.chargeSet.add(charge);

        float lightIntensity = 0, mediumIntensity = 0, heavyIntensity = 0;
        if (ionPair.lightIonEntry != null) {
          lightIntensity = ionPair.lightIonEntry.intensity;
        }
        if (ionPair.mediumIonEntry != null) {
          mediumIntensity = ionPair.mediumIonEntry.intensity;
        }
        if (ionPair.heavyIonEntry != null) {
          heavyIntensity = ionPair.heavyIonEntry.intensity;
        }

        if (lightIntensity > 0 && mediumIntensity > 0) {
          sequencePair.logRatioMLList.add((float) (Math.log(mediumIntensity / lightIntensity) / log2));
        }
        if (lightIntensity > 0 && heavyIntensity > 0) {
          sequencePair.logRatioHLList.add((float) (Math.log(heavyIntensity / lightIntensity) / log2));
        }
        if (mediumIntensity > 0 && heavyIntensity > 0) {
          sequencePair.logRatioHMList.add((float) (Math.log(heavyIntensity / mediumIntensity) / log2));
        }
      }
    }

    BufferedWriter writer = Files.newBufferedWriter(outputPath);
    writer.write("Peptide Sequence"
        + "\tPeptide Length"
        + "\tCharges"
        + "\tLabel Count"
    );

    if (lightAaMassMap != null && mediumAaMassMap != null) {
      for (String run : sequenceRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio ML");
      }
    }
    if (lightAaMassMap != null && heavyAaMassMap != null) {
      for (String run : sequenceRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio HL");
      }
    }
    if (mediumAaMassMap != null && heavyAaMassMap != null) {
      for (String run : sequenceRunPairTable.columnKeySet()) {
        writer.write("\t" + run + " Log2 Ratio HM");
      }
    }

    writer.write("\tProtein Group\tProtein Ids\tProtein Names\tGenes\n");

    String[] runArray = sequenceRunPairTable.columnKeySet().toArray(new String[0]);
    for (Map.Entry<String, Map<String, PeptidePair>> e1 : sequenceRunPairTable.rowMap().entrySet()) {
      String sequenceID = e1.getKey();
      String sequence = IonEntry.getFirstPart(sequenceID);
      int labelCount = IonEntry.getSecondPart(sequenceID);

      Set<Integer> tt = new TreeSet<>();
      for (PeptidePair e2 : e1.getValue().values()) {
        tt.addAll(e2.chargeSet);
      }

      writer.write(sequence + "\t" + sequence.length() + "\t" + Joiner.on(";").join(tt) + "\t" + labelCount);

      // log2 ratio
      if (lightAaMassMap != null && mediumAaMassMap != null) {
        for (String run : runArray) {
          PeptidePair sequencePair = e1.getValue().get(run);
          if (sequencePair == null || sequencePair.logRatioMLList.isEmpty()) {
            writer.write("\t");
          } else {
            writer.write("\t" + calMedian(sequencePair.logRatioMLList));
          }
        }
      }
      if (lightAaMassMap != null && heavyAaMassMap != null) {
        for (String run : runArray) {
          PeptidePair sequencePair = e1.getValue().get(run);
          if (sequencePair == null || sequencePair.logRatioHLList.isEmpty()) {
            writer.write("\t");
          } else {
            writer.write("\t" + calMedian(sequencePair.logRatioHLList));
          }
        }
      }
      if (mediumAaMassMap != null && heavyAaMassMap != null) {
        for (String run : runArray) {
          PeptidePair sequencePair = e1.getValue().get(run);
          if (sequencePair == null || sequencePair.logRatioHMList.isEmpty()) {
            writer.write("\t");
          } else {
            writer.write("\t" + calMedian(sequencePair.logRatioHMList));
          }
        }
      }

      String[] proteinArray = sequenceProteinMap.get(sequence);
      writer.write("\t" + proteinArray[0] + "\t" + proteinArray[1] + "\t" + proteinArray[2] + "\t" + proteinArray[3] + "\n");
    }

    writer.close();
  }

  void writeCombinedProteinLabelQuant(Path outputPath) throws Exception {
    // implement if needed
  }

  private static String stripPeptide(String peptide) {
    return pattern1.matcher(peptide).replaceAll("");
  }

  private static float calMedian(Collection<Float> inputList) {
    if (inputList.size() > 1) {
      float[] inputArray = Floats.toArray(inputList);
      Arrays.sort(inputArray);
      if (inputArray.length % 2 == 0) {
        return (inputArray[inputArray.length / 2] + inputArray[(inputArray.length / 2) - 1]) * 0.5f;
      } else {
        return inputArray[inputArray.length / 2];
      }
    } else {
      return inputList.iterator().next();
    }
  }
}
