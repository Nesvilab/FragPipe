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

import static com.dmtavt.fragpipe.util.Utils.AAMasses;
import static com.dmtavt.fragpipe.util.Utils.correctModMass;
import static com.dmtavt.fragpipe.util.Utils.removeClosedModifications;
import static com.dmtavt.fragpipe.util.Utils.threshold;

import com.dmtavt.fragpipe.util.UnimodOboReader;
import com.github.chhh.utils.StringUtils;
import com.google.common.collect.ComparisonChain;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Table;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class PreparePlexLibrary {

  private static final Pattern aaPattern = Pattern.compile("([A-Zn])(\\((UniMod:\\d+)\\))?(\\[([\\d+.-]+)\\])?"); // EasyPQP does not support C-term mods?
  static final Pattern tabPattern = Pattern.compile("\\t");

  private final Path libraryPath;
  private final int nThreads;
  private final Map<Character, Float> lightAaMassMap;
  private final Map<Character, Float> mediumAaMassMap;
  private final Map<Character, Float> heavyAaMassMap;
  private final Map<String, Float> unimodMassMap;
  private final Table<Float, Character, Integer> massSiteUnimodTable;

  float[] theoModMasses;

  public PreparePlexLibrary(int nThreads, Map<Character, Float> lightAaMassMap, Map<Character, Float> mediumAaMassMap, Map<Character, Float> heavyAaMassMap, Path libraryPath) throws Exception {
    this.libraryPath = libraryPath;
    this.nThreads = nThreads;
    this.lightAaMassMap = lightAaMassMap;
    this.mediumAaMassMap = mediumAaMassMap;
    this.heavyAaMassMap = heavyAaMassMap;

    Path unimodPath = Paths.get(Objects.requireNonNull(PreparePlexLibrary.class.getResource("/unimod.obo")).toURI());
    UnimodOboReader unimodOboReader = new UnimodOboReader(unimodPath);
    unimodMassMap = unimodOboReader.unimodMassMap;
    massSiteUnimodTable = unimodOboReader.massSiteUnimodTable;
  }

  public void generateNewLibrary(Path outputPath) throws Exception {
    BufferedReader reader = Files.newBufferedReader(libraryPath);

    ForkJoinPool forkJoinPool = new ForkJoinPool(nThreads);
    List<String[]> library = forkJoinPool.submit(() ->
        reader.lines()
            .parallel()
            .filter(l -> !l.isEmpty())
            .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
            .collect(Collectors.toList())
    ).get();
    forkJoinPool.shutdown();

    reader.close();

    Map<String, Integer> columnNameToIndex = getColumnIndexMap(library.get(0));

    int columnIdx = columnNameToIndex.get("ModifiedPeptideSequence");
    Set<String> modifiedPeptides = library.stream().skip(1).map(p -> "n" + p[columnIdx]).collect(Collectors.toSet());

    Set<Float> modMasses = collectAllMods(modifiedPeptides);

    if (lightAaMassMap != null) {
      modMasses.addAll(lightAaMassMap.values());
    }
    if (mediumAaMassMap != null) {
      modMasses.addAll(mediumAaMassMap.values());
    }
    if (heavyAaMassMap != null) {
      modMasses.addAll(heavyAaMassMap.values());
    }

    theoModMasses = removeClosedModifications(modMasses);

    if (lightAaMassMap != null) {
      Map<Character, Float> t = new HashMap<>();
      for (Map.Entry<Character, Float> e : lightAaMassMap.entrySet()) {
        t.put(e.getKey(), correctModMass(e.getValue(), theoModMasses));
      }
      lightAaMassMap.putAll(t);
    }

    if (mediumAaMassMap != null) {
      Map<Character, Float> t = new HashMap<>();
      for (Map.Entry<Character, Float> e : mediumAaMassMap.entrySet()) {
        t.put(e.getKey(), correctModMass(e.getValue(), theoModMasses));
      }
      mediumAaMassMap.putAll(t);
    }

    if (heavyAaMassMap != null) {
      Map<Character, Float> t = new HashMap<>();
      for (Map.Entry<Character, Float> e : heavyAaMassMap.entrySet()) {
        t.put(e.getKey(), correctModMass(e.getValue(), theoModMasses));
      }
      heavyAaMassMap.putAll(t);
    }

    Multimap<String, Transaction> transactions = collectTransactions(library, columnNameToIndex);
    appendComplementTransactions(transactions);

    if (outputPath == null) {
      outputPath = Paths.get(StringUtils.upToLastDot(libraryPath.toAbsolutePath().toString()) + "_plex.tsv");
    }
    writeLibrary(transactions, outputPath);
  }

  private Map<String, Integer> getColumnIndexMap(String[] header) {
    if(!header[0].contentEquals("PrecursorMz")) {
      throw new RuntimeException("The library file " + libraryPath.toAbsolutePath() + " does not start with PrecursorMz");
    }

    Map<String, Integer> columnNameToIndex = new HashMap<>();
    for (int i = 0; i < header.length; i++) {
      columnNameToIndex.put(header[i], i);
    }
    return columnNameToIndex;
  }

  private Set<Float> collectAllMods(Set<String> modifiedPeptides) throws Exception {
    ForkJoinPool forkJoinPool = new ForkJoinPool(nThreads);
    Set<String> mods =  forkJoinPool.submit(() ->
        modifiedPeptides.stream().parallel().flatMap(s -> {
          Set<String> ttt = new HashSet<>();
          Matcher aaMatcher = aaPattern.matcher(s);
          while (aaMatcher.find()) {
            if (aaMatcher.group(2) != null || aaMatcher.group(4) != null) {
              ttt.add(aaMatcher.group());
            }
          }
          return ttt.stream();
        })
        .collect(Collectors.toSet())
    ).get();
    forkJoinPool.shutdown();

    Set<Float> modMasses = new HashSet<>();
    for (String mod : mods) {
      Matcher aaMatcher = aaPattern.matcher(mod);
      if (aaMatcher.find()) {
        if (aaMatcher.group(2) != null) {
          float mass = unimodMassMap.get(aaMatcher.group(3).toLowerCase());
          modMasses.add(mass);
        } else if (aaMatcher.group(4) != null) {
          char aa = aaMatcher.group(1).charAt(0);
          float mass;
          if (aa == 'n' || aa == 'c') {
            mass = Float.parseFloat(aaMatcher.group(5));
          } else {
            mass = Float.parseFloat(aaMatcher.group(5)) - AAMasses[aa - 'A'];
          }
          modMasses.add(mass);
        }
      } else {
        throw new RuntimeException("Could not parse modification " + mod);
      }
    }

    return modMasses;
  }

  private String correctModifiedPeptide(String peptide) {
    StringBuilder sb = new StringBuilder();
    Matcher matcher = aaPattern.matcher(peptide);
    while (matcher.find()) {
      char aa = matcher.group(1).charAt(0);
      sb.append(aa);
      if (matcher.group(2) != null) {
        sb.append("[").append(correctModMass(unimodMassMap.get(matcher.group(3).toLowerCase()), theoModMasses)).append("]");
      } else if (matcher.group(4) != null) {
        if (aa == 'n' || aa == 'c') {
          sb.append("[").append(correctModMass(Float.parseFloat(matcher.group(5)), theoModMasses)).append("]");
        } else {
          sb.append("[").append(correctModMass(Float.parseFloat(matcher.group(5)) - AAMasses[aa - 'A'], theoModMasses)).append("]");
        }
      }
    }
    return sb.toString();
  }

  private Multimap<String, Transaction> collectTransactions(List<String[]> library, Map<String, Integer> columnNameToIndex) throws Exception {
    ForkJoinPool forkJoinPool = new ForkJoinPool(nThreads);
    Map<String, List<String[]>> transactionFragmentMap = forkJoinPool.submit(() ->
        library.stream()
            .skip(1)
            .parallel()
            .collect(Collectors.groupingBy(p ->
                p[columnNameToIndex.get("PrecursorMz")] + "-" +
                correctModifiedPeptide("n" + p[columnNameToIndex.get("ModifiedPeptideSequence")]) + "-" +
                p[columnNameToIndex.get("PrecursorCharge")] + "-" +
                p[columnNameToIndex.get("NormalizedRetentionTime")] + "-" +
                p[columnNameToIndex.get("PrecursorIonMobility")]))
    ).get();
    forkJoinPool.shutdown();

    List[] ttArray = transactionFragmentMap.values().toArray(new List[0]);
    ExecutorService executorService = Executors.newFixedThreadPool(nThreads);
    int multi = Math.min(nThreads * 8, ttArray.length);
    List<Future<Multimap<String, Transaction>>> futures = new ArrayList<>(multi);
    for (int i = 0; i < multi; ++i) {
      final int currentThread = i;
      futures.add(executorService.submit(() -> {
        Multimap<String, Transaction> localMap = HashMultimap.create();
        int start = (int) ((currentThread * ((long) ttArray.length)) / multi);
        int end = (int) (((currentThread + 1) * ((long) ttArray.length)) / multi);
        for (int j = start; j < end; ++j) {
          @SuppressWarnings("unchecked") List<String[]> tt = ttArray[j];
          if (tt.isEmpty()) {
            continue;
          }
          Map<Float, Fragment> mzFragmentMap = new TreeMap<>();
          for (String[] ss : tt) {
            Fragment fragment = new Fragment(
                Float.parseFloat(ss[columnNameToIndex.get("ProductMz")]),
                Float.parseFloat(ss[columnNameToIndex.get("LibraryIntensity")]),
                ss[columnNameToIndex.get("FragmentType")].charAt(0),
                Byte.parseByte(ss[columnNameToIndex.get("FragmentCharge")]),
                Integer.parseInt(ss[columnNameToIndex.get("FragmentSeriesNumber")]),
                ss[columnNameToIndex.get("FragmentLossType")]
            );
            Fragment oldFragment = mzFragmentMap.get(fragment.mz);
            if (oldFragment == null || oldFragment.intensity < fragment.intensity) {
              mzFragmentMap.put(fragment.mz, fragment);
            }
          }
          String[] ss = tt.get(0);
          Transaction transaction = new Transaction(
              Float.parseFloat(ss[columnNameToIndex.get("PrecursorMz")]),
              mzFragmentMap.values().toArray(new Fragment[0]),
              ss[columnNameToIndex.get("ProteinId")],
              ss[columnNameToIndex.get("GeneName")],
              new Peptide("n" + ss[columnNameToIndex.get("ModifiedPeptideSequence")], ss[columnNameToIndex.get("PeptideSequence")].length() + 1),
              Byte.parseByte(ss[columnNameToIndex.get("PrecursorCharge")]),
              myToFloat(ss[columnNameToIndex.get("NormalizedRetentionTime")], 0),
              myToFloat(ss[columnNameToIndex.get("PrecursorIonMobility")], 0),
              myToFloat(ss[columnNameToIndex.get("AverageExperimentalRetentionTime")], 0)
          );
          localMap.put(transaction.peptide.modifiedPeptide + transaction.peptideCharge, transaction);
        }
        return localMap;
      }));
    }

    Multimap<String, Transaction> transactions = HashMultimap.create();
    for (Future<Multimap<String, Transaction>> future : futures) {
      transactions.putAll(future.get());
    }

    executorService.shutdown();
    if (!executorService.awaitTermination(10, TimeUnit.SECONDS)) {
      executorService.shutdownNow();
      if (!executorService.awaitTermination(10, TimeUnit.SECONDS)) {
        throw new InterruptedException("Thread pool did not terminate normally.");
      }
    }

    return transactions;
  }

  private void appendComplementTransactions(Multimap<String, Transaction> transactions) {
    Multimap<String, Transaction> complementaryTransactions = HashMultimap.create();
    for (Map.Entry<String, Transaction> e : transactions.entries()) {
      Transaction transaction1 = e.getValue();
      Peptide peptide1 = transaction1.peptide;
      int labelType = peptide1.detectLabelTypes();
      if (labelType == 1) {
        if (mediumAaMassMap != null && lightAaMassMap != null) {
          sub(transactions, peptide1, transaction1, complementaryTransactions, lightAaMassMap, mediumAaMassMap);
        }
        if (heavyAaMassMap != null && lightAaMassMap != null) {
          sub(transactions, peptide1, transaction1, complementaryTransactions, lightAaMassMap, heavyAaMassMap);
        }
      } else if (labelType == 2) {
        if (mediumAaMassMap != null && lightAaMassMap != null) {
          sub(transactions, peptide1, transaction1, complementaryTransactions, mediumAaMassMap, lightAaMassMap);
        }
        if (mediumAaMassMap != null && heavyAaMassMap != null) {
          sub(transactions, peptide1, transaction1, complementaryTransactions, mediumAaMassMap, heavyAaMassMap);
        }
      } else if (labelType == 3) {
        if (heavyAaMassMap != null && lightAaMassMap != null) {
          sub(transactions, peptide1, transaction1, complementaryTransactions, heavyAaMassMap, lightAaMassMap);
        }
        if (heavyAaMassMap != null && mediumAaMassMap != null) {
          sub(transactions, peptide1, transaction1, complementaryTransactions, heavyAaMassMap, mediumAaMassMap);
        }
      }
    }
    transactions.putAll(complementaryTransactions);
  }

  private static void sub(Multimap<String, Transaction> transactions, Peptide peptide1, Transaction transaction1, Multimap<String, Transaction> complementaryTransactions, Map<Character, Float> aaMassMap1, Map<Character, Float> aaMassMap2) {
    Peptide peptide2 = peptide1.getComplementaryPeptide(aaMassMap1, aaMassMap2);

    Collection<Transaction> tt = transactions.get(peptide2.modifiedPeptide + transaction1.peptideCharge);
    if (tt.isEmpty()) {
      Transaction transaction2 = getComplementaryTransaction(transaction1, peptide1, peptide2);
      complementaryTransactions.put(peptide2.modifiedPeptide + transaction1.peptideCharge, transaction2);
    }
  }

  private static Transaction getComplementaryTransaction(Transaction transaction1, Peptide peptide1, Peptide peptide2) {
    float[] modMasses1 = peptide1.modMasses;
    float[] modMasses2 = peptide2.modMasses;
    float[] massDiffArray = new float[modMasses1.length];
    for (int i = 0; i < modMasses1.length; ++i) {
      massDiffArray[i] = modMasses2[i] - modMasses1[i];
    }

    float precursorMz2 = transaction1.precursorMz;
    for (float deltaMass : massDiffArray) {
      precursorMz2 += deltaMass / transaction1.peptideCharge;
    }

    float[] bIonMassDiffArray = new float[massDiffArray.length];
    bIonMassDiffArray[1] = massDiffArray[0]; // add N-term mod
    for (int i = 1; i < bIonMassDiffArray.length; ++i) {
      bIonMassDiffArray[i] += bIonMassDiffArray[i - 1] + massDiffArray[i];
    }

    float[] yIonMassDiffArray = new float[massDiffArray.length];
    yIonMassDiffArray[yIonMassDiffArray.length - 1] = massDiffArray[0]; // add N-term mod
    for (int i = 1; i < yIonMassDiffArray.length; ++i) {
      yIonMassDiffArray[i] += yIonMassDiffArray[i - 1] + massDiffArray[massDiffArray.length - i];
    }

    // Use a float[] instead of a Map for fragments2
    Fragment[] fragments2 = new Fragment[transaction1.fragments.length];
    int index = 0;
    for (Fragment fragment1 : transaction1.fragments) {
      float mz2 = fragment1.mz;

      if (fragment1.type == 'a' || fragment1.type == 'b' || fragment1.type == 'c') {
        mz2 += bIonMassDiffArray[fragment1.ordinal] / fragment1.charge;
      } else {
        mz2 += yIonMassDiffArray[fragment1.ordinal] / fragment1.charge;
      }
      fragments2[index++] = new Fragment(mz2, fragment1.intensity, fragment1.type, fragment1.charge, fragment1.ordinal, fragment1.lossType);
    }

    return new Transaction(precursorMz2, fragments2, transaction1.proteinId, transaction1.geneName, peptide2, transaction1.peptideCharge, transaction1.normalizedRetentionTime, transaction1.precursorIonMobility, transaction1.averageExperimentRetentionTime);
  }

  private static void writeLibrary(Multimap<String, Transaction> transactions, Path outputPath) throws Exception {
    BufferedWriter writer = Files.newBufferedWriter(outputPath);
    writer.write("PrecursorMz\t"
        + "ProductMz\t"
        + "Annotation\t"
        + "ProteinId\t"
        + "GeneName\t"
        + "PeptideSequence\t"
        + "ModifiedPeptideSequence\t"
        + "PrecursorCharge\t"
        + "LibraryIntensity\t"
        + "NormalizedRetentionTime\t"
        + "PrecursorIonMobility\t"
        + "FragmentType\t"
        + "FragmentCharge\t"
        + "FragmentSeriesNumber\t"
        + "FragmentLossType\t"
        + "AverageExperimentalRetentionTime\n");

    List<Transaction> allTransactions = new ArrayList<>(transactions.values());
    allTransactions.sort(Comparator.naturalOrder());

    // Create a single StringBuilder instance
    StringBuilder sb = new StringBuilder();

    for (Transaction transaction : allTransactions) {
      String peptideSequence = transaction.peptide.peptideSequence;
      String unimodPeptide = transaction.peptide.getUnimodPeptide();

      for (Fragment fragment : transaction.fragments) {
        // Reset the StringBuilder
        sb.setLength(0);

        // Use the StringBuilder to build the output line
        sb.append(transaction.precursorMz).append("\t")
            .append(fragment.mz).append("\t")
            .append(fragment).append("\t")
            .append(transaction.proteinId).append("\t")
            .append(transaction.geneName).append("\t")
            .append(peptideSequence).append("\t")
            .append(unimodPeptide).append("\t")
            .append(transaction.peptideCharge).append("\t")
            .append(fragment.intensity).append("\t")
            .append(transaction.normalizedRetentionTime).append("\t")
            .append(Math.abs(transaction.precursorIonMobility) > threshold ? transaction.precursorIonMobility : "").append("\t")
            .append(fragment.type).append("\t")
            .append(fragment.charge).append("\t")
            .append(fragment.ordinal).append("\t")
            .append(fragment.lossType).append("\t")
            .append(transaction.averageExperimentRetentionTime).append("\n");

        // Write the output line
        writer.write(sb.toString());
      }
    }

    writer.close();
  }


  private static float myToFloat(String s, float defaultValue) {
    try {
      return Float.parseFloat(s);
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }


  private static class Transaction implements Comparable<Transaction> {

    final float precursorMz;
    final Fragment[] fragments;
    final String proteinId;
    final String geneName;
    final Peptide peptide;
    final byte peptideCharge;
    final float normalizedRetentionTime;
    final float precursorIonMobility;
    final float averageExperimentRetentionTime;

    public Transaction(float precursorMz, Fragment[] fragments, String proteinId, String geneName, Peptide peptide, byte peptideCharge, float normalizedRetentionTime, float precursorIonMobility, float averageExperimentRetentionTime) {
      this.precursorMz = precursorMz;
      this.fragments = fragments;
      this.proteinId = proteinId;
      this.geneName = geneName;
      this.peptide = peptide;
      this.peptideCharge = peptideCharge;
      this.normalizedRetentionTime = normalizedRetentionTime;
      this.precursorIonMobility = precursorIonMobility;
      this.averageExperimentRetentionTime = averageExperimentRetentionTime;
    }

    @Override
    public int compareTo(Transaction o) {
      return ComparisonChain.start()
          .compare(precursorMz, o.precursorMz)
          .compare(peptide, o.peptide)
          .compare(peptideCharge, o.peptideCharge)
          .compare(normalizedRetentionTime, o.normalizedRetentionTime)
          .compare(precursorIonMobility, o.precursorIonMobility)
          .result();
    }

    @Override
    public boolean equals(Object o) {
      if (o instanceof Transaction) {
        return compareTo((Transaction) o) == 0;
      } else {
        return false;
      }
    }

    @Override
    public String toString() {
      return peptide + "-" + peptideCharge + "-" + normalizedRetentionTime + "-" + precursorIonMobility + "-" + precursorMz;
    }

    @Override
    public int hashCode() {
      return toString().hashCode();
    }
  }


  class Peptide implements Comparable<Peptide> {

    final String modifiedPeptide; // with "n" as the first amino acid
    final String peptideSequence; // with "n" as the first amino acid
    final int peptideLength; // Amino acid count plus "n"
    final float[] modMasses;

    Integer labelType = null;

    public Peptide(String inputString, int peptideLength) {
      this.peptideLength = peptideLength;
      modMasses = new float[peptideLength];

      StringBuilder sb1 = new StringBuilder();
      StringBuilder sb2 = new StringBuilder(peptideLength);
      Matcher matcher = aaPattern.matcher(inputString);
      int idx = 0;
      while (matcher.find()) {
        char aa = matcher.group(1).charAt(0);
        sb1.append(aa);
        sb2.append(aa);
        if (matcher.group(2) != null) {
          modMasses[idx] = correctModMass(unimodMassMap.get(matcher.group(3).toLowerCase()), theoModMasses);
          sb1.append("[").append(modMasses[idx]).append("]");
        } else if (matcher.group(4) != null) {
          if (aa == 'n' || aa == 'c') {
            modMasses[idx] = correctModMass(Float.parseFloat(matcher.group(5)), theoModMasses);
          } else {
            modMasses[idx] = correctModMass(Float.parseFloat(matcher.group(5)) - AAMasses[aa - 'A'], theoModMasses);
          }
          sb1.append("[").append(modMasses[idx]).append("]");
        }
        ++idx;
      }

      this.modifiedPeptide = sb1.toString();
      peptideSequence = sb2.toString();
    }

    public Peptide(String peptideSequence, float[] modMasses) {
      this.peptideSequence = peptideSequence;
      peptideLength = modMasses.length;
      this.modMasses = modMasses;

      char[] aaArray = peptideSequence.toCharArray();
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < aaArray.length; ++i) {
        if (Math.abs(modMasses[i]) > threshold) {
          sb.append(aaArray[i]).append("[").append(modMasses[i]).append("]");
        } else {
          sb.append(aaArray[i]);
        }
      }
      modifiedPeptide = sb.toString();
    }

    public Peptide getComplementaryPeptide(Map<Character, Float> aaMassMap1, Map<Character, Float> aaMassMap2) {
      char[] aaArray = peptideSequence.toCharArray();
      float[] modMasses2 = Arrays.copyOf(modMasses, modMasses.length);

      for (int i = 0; i < aaArray.length; i++) {
        Float mass1 = aaMassMap1.get(aaArray[i]);
        if (mass1 != null && Math.abs(modMasses[i] - mass1) < threshold) {
          modMasses2[i] = aaMassMap2.get(aaArray[i]);
        }
      }

      return new Peptide(peptideSequence, modMasses2);
    }

    public String getUnimodPeptide() {
      StringBuilder sb = new StringBuilder();
      char[] aaArray = peptideSequence.toCharArray();
      for (int i = 0; i < aaArray.length; ++i) {
        if (i > 0) {
          sb.append(aaArray[i]);
        }
        if (Math.abs(modMasses[i]) > 0) {
          final int ii = i;
          Set<Float> massSet = massSiteUnimodTable.rowKeySet().stream()
              .filter(mass -> Math.abs(modMasses[ii] - mass) < threshold)
              .filter(mass -> massSiteUnimodTable.contains(mass, aaArray[ii]))
              .collect(Collectors.toSet());

          if (massSet.isEmpty()) {
            if (i == 0) {
              sb.append("[").append(modMasses[i]).append("]");
            } else {
              sb.append("[").append(modMasses[i] + AAMasses[aaArray[i] - 'A']).append("]");
            }
          } else {
            float gap = Float.MAX_VALUE;
            Float selectedKey = null;
            for (Float v : massSet) {
              if (Math.abs(modMasses[i] - v) < gap) {
                gap = Math.abs(modMasses[i] - v);
                selectedKey = v;
              }
            }
            sb.append("(").append("UniMod:").append(massSiteUnimodTable.get(selectedKey, aaArray[i])).append(")");
          }
        }
      }
      return sb.toString();
    }

    public Integer detectLabelTypes() { // 0: no labels or multiple labels, 1: light, 2: medium, 3: heavy
      if (labelType == null) {
        int labelFlags = 0;
        char[] aaArray = peptideSequence.toCharArray();
        for (int i = 0; i < aaArray.length; ++i) {
          char aa = aaArray[i];
          Float lightValue = lightAaMassMap != null ? lightAaMassMap.get(aa) : null;
          Float mediumValue = mediumAaMassMap != null ? mediumAaMassMap.get(aa) : null;
          Float heavyValue = heavyAaMassMap != null ? heavyAaMassMap.get(aa) : null;

          if (lightValue != null && Math.abs(modMasses[i] - lightValue) < threshold) {
            labelFlags |= 1;
          } else if (mediumValue != null && Math.abs(modMasses[i] - mediumValue) < threshold) {
            labelFlags |= 2;
          } else if (heavyValue != null && Math.abs(modMasses[i] - heavyValue) < threshold) {
            labelFlags |= 4;
          }

          if (labelFlags == 7) { // If all bits are set, no need to continue the loop
            break;
          }
        }

        switch (labelFlags) {
          case 1:
            return 1;
          case 2:
            return 2;
          case 4:
            return 3;
          default:
            return 0;
        }
      }

      return labelType;
    }

    @Override
    public int compareTo(Peptide o) {
      return modifiedPeptide.compareTo(o.modifiedPeptide);
    }

    @Override
    public boolean equals(Object o) {
      if (o instanceof Peptide) {
        return compareTo((Peptide) o) == 0;
      } else {
        return false;
      }
    }

    @Override
    public String toString() {
      return modifiedPeptide;
    }

    @Override
    public int hashCode() {
      return toString().hashCode();
    }
  }


  private static class Fragment implements Comparable<Fragment> {

    final float mz;
    final float intensity;
    final char type;
    final byte charge;
    final int ordinal;
    final String lossType;

    public Fragment(float mz, float intensity, char type, byte charge, int ordinal, String lossType) {
      this.mz = mz;
      this.intensity = intensity;
      this.type = type;
      this.charge = charge;
      this.ordinal = ordinal;
      this.lossType = lossType;
    }

    @Override
    public int compareTo(Fragment o) {
      return ComparisonChain.start()
          .compare(mz, o.mz)
          .compare(type, o.type)
          .compare(ordinal, o.ordinal)
          .compare(lossType, o.lossType)
          .compare(charge, o.charge)
          .result();
    }

    @Override
    public boolean equals(Object o) {
      if (o instanceof Fragment) {
        return compareTo((Fragment) o) == 0;
      } else {
        return false;
      }
    }

    @Override
    public String toString() {
      if (lossType.isEmpty()) {
        return String.valueOf(type) + ordinal + "^" + charge;
      } else {
        return String.valueOf(type) + ordinal + "-" + lossType + "^" + charge;
      }
    }

    @Override
    public int hashCode() {
      return toString().hashCode();
    }
  }
}
