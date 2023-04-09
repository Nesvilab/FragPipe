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
import static com.dmtavt.fragpipe.util.Utils.AAMasses;
import static com.dmtavt.fragpipe.util.Utils.correctModMass;
import static com.dmtavt.fragpipe.util.Utils.removeClosedModifications;
import static com.dmtavt.fragpipe.util.Utils.threshold;

import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.util.UnimodOboReader;
import com.google.common.collect.HashBasedTable;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Table;
import com.google.common.collect.Tables;
import com.google.common.collect.TreeBasedTable;
import com.google.common.primitives.Floats;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
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
import org.jooq.lambda.Seq;

public class PlexDiaHelper {

  private static final Pattern aaPattern = Pattern.compile("([A-Zn])(\\((UniMod:\\d+)\\))?([\\(\\[]([\\d+.-]+)[\\]\\)])?"); // EasyPQP does not support C-term mods?
  private static final Pattern labelPattern = Pattern.compile("([A-Znc*]+)([\\d.+-]+)");
  static final Pattern tabPattern = Pattern.compile("\\t");

  private final int nThreads;
  private final Map<Character, Float> lightAaMassMap;
  private final Map<Character, Float> mediumAaMassMap;
  private final Map<Character, Float> heavyAaMassMap;
  private final Map<String, Float> unimodMassMap;
  private final Table<Float, Character, Integer> massSiteUnimodTable;

  float[] theoModMasses;

  public static void main(String[] args) {
    long start = System.nanoTime();

    int nThreads = Runtime.getRuntime().availableProcessors();
    Map<Character, Float> lightAaMassMap = null;
    Map<Character, Float> mediumAaMassMap = null;
    Map<Character, Float> heavyAaMassMap = null;
    Path libraryPath = null;
    Path outputLibraryPath = null;
    Path diannReportPath = null;
    Path outputDirectory = null;

    for (int i = 0; i < args.length; ++i) {
      if (args[i].trim().contentEquals("--threads")) {
        int t = Integer.parseInt(args[++i]);
        if (t > 0) {
          nThreads = t;
        }
      } else if (args[i].trim().contentEquals("--light")) {
        if (!args[i + 1].trim().startsWith("--")) {
          String s = args[++i].trim();
          lightAaMassMap = parseLabel(s);
        }
      } else if (args[i].trim().contentEquals("--medium")) {
        if (!args[i + 1].trim().startsWith("--")) {
          String s = args[++i].trim();
          mediumAaMassMap = parseLabel(s);
        }
      } else if (args[i].trim().contentEquals("--heavy")) {
        if (!args[i + 1].trim().startsWith("--")) {
          String s = args[++i].trim();
          heavyAaMassMap = parseLabel(s);
        }
      } else if (args[i].trim().contentEquals("--library")) {
        libraryPath = Paths.get(args[++i].trim());
      } else if (args[i].trim().contentEquals("--out")) {
        outputLibraryPath = Paths.get(args[++i].trim());
      } else if (args[i].trim().contentEquals("--diann-report")) {
        diannReportPath = Paths.get(args[++i].trim());
      } else if (args[i].trim().contentEquals("--output-dir")) {
        outputDirectory = Paths.get(args[++i].trim());
      }
    }

    if (lightAaMassMap == null && mediumAaMassMap == null && heavyAaMassMap == null) {
      System.err.println("There are no light, medium, or heavy labels.");
      System.exit(1);
    }

    if (libraryPath == null) {
      System.err.println("There is no library path.");
      System.exit(1);
    }

    try {
      PlexDiaHelper plexDiaHelper = new PlexDiaHelper(nThreads, lightAaMassMap, mediumAaMassMap, heavyAaMassMap);

      if (outputLibraryPath != null) {
        plexDiaHelper.generateNewLibrary2(libraryPath, outputLibraryPath, true);
      } else if (diannReportPath != null && outputDirectory != null) {
        plexDiaHelper.pairAndWriteReport(libraryPath, diannReportPath, outputDirectory);
      }
    } catch (Exception ex) {
      ex.printStackTrace();
      System.exit(1);
    }

    System.out.printf("Done in %.1f s.%n", (System.nanoTime() - start) * 1e-9f);
  }

  private static Map<Character, Float> parseLabel(String inputStr) {
    Map<Character, Float> outputMap = new HashMap<>();
    Matcher matcher = labelPattern.matcher(inputStr.trim());
    while (matcher.find()) {
      char[] aas = matcher.group(1).toCharArray();
      float modMass = Float.parseFloat(matcher.group(2));
      for (char aa : aas) {
        if (aa == '*') {
          for (int i = 65; i < 91; ++i) {
            outputMap.put((char) i, modMass);
          }
          outputMap.put('n', modMass);
          outputMap.put('c', modMass);
        } else {
          outputMap.put(aa, modMass);
        }
      }
    }

    if (outputMap.isEmpty()) {
      return null;
    } else {
      return outputMap;
    }
  }

  PlexDiaHelper(int nThreads, Map<Character, Float> lightAaMassMap, Map<Character, Float> mediumAaMassMap, Map<Character, Float> heavyAaMassMap) throws Exception {
    this.nThreads = nThreads;
    this.lightAaMassMap = lightAaMassMap;
    this.mediumAaMassMap = mediumAaMassMap;
    this.heavyAaMassMap = heavyAaMassMap;

    final List<Path> tt = FragpipeLocations.checkToolsMissing(Seq.of(UNIMOD_OBO));
    if (tt == null || tt.size() != 1) {
      throw new FileNotFoundException("Could not find unimod.obo file from " + FragpipeLocations.get().getDirTools());
    }
    Path unimodPath = tt.get(0);
    UnimodOboReader unimodOboReader = new UnimodOboReader(unimodPath);
    unimodMassMap = unimodOboReader.unimodMassMap;
    massSiteUnimodTable = unimodOboReader.massSiteUnimodTable;
  }

  void generateNewLibrary(Path libraryPath, Path outputPath) throws Exception {
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

    Map<String, Integer> columnNameToIndex = getColumnIndexMap(libraryPath, "PrecursorMz", library.get(0));

    int columnIdx = columnNameToIndex.get("ModifiedPeptideSequence");
    Set<String> modifiedPeptides = library.stream().skip(1).map(p -> p[columnIdx]).collect(Collectors.toSet());

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

    Multimap<String, Transition> transitions = collectTransitions(library, columnNameToIndex);
    appendComplementTransitions(transitions);

    writeLibrary(transitions, outputPath);
  }

  void generateNewLibrary2(Path libraryPath, Path outputPath, boolean removeUnlabeledTransitions) throws Exception {
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

    Map<String, Integer> columnNameToIndex = getColumnIndexMap(libraryPath, "PrecursorMz", library.get(0));

    int columnIdx = columnNameToIndex.get("ModifiedPeptideSequence");
    Set<String> modifiedPeptides = library.stream().skip(1).map(p -> p[columnIdx]).collect(Collectors.toSet());

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

    Multimap<String, Transition> transitions = collectTransitions(library, columnNameToIndex);
    generateLightOnlyTransitions(transitions, removeUnlabeledTransitions);

    writeLibrary(transitions, outputPath);
  }

  void pairAndWriteReport(Path libraryPath, Path diannReportPath, Path outputDirectory) throws Exception {
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

    Map<String, Integer> columnNameToIndex = getColumnIndexMap(libraryPath, "PrecursorMz", library.get(0));

    int columnIdx = columnNameToIndex.get("ModifiedPeptideSequence");
    Set<String> modifiedPeptides = library.stream().skip(1).map(p -> p[columnIdx]).collect(Collectors.toSet());
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

    Table<String, String, IonEntry> diannTable = HashBasedTable.create();
    Map<String, String[]> sequenceProteinMap = readDiannReport(diannReportPath, diannTable);

    Table<String, String, IonPair> ionRunPairTable = pairIons(diannTable);

    WriteCombinedLabelQuantTables writeCombinedLabelQuantTables = new WriteCombinedLabelQuantTables(lightAaMassMap, mediumAaMassMap, heavyAaMassMap, ionRunPairTable, sequenceProteinMap);
    writeCombinedLabelQuantTables.writeCombinedIonLabelQuant(outputDirectory.resolve("combined_ion_label_quant.tsv"));
    writeCombinedLabelQuantTables.writeCombinedModifiedPeptideLabelQuant(outputDirectory.resolve("combined_modified_peptide_label_quant.tsv"));
    writeCombinedLabelQuantTables.writeCombinedSequenceLabelQuant(outputDirectory.resolve("combined_peptide_label_quant.tsv"));
    writeCombinedLabelQuantTables.writeCombinedProteinLabelQuant(outputDirectory.resolve("combined_protein_label_quant.tsv"));
  }

  private Table<String, String, IonPair> pairIons(Table<String, String, IonEntry> diannTable) throws Exception {
    String[] runArray = diannTable.rowKeySet().toArray(new String[0]);
    ExecutorService executorService = Executors.newFixedThreadPool(nThreads);
    int multi = Math.min(nThreads * 8, runArray.length);
    List<Future<Table<String, String, IonPair>>> futureList = new ArrayList<>(multi);
    for (int i = 0; i < multi; ++i) {
      final int currentThread = i;
      futureList.add(executorService.submit(() -> {
        Table<String, String, IonPair> localTable = HashBasedTable.create();
        int start = (int) ((currentThread * ((long) runArray.length)) / multi);
        int end = (int) (((currentThread + 1) * ((long) runArray.length)) / multi);
        for (int j = start; j < end; ++j) {
          String run = runArray[j];
          Collection<IonEntry> ionEntries = diannTable.row(run).values();
          Map<String, List<IonEntry>> labelFreeIonEntryMap = ionEntries.stream().collect(Collectors.groupingBy(e -> e.labelFreeIonID));
          for (Map.Entry<String, List<IonEntry>> e : labelFreeIonEntryMap.entrySet()) {
            String labelFreeIonID = e.getKey();
            Map<Integer, List<IonEntry>> labelTypeIonEntryMap = e.getValue().stream().collect(Collectors.groupingBy(ee -> ee.labelType));
            IonPair ionPair = null;
            if (labelTypeIonEntryMap.size() == 3) {
              IonEntry[] bestPair = findBestPair(labelTypeIonEntryMap.get(0), labelTypeIonEntryMap.get(1), labelTypeIonEntryMap.get(2));
              ionPair = new IonPair(bestPair[0], bestPair[1], bestPair[2]);
            } else if (labelTypeIonEntryMap.size() == 2) {
              Integer[] keyArray = labelTypeIonEntryMap.keySet().toArray(new Integer[0]);
              IonEntry[] bestPair = findBestPair(labelTypeIonEntryMap.get(keyArray[0]), labelTypeIonEntryMap.get(keyArray[1]));
              if (keyArray[0] == 1 && keyArray[1] == 2) {
                ionPair = new IonPair(bestPair[0], bestPair[1], null);
              } else if (keyArray[0] == 1 && keyArray[1] == 3) {
                ionPair = new IonPair(bestPair[0], null, bestPair[1]);
              } else if (keyArray[0] == 2 && keyArray[1] == 3) {
                ionPair = new IonPair(null, bestPair[0], bestPair[1]);
              }
            } else if (labelTypeIonEntryMap.size() == 1) {
              float tt = 0;
              IonEntry bestIonEntry = null;
              for (IonEntry ionEntry : labelTypeIonEntryMap.values().iterator().next()) {
                if (ionEntry.intensity > tt) {
                  tt = ionEntry.intensity;
                  bestIonEntry = ionEntry;
                }
              }

              Integer[] keyArray = labelTypeIonEntryMap.keySet().toArray(new Integer[0]);
              if (keyArray[0] == 1) {
                ionPair = new IonPair(bestIonEntry, null, null);
              } else if (keyArray[0] == 2) {
                ionPair = new IonPair(null, bestIonEntry, null);
              } else if (keyArray[0] == 3) {
                ionPair = new IonPair(null, null, bestIonEntry);
              }
            }

            if (ionPair != null) {
              IonPair old = localTable.get(run, labelFreeIonID);
              if (old == null || old.getTotalIntensity() < ionPair.getTotalIntensity()) {
                localTable.put(run, labelFreeIonID, ionPair);
              }
            }
          }
        }

        return localTable;
      }));
    }

    Table<String, String, IonPair> ionRunPairTable = TreeBasedTable.create();
    for (Future<Table<String, String, IonPair>> future : futureList) {
      ionRunPairTable.putAll(Tables.transpose(future.get()));
    }

    executorService.shutdown();
    if (!executorService.awaitTermination(10, TimeUnit.SECONDS)) {
      executorService.shutdownNow();
      if (!executorService.awaitTermination(10, TimeUnit.SECONDS)) {
        throw new InterruptedException("Thread pool did not terminate normally.");
      }
    }

    return ionRunPairTable;
  }

  private IonEntry[] findBestPair(List<IonEntry> list1, List<IonEntry> list2, List<IonEntry> list3) {
    float gap = Float.MAX_VALUE;
    float summedIntensity = 0;
    IonEntry[] pickedPair = new IonEntry[3];
    for (IonEntry x : list1) {
      for (IonEntry y : list2) {
        for (IonEntry z : list3) {
          float rtDiff = Math.abs(x.apexRt - y.apexRt) + Math.abs(y.apexRt - z.apexRt) + Math.abs(z.apexRt - x.apexRt);
          float tt = x.intensity + y.intensity + z.intensity;
          if (rtDiff < gap || (rtDiff == gap && tt > summedIntensity)) {
            gap = rtDiff;
            summedIntensity = tt;
            pickedPair[0] = x;
            pickedPair[1] = y;
            pickedPair[2] = z;
          }
        }
      }
    }
    return pickedPair;
  }

  private IonEntry[] findBestPair(List<IonEntry> list1, List<IonEntry> list2) {
    float gap = Float.MAX_VALUE;
    float summedIntensity = 0;
    IonEntry[] pickedPair = new IonEntry[2];
    for (IonEntry x : list1) {
      for (IonEntry y : list2) {
        float rtDiff = Math.abs(x.apexRt - y.apexRt);
        float tt = x.intensity + y.intensity;
        if ((rtDiff < gap) || (rtDiff == gap && tt > summedIntensity)) {
          gap = rtDiff;
          summedIntensity = tt;
          pickedPair[0] = x;
          pickedPair[1] = y;
        }
      }
    }
    return pickedPair;
  }

  private Map<String, Integer> getColumnIndexMap(Path path, String firstColumnName, String[] header) {
    if(!header[0].contentEquals(firstColumnName)) {
      throw new RuntimeException("The library file " + path.toAbsolutePath() + " does not start with " + firstColumnName + " column");
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
          if (!s.startsWith("n")) {
            s = "n" + s;
          }
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
    if (!peptide.startsWith("n")) {
      peptide = "n" + peptide;
    }

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

  private Multimap<String, Transition> collectTransitions(List<String[]> library, Map<String, Integer> columnNameToIndex) throws Exception {
    int precursorMzIdx = columnNameToIndex.get("PrecursorMz");
    int modifiedPeptideSequenceIdx = columnNameToIndex.get("ModifiedPeptideSequence");
    int precursorChargeIdx = columnNameToIndex.get("PrecursorCharge");
    int normalizedRetentionTimeIdx = columnNameToIndex.get("NormalizedRetentionTime");
    int precursorIonMobilityIdx = columnNameToIndex.get("PrecursorIonMobility");
    int productMzIdx = columnNameToIndex.get("ProductMz");
    int libraryIntensityIdx = columnNameToIndex.get("LibraryIntensity");
    int fragmentTypeIdx = columnNameToIndex.get("FragmentType");
    int fragmentChargeIdx = columnNameToIndex.get("FragmentCharge");
    int fragmentSeriesNumberIdx = columnNameToIndex.get("FragmentSeriesNumber");
    int fragmentLossTypeIdx = columnNameToIndex.get("FragmentLossType");
    int proteinIdIdx = columnNameToIndex.get("ProteinId");
    int geneNameIdx = columnNameToIndex.get("GeneName");
    int averageExperimentalRetentionTimeIdx = columnNameToIndex.get("AverageExperimentalRetentionTime");

    ForkJoinPool forkJoinPool = new ForkJoinPool(nThreads);
    Map<String, List<String[]>> transitionFragmentMap = forkJoinPool.submit(() ->
        library.stream()
            .skip(1)
            .parallel()
            .collect(Collectors.groupingBy(p ->
                p[precursorMzIdx] + "-" +
                correctModifiedPeptide(p[modifiedPeptideSequenceIdx]) + "-" +
                p[precursorChargeIdx] + "-" +
                p[normalizedRetentionTimeIdx] + "-" +
                p[precursorIonMobilityIdx]))
    ).get();
    forkJoinPool.shutdown();

    List[] ttArray = transitionFragmentMap.values().toArray(new List[0]);
    ExecutorService executorService = Executors.newFixedThreadPool(nThreads);
    int multi = Math.min(nThreads * 8, ttArray.length);
    List<Future<Multimap<String, Transition>>> futures = new ArrayList<>(multi);
    for (int i = 0; i < multi; ++i) {
      final int currentThread = i;
      futures.add(executorService.submit(() -> {
        Multimap<String, Transition> localMap = HashMultimap.create();
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
                Float.parseFloat(ss[productMzIdx]),
                Float.parseFloat(ss[libraryIntensityIdx]),
                ss[fragmentTypeIdx].charAt(0),
                Byte.parseByte(ss[fragmentChargeIdx]),
                Integer.parseInt(ss[fragmentSeriesNumberIdx]),
                ss[fragmentLossTypeIdx]
            );
            Fragment oldFragment = mzFragmentMap.get(fragment.mz);
            if (oldFragment == null || oldFragment.intensity < fragment.intensity) {
              mzFragmentMap.put(fragment.mz, fragment);
            }
          }
          String[] ss = tt.get(0);
          Transition transition = new Transition(
              Float.parseFloat(ss[precursorMzIdx]),
              mzFragmentMap.values().toArray(new Fragment[0]),
              ss[proteinIdIdx],
              ss[geneNameIdx],
              new Peptide(ss[modifiedPeptideSequenceIdx]),
              Byte.parseByte(ss[precursorChargeIdx]),
              myToFloat(ss[normalizedRetentionTimeIdx], 0),
              myToFloat(ss[precursorIonMobilityIdx], 0),
              myToFloat(ss[averageExperimentalRetentionTimeIdx], 0)
          );
          localMap.put(transition.peptide.modifiedPeptide + transition.peptideCharge, transition);
        }
        return localMap;
      }));
    }

    Multimap<String, Transition> transitions = HashMultimap.create();
    for (Future<Multimap<String, Transition>> future : futures) {
      transitions.putAll(future.get());
    }

    executorService.shutdown();
    if (!executorService.awaitTermination(10, TimeUnit.SECONDS)) {
      executorService.shutdownNow();
      if (!executorService.awaitTermination(10, TimeUnit.SECONDS)) {
        throw new InterruptedException("Thread pool did not terminate normally.");
      }
    }

    return transitions;
  }

  private void appendComplementTransitions(Multimap<String, Transition> transitions) {
    Multimap<String, Transition> complementaryTransitions = HashMultimap.create();
    for (Map.Entry<String, Transition> e : transitions.entries()) {
      Transition transition1 = e.getValue();
      Peptide peptide1 = transition1.peptide;
      int labelType = peptide1.detectLabelTypes();
      if (labelType == 1) {
        if (mediumAaMassMap != null && lightAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryTransitions, lightAaMassMap, mediumAaMassMap);
        }
        if (heavyAaMassMap != null && lightAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryTransitions, lightAaMassMap, heavyAaMassMap);
        }
      } else if (labelType == 2) {
        if (mediumAaMassMap != null && lightAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryTransitions, mediumAaMassMap, lightAaMassMap);
        }
        if (mediumAaMassMap != null && heavyAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryTransitions, mediumAaMassMap, heavyAaMassMap);
        }
      } else if (labelType == 3) {
        if (heavyAaMassMap != null && lightAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryTransitions, heavyAaMassMap, lightAaMassMap);
        }
        if (heavyAaMassMap != null && mediumAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryTransitions, heavyAaMassMap, mediumAaMassMap);
        }
      }
    }
    transitions.putAll(complementaryTransitions);
  }

  private void generateLightOnlyTransitions(Multimap<String, Transition> transitions, boolean removeUnlabeledTransitions) { // remove heavy precursors and append light precursors if there are only heavy ones
    // assume that the light label always exist. Generate light precursors for all medium and heavy ones
    Multimap<String, Transition> complementaryLightTransitions = HashMultimap.create();
    Set<String> transitionsToRemove = new HashSet<>();
    for (Map.Entry<String, Transition> e : transitions.entries()) {
      Transition transition1 = e.getValue();
      Peptide peptide1 = transition1.peptide;
      int labelType = peptide1.detectLabelTypes();
      if (labelType == 2) {
        transitionsToRemove.add(e.getKey());
        if (mediumAaMassMap != null && lightAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryLightTransitions, mediumAaMassMap, lightAaMassMap);
        }
      } else if (labelType == 3) {
        transitionsToRemove.add(e.getKey());
        if (heavyAaMassMap != null && lightAaMassMap != null) {
          sub(transitions, peptide1, transition1, complementaryLightTransitions, heavyAaMassMap, lightAaMassMap);
        }
      } else if (labelType == 0 && removeUnlabeledTransitions) {
        transitionsToRemove.add(e.getKey());
      }
    }

    // remove medium and heavy precursors
    for (String key : transitionsToRemove) {
      transitions.removeAll(key);
    }
    transitions.putAll(complementaryLightTransitions);
  }

  private static void sub(Multimap<String, Transition> transitions, Peptide peptide1, Transition transition1, Multimap<String, Transition> complementaryTransitions, Map<Character, Float> aaMassMap1, Map<Character, Float> aaMassMap2) {
    Peptide peptide2 = peptide1.getComplementaryPeptide(aaMassMap1, aaMassMap2);

    Collection<Transition> tt = transitions.get(peptide2.modifiedPeptide + transition1.peptideCharge);
    if (tt.isEmpty()) {
      Transition transition2 = getComplementaryTransition(transition1, peptide1, peptide2);
      complementaryTransitions.put(peptide2.modifiedPeptide + transition1.peptideCharge, transition2);
    }
  }

  private static Transition getComplementaryTransition(Transition transition1, Peptide peptide1, Peptide peptide2) {
    float[] modMasses1 = peptide1.modMasses;
    float[] modMasses2 = peptide2.modMasses;
    float[] massDiffArray = new float[modMasses1.length];
    for (int i = 0; i < modMasses1.length; ++i) {
      massDiffArray[i] = modMasses2[i] - modMasses1[i];
    }

    float precursorMz2 = transition1.precursorMz;
    for (float deltaMass : massDiffArray) {
      precursorMz2 += deltaMass / transition1.peptideCharge;
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
    Fragment[] fragments2 = new Fragment[transition1.fragments.length];
    int index = 0;
    for (Fragment fragment1 : transition1.fragments) {
      float mz2 = fragment1.mz;

      if (fragment1.type == 'a' || fragment1.type == 'b' || fragment1.type == 'c') {
        mz2 += bIonMassDiffArray[fragment1.ordinal] / fragment1.charge;
      } else {
        mz2 += yIonMassDiffArray[fragment1.ordinal] / fragment1.charge;
      }
      fragments2[index++] = new Fragment(mz2, fragment1.intensity, fragment1.type, fragment1.charge, fragment1.ordinal, fragment1.lossType);
    }

    return new Transition(precursorMz2, fragments2, transition1.proteinId, transition1.geneName, peptide2, transition1.peptideCharge, transition1.normalizedRetentionTime, transition1.precursorIonMobility, transition1.averageExperimentRetentionTime);
  }

  private static void writeLibrary(Multimap<String, Transition> transitions, Path outputPath) throws Exception {
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

    List<Transition> allTransitions = new ArrayList<>(transitions.values());
    allTransitions.sort(Comparator.naturalOrder());

    // Create a single StringBuilder instance
    StringBuilder sb = new StringBuilder();

    for (Transition transition : allTransitions) {
      String peptideSequence = transition.peptide.peptideSequence;
      String unimodPeptide = transition.peptide.getUnimodPeptide();

      for (Fragment fragment : transition.fragments) {
        // Reset the StringBuilder
        sb.setLength(0);

        // Use the StringBuilder to build the output line
        sb.append(transition.precursorMz).append("\t")
            .append(fragment.mz).append("\t")
            .append(fragment).append("\t")
            .append(transition.proteinId).append("\t")
            .append(transition.geneName).append("\t")
            .append(peptideSequence).append("\t")
            .append(unimodPeptide).append("\t")
            .append(transition.peptideCharge).append("\t")
            .append(fragment.intensity).append("\t")
            .append(transition.normalizedRetentionTime).append("\t")
            .append(Math.abs(transition.precursorIonMobility) > threshold ? transition.precursorIonMobility : "").append("\t")
            .append(fragment.type).append("\t")
            .append(fragment.charge).append("\t")
            .append(fragment.ordinal).append("\t")
            .append(fragment.lossType).append("\t")
            .append(transition.averageExperimentRetentionTime).append("\n");

        // Write the output line
        writer.write(sb.toString());
      }
    }

    writer.close();
  }

  private Map<String, String[]> readDiannReport(Path path, Table<String, String, IonEntry> diannTable) throws Exception {
    BufferedReader reader = Files.newBufferedReader(path);
    ForkJoinPool forkJoinPool = new ForkJoinPool(nThreads);
    List<String[]> diann = forkJoinPool.submit(() ->
        reader.lines()
            .parallel()
            .filter(l -> !l.isEmpty())
            .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
            .collect(Collectors.toList())
    ).get();
    forkJoinPool.shutdown();
    reader.close();

    Map<String, Integer> columnNameToIndex = getColumnIndexMap(path, "File.Name", diann.get(0));

    int runIdx = columnNameToIndex.get("Run");
    int modifiedSequenceIdx = columnNameToIndex.get("Modified.Sequence");
    int strippedSequenceIdx = columnNameToIndex.get("Stripped.Sequence");
    int precursorChargeIdx = columnNameToIndex.get("Precursor.Charge");
    int rtIdx = columnNameToIndex.get("RT");
    int precursorNormalisedIdx = columnNameToIndex.get("Precursor.Normalised");
    int proteinGroupIdx = columnNameToIndex.get("Protein.Group");
    int proteinIdsIdx = columnNameToIndex.get("Protein.Ids");
    int proteinNamesIdx = columnNameToIndex.get("Protein.Names");
    int genesIdx = columnNameToIndex.get("Genes");

    forkJoinPool = new ForkJoinPool(nThreads);
    List<IonEntry> ionEntryList = forkJoinPool.submit(() ->
        diann.parallelStream()
            .skip(1)
            .map(l -> new IonEntry(
                l[runIdx],
                new Peptide(l[modifiedSequenceIdx]),
                Byte.parseByte(l[precursorChargeIdx]),
                Float.parseFloat(l[rtIdx]),
                Float.parseFloat(l[precursorNormalisedIdx])
            ))
            .collect(Collectors.toList())
    ).get();
    forkJoinPool.shutdown();

    for (IonEntry ionEntry : ionEntryList) {
      IonEntry tt = diannTable.get(ionEntry.run, ionEntry.ion);
      if (tt == null || tt.intensity < ionEntry.intensity) {
        diannTable.put(ionEntry.run, ionEntry.ion, ionEntry);
      }
    }

    forkJoinPool = new ForkJoinPool(nThreads);
    Map<String, String[]> sequenceProteinMap = forkJoinPool.submit(() -> // todo: check
        diann.parallelStream()
            .skip(1)
            .collect(Collectors.groupingBy(
                l -> l[strippedSequenceIdx],
                HashMap::new,
                Collectors.mapping(l -> new String[]{
                    l[proteinGroupIdx],
                      l[proteinIdsIdx],
                      l[proteinNamesIdx],
                      l[genesIdx]},
                    Collectors.collectingAndThen(Collectors.toList(), list -> list.get(0)))))
    ).get();
    forkJoinPool.shutdown();

    return sequenceProteinMap;
  }

  private static float myToFloat(String s, float defaultValue) {
    try {
      return Float.parseFloat(s);
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }


  class Peptide implements Comparable<Peptide> {

    final String modifiedPeptide;
    final String peptideSequence;
    final int peptideLength;
    final float[] modMasses; // The first element is N-term modification

    private String labelFreePeptide = null;
    private Integer labelType = null;
    private Integer labelCount = null;

    Peptide(String inputString) {
      if (!inputString.startsWith("n")) {
        inputString = "n" + inputString;
      }

      List<Float> modMassList = new ArrayList<>(inputString.length());

      StringBuilder sb1 = new StringBuilder();
      StringBuilder sb2 = new StringBuilder();
      Matcher matcher = aaPattern.matcher(inputString);
      while (matcher.find()) {
        char aa = matcher.group(1).charAt(0);
        if (aa != 'n') {
          sb2.append(aa);
        }

        if (matcher.group(2) != null) {
          float modMass = correctModMass(unimodMassMap.get(matcher.group(3).toLowerCase()), theoModMasses);
          modMassList.add(modMass);
          sb1.append(aa).append("[").append(modMass).append("]");
        } else if (matcher.group(4) != null) {
          float modMass;
          if (aa == 'n' || aa == 'c') {
            modMass = correctModMass(Float.parseFloat(matcher.group(5)), theoModMasses);
          } else {
            modMass = correctModMass(Float.parseFloat(matcher.group(5)) - AAMasses[aa - 'A'], theoModMasses);
          }
          modMassList.add(modMass);
          sb1.append(aa).append("[").append(modMass).append("]");
        } else  {
          modMassList.add(0f);
          if (aa != 'n') {
            sb1.append(aa);
          }
        }
      }

      this.modifiedPeptide = sb1.toString();
      peptideSequence = sb2.toString();
      peptideLength = peptideSequence.length();
      modMasses = Floats.toArray(modMassList);
    }

    Peptide(String peptideSequence, float[] modMasses) {
      if (peptideSequence.length() != modMasses.length) {
        throw new RuntimeException("peptideSequence and modMasses must have the same length");
      }

      if (!peptideSequence.startsWith("n")) {
        peptideSequence = "n" + peptideSequence;
        float[] tt = new float[modMasses.length + 1];
        System.arraycopy(modMasses, 0, tt, 1, modMasses.length);
        modMasses = tt;
      }

      this.peptideSequence = peptideSequence.substring(1);
      peptideLength = modMasses.length - 1;
      this.modMasses = modMasses;

      char[] aaArray = peptideSequence.toCharArray();
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < aaArray.length; ++i) {
        if (Math.abs(modMasses[i]) > threshold) {
          sb.append(aaArray[i]).append("[").append(modMasses[i]).append("]");
        } else if (aaArray[i] != 'n') {
          sb.append(aaArray[i]);
        }
      }
      modifiedPeptide = sb.toString();
    }

    Peptide getComplementaryPeptide(Map<Character, Float> aaMassMap1, Map<Character, Float> aaMassMap2) {
      char[] aaArray = ("n" + peptideSequence).toCharArray();
      float[] modMasses2 = Arrays.copyOf(modMasses, modMasses.length);

      for (int i = 0; i < aaArray.length; i++) {
        Float mass1 = aaMassMap1.get(aaArray[i]);
        if (mass1 != null && Math.abs(modMasses[i] - mass1) < threshold) {
          modMasses2[i] = aaMassMap2.get(aaArray[i]);
        }
      }

      return new Peptide("n" + peptideSequence, modMasses2);
    }

    String getUnimodPeptide() {
      StringBuilder sb = new StringBuilder();
      char[] aaArray = ("n" + peptideSequence).toCharArray();
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

    Integer detectLabelTypes() { // 0: no labels or multiple labels, 1: light, 2: medium, 3: heavy
      if (labelType == null) {
        int labelFlags = 0;
        char[] aaArray = ("n" + peptideSequence).toCharArray();
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
            labelType =  1;
            break;
          case 2:
            labelType = 2;
            break;
          case 4:
            labelType = 3;
            break;
          default:
            labelType =  0;
        }
      }

      return labelType;
    }

    int getLabelCount() {
      if (labelCount == null) {
        labelCount = 0;
        char[] aaArray = ("n" + peptideSequence).toCharArray();
        int labelType = detectLabelTypes();
        if (labelType == 1) {
          for (int i = 0; i < modMasses.length; ++i) {
            Float tt = lightAaMassMap.get(aaArray[i]);
            if (tt != null && Math.abs(modMasses[i] - tt) < threshold) {
              ++labelCount;
            }
          }
        } else if (labelType == 2) {
          for (int i = 0; i < modMasses.length; ++i) {
            Float tt = mediumAaMassMap.get(aaArray[i]);
            if (tt != null && Math.abs(modMasses[i] - tt) < threshold) {
              ++labelCount;
            }
          }
        } else if (labelType == 3) {
          for (int i = 0; i < modMasses.length; ++i) {
            Float tt = heavyAaMassMap.get(aaArray[i]);
            if (tt != null && Math.abs(modMasses[i] - tt) < threshold) {
              ++labelCount;
            }
          }
        }
      }
      return labelCount;
    }

    String getLabelFreePeptide() {
      if (labelFreePeptide == null) {
        int labelType = detectLabelTypes();
        if (labelType == 1) {
          labelFreePeptide = stripLabels(lightAaMassMap);
        } else if (labelType == 2) {
          labelFreePeptide = stripLabels(mediumAaMassMap);
        } else if (labelType == 3) {
          labelFreePeptide = stripLabels(heavyAaMassMap);
        } else {
          labelFreePeptide = modifiedPeptide;
        }
      }
      return labelFreePeptide;
    }

    private String stripLabels(Map<Character, Float> aaMassMap) {
      char[] aaArray = ("n" + peptideSequence).toCharArray();
      float[] modMasses2 = Arrays.copyOf(modMasses, modMasses.length);

      for (int i = 0; i < aaArray.length; i++) {
        Float mass1 = aaMassMap.get(aaArray[i]);
        if (mass1 != null && Math.abs(modMasses[i] - mass1) < threshold) {
          modMasses2[i] = 0;
        }
      }

      return (new Peptide("n" + peptideSequence, modMasses2)).modifiedPeptide;
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
}
