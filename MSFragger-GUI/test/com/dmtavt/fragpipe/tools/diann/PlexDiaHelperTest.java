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

import static com.dmtavt.fragpipe.tools.diann.PlexDiaHelper.tabPattern;
import static com.dmtavt.fragpipe.util.Utils.AAMasses;
import static com.dmtavt.fragpipe.util.Utils.removeClosedModifications;
import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.junit.Test;

public class PlexDiaHelperTest {

  @Test
  public void testPreparePlexLibrary1() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI());
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_1_plex.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = new TreeMap<>();
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('K', 0f);
    lightLabels.put('R', 0f);
    mediumLabels.put('K', 4.025107f);
    mediumLabels.put('R', 6.020129f);
    heavyLabels.put('K', 8.014199f);
    heavyLabels.put('R', 10.008269f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, false);

    Path expectedLibraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1_expected_plex.tsv")).toURI());

    // load expected library
    BufferedReader reader = Files.newBufferedReader(expectedLibraryPath);
    List<String[]> expectedLibrary = reader.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // load generated library
    BufferedReader reader2 = Files.newBufferedReader(generatedLibraryPath);
    List<String[]> generatedLibrary = reader2.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // compare results
    assertEquals(expectedLibrary.size(), generatedLibrary.size());
    for (int i = 0; i < expectedLibrary.size(); ++i) {
      String[] expected = expectedLibrary.get(i);
      String[] generated = generatedLibrary.get(i);
      assertEquals(expected.length, generated.length);
      for (int j = 0; j < expected.length; ++j) {
        if (i > 0 && (j == 0 || j == 1 || j == 8 || j == 9 || j == 15)) {
          assertEquals(Float.parseFloat(expected[j]), Float.parseFloat(generated[j]), 0.001);
        } else {
          assertEquals(expected[j], generated[j]);
        }
      }
    }
  }

  @Test
  public void testPreparePlexLibrary2() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2.tsv")).toURI());
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_2_plex.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = null;
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('C', 618.36036f);
    heavyLabels.put('C', 624.36722f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, false);

    Path expectedLibraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2_expected_plex.tsv")).toURI());

    // load expected library
    BufferedReader reader = Files.newBufferedReader(expectedLibraryPath);
    List<String[]> expectedLibrary = reader.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // load generated library
    BufferedReader reader2 = Files.newBufferedReader(generatedLibraryPath);
    List<String[]> generatedLibrary = reader2.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // compare results
    assertEquals(expectedLibrary.size(), generatedLibrary.size());
    for (int i = 0; i < expectedLibrary.size(); ++i) {
      String[] expected = expectedLibrary.get(i);
      String[] generated = generatedLibrary.get(i);
      assertEquals(expected.length, generated.length);
      for (int j = 0; j < expected.length; ++j) {
        if (i > 0 && (j == 0 || j == 1 || j == 8 || j == 9 || j == 15)) {
          assertEquals(Float.parseFloat(expected[j]), Float.parseFloat(generated[j]), 0.001);
        } else {
          assertEquals(expected[j], generated[j]);
        }
      }
    }
  }

  @Test
  public void testPreparePlexLibrary3() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI());
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_1_light_only.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = new TreeMap<>();
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('K', 0f);
    lightLabels.put('R', 0f);
    mediumLabels.put('K', 4.025107f);
    mediumLabels.put('R', 6.020129f);
    heavyLabels.put('K', 8.014199f);
    heavyLabels.put('R', 10.008269f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary2(libraryPath, generatedLibraryPath, false, false);

    Path expectedLibraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1_expected_light_only.tsv")).toURI());

    // load expected library
    BufferedReader reader = Files.newBufferedReader(expectedLibraryPath);
    List<String[]> expectedLibrary = reader.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // load generated library
    BufferedReader reader2 = Files.newBufferedReader(generatedLibraryPath);
    List<String[]> generatedLibrary = reader2.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // compare results
    assertEquals(expectedLibrary.size(), generatedLibrary.size());
    for (int i = 0; i < expectedLibrary.size(); ++i) {
      String[] expected = expectedLibrary.get(i);
      String[] generated = generatedLibrary.get(i);
      assertEquals(expected.length, generated.length);
      for (int j = 0; j < expected.length; ++j) {
        if (i > 0 && (j == 0 || j == 1 || j == 8 || j == 9 || j == 15)) {
          assertEquals(Float.parseFloat(expected[j]), Float.parseFloat(generated[j]), 0.001);
        } else {
          assertEquals(expected[j], generated[j]);
        }
      }
    }
  }

  @Test
  public void testPreparePlexLibrary4() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2.tsv")).toURI());
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_2_light_only.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = null;
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('C', 618.36036f);
    heavyLabels.put('C', 624.36722f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary2(libraryPath, generatedLibraryPath, false, false);

    Path expectedLibraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2_expected_light_only.tsv")).toURI());

    // load expected library
    BufferedReader reader = Files.newBufferedReader(expectedLibraryPath);
    List<String[]> expectedLibrary = reader.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // load generated library
    BufferedReader reader2 = Files.newBufferedReader(generatedLibraryPath);
    List<String[]> generatedLibrary = reader2.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // compare results
    assertEquals(expectedLibrary.size(), generatedLibrary.size());
    for (int i = 0; i < expectedLibrary.size(); ++i) {
      String[] expected = expectedLibrary.get(i);
      String[] generated = generatedLibrary.get(i);
      assertEquals(expected.length, generated.length);
      for (int j = 0; j < expected.length; ++j) {
        if (i > 0 && (j == 0 || j == 1 || j == 8 || j == 9 || j == 15)) {
          assertEquals(Float.parseFloat(expected[j]), Float.parseFloat(generated[j]), 0.001);
        } else {
          assertEquals(expected[j], generated[j]);
        }
      }
    }
  }

  @Test
  public void testPreparePlexLibrary5() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI());
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_1_light_only.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = new TreeMap<>();
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('K', 0f);
    lightLabels.put('R', 0f);
    mediumLabels.put('K', 4.025107f);
    mediumLabels.put('R', 6.020129f);
    heavyLabels.put('K', 8.014199f);
    heavyLabels.put('R', 10.008269f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary2(libraryPath, generatedLibraryPath, true, false);

    Path expectedLibraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1_expected_light_only_2.tsv")).toURI());

    // load expected library
    BufferedReader reader = Files.newBufferedReader(expectedLibraryPath);
    List<String[]> expectedLibrary = reader.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // load generated library
    BufferedReader reader2 = Files.newBufferedReader(generatedLibraryPath);
    List<String[]> generatedLibrary = reader2.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // compare results
    assertEquals(expectedLibrary.size(), generatedLibrary.size());
    for (int i = 0; i < expectedLibrary.size(); ++i) {
      String[] expected = expectedLibrary.get(i);
      String[] generated = generatedLibrary.get(i);
      assertEquals(expected.length, generated.length);
      for (int j = 0; j < expected.length; ++j) {
        if (i > 0 && (j == 0 || j == 1 || j == 8 || j == 9 || j == 15)) {
          assertEquals(Float.parseFloat(expected[j]), Float.parseFloat(generated[j]), 0.001);
        } else {
          assertEquals(expected[j], generated[j]);
        }
      }
    }
  }

  @Test
  public void testPreparePlexLibrary6() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2.tsv")).toURI());
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_2_light_only_2.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = null;
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('C', 618.36036f);
    heavyLabels.put('C', 624.36722f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary2(libraryPath, generatedLibraryPath, true, false);

    Path expectedLibraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2_expected_light_only_2.tsv")).toURI());

    // load expected library
    BufferedReader reader = Files.newBufferedReader(expectedLibraryPath);
    List<String[]> expectedLibrary = reader.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // load generated library
    BufferedReader reader2 = Files.newBufferedReader(generatedLibraryPath);
    List<String[]> generatedLibrary = reader2.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // compare results
    assertEquals(expectedLibrary.size(), generatedLibrary.size());
    for (int i = 0; i < expectedLibrary.size(); ++i) {
      String[] expected = expectedLibrary.get(i);
      String[] generated = generatedLibrary.get(i);
      assertEquals(expected.length, generated.length);
      for (int j = 0; j < expected.length; ++j) {
        if (i > 0 && (j == 0 || j == 1 || j == 8 || j == 9 || j == 15)) {
          assertEquals(Float.parseFloat(expected[j]), Float.parseFloat(generated[j]), 0.001);
        } else {
          assertEquals(expected[j], generated[j]);
        }
      }
    }
  }

  @Test
  public void testPreparePlexLibrary7() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_3.tsv")).toURI());
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_3_light_only.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = new TreeMap<>();
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('K', 0f);
    lightLabels.put('R', 0f);
    mediumLabels.put('K', 4.025107f);
    mediumLabels.put('R', 6.020129f);
    heavyLabels.put('K', 8.014199f);
    heavyLabels.put('R', 10.008269f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary2(libraryPath, generatedLibraryPath, true, false);

    Path expectedLibraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_3_expected_light_only_2.tsv")).toURI());

    // load expected library
    BufferedReader reader = Files.newBufferedReader(expectedLibraryPath);
    List<String[]> expectedLibrary = reader.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // load generated library
    BufferedReader reader2 = Files.newBufferedReader(generatedLibraryPath);
    List<String[]> generatedLibrary = reader2.lines()
        .parallel()
        .filter(l -> !l.isEmpty())
        .map(l -> tabPattern.split(l, -1)) // -1 to keep trailing empty strings
        .collect(Collectors.toList());
    reader.close();

    // compare results
    assertEquals(expectedLibrary.size(), generatedLibrary.size());
    for (int i = 0; i < expectedLibrary.size(); ++i) {
      String[] expected = expectedLibrary.get(i);
      String[] generated = generatedLibrary.get(i);
      assertEquals(expected.length, generated.length);
      for (int j = 0; j < expected.length; ++j) {
        if (i > 0 && (j == 0 || j == 1 || j == 8 || j == 9 || j == 15)) {
          assertEquals(Float.parseFloat(expected[j]), Float.parseFloat(generated[j]), 0.001);
        } else {
          assertEquals(expected[j], generated[j]);
        }
      }
    }
  }

  private PlexDiaHelper prepareForReflection() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI());
    if (!Files.exists(libraryPath) || !Files.isRegularFile(libraryPath) || !Files.isReadable(libraryPath)) {
      throw new IllegalStateException("Test library file not found: " + libraryPath);
    }

    Map<Character, Float> lightLabels = new HashMap<>();
    Map<Character, Float> mediumLabels = new HashMap<>();
    Map<Character, Float> heavyLabels = new HashMap<>();

    lightLabels.put('K', 0f);
    mediumLabels.put('K', 4.025107f);
    heavyLabels.put('K', 8.014199f);

    return new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
  }

  @Test
  public void testCollectAllMods() throws Exception {
    PlexDiaHelper plexDiaHelper = prepareForReflection();

    Set<String> modifiedPeptides = new HashSet<>(Arrays.asList(
        "DLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)",
        "nDLC(UniMod:4)IPIK[120.0023]K",
        "(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)",
        "n[20.093]DLDHAC(UniMod:4)IIK[140.3424]"
    ));

    Set<Float> expectedModMasses = new HashSet<>(Arrays.asList(
        8.014199f,
        57.021464f,
        20.093f,
        42.010565f,
        120.0023f - AAMasses['K' - 'A'],
        140.3424f - AAMasses['K' - 'A']
    ));

    Method collectAllModsMethod = PlexDiaHelper.class.getDeclaredMethod("collectAllMods", Set.class);
    collectAllModsMethod.setAccessible(true);
    @SuppressWarnings("unchecked") Set<Float> actualModMasses = (Set<Float>) collectAllModsMethod.invoke(plexDiaHelper, modifiedPeptides);

    assertEquals(expectedModMasses, actualModMasses);
  }

  @Test
  public void testCorrectModifiedPeptide() throws Exception {
    PlexDiaHelper plexDiaHelper = prepareForReflection();

    String[] modifiedPeptides = new String[]{
        "nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)",
        "DLC(UniMod:4)IPIK[120.0023]K",
        "n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)",
        "[20.093]DLDHAC(UniMod:4)IIK[140.3424]"
    };

    String[] expectedModifiedPeptides = new String[]{
        "nDLEEDHAC[57.021465]IPIK[8.014199]K[8.014199]",
        "nDLC[57.021465]IPIK[-8.092659]K",
        "n[42.010567]DLEEDHAC[57.021465]IPKK[8.014199]",
        "n[20.093]DLDHAC[57.021465]IIK[12.247452]"
    };

    Method collectAllModsMethod = PlexDiaHelper.class.getDeclaredMethod("collectAllMods", Set.class);
    collectAllModsMethod.setAccessible(true);
    @SuppressWarnings("unchecked") Set<Float> modMasses = (Set<Float>) collectAllModsMethod.invoke(plexDiaHelper, new HashSet<>(Arrays.asList(modifiedPeptides)));
    plexDiaHelper.theoModMasses = removeClosedModifications(modMasses);

    Method correctModifiedPeptideMethod = PlexDiaHelper.class.getDeclaredMethod("correctModifiedPeptide", String.class);
    correctModifiedPeptideMethod.setAccessible(true);

    for (int i = 0; i < modifiedPeptides.length; ++i) {
      String correctedPeptide = (String) correctModifiedPeptideMethod.invoke(plexDiaHelper, modifiedPeptides[i]);
      assertEquals(expectedModifiedPeptides[i], correctedPeptide);
    }
  }
}