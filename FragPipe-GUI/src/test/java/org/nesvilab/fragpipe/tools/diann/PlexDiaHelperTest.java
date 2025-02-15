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

import static org.nesvilab.fragpipe.tools.diann.PlexDiaHelper.tabPattern;
import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.junit.Test;

public class PlexDiaHelperTest {

  @Test
  public void testPreparePlexLibrary1() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI()).toAbsolutePath();
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
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, false, false, 1);

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
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2.tsv")).toURI()).toAbsolutePath();
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_2_plex.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = null;
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('C', 618.36036f);
    heavyLabels.put('C', 624.36722f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, false, false, 1);

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
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI()).toAbsolutePath();
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
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, false, false, 2);

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
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2.tsv")).toURI()).toAbsolutePath();
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_2_light_only.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = null;
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('C', 618.36036f);
    heavyLabels.put('C', 624.36722f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, false, false, 2);

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
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI()).toAbsolutePath();
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
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, true, false, 2);

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
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_2.tsv")).toURI()).toAbsolutePath();
    Path generatedLibraryPath = libraryPath.getParent().resolve("library_2_light_only_2.tsv");

    TreeMap<Character, Float> lightLabels = new TreeMap<>();
    TreeMap<Character, Float> mediumLabels = null;
    TreeMap<Character, Float> heavyLabels = new TreeMap<>();

    lightLabels.put('C', 618.36036f);
    heavyLabels.put('C', 624.36722f);

    PlexDiaHelper plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, true, false, 2);

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
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_3.tsv")).toURI()).toAbsolutePath();
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
    plexDiaHelper.generateNewLibrary(libraryPath, generatedLibraryPath, true, false, 2);

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
}