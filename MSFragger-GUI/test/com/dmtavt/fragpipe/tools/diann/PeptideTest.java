package com.dmtavt.fragpipe.tools.diann;

import static com.dmtavt.fragpipe.util.Utils.removeClosedModifications;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import com.dmtavt.fragpipe.tools.diann.PreparePlexLibrary.Peptide;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;

public class PeptideTest {

  private PreparePlexLibrary preparePlexLibrary;
  private String[] modifiedPeptides;
  private int[] peptideLengths;
  private String[] expectedModifiedPeptides;
  private String[] expectedPeptideSequences;
  private float[][] expectedModMasses;
  private final Map<Character, Float> lightLabels = new HashMap<>();
  private final Map<Character, Float> mediumLabels = new HashMap<>();
  private final Map<Character, Float> heavyLabels = new HashMap<>();

  @Before
  public void setUp() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PreparePlexLibraryTest.class.getResource("/library_1.tsv")).toURI());
    if (!Files.exists(libraryPath) || !Files.isRegularFile(libraryPath) || !Files.isReadable(libraryPath)) {
      throw new IllegalStateException("Test library file not found: " + libraryPath);
    }

    lightLabels.put('K', 0f);
    lightLabels.put('n', 0f);
    mediumLabels.put('K', 4.025107f);
    mediumLabels.put('n', 4.025107f);
    heavyLabels.put('K', 8.014199f);
    heavyLabels.put('n', 8.014199f);

    modifiedPeptides = new String[]{
        "nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)",
        "nDLC(UniMod:4)IPIK[120.0023]K",
        "n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)",
        "n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]"
    };

    preparePlexLibrary = new PreparePlexLibrary(11, lightLabels, mediumLabels, heavyLabels, libraryPath);

    Method collectAllModsMethod = PreparePlexLibrary.class.getDeclaredMethod("collectAllMods", Set.class);
    collectAllModsMethod.setAccessible(true);
    @SuppressWarnings("unchecked") Set<Float> modMasses = (Set<Float>) collectAllModsMethod.invoke(preparePlexLibrary, new HashSet<>(Arrays.asList(modifiedPeptides)));
    preparePlexLibrary.theoModMasses = removeClosedModifications(modMasses);

    peptideLengths = new int[]{
        "nDLEEDHACIPIKK".length(),
        "nDLCIPIKK".length(),
        "nDLEEDHACIPKK".length(),
        "nDLDHACIIK".length()
    };

    expectedModifiedPeptides = new String[]{
        "nDLEEDHAC[57.021465]IPIK[8.014199]K[8.014199]",
        "nDLC[57.021465]IPIK[-8.092659]K",
        "n[42.010567]DLEEDHAC[57.021465]IPKK[8.014199]",
        "n[4.025107]DLDHAC[57.021465]IIK[12.247452]"
    };

    expectedPeptideSequences = new String[]{
        "nDLEEDHACIPIKK",
        "nDLCIPIKK",
        "nDLEEDHACIPKK",
        "nDLDHACIIK"
    };

    expectedModMasses = new float[][]{
        {0, 0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f, 8.014199f},
        {0, 0, 0, 57.021465f, 0, 0, 0, -8.092659f, 0},
        {42.010567f, 0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f},
        {4.025107f, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 12.247452f}
    };
  }

  @Test
  public void testPeptide1() {
    for (int i = 0; i < modifiedPeptides.length; ++i) {
      Peptide peptide = preparePlexLibrary.new Peptide(modifiedPeptides[i], peptideLengths[i]);
      assertEquals(expectedModifiedPeptides[i], peptide.modifiedPeptide);
      assertEquals(expectedPeptideSequences[i], peptide.peptideSequence);
      assertEquals(peptideLengths[i], peptide.peptideLength);
      assertArrayEquals(expectedModMasses[i], peptide.modMasses, 0.0001f);
    }
  }

  @Test
  public void testPeptide2() {
    for (int i = 0; i < expectedPeptideSequences.length; ++i) {
      Peptide peptide = preparePlexLibrary.new Peptide(expectedPeptideSequences[i], expectedModMasses[i]);
      assertEquals(expectedModifiedPeptides[i], peptide.modifiedPeptide);
      assertEquals(expectedPeptideSequences[i], peptide.peptideSequence);
      assertEquals(peptideLengths[i], peptide.peptideLength);
      assertArrayEquals(expectedModMasses[i], peptide.modMasses, 0.0001f);
    }
  }

  @Test
  public void testGetComplementaryPeptide() {
    Peptide peptide = preparePlexLibrary.new Peptide("nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)", "nDLEEDHACIPIKK".length());
    Peptide complementaryPeptide = peptide.getComplementaryPeptide(heavyLabels, lightLabels);
    Peptide expectedComplementaryPeptide = preparePlexLibrary.new Peptide("nDLEEDHAC(UniMod:4)IPIKK", "nDLEEDHACIPIKK".length());
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);

    peptide = preparePlexLibrary.new Peptide("nDLC(UniMod:4)IPIK[120.0023]K", "nDLCIPIKK".length());
    complementaryPeptide = peptide.getComplementaryPeptide(lightLabels, mediumLabels);
    expectedComplementaryPeptide = preparePlexLibrary.new Peptide("n[4.025107]DLC(UniMod:4)IPIK[120.0023]K(UniMod:481)", "nDLCIPIKK".length());
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);

    peptide = preparePlexLibrary.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)", "nDLEEDHACIPKK".length());
    complementaryPeptide = peptide.getComplementaryPeptide(heavyLabels, lightLabels);
    expectedComplementaryPeptide = preparePlexLibrary.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK", "nDLEEDHACIPKK".length());
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);

    peptide = preparePlexLibrary.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]", "nDLDHACIIK".length());
    complementaryPeptide = peptide.getComplementaryPeptide(mediumLabels, heavyLabels);
    expectedComplementaryPeptide = preparePlexLibrary.new Peptide("n(UniMod:259)DLDHAC(UniMod:4)IIK[140.3424]", "nDLDHACIIK".length());
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);
  }

  @Test
  public void testGetUnimodPeptide() {
    Peptide peptide = preparePlexLibrary.new Peptide("nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)", "nDLEEDHACIPIKK".length());
    String outString = peptide.getUnimodPeptide();
    assertEquals("DLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)", outString);

    peptide = preparePlexLibrary.new Peptide("nDLC[160.030]IPIK[120.0023]K", "nDLCIPIKK".length());
    outString = peptide.getUnimodPeptide();
    assertEquals("DLC(UniMod:4)IPIK[120.0023]K", outString);

    peptide = preparePlexLibrary.new Peptide("n[42.010569]DLEEDHAC[160.030658]IPKK[136.109159]", "nDLEEDHACIPKK".length());
    outString = peptide.getUnimodPeptide();
    assertEquals("(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)", outString);

    peptide = preparePlexLibrary.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[132.120067]", "nDLDHACIIK".length());
    outString = peptide.getUnimodPeptide();
    assertEquals("[4.025107]DLDHAC(UniMod:4)IIK(UniMod:481)", outString);
  }

  @Test
  public void testDetectLabelTypes() {
    Peptide peptide = preparePlexLibrary.new Peptide("n(UniMod:259)DLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)", "nDLEEDHACIPIKK".length());
    assertEquals(Integer.valueOf(3), peptide.detectLabelTypes());

    peptide = preparePlexLibrary.new Peptide("nDLC(UniMod:4)IPIK[120.0023]K", "nDLCIPIKK".length());
    assertEquals(Integer.valueOf(1), peptide.detectLabelTypes());

    peptide = preparePlexLibrary.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:481)", "nDLEEDHACIPKK".length());
    assertEquals(Integer.valueOf(0), peptide.detectLabelTypes());

    peptide = preparePlexLibrary.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]", "nDLDHACIIK".length());
    assertEquals(Integer.valueOf(2), peptide.detectLabelTypes());
  }
}