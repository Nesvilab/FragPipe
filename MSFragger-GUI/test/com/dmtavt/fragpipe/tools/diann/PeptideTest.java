package com.dmtavt.fragpipe.tools.diann;

import static com.dmtavt.fragpipe.util.Utils.removeClosedModifications;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import com.dmtavt.fragpipe.tools.diann.PlexDiaHelper.Peptide;
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

  private PlexDiaHelper plexDiaHelper;
  private final Map<Character, Float> lightLabels = new HashMap<>();
  private final Map<Character, Float> mediumLabels = new HashMap<>();
  private final Map<Character, Float> heavyLabels = new HashMap<>();

  @Before
  public void setUp() throws Exception {
    Path libraryPath = Paths.get(Objects.requireNonNull(PlexDiaHelperTest.class.getResource("/library_1.tsv")).toURI());
    if (!Files.exists(libraryPath) || !Files.isRegularFile(libraryPath) || !Files.isReadable(libraryPath)) {
      throw new IllegalStateException("Test library file not found: " + libraryPath);
    }

    lightLabels.put('K', 0f);
    lightLabels.put('n', 0f);
    mediumLabels.put('K', 4.025107f);
    mediumLabels.put('n', 4.025107f);
    heavyLabels.put('K', 8.014199f);
    heavyLabels.put('n', 8.014199f);

    String[] modifiedPeptides = new String[]{
        "nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)",
        "nDLC(UniMod:4)IPIK[120.0023]K",
        "n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)",
        "n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]"
    };

    plexDiaHelper = new PlexDiaHelper(11, lightLabels, mediumLabels, heavyLabels);

    Method collectAllModsMethod = PlexDiaHelper.class.getDeclaredMethod("collectAllMods", Set.class);
    collectAllModsMethod.setAccessible(true);
    @SuppressWarnings("unchecked") Set<Float> modMasses = (Set<Float>) collectAllModsMethod.invoke(plexDiaHelper, new HashSet<>(Arrays.asList(modifiedPeptides)));
    plexDiaHelper.theoModMasses = removeClosedModifications(modMasses);
  }

  @Test
  public void testPeptide1() {
    Peptide peptide = plexDiaHelper.new Peptide("nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)");
    assertEquals("DLEEDHAC[57.021465]IPIK[8.014199]K[8.014199]", peptide.modifiedPeptide);
    assertEquals("DLEEDHACIPIKK", peptide.peptideSequence);
    assertEquals("DLEEDHACIPIKK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{0, 0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f, 8.014199f}, peptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("nDLC(UniMod:4)IPIK[120.0023]K");
    assertEquals("DLC[57.021465]IPIK[-8.092659]K", peptide.modifiedPeptide);
    assertEquals("DLCIPIKK", peptide.peptideSequence);
    assertEquals("DLCIPIKK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{0, 0, 0, 57.021465f, 0, 0, 0, -8.092659f, 0}, peptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)");
    assertEquals("n[42.010567]DLEEDHAC[57.021465]IPKK[8.014199]", peptide.modifiedPeptide);
    assertEquals("DLEEDHACIPKK", peptide.peptideSequence);
    assertEquals("DLEEDHACIPKK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{42.010567f, 0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f}, peptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]");
    assertEquals("n[4.025107]DLDHAC[57.021465]IIK[12.247452]", peptide.modifiedPeptide);
    assertEquals("DLDHACIIK", peptide.peptideSequence);
    assertEquals("DLDHACIIK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{4.025107f, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 12.247452f}, peptide.modMasses, 0.0001f);
  }

  @Test
  public void testPeptide2() {
    Peptide peptide = plexDiaHelper.new Peptide("DLEEDHACIPIKK", new float[]{0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f, 8.014199f});
    assertEquals("DLEEDHAC[57.021465]IPIK[8.014199]K[8.014199]", peptide.modifiedPeptide);
    assertEquals("DLEEDHACIPIKK", peptide.peptideSequence);
    assertEquals("DLEEDHACIPIKK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{0, 0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f, 8.014199f}, peptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("DLCIPIKK", new float[]{0, 0, 57.021465f, 0, 0, 0, -8.092659f, 0});
    assertEquals("DLC[57.021465]IPIK[-8.092659]K", peptide.modifiedPeptide);
    assertEquals("DLCIPIKK", peptide.peptideSequence);
    assertEquals("DLCIPIKK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{0, 0, 0, 57.021465f, 0, 0, 0, -8.092659f, 0}, peptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("nDLEEDHACIPKK", new float[]{42.010567f, 0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f});
    assertEquals("n[42.010567]DLEEDHAC[57.021465]IPKK[8.014199]", peptide.modifiedPeptide);
    assertEquals("DLEEDHACIPKK", peptide.peptideSequence);
    assertEquals("DLEEDHACIPKK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{42.010567f, 0, 0, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 0, 8.014199f}, peptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("nDLDHACIIK", new float[]{4.025107f, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 12.247452f});
    assertEquals("n[4.025107]DLDHAC[57.021465]IIK[12.247452]", peptide.modifiedPeptide);
    assertEquals("DLDHACIIK", peptide.peptideSequence);
    assertEquals("DLDHACIIK".length(), peptide.peptideLength);
    assertArrayEquals(new float[]{4.025107f, 0, 0, 0, 0, 0, 57.021465f, 0, 0, 12.247452f}, peptide.modMasses, 0.0001f);
  }

  @Test
  public void testGetComplementaryPeptide() {
    Peptide peptide = plexDiaHelper.new Peptide("nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)");
    Peptide complementaryPeptide = peptide.getComplementaryPeptide(heavyLabels, lightLabels);
    Peptide expectedComplementaryPeptide = plexDiaHelper.new Peptide("nDLEEDHAC(UniMod:4)IPIKK");
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("nDLC(UniMod:4)IPIK[120.0023]K");
    complementaryPeptide = peptide.getComplementaryPeptide(lightLabels, mediumLabels);
    expectedComplementaryPeptide = plexDiaHelper.new Peptide("n[4.025107]DLC(UniMod:4)IPIK[120.0023]K(UniMod:481)");
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)");
    complementaryPeptide = peptide.getComplementaryPeptide(heavyLabels, lightLabels);
    expectedComplementaryPeptide = plexDiaHelper.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK");
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);

    peptide = plexDiaHelper.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]");
    complementaryPeptide = peptide.getComplementaryPeptide(mediumLabels, heavyLabels);
    expectedComplementaryPeptide = plexDiaHelper.new Peptide("n(UniMod:259)DLDHAC(UniMod:4)IIK[140.3424]");
    assertEquals(expectedComplementaryPeptide.modifiedPeptide, complementaryPeptide.modifiedPeptide);
    assertEquals(expectedComplementaryPeptide.peptideSequence, complementaryPeptide.peptideSequence);
    assertArrayEquals(expectedComplementaryPeptide.modMasses, complementaryPeptide.modMasses, 0.0001f);
  }

  @Test
  public void testGetUnimodPeptide() {
    Peptide peptide = plexDiaHelper.new Peptide("nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)");
    String outString = peptide.getUnimodPeptide(false);
    assertEquals("DLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)", outString);

    peptide = plexDiaHelper.new Peptide("nDLC[160.030]IPIK[120.0023]K");
    outString = peptide.getUnimodPeptide(false);
    assertEquals("DLC(UniMod:4)IPIK[120.0023]K", outString);

    peptide = plexDiaHelper.new Peptide("[42.010569]DLEEDHAC[160.030658]IPKK[136.109159]");
    outString = peptide.getUnimodPeptide(false);
    assertEquals("(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:259)", outString);

    peptide = plexDiaHelper.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[132.120067]");
    outString = peptide.getUnimodPeptide(false);
    assertEquals("[4.025107]DLDHAC(UniMod:4)IIK(UniMod:481)", outString);
  }

  @Test
  public void testGetUnimodPeptide2() {
    Peptide peptide = plexDiaHelper.new Peptide("nDLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)");
    String outString = peptide.getUnimodPeptide(true);
    assertEquals("[label]DLEEDHAC(UniMod:4)IPIK[label_H]K[label_H]", outString);

    peptide = plexDiaHelper.new Peptide("nDLC[160.030]IPIK[120.0023]K");
    outString = peptide.getUnimodPeptide(true);
    assertEquals("[label]DLC(UniMod:4)IPIK[120.0023]K[label]", outString);

    peptide = plexDiaHelper.new Peptide("[42.010569]DLEEDHAC[160.030658]IPKK[136.109159]");
    outString = peptide.getUnimodPeptide(true);
    assertEquals("(UniMod:1)DLEEDHAC(UniMod:4)IPK[label]K[label_H]", outString);

    peptide = plexDiaHelper.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[132.120067]");
    outString = peptide.getUnimodPeptide(true);
    assertEquals("[label_M]DLDHAC(UniMod:4)IIK[label_M]", outString);
  }

  @Test
  public void testDetectLabelTypes() {
    Peptide peptide = plexDiaHelper.new Peptide("n(UniMod:259)DLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)");
    assertEquals(Integer.valueOf(3), peptide.detectLabelTypes());

    peptide = plexDiaHelper.new Peptide("nDLC(UniMod:4)IPIK[120.0023]K");
    assertEquals(Integer.valueOf(1), peptide.detectLabelTypes());

    peptide = plexDiaHelper.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:481)");
    assertEquals(Integer.valueOf(0), peptide.detectLabelTypes());

    peptide = plexDiaHelper.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]");
    assertEquals(Integer.valueOf(2), peptide.detectLabelTypes());
  }

  @Test
  public void testGetLabelCount() {
    Peptide peptide = plexDiaHelper.new Peptide("n(UniMod:259)DLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)");
    assertEquals(3, peptide.getLabelCount());

    peptide = plexDiaHelper.new Peptide("nDLC(UniMod:4)IPIK[120.0023]K");
    assertEquals(2, peptide.getLabelCount());

    peptide = plexDiaHelper.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:481)");
    assertEquals(0, peptide.getLabelCount());

    peptide = plexDiaHelper.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]");
    assertEquals(1, peptide.getLabelCount());
  }

  @Test
  public void testGetLabelFreePeptide() {
    Peptide peptide = plexDiaHelper.new Peptide("n(UniMod:259)DLEEDHAC(UniMod:4)IPIK(UniMod:259)K(UniMod:259)");
    assertEquals("DLEEDHAC[57.021465]IPIKK", peptide.getLabelFreePeptide());

    peptide = plexDiaHelper.new Peptide("nDLC(UniMod:4)IPIK[120.0023]K");
    assertEquals("DLC[57.021465]IPIK[-8.092659]K", peptide.getLabelFreePeptide());

    peptide = plexDiaHelper.new Peptide("n(UniMod:1)DLEEDHAC(UniMod:4)IPKK(UniMod:481)");
    assertEquals("n[42.010567]DLEEDHAC[57.021465]IPKK[4.025107]", peptide.getLabelFreePeptide());

    peptide = plexDiaHelper.new Peptide("n[4.025107]DLDHAC(UniMod:4)IIK[140.3424]");
    assertEquals("DLDHAC[57.021465]IIK[12.247452]", peptide.getLabelFreePeptide());
  }
}