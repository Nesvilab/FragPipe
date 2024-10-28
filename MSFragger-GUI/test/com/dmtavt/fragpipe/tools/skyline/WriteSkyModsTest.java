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

package com.dmtavt.fragpipe.tools.skyline;

import static com.dmtavt.fragpipe.tools.skyline.WriteSkyMods.convertMods;
import static com.dmtavt.fragpipe.tools.skyline.WriteSkyMods.parseMods;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.dmtavt.fragpipe.tools.skyline.WriteSkyMods.Mod;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.junit.Test;

public class WriteSkyModsTest {

  @Test
  public void testConvertMods() {
    Set<UnimodData> unimods = new TreeSet<>();
    List<Mod> nonUnimods = new ArrayList<>(1);
    convertMods("[^", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    Mod mod = nonUnimods.get(0);
    assertEquals("_N_true_10.0", mod.name);
    assertEquals("", mod.aas);
    assertEquals('N', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("n^nEcC", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("_N_true_10.0", mod.name);
    assertTrue(unimods.isEmpty());
    assertEquals('N', mod.terminus);
    mod = nonUnimods.get(1);
    assertEquals("E_N_true_10.0", mod.name);
    assertEquals("E", mod.aas);
    assertEquals('N', mod.terminus);
    mod = nonUnimods.get(2);
    assertEquals("C_C_true_10.0", mod.name);
    assertEquals("C", mod.aas);
    assertEquals('C', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("[RF", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("R_N_true_10.0", mod.name);
    assertEquals("R", mod.aas);
    assertEquals('N', mod.terminus);
    mod = nonUnimods.get(1);
    assertEquals("F__true_10.0", mod.name);
    assertEquals("F", mod.aas);
    assertEquals('\0', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("KcH]E", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("E,H_C_true_10.0", mod.name);
    assertEquals("E,H", mod.aas);
    assertEquals('C', mod.terminus);
    mod = nonUnimods.get(1);
    assertEquals("K__true_10.0", mod.name);
    assertEquals("K", mod.aas);
    assertEquals('\0', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("KcH[E", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("E_N_true_10.0", mod.name);
    assertEquals("E", mod.aas);
    assertEquals('N', mod.terminus);
    mod = nonUnimods.get(1);
    assertEquals("H_C_true_10.0", mod.name);
    assertEquals("H", mod.aas);
    assertEquals('C', mod.terminus);
    mod = nonUnimods.get(2);
    assertEquals("K__true_10.0", mod.name);
    assertEquals("K", mod.aas);
    assertEquals('\0', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("[*", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("A,C,D,E,F,G,H,I,K,L,M,N,P,Q,R,S,T,V,W,Y_N_true_10.0", mod.name);
    assertEquals("A,C,D,E,F,G,H,I,K,L,M,N,P,Q,R,S,T,V,W,Y", mod.aas);
    assertEquals('N', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("R*", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("A,C,D,E,F,G,H,I,K,L,M,N,P,Q,R,S,T,V,W,Y__true_10.0", mod.name);
    assertEquals("A,C,D,E,F,G,H,I,K,L,M,N,P,Q,R,S,T,V,W,Y", mod.aas);
    assertEquals('\0', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("all", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("A,C,D,E,F,G,H,I,K,L,M,N,P,Q,R,S,T,V,W,Y__true_10.0", mod.name);
    assertEquals("A,C,D,E,F,G,H,I,K,L,M,N,P,Q,R,S,T,V,W,Y", mod.aas);
    assertEquals('\0', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    convertMods("AB-", true, 10, 10, new ArrayList<>(1), new ArrayList<>(1), true, unimods, nonUnimods);
    mod = nonUnimods.get(0);
    assertEquals("A,B__true_10.0", mod.name);
    assertEquals("A,B", mod.aas);
    assertEquals('\0', mod.terminus);

    unimods.clear();
    nonUnimods.clear();
    parseMods("C", '\0', true, 57.02146f, 57.02146f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertTrue(unimods.isEmpty());
    assertEquals(1, nonUnimods.size());
    assertEquals("C__true_57.02146", nonUnimods.iterator().next().name);
    assertEquals("C", nonUnimods.iterator().next().aas);
    assertEquals('\0', nonUnimods.iterator().next().terminus);
  }

  @Test
  public void testUnimod() {
    Set<UnimodData> unimods = new TreeSet<>();
    List<Mod> nonUnimods = new ArrayList<>(1);

    parseMods("M", 'C', true, 15.994915f, 15.9949f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals(35, unimods.iterator().next().id);

    unimods.clear();
    nonUnimods.clear();
    parseMods("M", '\0', true, 15.994915f, 15.994915f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals("Oxidation (M)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("S, T, Y", 'C', true, 79.966331f, 79.966331f, Arrays.asList(97.976896f, 11f), Arrays.asList(97.976896f, 11f), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(2, unimods.size());
    assertEquals("Phospho (ST)", unimods.toArray(new UnimodData[0])[0].name);
    assertEquals("Phospho (Y)", unimods.toArray(new UnimodData[0])[1].name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("S", 'C', true, 79.966331f, 79.966331f, java.util.List.of(97.976896f), java.util.List.of(97.976896f), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals(21, unimods.iterator().next().id);
    assertEquals("Phospho (ST)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("S, T", 'C', true, 79.966331f, 79.966331f, Arrays.asList(97.976896f, 11f), Arrays.asList(97.976896f, 11f), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals(21, unimods.iterator().next().id);
    assertEquals("Phospho (ST)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("W, H", '\0', true, 15.994915f, 15.994915f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals("Oxidation (HW)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("E", '\0', true, -18.010565f, -18.010565f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals("Glu->pyro-Glu (N-term E)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("", 'N', true, 42.010565f, 42.010565f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals("Acetyl (N-term)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("Q", 'N', true, -17.026549f, -17.026549f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals("Gln->pyro-Glu (N-term Q)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("E", 'N', true, -18.010565f, -18.010565f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(1, unimods.size());
    assertEquals("Glu->pyro-Glu (N-term E)", unimods.iterator().next().name);

    unimods.clear();
    nonUnimods.clear();
    parseMods("C", '\0', false, 57.02146f, 57.02146f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertTrue(nonUnimods.isEmpty());
    assertEquals(1, unimods.size());
    assertEquals("Carbamidomethyl (C)", unimods.iterator().next().name);
  }

  @Test
  public void testHybrid() {
    Set<UnimodData> unimods = new TreeSet<>();
    List<Mod> nonUnimods = new ArrayList<>(1);

    parseMods("M, C, J, O", '\0', true, 15.994915f, 15.994915f, new ArrayList<>(1), new ArrayList<>(1), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(2, unimods.size());
    assertEquals("Oxidation (M)",  unimods.toArray(new UnimodData[0])[0].name);
    assertEquals("Oxidation (C)",  unimods.toArray(new UnimodData[0])[1].name);
    assertEquals(1, nonUnimods.size());
    Mod mod = nonUnimods.get(0);
    assertEquals("J,O__true_15.994915", mod.name);
    assertEquals("J,O", mod.aas);

    unimods.clear();
    nonUnimods.clear();
    parseMods("S, T, Y, M", 'C', true, 79.966331f, 79.966331f, Arrays.asList(97.976896f, 11f), Arrays.asList(97.976896f, 11f), "", new ArrayList<>(1), true, unimods, nonUnimods);
    assertEquals(2, unimods.size());
    assertEquals("Phospho (ST)", unimods.toArray(new UnimodData[0])[0].name);
    assertEquals("Phospho (Y)", unimods.toArray(new UnimodData[0])[1].name);
    assertEquals(1, nonUnimods.size());
    mod = nonUnimods.get(0);
    assertEquals("M_C_true_79.96633", mod.name);
    assertEquals("M", mod.aas);
  }
}