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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import com.dmtavt.fragpipe.tools.skyline.WriteSkyMods.Mod;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;

public class WriteSkyModsTest {

  @Test
  public void testConvertMods() {
    List<Mod> mods = convertMods("[^", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    Mod mod = mods.get(0);
    assertEquals("n_10.0", mod.name);
    assertEquals("", mod.aa);
    assertEquals('N', mod.terminus);

    mods = convertMods("n^nEcC", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("n_10.0", mod.name);
    assertEquals("", mod.aa);
    assertEquals('N', mod.terminus);
    mod = mods.get(1);
    assertEquals("E_10.0", mod.name);
    assertEquals("E", mod.aa);
    assertEquals('N', mod.terminus);
    mod = mods.get(2);
    assertEquals("C_10.0", mod.name);
    assertEquals("C", mod.aa);
    assertEquals('C', mod.terminus);

    mods = convertMods("[RF", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("R_10.0", mod.name);
    assertEquals("R", mod.aa);
    assertEquals('N', mod.terminus);
    mod = mods.get(1);
    assertEquals("F_10.0", mod.name);
    assertEquals("F", mod.aa);
    assertEquals('\0', mod.terminus);

    mods = convertMods("KcH]E", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("HE_10.0", mod.name);
    assertEquals("H, E", mod.aa);
    assertEquals('C', mod.terminus);
    mod = mods.get(1);
    assertEquals("K_10.0", mod.name);
    assertEquals("K", mod.aa);
    assertEquals('\0', mod.terminus);

    mods = convertMods("KcH[E", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("E_10.0", mod.name);
    assertEquals("E", mod.aa);
    assertEquals('N', mod.terminus);
    mod = mods.get(1);
    assertEquals("H_10.0", mod.name);
    assertEquals("H", mod.aa);
    assertEquals('C', mod.terminus);
    mod = mods.get(2);
    assertEquals("K_10.0", mod.name);
    assertEquals("K", mod.aa);
    assertEquals('\0', mod.terminus);

    mods = convertMods("[*", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("ACDEFGHIKLMNPQRSTVWY_10.0", mod.name);
    assertEquals("A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y", mod.aa);
    assertEquals('N', mod.terminus);

    mods = convertMods("R*", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("ACDEFGHIKLMNPQRSTVWY_10.0", mod.name);
    assertEquals("A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y", mod.aa);
    assertEquals('\0', mod.terminus);

    mods = convertMods("all", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("ACDEFGHIKLMNPQRSTVWY_10.0", mod.name);
    assertEquals("A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y", mod.aa);
    assertEquals('\0', mod.terminus);

    mods = convertMods("AB-", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    mod = mods.get(0);
    assertEquals("AB_10.0", mod.name);
    assertEquals("A, B", mod.aa);
    assertEquals('\0', mod.terminus);
  }

  @Test
  public void testUnimod() {
    Mod mod = new Mod("Oxidation", "M", 'C', true, 15.994915f, 15.9949f, new ArrayList<>(0), new ArrayList<>(0), "", new ArrayList<>(0));
    assertEquals(35, mod.unimodData.id);

    mod = new Mod("Oxidation", "M", '\0', true, 15.994915f, 15.994915f, new ArrayList<>(0), new ArrayList<>(0), "", new ArrayList<>(0));
    assertEquals("Oxidation (M)", mod.unimodData.name);

    mod = new Mod("Phospho", "S, T, Y", 'C', true, 79.966331f, 79.966331f, Arrays.asList(97.976896f, 11f), Arrays.asList(97.976896f, 11f), "", new ArrayList<>(0));
    assertNull(mod.unimodData);

    mod = new Mod("Phospho", "S", 'C', true, 79.966331f, 79.966331f, java.util.List.of(97.976896f), java.util.List.of(97.976896f), "", new ArrayList<>(0));
    assertEquals(21, mod.unimodData.id);

    mod = new Mod("Phospho", "S, T", 'C', true, 79.966331f, 79.966331f, Arrays.asList(97.976896f, 11f), Arrays.asList(97.976896f, 11f), "", new ArrayList<>(0));
    assertEquals(21, mod.unimodData.id);
    assertEquals("Phospho (ST)", mod.unimodData.name);

    mod = new Mod("Oxidation", "W, H", '\0', true, 15.994915f, 15.994915f, new ArrayList<>(0), new ArrayList<>(0), "", new ArrayList<>(0));
    assertEquals("Oxidation (HW)", mod.unimodData.name);

    mod = new Mod("Water loss", "E", '\0', true, -18.010565f, -18.010565f, new ArrayList<>(0), new ArrayList<>(0), "", new ArrayList<>(0));
    assertEquals("Water Loss (D, E, S, T)", mod.unimodData.name);

    mod = new Mod("Acetyl", "", 'N', true, 42.010565f, 42.010565f, new ArrayList<>(0), new ArrayList<>(0), "", new ArrayList<>(0));
    assertEquals("Acetyl (N-term)", mod.unimodData.name);

    mod = new Mod("Gln->pyro-Glu", "Q", 'N', true, -17.026549f, -17.026549f, new ArrayList<>(0), new ArrayList<>(0), "", new ArrayList<>(0));
    assertEquals("Gln->pyro-Glu (N-term Q)", mod.unimodData.name);

    mod = new Mod("Glu->pyro-Glu", "E", 'N', true, -18.010565f, -18.010565f, new ArrayList<>(0), new ArrayList<>(0), "", new ArrayList<>(0));
    assertEquals("Glu->pyro-Glu (N-term E)", mod.unimodData.name);
  }
}