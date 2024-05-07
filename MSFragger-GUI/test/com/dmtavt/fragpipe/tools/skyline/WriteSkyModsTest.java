package com.dmtavt.fragpipe.tools.skyline;

import static com.dmtavt.fragpipe.tools.skyline.WriteSkyMods.convertMods;
import static org.junit.Assert.assertEquals;

import com.dmtavt.fragpipe.tools.skyline.WriteSkyMods.Mod;
import java.util.ArrayList;
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
}