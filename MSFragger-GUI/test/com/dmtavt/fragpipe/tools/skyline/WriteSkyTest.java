package com.dmtavt.fragpipe.tools.skyline;

import static com.dmtavt.fragpipe.tools.skyline.WriteSky.convertMods;
import static org.junit.Assert.assertEquals;

import com.dmtavt.fragpipe.tools.skyline.WriteSky.Mod;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class WriteSkyTest {

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
    assertEquals("H_10.0", mod.name);
    assertEquals("H", mod.aa);
    assertEquals('C', mod.terminus);
    mod = mods.get(1);
    assertEquals("E_10.0", mod.name);
    assertEquals("E", mod.aa);
    assertEquals('C', mod.terminus);
    mod = mods.get(2);
    assertEquals("K_10.0", mod.name);
    assertEquals("K", mod.aa);
    assertEquals('\0', mod.terminus);

    mods = convertMods("[*", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    assertEquals(20, mods.size());
    mod = mods.get(0);
    assertEquals("A_10.0", mod.name);
    assertEquals("A", mod.aa);
    assertEquals('N', mod.terminus);
    mod = mods.get(1);
    assertEquals("C_10.0", mod.name);
    assertEquals("C", mod.aa);
    assertEquals('N', mod.terminus);
    mod = mods.get(2);
    assertEquals("D_10.0", mod.name);
    assertEquals("D", mod.aa);
    assertEquals('N', mod.terminus);

    mods = convertMods("R*", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    assertEquals(21, mods.size());
    mod = mods.get(0);
    assertEquals("R_10.0", mod.name);
    assertEquals("R", mod.aa);
    assertEquals('\0', mod.terminus);
    mod = mods.get(1);
    assertEquals("A_10.0", mod.name);
    assertEquals("A", mod.aa);
    assertEquals('\0', mod.terminus);
    mod = mods.get(2);
    assertEquals("C_10.0", mod.name);
    assertEquals("C", mod.aa);
    assertEquals('\0', mod.terminus);

    mods = convertMods("all", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    assertEquals(20, mods.size());
    mod = mods.get(0);
    assertEquals("A_10.0", mod.name);
    assertEquals("A", mod.aa);
    assertEquals('\0', mod.terminus);
    mod = mods.get(1);
    assertEquals("C_10.0", mod.name);
    assertEquals("C", mod.aa);
    assertEquals('\0', mod.terminus);


    mods = convertMods("AB-", true, 10, 10, new ArrayList<>(0), new ArrayList<>(0));
    assertEquals(2, mods.size());
    mod = mods.get(0);
    assertEquals("A_10.0", mod.name);
    assertEquals("A", mod.aa);
    assertEquals('\0', mod.terminus);
    mod = mods.get(1);
    assertEquals("B_10.0", mod.name);
    assertEquals("B", mod.aa);
    assertEquals('\0', mod.terminus);
  }
}