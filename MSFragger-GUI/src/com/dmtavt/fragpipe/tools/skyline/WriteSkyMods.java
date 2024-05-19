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

import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.PropsFile;
import umich.ms.glyco.Glycan;
import umich.ms.glyco.GlycanParser;
import umich.ms.glyco.GlycanResidue;
import umich.ms.util.ElementalComposition;

import static com.dmtavt.fragpipe.tools.glyco.GlycoMassLoader.GLYCAN_RESIDUES_NAME;
import static com.dmtavt.fragpipe.tools.skyline.Skyline.getSkylineVersion;

import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class WriteSkyMods {

  private static final Pattern p = Pattern.compile("([\\d.-]+),([^,]+),(true),[\\d-]+;");
  private static final Pattern p1 = Pattern.compile("[n\\[](.)");
  private static final Pattern p2 = Pattern.compile("[c\\]](.)");
  private static final Pattern p3 = Pattern.compile("([\\d.-]+)\\((aa=([^=_();]+)?)?(_d=([\\d., -]+))?(_p=([\\d., -]+))?(_f=([\\d., -]+))?\\)");
  private static final ArrayList<String> allAAs = new ArrayList<>(Arrays.asList("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y"));

  public WriteSkyMods(Path path, PropsFile pf, int modsMode) throws Exception {
    List<Mod> mods = new ArrayList<>(4);

    String fixModStr = pf.getProperty("msfragger.table.fix-mods");
    String varModStr = pf.getProperty("msfragger.table.var-mods");
    String massOffsetStr = pf.getProperty("msfragger.mass_offsets");
    String massOffsetSites = pf.getProperty("msfragger.restrict_deltamass_to");
    String massOffsetRemainders = pf.getProperty("msfragger.remainder_fragment_masses");
    String detailedMassOffsetStr = pf.getProperty("msfragger.mass_offsets_detailed");
    String labileMode = pf.getProperty("msfragger.labile_search_mode");
    boolean isLabile = labileMode.equals("labile") || labileMode.equals("nglycan");
    boolean isOglyco = modsMode == 1;
    boolean isNglyco = modsMode == 2;

    float mass;
    Matcher m;
    if (fixModStr != null && !fixModStr.isEmpty()) {
      m = p.matcher(fixModStr);
      while (m.find()) {
        if (m.group(3).equalsIgnoreCase("true")) {
          mass = Float.parseFloat(m.group(1));
          if (Math.abs(mass) > 0.1) {
            mods.addAll(convertMods(m.group(2), false, mass, mass, new ArrayList<>(0), new ArrayList<>(0)));
          }
        }
      }
    }

    if (varModStr != null && !varModStr.isEmpty()) {
      m = p.matcher(varModStr);
      while (m.find()) {
        if (m.group(3).equalsIgnoreCase("true")) {
          mass = Float.parseFloat(m.group(1));
          if (Math.abs(mass) > 0.1) {
            mods.addAll(convertMods(m.group(2), true, mass, mass, new ArrayList<>(0), new ArrayList<>(0)));
          }
        }
      }
    }

    // Override offsets from MSFragger for glyco searches get correct glycan masses, neutral losses, and elemental compositions
    if (isOglyco) {
      String oglycoList = pf.getProperty("opair.glyco_db");
      mods.addAll(generateGlycoMods(massOffsetSites, oglycoList, 0, new ElementalComposition("")));
    } else if (isNglyco) {
      String nglycoList = pf.getProperty("ptmshepherd.glycodatabase");
      mods.addAll(generateGlycoMods(massOffsetSites, nglycoList, (float) 203.07937, new ElementalComposition("C8H13NO5")));   // hardcoded N-glycan remainder
    } else {
      // non-glyco - use regular method for mass offsets conversion
      if (massOffsetStr != null && !massOffsetStr.isEmpty()) {
        String[] ss = massOffsetStr.split("[\\s/]");
        for (String s : ss) {
          mass = Float.parseFloat(s);
          if (Math.abs(mass) > 0.1) {
            List<Float> lossMonoMasses = new ArrayList<>(0);
            List<Float> lossAvgMasses = new ArrayList<>(0);
            if (!massOffsetRemainders.isEmpty()) {
              String[] splits = massOffsetRemainders.split("[\\s/,]");
              for (String sp : splits) {
                float remainderMass = Float.parseFloat(sp);
                lossMonoMasses.add(mass - remainderMass);
                lossAvgMasses.add(mass - remainderMass);
              }
            } else if (isLabile) {
              // if labile mode and no remainder fragments specified, add entire mod as a neutral loss
              lossMonoMasses.add(mass);
              lossAvgMasses.add(mass);
            }
            mods.addAll(convertMods(massOffsetSites, true, mass, mass, lossMonoMasses, lossAvgMasses));
          }
        }
      }

      if (detailedMassOffsetStr != null && !detailedMassOffsetStr.isEmpty()) {
        m = p3.matcher(detailedMassOffsetStr);
        while(m.find()) {
          mass = Float.parseFloat(m.group(1));
          if (Math.abs(mass) > 0.1) {
            String sites = "*";
            List<Float> lossMonoMasses = new ArrayList<>(0);
            List<Float> lossAvgMasses = new ArrayList<>(0);
            if (m.group(3) != null) {
              sites = m.group(3);
            }
            if (m.group(9) != null) {
              String[] ss = m.group(9).split("[, ]+");
              for (String s : ss) {
                float reminderMass = Float.parseFloat(s);
                lossMonoMasses.add(mass - reminderMass);
                lossAvgMasses.add(mass - reminderMass);
              }
            }
            mods.addAll(convertMods(sites, true, mass, mass, lossMonoMasses, lossAvgMasses));
          }
        }
      }
    }

    BufferedWriter bw = new BufferedWriter(Files.newBufferedWriter(path));
    bw.write("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
        + "<srm_settings format_version=\"23.1\" software_version=\"Skyline (64-bit) " + getSkylineVersion() + "\">\n"
        + "  <settings_summary name=\"Extra Mods\">\n"
        + "    <peptide_settings>\n"
        + "      <peptide_modifications>\n"
        + "        <static_modifications>\n");

    for (Mod mod : mods) {
      bw.write("          <static_modification name=\"" + mod.name +
              (mod.aa.isEmpty() ? "" : "\" aminoacid=\"" + mod.aa) +
              (mod.terminus == '\0' ? "" : "\" terminus=\"" + mod.terminus) +
              "\" variable=\"" + mod.isVariable +
              // Skyline does not allow masses in a mod if the elemental composition is specified
              (mod.elementalComposition.isEmpty() ? "\" massdiff_monoisotopic=\"" + mod.monoMass : "") +
              (mod.elementalComposition.isEmpty() ? "\" massdiff_average=\"" + mod.avgMass : "") +
              (mod.elementalComposition.isEmpty() ? "" : "\" formula=\"" + mod.elementalComposition) +
              "\">\n");
      for (int i = 0; i < mod.lossMonoMasses.size(); i++) {
        bw.write("            <potential_loss massdiff_monoisotopic=\"" + mod.lossMonoMasses.get(i) +
                "\" massdiff_average=\"" + mod.lossAvgMasses.get(i) +
                (mod.lossElementalComposition.isEmpty() ? "" : "\" formula=\"" + mod.lossElementalComposition.get(i)) +
                "\" />\n");
      }
      bw.write("          </static_modification>\n");
    }

    bw.write("        </static_modifications>\n"
        + "      </peptide_modifications>\n"
        + "    </peptide_settings>\n"
        + "  </settings_summary>\n"
        + "</srm_settings>\n");

    bw.close();
  }

  static List<Mod> convertMods(String sites, boolean isVariable, float monoMass, float avgMass, List<Float> lossMonoMasses, List<Float> lossAvgMasses) {
    List<Mod> out = new ArrayList<>(4);
    filterNeutralLosses(lossMonoMasses);
    filterNeutralLosses(lossAvgMasses);

    sites = cleanupSites(sites);

    // N-terminal mods
    Matcher m = p1.matcher(sites);
    ArrayList<String> nSites = new ArrayList<>();
    while (m.find()) {
      if (m.group(1).contentEquals("^")) {
        out.add(new Mod("n_" + monoMass, "", 'N', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>()));
      } else if (m.group(1).contentEquals("*")) {
        nSites = allAAs;
      } else {
        nSites.add(m.group(1));
      }
    }
    if (!nSites.isEmpty()) {
      out.add(new Mod(String.join("", nSites) + "_" + monoMass, String.join(", ", nSites), 'N', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>()));
    }
    sites = p1.matcher(sites).replaceAll("");

    // C-terminal mods
    m = p2.matcher(sites);
    ArrayList<String> cSites = new ArrayList<>();
    while (m.find()) {
      if (m.group(1).contentEquals("^")) {
        out.add(new Mod("c_" + monoMass, "", 'C', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>()));
      } else if (m.group(1).contentEquals("*")) {
        cSites = allAAs;
      } else {
        cSites.add(m.group(1));
      }
    }
    if (!cSites.isEmpty()) {
      out.add(new Mod(String.join("", cSites) + "_" + monoMass, String.join(", ", cSites), 'C', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>()));
    }
    sites = p2.matcher(sites).replaceAll("");

    // sequence mods
    List<String> resSites = new ArrayList<>();
    if (sites.contains("*")) {
      resSites = allAAs;
    } else {
      resSites = sites.chars().mapToObj(c -> String.valueOf((char) c)).collect(Collectors.toList());
    }
    if (!resSites.isEmpty()) {
      out.add(new Mod(String.join("", resSites) + "_" + monoMass, String.join(", ", resSites), '\0', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>()));
    }
    return out;
  }

  /**
   * Read the non-combination glycan list passed to O-Pair search. Generate a mass offset string consisting of
   * the masses of these glycans.
   * @param oglycoList FragPipe glycan database string passed to O-Pair
   * @return mass offset string
   */
  private String overrideOffsetsOPair(String oglycoList) {
    if (oglycoList != null && !oglycoList.isEmpty()) {
      HashMap<String, GlycanResidue> glycanResidues = GlycanParser.parseGlycoResiduesDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_RESIDUES_NAME).toString());
      ArrayList<Glycan> parsedGlycans = GlycanParser.parseGlycanDatabaseString(oglycoList, glycanResidues);
      StringBuilder sb = new StringBuilder();
      for (Glycan g : parsedGlycans) {
        sb.append(g.mass);
        sb.append(" ");
      }
      return sb.toString();
    }
    return "";
  }

  private List<Mod> generateGlycoMods(String sites, String glycoList, float remainderMass, ElementalComposition remainderComposition) {
    ArrayList<Mod> mods = new ArrayList<>();
    sites = cleanupSites(sites);

    if (glycoList != null && !glycoList.isEmpty()) {
      HashMap<String, GlycanResidue> glycanResidues = GlycanParser.parseGlycoResiduesDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_RESIDUES_NAME).toString());
      ArrayList<Glycan> parsedGlycans = GlycanParser.parseGlycanDatabaseString(glycoList, glycanResidues);
      // sequence mods
      List<String> resSites = new ArrayList<>();
      if (sites.contains("*")) {
        resSites = allAAs;
      } else {
        resSites = sites.chars().mapToObj(c -> String.valueOf((char) c)).collect(Collectors.toList());
      }

      for (Glycan glycan : parsedGlycans) {
        if (!resSites.isEmpty()) {
          ArrayList<Float> losses = new ArrayList<>();
          losses.add((float) glycan.mass - remainderMass);

          ElementalComposition totalElementalComp = glycan.getElementalComposition();
          if (!remainderComposition.composition.isEmpty()) {
            totalElementalComp.addComposition(remainderComposition, true, 1);
          }
          String elementalComp = totalElementalComp.toString();

          ArrayList<String> lossElementalComps = new ArrayList<>();
          lossElementalComps.add(elementalComp);
          mods.add(new Mod(glycan.name, String.join(", ", resSites), '\0', true, (float) glycan.mass, (float) glycan.mass, losses, losses, elementalComp, lossElementalComps));
        }
      }
    }
    return mods;
  }

  /**
   * Skyline does not accept neutral losses >5000 Da. Remove them to avoid a crash
   * @param losses
   * @return
   */
  private static List<Float> filterNeutralLosses(List<Float> losses) {
    losses.removeIf(loss -> loss > 5000);
    return losses;
  }

  private static String cleanupSites(String sites) {
    if (sites.contains(" ")) {
      sites = sites.substring(0, sites.indexOf(" "));
    }

    if (sites.contains("all")) {
      sites = "*";
    }
    sites = sites.replaceAll("-", "");
    return sites;
  }


  static class Mod {

    public final String name;
    public final String aa;
    public final char terminus;
    public final boolean isVariable;
    public final float monoMass;
    public final float avgMass;
    public final List<Float> lossMonoMasses;
    public final List<Float> lossAvgMasses;
    public final String elementalComposition;
    public final List<String> lossElementalComposition;

    Mod(String name, String aa, char terminus, boolean isVariable, float monoMass, float avgMass, List<Float> lossMonoMasses, List<Float> lossAvgMasses, String elementalComposition, List<String> lossElementalComposition) {
      this.name = name;
      this.aa = aa;
      this.terminus = terminus;
      this.isVariable = isVariable;
      this.monoMass = monoMass;
      this.avgMass = avgMass;
      this.lossMonoMasses = lossMonoMasses;
      this.lossAvgMasses = lossAvgMasses;
      this.elementalComposition = elementalComposition;
      this.lossElementalComposition = lossElementalComposition;
    }

    public String toString() {
      return name + " " + aa + " " + terminus + " " + isVariable + " " + monoMass + " " + avgMass + " " + lossMonoMasses + " " + lossAvgMasses + " " + elementalComposition;
    }
  }
}
