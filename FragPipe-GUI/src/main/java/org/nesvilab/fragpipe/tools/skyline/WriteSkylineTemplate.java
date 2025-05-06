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

package org.nesvilab.fragpipe.tools.skyline;


import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;
import static org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader.GLYCAN_MODS_NAME;
import static org.nesvilab.fragpipe.tools.glyco.GlycoMassLoader.GLYCAN_RESIDUES_NAME;
import static org.nesvilab.fragpipe.tools.skyline.Skyline.getSkylineVersion;

import com.google.common.collect.TreeMultimap;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.PropsFile;
import umich.ms.glyco.Glycan;
import umich.ms.glyco.GlycanMod;
import umich.ms.glyco.GlycanParser;
import umich.ms.glyco.GlycanResidue;
import umich.ms.util.ElementalComposition;

public class WriteSkylineTemplate {

  private static final Pattern p = Pattern.compile("([\\d.-]+),([^,]+),(true),[\\d-]+;");
  private static final Pattern p1 = Pattern.compile("[n\\[](.)");
  private static final Pattern p2 = Pattern.compile("[c\\]](.)");
  private static final Pattern p3 = Pattern.compile("([\\d.-]+)\\((aa\\\\=([^=_();]+)?)?(_d\\\\=([\\d., -]+))?(_p\\\\=([\\d., -]+))?(_f\\\\=([\\d., -]+))?\\)");
  private static final ArrayList<String> allAAs = new ArrayList<>(Arrays.asList("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y"));
  private static final float smallFloat = 0.001f;
  private static List<UnimodData> defaultUnimods;
  private static List<UnimodData> skylineHardcodedUnimods;
  private static TreeMultimap<Float, UnimodData> massToUnimod;

  public final Set<UnimodData> unimodMods = new TreeSet<>();
  public final List<Mod> nonUnimodMods = new ArrayList<>(1);

  public boolean needTemplate = false;

  static {
    final List<Path> tt = FragpipeLocations.checkToolsMissing(Seq.of("UniModData.tsv"));
    if (tt == null || tt.size() != 1) {
      System.err.println("Could not find UniModData.tsv from " + FragpipeLocations.get().getDirTools());
      System.exit(1);
    }

    try {
      UnimodDataReader unimodDataReader = new UnimodDataReader(tt.get(0));
      defaultUnimods = unimodDataReader.defaultUnimods;
      skylineHardcodedUnimods = unimodDataReader.skylineHardcodedUnimods;
      massToUnimod = unimodDataReader.massToUnimod;
    } catch (Exception ex) {
      ex.printStackTrace();
      System.exit(1);
    }
  }

  public WriteSkylineTemplate(Path path, PropsFile pf, int modsMode, boolean matchUnimod, boolean isSSL, Map<Float, Set<String>> addedMods, boolean hasCv) throws Exception {
    String fixModStr = pf.getProperty("msfragger.table.fix-mods");
    String varModStr = pf.getProperty("msfragger.table.var-mods");
    String massOffsetStr = pf.getProperty("msfragger.mass_offsets");
    String massOffsetSites = pf.getProperty("msfragger.restrict_deltamass_to");
    String massOffsetRemainders = pf.getProperty("msfragger.remainder_fragment_masses");
    String detailedMassOffsetStr = pf.getProperty("msfragger.mass_offsets_detailed");
    boolean useDetailedMassOffsets = pf.containsKey("msfragger.use_detailed_offsets") && Boolean.parseBoolean(pf.getProperty("msfragger.use_detailed_offsets"));
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
            convertMods(m.group(2), false, mass, mass, new ArrayList<>(1), new ArrayList<>(1), matchUnimod, unimodMods, nonUnimodMods);
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
            convertMods(m.group(2), true, mass, mass, new ArrayList<>(1), new ArrayList<>(1), matchUnimod, unimodMods, nonUnimodMods);
          }
        }
      }
    }

    // add any combined mods (multiple at one site) found during peptide list generation
    for (Map.Entry<Float, Set<String>> entry : addedMods.entrySet()) {
      mass = entry.getKey();
      convertMods(String.join("", entry.getValue()), true, mass, mass, new ArrayList<>(), new ArrayList<>(), false, unimodMods, nonUnimodMods);
    }

    // Override offsets from MSFragger for glyco searches get correct glycan masses, neutral losses, and elemental compositions
    // also override unimod matching because it doesn't have the glyco neutral losses
    if (isOglyco) {
      String oglycoList = pf.getProperty("opair.glyco_db");
      generateGlycoMods(massOffsetSites, oglycoList, 0, new ElementalComposition(""), false, unimodMods, nonUnimodMods);
    } else if (isNglyco) {
      String nglycoList = pf.getProperty("ptmshepherd.glycodatabase");
      generateGlycoMods(massOffsetSites, nglycoList, (float) 203.07937, new ElementalComposition("C8H13N1O5"), false, unimodMods, nonUnimodMods);   // hardcoded N-glycan remainder
    } else {
      // non-glyco - use regular method for mass offsets conversion
      if (!useDetailedMassOffsets && massOffsetStr != null && !massOffsetStr.isEmpty()) {
        String[] ss = massOffsetStr.split("[\\s/]");
        for (String s : ss) {
          mass = Float.parseFloat(s);
          if (Math.abs(mass) > 0.1) {
            List<Float> lossMonoMasses = new ArrayList<>(1);
            List<Float> lossAvgMasses = new ArrayList<>(1);
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
            convertMods(massOffsetSites, true, mass, mass, lossMonoMasses, lossAvgMasses, matchUnimod, unimodMods, nonUnimodMods);
          }
        }
      }

      if (useDetailedMassOffsets && detailedMassOffsetStr != null && !detailedMassOffsetStr.isEmpty()) {
        m = p3.matcher(detailedMassOffsetStr);
        while(m.find()) {
          mass = Float.parseFloat(m.group(1));
          if (Math.abs(mass) > 0.1) {
            String sites = "*";
            List<Float> lossMonoMasses = new ArrayList<>(1);
            List<Float> lossAvgMasses = new ArrayList<>(1);
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
            convertMods(sites, true, mass, mass, lossMonoMasses, lossAvgMasses, matchUnimod, unimodMods, nonUnimodMods);
          }
        }
      }
    }

    needTemplate = !nonUnimodMods.isEmpty() || hasCv;

    if (needTemplate) {
      BufferedWriter bw = new BufferedWriter(Files.newBufferedWriter(path));
      bw.write("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
          + "<srm_settings format_version=\"23.1\" software_version=\"Skyline (64-bit) " + getSkylineVersion() + "\">\n"
          + "  <settings_summary name=\"" + PROGRAM_TITLE + "\">\n");

      if (!nonUnimodMods.isEmpty()) {
        bw.write("    <peptide_settings>\n"
            + "      <peptide_modifications>\n"
            + "        <static_modifications>\n");

        for (Mod mod : nonUnimodMods) {
          bw.write("          <static_modification name=\"" + mod.name +
              (mod.aas.isEmpty() ? "" : "\" aminoacid=\"" + mod.aas) +
              (mod.terminus == '\0' ? "" : "\" terminus=\"" + mod.terminus) +
              "\" variable=\"" + mod.isVariable +
              // Skyline does not allow masses in a mod if the elemental composition is specified
              (mod.elementalComposition.isEmpty() ? "\" massdiff_monoisotopic=\"" + mod.monoMass : "") +
              (mod.elementalComposition.isEmpty() ? "\" massdiff_average=\"" + mod.avgMass : "") +
              (mod.elementalComposition.isEmpty() ? "" : "\" formula=\"" + mod.elementalComposition) +
              "\">\n");
          String alwaysStr = isSSL ? "\" inclusion=\"Always" : "";  // Add NLs always for SSL list import, otherwise use library
          for (int i = 0; i < mod.lossMonoMasses.size(); i++) {
            bw.write("            <potential_loss massdiff_monoisotopic=\"" + mod.lossMonoMasses.get(i) +
                "\" massdiff_average=\"" + mod.lossAvgMasses.get(i) +
                (mod.lossElementalComposition.isEmpty() ? "" : "\" formula=\"" + mod.lossElementalComposition.get(i)) +
                alwaysStr +
                "\" />\n");
          }
          bw.write("          </static_modification>\n");
        }

        bw.write("        </static_modifications>\n"
            + "      </peptide_modifications>\n"
            + "    </peptide_settings>\n");
      }

      if (hasCv) {
        bw.write("    <transition_settings>\n"
            + "      <ion_mobility_filtering use_spectral_library_ion_mobility_values=\"true\" window_width_calc_type=\"fixed_width\" fixed_width=\"1\" />\n"
            + "    </transition_settings>\n");
      }

      bw.write("  </settings_summary>\n"
          + "</srm_settings>\n");

      bw.close();
    }
  }

  static void convertMods(String sites, boolean isVariable, float monoMass, float avgMass, List<Float> lossMonoMasses, List<Float> lossAvgMasses, boolean matchUnimod, Set<UnimodData> unimods, List<Mod> nonUnimods) {
    filterNeutralLosses(lossMonoMasses);
    filterNeutralLosses(lossAvgMasses);

    sites = cleanupSites(sites);

    // N-terminal mods
    Matcher m = p1.matcher(sites);
    ArrayList<String> nSites = new ArrayList<>();
    while (m.find()) {
      if (m.group(1).contentEquals("^")) {
        parseMods(null, "", 'N', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>(), matchUnimod, unimods, nonUnimods);
      } else if (m.group(1).contentEquals("*")) {
        nSites = allAAs;
      } else {
        nSites.add(m.group(1));
      }
    }
    if (!nSites.isEmpty()) {
      parseMods(null, String.join(", ", nSites), 'N', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>(), matchUnimod, unimods, nonUnimods);
    }
    sites = p1.matcher(sites).replaceAll("");

    // C-terminal mods
    m = p2.matcher(sites);
    ArrayList<String> cSites = new ArrayList<>();
    while (m.find()) {
      if (m.group(1).contentEquals("^")) {
        parseMods(null, "", 'C', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>(), matchUnimod, unimods, nonUnimods);
      } else if (m.group(1).contentEquals("*")) {
        cSites = allAAs;
      } else {
        cSites.add(m.group(1));
      }
    }
    if (!cSites.isEmpty()) {
      parseMods(null, String.join(", ", cSites), 'C', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>(), matchUnimod, unimods, nonUnimods);
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
      parseMods(null, String.join(", ", resSites), '\0', isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, "", new ArrayList<>(), matchUnimod, unimods, nonUnimods);
    }
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

  private void generateGlycoMods(String sites, String glycoList, float remainderMass, ElementalComposition remainderComposition, boolean matchUnimod, Set<UnimodData> unimods, List<Mod> nonUnimods) {
    sites = cleanupSites(sites);

    if (glycoList != null && !glycoList.isEmpty()) {
      // load glycan residue and mod definitions to use in parsing the glycan list
      HashMap<String, GlycanResidue> glycanResidues = GlycanParser.parseGlycoResiduesDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_RESIDUES_NAME).toString());
      HashMap<String, GlycanMod> glycanMods = GlycanParser.parseGlycoModsDB(FragpipeLocations.get().getDirTools().resolve("Glycan_Databases").resolve(GLYCAN_MODS_NAME).toString(), glycanResidues.size(), glycanResidues);
      glycanResidues.putAll(glycanMods);

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
          float neutralLossMass = (float) glycan.mass - remainderMass;
          if (neutralLossMass < 5000 && neutralLossMass != 0) {    // Skyline does not accept neutral losses above 5000 Da
            losses.add(neutralLossMass);
          }

          ArrayList<String> lossElementalComps = new ArrayList<>();
          if (!losses.isEmpty()) {
            ElementalComposition lossElementalComp = glycan.getElementalCompositionOfIon();
            if (!remainderComposition.composition.isEmpty()) {
              if (lossElementalComp.isValidSubtraction(remainderComposition, 1)) {
                lossElementalComp.addComposition(remainderComposition, true, 1);
              }
            }
            lossElementalComps.add(lossElementalComp.toString());
          }

          parseMods(glycan.name, String.join(", ", resSites), '\0', true, (float) glycan.mass, (float) glycan.mass, losses, losses, glycan.getElementalCompositionOfIon().toString(), lossElementalComps, matchUnimod, unimods, nonUnimods);
        }
      }
    }
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
    sites = sites.replace("N-Term Peptide", "n^");
    sites = sites.replace("C-Term Peptide", "c^");
    sites = sites.replace("N-Term Protein", "[^");
    sites = sites.replace("C-Term Protein", "]^");
    sites = sites.replace("N-Term", "n^");
    sites = sites.replace("C-Term", "c^");

    if (sites.contains(" ")) {
      sites = sites.substring(0, sites.indexOf(" "));
    }

    if (sites.contains("all")) {
      sites = "*";
    }
    sites = sites.replaceAll("-", "");
    return sites;
  }

  static void parseMods(String optionalName, String aas, char terminus, boolean isVariable, float monoMass, float avgMass, List<Float> lossMonoMasses, List<Float> lossAvgMasses, String elementalComposition, List<String> lossElementalComposition, boolean matchUnimod, Set<UnimodData> unimods, List<Mod> nonUnimods) {
    Set<String> nonUnimodAas = new TreeSet<>();
    if (matchUnimod) {
      if (aas.isEmpty()) {
        UnimodData unimodData = null;
        for (float f : massToUnimod.keySet().subSet(monoMass - smallFloat, false, monoMass + smallFloat, false)) {
          for (UnimodData m : massToUnimod.get(f)) {
            if (m.aas.isEmpty() && m.terminus == terminus) {
              unimodData = m;
              break;
            }
          }
        }
        if (unimodData == null) {
          nonUnimodAas.add(terminus == 'N' ? "N-term" : "C-term"); // terminal modification placeholder
        } else {
          unimods.add(unimodData);
        }
      } else{
        for (String aa : aas.split(",")) {
          aa = aa.trim();
          UnimodData unimodData = null;
          for (UnimodData m : defaultUnimods) {
            if (Math.abs(monoMass - m.monoMass) < smallFloat && siteMatch(aa, terminus, m, true)) {
              unimodData = m;
              break;
            }
          }

          if (unimodData == null) {
            for (UnimodData m : skylineHardcodedUnimods) {
              if (Math.abs(monoMass - m.monoMass) < smallFloat && siteMatch(aa, terminus, m, true)) {
                unimodData = m;
                break;
              }
            }
          }

          if (unimodData == null) {
            for (float f : massToUnimod.keySet().subSet(monoMass - smallFloat, false, monoMass + smallFloat, false)) {
              for (UnimodData m : massToUnimod.get(f)) {
                if (siteMatch(aa, terminus, m, true)) {
                  if (unimodData == null || m.compareTo(unimodData) < 0) {
                    unimodData = m;
                  }
                }
              }
            }
          }

          if (unimodData == null) {
            for (float f : massToUnimod.keySet().subSet(monoMass - smallFloat, false, monoMass + smallFloat, false)) {
              for (UnimodData m : massToUnimod.get(f)) {
                if (siteMatch(aa, terminus, m, false)) {
                  if (unimodData == null || m.compareTo(unimodData) < 0) {
                    unimodData = m;
                  }
                }
              }
            }
          }

          // Carbamidomethylation only uses the Unimod definition if it is a fixed mod (because Skyline assumes it is fixed if defined as Unimod).
          if (isVariable && aa.contentEquals("C") && Math.abs(monoMass - 57.02146) < smallFloat) {
            unimodData = null;
          }

          if (unimodData == null) {
            nonUnimodAas.add(aa);
          } else {
            unimods.add(unimodData);
          }
        }
      }

      if (!nonUnimodAas.isEmpty()) {
        // The terminal modification doesn't have amino acids.
        nonUnimodAas.remove("N-term");
        nonUnimodAas.remove("C-term");
        nonUnimods.add(new Mod(optionalName, String.join(",", nonUnimodAas), terminus, isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, elementalComposition, lossElementalComposition));
      }
    } else {
      nonUnimods.add(new Mod(optionalName, aas, terminus, isVariable, monoMass, avgMass, lossMonoMasses, lossAvgMasses, elementalComposition, lossElementalComposition));
    }
  }

  private static boolean siteMatch(String aa, char terminus, UnimodData unimodData, boolean matchTerminus) {
    if (unimodData.aas.isEmpty()) {
      return false;
    } else {
      if (matchTerminus && terminus != unimodData.terminus) {
        return false;
      }
      return unimodData.aas.contains(aa.charAt(0));
    }
  }


  static class Mod {

    public final String name;
    public final String aas;
    public final char terminus;
    public final boolean isVariable;
    public final float monoMass;
    public final float avgMass;
    public final List<Float> lossMonoMasses;
    public final List<Float> lossAvgMasses;
    public final String elementalComposition;
    public final List<String> lossElementalComposition;

    Mod(String name, String aas, char terminus, boolean isVariable, float monoMass, float avgMass, List<Float> lossMonoMasses, List<Float> lossAvgMasses, String elementalComposition, List<String> lossElementalComposition) {
      if (name == null) {
        name = aas + "_" + (terminus == '\0' ? "" : terminus) + "_" + isVariable + "_" + monoMass;
      }
      this.name = name;
      this.aas = aas;
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
      return name + " " + aas + " " + terminus + " " + isVariable + " " + monoMass + " " + avgMass + " " + lossMonoMasses + " " + lossAvgMasses + " " + elementalComposition;
    }
  }
}
