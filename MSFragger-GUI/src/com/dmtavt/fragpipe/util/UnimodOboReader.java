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

package com.dmtavt.fragpipe.util;

import static com.dmtavt.fragpipe.util.Utils.AAMasses;

import com.google.common.collect.TreeBasedTable;
import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class UnimodOboReader {

  private static final Pattern pattern1 = Pattern.compile("id:\\s+UNIMOD:(\\d+)");
  private static final Pattern pattern2 = Pattern.compile("xref:\\s+delta_mono_mass\\s+\"([\\d.+-]+)\"");
  private static final Pattern pattern3 = Pattern.compile("xref:\\s+spec_\\d+_site\\s+\"(\\S+)\"");
  private static final Pattern pattern4 = Pattern.compile("([A-Z])(\\(([^()]+)\\))?");
  private static final Pattern pattern5 = Pattern.compile("^\\(([^()]+)\\)");
  private static final Pattern varModPattern = Pattern.compile("([\\d]+)([A-Z])\\(([\\d.-]+)\\)");
  private static final Pattern nTermModPattern = Pattern.compile("N-term\\(([\\d.-]+)\\)");
  private static final Pattern cTermModPattern = Pattern.compile("C-term\\(([\\d.-]+)\\)");

  public final Map<String, Float> unimodMassMap;                        // unimod ID (as "unimod: ##"), mass
  public final TreeBasedTable<Float, Character, Integer> massSiteUnimodTable;    // mass, allowed site, unimod ID number
  public final ArrayList<String> unimodDB;                              // mod info string ("mass;ID;name;sites")

  public UnimodOboReader(Path path) throws Exception {
    unimodMassMap = new HashMap<>();
    massSiteUnimodTable = TreeBasedTable.create();
    unimodDB = new ArrayList<>();

    BufferedReader reader = new BufferedReader(new FileReader(path.toFile()));
    String line;
    OboTerm term = null;
    while ((line = reader.readLine()) != null) {
      line = line.trim();
      if (line.isEmpty()) {
        continue;
      }
      if (line.startsWith("[Term]")) {
        if (term != null) {
          unimodMassMap.put("unimod:" + term.id, term.mass);
          for (char site : term.sites) {
            Integer tt = massSiteUnimodTable.get(term.mass, site);
            if (tt == null || tt > term.id) {
              massSiteUnimodTable.put(term.mass, site, term.id);
            }
          }
          StringBuilder siteBuilder = new StringBuilder();
          term.sites.forEach(siteBuilder::append);
          unimodDB.add(String.format("%s;%s;%s;%s", term.mass, term.id, term.name, siteBuilder));
        }
        term = new OboTerm();
      } else if (line.startsWith("id:")) {
        Matcher matcher = pattern1.matcher(line);
        if (matcher.matches()) {
          term.id = Integer.parseInt(matcher.group(1));
        } else {
          throw new RuntimeException("Unexpected id format: " + line);
        }
      } else if (term != null && term.id != Integer.MIN_VALUE && line.startsWith("xref: delta_mono_mass")) {
        Matcher matcher = pattern2.matcher(line);
        if (matcher.matches()) {
          term.mass = Float.parseFloat(matcher.group(1));
        } else {
          throw new RuntimeException("Unexpected xref: delta_mono_mass format: " + line);
        }
      } else if (term != null && term.id != Integer.MIN_VALUE && line.startsWith("name: ")) {
        term.name = line.split(": ")[1];
      } else if (term != null && term.id != Integer.MIN_VALUE) {
        Matcher matcher = pattern3.matcher(line);
        if (matcher.matches()) {
          String tt = matcher.group(1);
          if (tt.contentEquals("N-term")) {
            term.sites.add('n');
          } else if (tt.contentEquals("C-term")) {
            term.sites.add('c');
          } else {
            term.sites.add(tt.charAt(0));
          }
        }
      }
    }

    if (term != null) {
      unimodMassMap.put("unimod:" + term.id, term.mass);
      for (char site : term.sites) {
        Integer tt = massSiteUnimodTable.get(term.mass, site);
        if (tt == null || tt > term.id) {
          massSiteUnimodTable.put(term.mass, site, term.id);
        }
      }
      StringBuilder siteBuilder = new StringBuilder();
      term.sites.forEach(siteBuilder::append);
      unimodDB.add(String.format("%s;%s;%s;%s", term.mass, term.id, term.name, siteBuilder));
    }

    reader.close();
  }

  public Precursor convertPrecursor(String peptide, String assignedModifications, int charge) {
    float[] modifications = new float[peptide.length()];
    float nTermMod = 0, cTermMod = 0;
    Matcher matcher = nTermModPattern.matcher(assignedModifications);
    if (matcher.find()) {
      nTermMod = Float.parseFloat(matcher.group(1));
    }

    matcher = cTermModPattern.matcher(assignedModifications);
    if (matcher.find()) {
      cTermMod = Float.parseFloat(matcher.group(1));
    }

    matcher = varModPattern.matcher(assignedModifications);
    while (matcher.find()) {
      modifications[Integer.parseInt(matcher.group(1)) - 1] = Float.parseFloat(matcher.group(3));
    }

    return new Precursor(peptide, modifications, nTermMod, cTermMod, charge);
  }

  public String convertModifications(float mass, char site) {
    float gap = Float.MAX_VALUE;
    int unimodId = -1;
    for (Map.Entry<Float, Map<Character, Integer>> e1 : massSiteUnimodTable.rowMap().subMap(mass - 0.001f, mass + 0.001f).entrySet()) {
      Integer tt = e1.getValue().get(site);
      if (tt != null && Math.abs(e1.getKey() - mass) < gap) {
        gap = Math.abs(e1.getKey() - mass);
        unimodId = tt;
      }
    }

    if (unimodId > 0) {
      return "UniMod:" + unimodId;
    } else {
      return String.valueOf(mass);
    }
  }


  public static class OboTerm {
    int id = Integer.MIN_VALUE;
    float mass = Float.NaN;
    Set<Character> sites = new HashSet<>();
    String name = "";
  }


  public static class Precursor implements Comparable<Precursor> {

    final String sequence;
    final float[] modifications;
    final float nTermMod, cTermMod;
    final int charge;
    final String precursorStr;

    public Precursor(String sequence, float[] modifications, float nTermMod, float cTermMod, int charge) {
      this.sequence = sequence;
      this.modifications = modifications;
      this.nTermMod = nTermMod;
      this.cTermMod = cTermMod;
      this.charge = charge;
      precursorStr = getPrecursorStr();
    }
    
    public Precursor(String modifiedSequence, int length, int charge, Map<String, Float> unimodMassMap, Map<String, Float> diannLabelMassMap) {
      modifications = new float[length];
      this.charge = charge;
      cTermMod = 0; // DIA-NN seems not support C-term modification
      float a = 0;
      String s = modifiedSequence;

      Matcher matcher = pattern5.matcher(s);
      if (matcher.find()) {
        a = getModMass(matcher.group(1), unimodMassMap, diannLabelMassMap);
        s = s.substring(matcher.end());
      }

      int idx = 0;
      StringBuilder sb = new StringBuilder(length);
      matcher = pattern4.matcher(s);
      while (matcher.find()) {
        if (matcher.group(2) == null) {
          modifications[idx++] = 0;
          sb.append(matcher.group(1));
        } else {
          modifications[idx++] = getModMass(matcher.group(3), matcher.group(1).charAt(0), unimodMassMap, diannLabelMassMap);
          sb.append(matcher.group(1));
        }
      }

      sequence = sb.toString();
      nTermMod = a;
      precursorStr = getPrecursorStr();
    }

    public String getSequence() {
      return sequence;
    }

    private static float getModMass(String s, char site, Map<String, Float> unimodMassMap, Map<String, Float> diannLabelMassMap) {
      if (s.startsWith("UniMod:")) {
        return unimodMassMap.get(s.toLowerCase());
      } else if (diannLabelMassMap != null && !diannLabelMassMap.isEmpty() && s.startsWith("label-")) {
        return diannLabelMassMap.get(s);
      } else {
        return Float.parseFloat(s) - AAMasses[site - 'A'];
      }
    }

    private static float getModMass(String s, Map<String, Float> unimodMassMap, Map<String, Float> diannLabelMassMap  ) {
      if (s.startsWith("UniMod:")) {
        return unimodMassMap.get(s.toLowerCase());
      } else if (diannLabelMassMap != null && !diannLabelMassMap.isEmpty() && s.startsWith("label-")) {
        return diannLabelMassMap.get(s);
      } else {
        return Float.parseFloat(s);
      }
    }

    private String getPrecursorStr() {
      char[] ncAaArray = sequence.toCharArray();
      StringBuilder sb = new StringBuilder();

      if (Math.abs(nTermMod) > 0) {
        sb.append(String.format("n[%.2f]", nTermMod));
      }

      for (int i = 0; i < modifications.length; ++i) {
        if (Math.abs(modifications[i]) > 0) {
          sb.append(String.format("%c[%.2f]", ncAaArray[i], modifications[i]));
        } else {
          sb.append(ncAaArray[i]);
        }
      }

      if (Math.abs(cTermMod) > 0) {
        sb.append(String.format("c[%.2f]", cTermMod));
      }

      sb.append(charge);

      return sb.toString();
    }

    @Override
    public int compareTo(Precursor o) {
      return precursorStr.compareTo(o.precursorStr);
    }

    @Override
    public boolean equals(Object o) {
      if (o instanceof Precursor) {
        return this.compareTo((Precursor) o) == 0;
      } else {
        return false;
      }
    }

    @Override
    public String toString() {
      return precursorStr;
    }

    @Override
    public int hashCode() {
      return precursorStr.hashCode();
    }
  }
}

