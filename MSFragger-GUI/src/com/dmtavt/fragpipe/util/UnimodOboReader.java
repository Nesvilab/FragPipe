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

  public String convertPeptide(String peptide, String assignedModifications) {
    String[] aaArray = new String[peptide.length() + 2];
    Matcher matcher = nTermModPattern.matcher(assignedModifications);
    if (matcher.find()) {
      aaArray[0] = "(" + convertModifications(Float.parseFloat(matcher.group(1)), 'n') + ")";
    } else {
      aaArray[0] = "";
    }

    matcher = cTermModPattern.matcher(assignedModifications);
    if (matcher.find()) {
      aaArray[aaArray.length - 1] = "(" + convertModifications(Float.parseFloat(matcher.group(1)), 'c') + ")";
    } else {
      aaArray[aaArray.length - 1] = "";
    }

    matcher = varModPattern.matcher(assignedModifications);
    while (matcher.find()) {
      aaArray[Integer.parseInt(matcher.group(1))] = matcher.group(2) + "(" + convertModifications(Float.parseFloat(matcher.group(3)), matcher.group(2).charAt(0)) + ")";
    }

    char[] tempArray = peptide.toCharArray();
    for (int i = 0; i < tempArray.length; ++i) {
      if (aaArray[i + 1] == null) {
        aaArray[i + 1] = String.valueOf(tempArray[i]);
      }
    }

    return String.join("", aaArray);
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
}

