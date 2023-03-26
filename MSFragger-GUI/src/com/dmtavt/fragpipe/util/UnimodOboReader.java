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

import com.google.common.collect.Table;
import com.google.common.collect.TreeBasedTable;
import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.file.Path;
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

  public final Map<String, Float> unimodMassMap;
  public final Table<Float, Character, Integer> massSiteUnimodTable;

  public  UnimodOboReader(Path path) throws Exception {
    unimodMassMap = new HashMap<>();
    massSiteUnimodTable = TreeBasedTable.create();

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
    }

    reader.close();
  }


  public static class OboTerm {
    int id = Integer.MIN_VALUE;
    float mass = Float.NaN;
    Set<Character> sites = new HashSet<>();
  }
}

