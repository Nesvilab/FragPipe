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

import com.google.common.collect.TreeMultimap;
import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class UnimodDataReader {

  public final List<UnimodData> defaultUnimods = new ArrayList<>(1);
  public final List<UnimodData> skylineHardcodedUnimods= new ArrayList<>(1);
  public final TreeMultimap<Float, UnimodData> massToUnimod = TreeMultimap.create();

  public UnimodDataReader(Path path) throws Exception {
    int nameIdx = -1;
    int aasIdx = -1;
    int terminusIdx = -1;
    int idIdx = -1;
    int monoMassIdx = -1;
    int structuralIdx = -1;
    int hiddenIdx = -1;
    int typeIdx = -1;
    String line;
    BufferedReader br = Files.newBufferedReader(path);
    while ((line = br.readLine()) != null) {
      line = line.trim();
      line = line.replaceAll("#.*", "");
      if (line.isEmpty()) {
        continue;
      }
      String[] splits = line.split("\t");
      if (splits[0].trim().equalsIgnoreCase("name")) {
        for (int i = 0; i < splits.length; ++i) {
          switch (splits[i].trim().toLowerCase()) {
            case "name":
              nameIdx = i;
              break;
            case "aas":
              aasIdx = i;
              break;
            case "terminus":
              terminusIdx = i;
              break;
            case "id":
              idIdx = i;
              break;
            case "monomass":
              monoMassIdx = i;
              break;
            case "structural":
              structuralIdx = i;
              break;
            case "hidden":
              hiddenIdx = i;
              break;
            case "type":
              typeIdx = i;
              break;
          }
        }
        if (nameIdx == -1 || aasIdx == -1 || terminusIdx == -1 || idIdx == -1 || monoMassIdx == -1 || structuralIdx == -1 || hiddenIdx == -1 || typeIdx == -1) {
          throw new NullPointerException("Missing columns in UniModData.tsv: " + line);
        }
      } else {
        String name = splits[nameIdx].trim();

        if (name.contentEquals("Ammonia Loss (K, N, Q, R)") || name.contentEquals("Water Loss (D, E, S, T)")) {
          continue; // Those are not really modifications in Skyline.
        }

        Set<Character> aas = splits[aasIdx].trim().isEmpty() ? new HashSet<>() : Arrays.stream(splits[aasIdx].split(",")).map(e -> e.trim().charAt(0)).collect(Collectors.toSet());
        char terminus = splits[terminusIdx].trim().isEmpty() ? '\0' : splits[terminusIdx].trim().charAt(0);
        int id = splits[idIdx].trim().isEmpty() ? -1 : Integer.parseInt(splits[idIdx].trim());
        float monoMass = Float.parseFloat(splits[monoMassIdx].trim());
        boolean structural = splits[structuralIdx].trim().equalsIgnoreCase("true");
        boolean hidden = splits[hiddenIdx].trim().equalsIgnoreCase("true");
        String type = splits[typeIdx].trim();
        UnimodData umd = new UnimodData(name, aas, terminus, id, monoMass, structural, hidden);

        if (type.equalsIgnoreCase("default")) {
          defaultUnimods.add(umd);
        } else if (type.equalsIgnoreCase("hardcoded")) {
          skylineHardcodedUnimods.add(umd);
        } else {
          massToUnimod.put(monoMass, umd);
        }
      }
    }
    br.close();
  }
}
