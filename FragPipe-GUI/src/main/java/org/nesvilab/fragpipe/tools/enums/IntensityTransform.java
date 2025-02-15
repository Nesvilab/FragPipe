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

package org.nesvilab.fragpipe.tools.enums;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class IntensityTransform {
  public static final String NAME = "intensity_transform";

  private static final Map<String, Integer> mapTextNum = new LinkedHashMap<>();
  private static final Map<Integer, String> mapNumText = new LinkedHashMap<>();

  static {
    add("None", 0);
    add("Square root", 1);
  }

  private static void add(String text, int num) {
    mapNumText.put(num, text);
    mapTextNum.put(text, num);
  }

  public static int get(String text) {
    Integer num = mapTextNum.get(text);
    if (num == null) {
      throw new IllegalStateException("No known mapping for: " + text);
    }
    return num;
  }

  public static String get(int num) {
    String text = mapNumText.get(num);
    if (text == null) {
      throw new IllegalStateException("No known mapping for: " + Integer.toString(num));
    }
    return text;
  }

  public static String[] getNames() {
    return mapNumText.entrySet().stream()
        .sorted(Comparator.comparing(Entry::getKey))
        .map(Entry::getValue).toArray(String[]::new);
  }
}
