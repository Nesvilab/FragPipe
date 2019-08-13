package umich.msfragger.params.enums;

import java.util.Comparator;
import java.util.HashMap;
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
