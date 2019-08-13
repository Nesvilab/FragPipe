package umich.msfragger.params.enums;

import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class RemovePrecursorPeak {
  public static final String NAME = "remove_precursor_peak";

  private static final Map<String, Integer> mapTextNum = new LinkedHashMap<>();
  private static final Map<Integer, String> mapNumText = new LinkedHashMap<>();

  static {
    add("Do not remove", 0);
    add("Only peak with precursor charge", 1);
    add("Peaks with all charge states", 2);
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
