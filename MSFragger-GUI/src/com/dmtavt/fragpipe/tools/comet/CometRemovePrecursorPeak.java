package com.dmtavt.fragpipe.tools.comet;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;

public class CometRemovePrecursorPeak {
    public static final String NAME = "remove_precursor_peak";

    private static final Map<String, Integer> mapTextNum = new LinkedHashMap<>();
    private static final Map<Integer, String> mapNumText = new LinkedHashMap<>();

    static {
        add("Do not remove", 0);
        add("Remove all peaks around the precursor m/z", 1);
        add("Remove all charge reduced precursor peaks (ETD/ECD)", 2);
        add("Remove the HPO3 (-80) and H3PO4 (-98) precursor phosphate neutral loss", 3);
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
                .sorted(Comparator.comparing(Map.Entry::getKey))
                .map(Map.Entry::getValue).toArray(String[]::new);
    }
}
