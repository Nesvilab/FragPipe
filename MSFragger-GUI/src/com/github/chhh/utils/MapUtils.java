package com.github.chhh.utils;

import java.util.Map;
import java.util.stream.Stream;

public class MapUtils {

  private MapUtils() {
  }

  /** Keys that are in both maps. */
  public static <K> Stream<K> keysIntersection(Map<K, ?> m1, Map<K, ?> m2) {
    return m1.keySet().stream().filter(m2::containsKey);
  }

  /** Keys that are only in the 'left' map (m1). */
  public static <K> Stream<K> keysDiffLeft(Map<K, ?> m1, Map<K, ?> m2) {
    return m1.keySet().stream().filter(k -> !m2.containsKey(k));
  }

  /** Keys that are only in the 'right' map (m2). */
  public static <K> Stream<K> keysDiffRight(Map<K, ?> m1, Map<K, ?> m2) {
    return keysDiffLeft(m2, m1);
  }
}
