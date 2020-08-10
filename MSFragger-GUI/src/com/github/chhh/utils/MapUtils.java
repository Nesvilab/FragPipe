package com.github.chhh.utils;

import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
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

  public static<K,V,R> Map<R,V> remapKeys(Map<K, V> map, Function<K,R> keyMapper) {
      return map.entrySet().stream()
          .collect(Collectors.toMap(kv -> keyMapper.apply(kv.getKey()), Entry::getValue));
  }

  public static<K,V,R> Map<R,V> remapKeys(Map<K, V> map, BiFunction<K,V,R> keyMapper) {
    return map.entrySet().stream()
        .collect(Collectors.toMap(kv -> keyMapper.apply(kv.getKey(), kv.getValue()), Entry::getValue));
  }

  public static<K,V,R> Map<K,R> remapValues(Map<K, V> map, BiFunction<K,V,R> valMapper) {
      return map.entrySet().stream()
          .filter(kv -> valMapper.apply(kv.getKey(), kv.getValue()) != null)
          .collect(Collectors.toMap(Entry::getKey, kv -> valMapper.apply(kv.getKey(), kv.getValue())));
  }

  public static<K,V,RK, RV> Map<RK,RV> remap(Map<K, V> map, BiFunction<K, V, RK> keyMapper, BiFunction<K, V, RV> valMapper) {
      return map.entrySet().stream()
          .collect(Collectors.toMap(kv -> keyMapper.apply(kv.getKey(), kv.getValue()), kv -> valMapper.apply(kv.getKey(), kv.getValue())));
  }

  public static<K,V> void refill(Map<K,V> target, Map<K,V> source) {
    target.clear();
    target.putAll(source);
  }
}
