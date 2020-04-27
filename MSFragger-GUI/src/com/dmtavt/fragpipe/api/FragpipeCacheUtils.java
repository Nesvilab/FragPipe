package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.exceptions.NoSuchElementInModelException;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.UiCombo;
import java.awt.Component;
import java.awt.Container;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.swing.JComboBox;
import javax.swing.JTabbedPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.params.ThisAppProps;

public class FragpipeCacheUtils {
  private static final Logger log = LoggerFactory.getLogger(FragpipeCacheUtils.class);

  private FragpipeCacheUtils() {
  }

  public static void saveToFileSorted(Properties props, Path location, String comments) {
    PropsFile pf = new PropsFile(location, comments);
    PropertiesUtils.merge(pf, Collections.singletonList(props));
    try {
      pf.save();
    } catch (IOException ex) {
      log.error("Error during FragpipeCacheUtils.saveToFileSorted()", ex);
    }
  }

  /**
   * Fills all tabs' components that have names with values from the map.
   */
  public static void tabPaneFromMap(JTabbedPane tabs, Map<String, String> map) {
    for (int i = 0; i < tabs.getTabCount(); i++) {
      Component compAt = tabs.getComponentAt(i);
      String tabTitle = tabs.getTitleAt(i);
      log.debug("Loading tab pane {} from map", tabTitle);
      if (compAt instanceof Container) {
        try {
          log.trace("Calling SwingUtils.valuesSet((Container) compAt, map) from tabPaneFromMap");
          SwingUtils.valuesSet((Container) compAt, map);
        } catch (NoSuchElementInModelException e) {
          if (UiCombo.class.equals(e.clazz)) {
            // some combo does not have such an element anymore
            log.warn("UiCombo {} does not have an option {} at this time", e.uiElemName, e.missingElement);
          } else {
            throw e;
          }
        }
      }
    }
  }

  /**
   * Collects all tabs' components that have names with values from the map.
   * @param tabs
   */
  public static Map<String, String> tabPaneToMap(final JTabbedPane tabs, final boolean saveFieldTypes) {
    // getting tab names
    Map<Integer, String> mapTabNameToIdx = new HashMap<>();
    for (int i = 0, tabCount = tabs.getTabCount(); i < tabCount; i++) {
      mapTabNameToIdx.put(i, tabs.getTitleAt(i));
    }

    final Map<String, String> m = new HashMap<>();
    SwingUtils.traverse(tabs, true, comp -> {
      final String name = comp.getName();
      if (StringUtils.isBlank(name)
          || name.toLowerCase().contains(Fragpipe.PROP_NOCACHE.toLowerCase())
          || name.contains("Spinner.formattedTextField")
      ) {
        return;
      }
      String v = SwingUtils.valueGet(comp);
      if (saveFieldTypes) {
        if (comp instanceof JComboBox<?>) {
          JComboBox<?> combo = (JComboBox<?>)comp;
          String choices = Seq.range(0, combo.getItemCount()).map(combo::getItemAt)
              .filter(o -> o instanceof String).map(o -> (String)o)
              .toString("; ");
          v += String.format(" [%s]", choices);
        }
      }
      //String k = saveFieldTypes ? name + "." + comp.getClass().getSimpleName() : name;
      String k = saveFieldTypes ? String.format("%s.%s",comp.getClass().getSimpleName(), name) : name;
      if (v == null) {
        log.warn("Found named component [{}] with no String value extraceted", k);
        return;
      }
      if (m.containsKey(k)) {
        throw new IllegalStateException("UI Elements with duplicate keys: " + k);
      }
      m.put(k, v);
    });
    return m;

    ////////////////////////////////////////////////////////////


//
//
//    final Function<Component, Map<String, String>> compToMap = (awtComponent) -> {
//      if (!(awtComponent instanceof Container)) {
//        return Collections.emptyMap();
//      }
//      Container awtContainer = (Container)awtComponent;
//      Predicate<String> filter = name -> !name.toLowerCase().contains(Fragpipe.PROP_NOCACHE.toLowerCase()) && !name.contains("Spinner.formattedTextField");
//      return SwingUtils.valuesGet(awtContainer, filter);
//    };
//
//    Map<String, String> whole = new HashMap<>();
//    for (int i = 0; i < tabs.getTabCount(); i++) {
//      Component tab = tabs.getComponentAt(i);
//      final String tabname = mapTabNameToIdx.getOrDefault(i, "?");
//
//      Map<String, String> map = compToMap.apply(tab).entrySet().stream()
////          .filter(kv -> {
////            boolean b1 = !kv.getKey().equalsIgnoreCase("Spinner.formattedTextField");
////            boolean b2 = !kv.getKey().toLowerCase().contains(Fragpipe.PROP_NOCACHE.toLowerCase());
////            return b1 && b2;
////          })
//          .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
//
//      if (map.isEmpty()) {
//        log.debug("No mapping for Tab #{} [{}]", i, tabname);
//      } else {
//
//        log.debug("Got mapping for Tab #{} [{}]: {}", i, tabname, map);
//        for (Entry<String, String> e : map.entrySet()) {
//          whole.merge(e.getKey(), e.getValue(), (s1, s2) -> {
//            String msg = String.format("Duplicate ui-element key '%s' in tab [%s]", e.getKey(), tabname);
//            throw new IllegalStateException(msg);
//          });
//        }
//      }
//    }
//    return whole;


    ////////////////////////////////////////////////////////////
  }

  public static Properties tabsSave(JTabbedPane tabs) {
    return tabsSave(tabs, false);
  }

  public static Properties tabsSave(JTabbedPane tabs, boolean saveFieldTypes) {
    Map<String, String> map = tabPaneToMap(tabs, saveFieldTypes);
    Properties props = PropertiesUtils.from(map);
    return props;
  }

  /**
 * @param os
 * @param tabs
   */
  public static void tabsSave(OutputStream os, JTabbedPane tabs)
      throws IOException {
    tabsSave(os, tabs, false);
  }

  /**
   * @param saveWithFieldTypes only used for development purposes to save properties with names
   *                           prepended with field types.
   */
  public static void tabsSave(OutputStream os, JTabbedPane tabs, boolean saveWithFieldTypes)
      throws IOException {
    Properties props = tabsSave(tabs, saveWithFieldTypes);
    try (BufferedOutputStream bos = new BufferedOutputStream(os)) {
      //props.store(bos, ThisAppProps.cacheComments()); // This is from Java's Properties - the order or things looks to be random
      PropertiesUtils.storeSorted(props, bos, ThisAppProps.cacheComments(), true);
    }
  }

  public static Properties loadAsProperties(InputStream is) throws IOException {
    Properties props = new Properties();
    try (BufferedInputStream bis = new BufferedInputStream(is)) {
      props.load(bis);
    }
    return props;
  }

  public static void tabsLoad(InputStream is, JTabbedPane tabs) throws IOException {
    Map<String, String> map = PropertiesUtils.toMap(loadAsProperties(is));
    tabPaneFromMap(tabs, map);
  }

  public static void tabsLoad(Properties props, JTabbedPane tabs) {
    Map<String, String> map = PropertiesUtils.toMap(props);
    tabPaneFromMap(tabs, map);
  }

}
