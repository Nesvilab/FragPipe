package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.exceptions.NoSuchElementInModelException;
import com.github.chhh.utils.PropertiesUtils;
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
import javax.swing.JTabbedPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.ThisAppProps;

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
      if (compAt instanceof Container) {
        try {
          SwingUtils.valuesFromMap((Container) compAt, map);
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
  public static Map<String, String> tabPaneToMap(JTabbedPane tabs) {
    // getting tab names
    Map<Integer, String> mapTabNameToIdx = new HashMap<>();
    for (int i = 0, tabCount = tabs.getTabCount(); i < tabCount; i++) {
      mapTabNameToIdx.put(i, tabs.getTitleAt(i));
    }

    final Function<Component, Map<String, String>> compToMap = awtComponent -> {
      if (!(awtComponent instanceof Container)) {
        return Collections.emptyMap();
      }
      Container awtContainer = (Container)awtComponent;
      Predicate<String> filter = name -> !name.toLowerCase().contains(Fragpipe.PROP_NOCACHE.toLowerCase()) && !name.contains("Spinner.formattedTextField");
      return SwingUtils.valuesToMap(awtContainer, filter);
    };

    Map<String, String> whole = new HashMap<>();
    for (int i = 0; i < tabs.getTabCount(); i++) {
      Component compAt = tabs.getComponentAt(i);
      final String tabname = mapTabNameToIdx.getOrDefault(i, "?");

      Map<String, String> map = compToMap.apply(compAt).entrySet().stream()
//          .filter(kv -> {
//            boolean b1 = !kv.getKey().equalsIgnoreCase("Spinner.formattedTextField");
//            boolean b2 = !kv.getKey().toLowerCase().contains(Fragpipe.PROP_NOCACHE.toLowerCase());
//            return b1 && b2;
//          })
          .collect(Collectors.toMap(Entry::getKey, Entry::getValue));

      if (map.isEmpty()) {
        log.debug("No mapping for Tab #{} [{}]", i, tabname);
      } else {

        log.debug("Got mapping for Tab #{} [{}]: {}", i, tabname, map);
        for (Entry<String, String> e : map.entrySet()) {
          whole.merge(e.getKey(), e.getValue(), (s1, s2) -> {
            String msg = String.format("Duplicate ui-element key '%s' in tab [%s]", e.getKey(), tabname);
            throw new IllegalStateException(msg);
          });
        }
      }
    }
    return whole;
  }

  public static Properties tabsSave(JTabbedPane tabs) {
    Map<String, String> map = tabPaneToMap(tabs);
    Properties props = PropertiesUtils.from(map);
    return props;
  }

  public static void tabsSave(OutputStream os, JTabbedPane tabs) throws IOException {
    Properties props = tabsSave(tabs);
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
