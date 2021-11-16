package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.WorkflowTranslator;
import com.dmtavt.fragpipe.exceptions.NoSuchElementInModelException;
import com.github.chhh.utils.MapUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.swing.UiCombo;
import java.awt.Component;
import java.awt.Container;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
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
        //log.warn("Found named component [{}] with no String value extraceted", k);
        return;
      }
      if (m.containsKey(k)) {
        throw new IllegalStateException("UI Elements with duplicate keys: " + k);
      }
      m.put(k, v);
    });
    return m;
  }

  public static Properties tabsSave(JTabbedPane tabs) {
    return tabsSave0(tabs, false);
  }

  /** Main implementation of the method, all other variants refer to this one. */
  public static Properties tabsSave0(JTabbedPane tabs, boolean saveFieldTypes) {
    Map<String, String> map = tabPaneToMap(tabs, saveFieldTypes);
    Properties props = PropertiesUtils.from(translateValuesFromUi(map));
    return props;
  }

  public static String translateValueFromUi(String k, String v) {
    final Map<String, List<UiTranslation>> translations = WorkflowTranslator.readTranslations();
    List<UiTranslation> ts = translations.get(k);
    if (ts == null)
      return v;
    List<UiTranslation> matchingTs = Seq.seq(ts).filter(t -> t.inUi.equalsIgnoreCase(v)).toList();
    if (matchingTs.isEmpty()) {
      throw new IllegalStateException(String.format("Found no ui.translation for UI element key [%s] with value [%s]", k, v));
    }
    if (matchingTs.size() > 1) {
      log.debug(String.format("Found multiple ui.translations for UI element key [%s] with value [%s]", k, v));
    }
    UiTranslation t = matchingTs.get(0);
    log.debug("Found translation for key [{}]. Value mapping [{}] -> [{}]", k, v, t.inConf);
    return t.inConf;
  }

  public static Map<String, String> translateValuesFromUi(Map<String, String> uiMapping) {
    final Map<String, List<UiTranslation>> translations = WorkflowTranslator.readTranslations();
    Map<String, String> remapped = MapUtils.remapValues(uiMapping, (k, v) -> {
      List<UiTranslation> ts = translations.get(k);
      if (ts == null)
        return v;
      List<UiTranslation> matchingTs = Seq.seq(ts).filter(t -> t.inUi.equalsIgnoreCase(v)).toList();
      if (matchingTs.isEmpty()) {
        throw new IllegalStateException(String.format("Found no ui.translation for UI element key [%s] with value [%s]", k, v));
      }
      if (matchingTs.size() > 1) {
        log.debug(String.format("Found multiple ui.translations for UI element key [%s] with value [%s]", k, v));
      }
      UiTranslation t = matchingTs.get(0);
      log.debug("Found translation for key [{}]. Value mapping [{}] -> [{}]", k, v, t.inConf);
      return t.inConf;
    });
    return remapped;
  }

  public static Map<String, String> translateValuesToUi(Map<String, String> loadedMap) {
    final Map<String, List<UiTranslation>> translations = WorkflowTranslator.readTranslations();
    Map<String, String> remapped = MapUtils.remapValues(loadedMap, (k, v) -> {
      List<UiTranslation> ts = translations.get(k);
      if (ts == null)
        return v;
      List<UiTranslation> matchingCurrentUiOptions = Seq.seq(ts).filter(t -> t.inUi.contentEquals(v)).toList();
      if (!matchingCurrentUiOptions.isEmpty()) {
        // value is already among the currently supported ones
        log.debug("No translation needed, currently supported value: {}, {}", k, v);
        return v;
      }
      List<UiTranslation> matchingTs = Seq.seq(ts).filter(t -> t.inConf.equalsIgnoreCase(v)).toList();
      if (matchingTs.isEmpty()) {
        throw new IllegalStateException(String.format("Found no ui.translation for loaded element key [%s] with value [%s]", k, v));
      }
      if (matchingTs.size() > 1) {
        log.debug(String.format("Found multiple ui.translations for loaded element key [%s] with value [%s]", k, v));
      }
      UiTranslation t = matchingTs.get(0);
      log.debug("Found translation for key [{}]. Value mapping [{}] -> [{}]", k, v, t.inUi);
      return t.inUi;
    });
    return remapped;
  }

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
    Properties props = tabsSave0(tabs, saveWithFieldTypes);
    try (BufferedOutputStream bos = new BufferedOutputStream(os)) {
      //props.store(bos, ThisAppProps.cacheComments()); // This is from Java's Properties - the order or things looks to be random
      PropertiesUtils.storeSorted(props, bos, ThisAppProps.cacheComments(), true);
    }
  }

  public static void tabsLoad(final Map<String, String> props, JTabbedPane tabs) {
    tabPaneFromMap(tabs, translateValuesToUi(props));
  }

}
