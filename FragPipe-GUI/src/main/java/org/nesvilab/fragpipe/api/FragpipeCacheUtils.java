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

package org.nesvilab.fragpipe.api;

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
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.WorkflowTranslator;
import org.nesvilab.fragpipe.exceptions.NoSuchElementInModelException;
import org.nesvilab.fragpipe.params.ThisAppProps;
import org.nesvilab.fragpipe.tools.tmtintegrator.TmtiConfProps;
import org.nesvilab.utils.MapUtils;
import org.nesvilab.utils.PropertiesUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.swing.UiCombo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
      if (k.contains(TmtiConfProps.PROP_channel_num)) {
        return v;   // skip translating TMT-I channel num when saving to workflow file. String name of label will get saved, avoiding conflict when multiple labels have same num of channels.
      }
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
        log.warn(String.format("Found no ui.translation for loaded element key [%s] with value [%s]", k, v));
        return v;
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

  public static void tabsSave(OutputStream os, JTabbedPane tabs) throws IOException {
    tabsSave(os, tabs, false);
  }

  /**
   * @param saveWithFieldTypes only used for development purposes to save properties with names
   *                           prepended with field types.
   */
  public static void tabsSave(OutputStream os, JTabbedPane tabs, boolean saveWithFieldTypes) throws IOException {
    Properties props = tabsSave0(tabs, saveWithFieldTypes);
    try (BufferedOutputStream bos = new BufferedOutputStream(os)) {
      PropertiesUtils.storeSorted(props, bos, ThisAppProps.cacheComments(), true);
    }
  }

  public static void tabsLoad(final Map<String, String> props, JTabbedPane tabs) {
    tabPaneFromMap(tabs, translateValuesToUi(props));
  }

}
