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

package org.nesvilab.fragpipe.tools.ptmshepherd;

import org.nesvilab.utils.StringUtils;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.utils.PropertiesUtils;

public class PtmshepherdParams {
  private static final Logger log = LoggerFactory.getLogger(PtmshepherdParams.class);
  public static final String DEFAULT_PROPERTIES_FN = "shepherd_default_open.config";
  public static final String DEFAULT_PROPERTIES_FN_BASE = "shepherd_default_";
  public static final String DEFAULT_PROPERTIES_FN_EXT = ".config";
  public static final String PROP_DATABASE = "database";
  /**
   * Dataset string format is `dataset = (set-name) (path-to-psm.tsv) (path-to-mzml-folder)`
   */
  public static final String PROP_DATASET = "dataset";
  public static final String DEFAULT_DATASET_NAME = "dataset01";

  private Path workDir;
  private Path db;
  private Map<LcmsFileGroup, Path> groups;
  private Map<String, String> props;

  public PtmshepherdParams(Path workDir, Path db, Map<LcmsFileGroup, Path> groups) {
    this.workDir = workDir;
    this.db = db;
    this.groups = groups;
  }

  public PtmshepherdParams(Path workDir, Path db, Map<LcmsFileGroup, Path> groups, Map<String, String> additionalProperties) {
    this(workDir, db, groups);
    Properties defaults = PropertiesUtils
        .loadPropertiesLocal(PtmshepherdParams.class, DEFAULT_PROPERTIES_FN);
    props = new HashMap<>(PropertiesUtils.toMap(defaults));

    props.putAll(additionalProperties);
  }

  public static String configFn(String searchTypeSuffix) {
    return DEFAULT_PROPERTIES_FN_BASE + searchTypeSuffix + DEFAULT_PROPERTIES_FN_EXT;
  }

  public String createConfig() {
    StringBuilder sb = new StringBuilder();
    sb.append(PROP_DATABASE).append(" = ").append(db.toAbsolutePath().normalize().toString()).append("\n");
    for (Entry<LcmsFileGroup, Path> e : groups.entrySet()) {
      LcmsFileGroup g = e.getKey();
      List<Path> lcmsPathsForGroup = g.lcmsFiles.stream().map(inputLcmsFile -> inputLcmsFile
          .getPath().toAbsolutePath().getParent())
          .distinct().collect(Collectors.toList());
      if (lcmsPathsForGroup.size() != 1) {
        String msg = "PTM Shepherd config only works when LCMS files in a group are in one directory.";
        log.error(msg);
        throw new IllegalArgumentException(msg);
      }
      Path lcmsFilesDir = lcmsPathsForGroup.get(0);
      Path p = e.getValue();
      Path psmTsv = g.outputDir(workDir).resolve("psm.tsv");
      final String datasetName = StringUtils.isNullOrWhitespace(g.name) ? DEFAULT_DATASET_NAME : g.name;
      sb.append(PROP_DATASET).append(" = ")
          .append(datasetName).append(" ")
          .append(psmTsv.toString()).append(" ")
          .append(lcmsFilesDir.toString()).append("\n");
    }

    if (props != null && !props.isEmpty()) {
      sb.append("\n");
      props.entrySet().stream()
          .filter(e -> !e.getKey().startsWith("ui."))
          .sorted(Comparator.comparing(Entry::getKey))
          .forEach(e -> sb.append(e.getKey()).append(" = ").append(e.getValue()).append("\n"));
    }

    return sb.toString();
  }

  public String getProp(String key) {
    if (props == null) {
      return null;
    }
    return props.getOrDefault(key, "");
  }

  public boolean needsIonQuant() {
    if (props == null) {
      return false;
    }
    return getProp("glyco_lda_features").contains("kl");
  }
}
