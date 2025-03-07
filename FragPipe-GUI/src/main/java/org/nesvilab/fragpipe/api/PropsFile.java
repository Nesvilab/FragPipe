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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Properties;
import org.nesvilab.fragpipe.tabs.TabMsfragger;
import org.nesvilab.fragpipe.tools.fragger.MsfraggerParams;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.PropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PropsFile extends Properties {
  private static final Logger log = LoggerFactory.getLogger(PropsFile.class);
  private Path path;
  private final String[] comments;

  public PropsFile(Path path, String comment) {
    super();
    Objects.requireNonNull(path);
    this.path = path;
    this.comments = new String[]{comment};
  }

  public String[] getComments() {
    return comments;
  }

  public boolean isBackingFileExists() {
    return Files.exists(path);
  }

  public void load() throws IOException {
    log.debug("Loading properties from: {}", path);
    if (!isBackingFileExists()) {
      log.debug("Backing file does not exist, not loading anything: {}", path.toString());
    } else {
      try (BufferedReader br = Files.newBufferedReader(path)) {
        this.load(br);
      }
    }

    final String p1 = getProperty(TabMsfragger.TAB_PREFIX + "search_enzyme_cutafter");
    if (p1 != null) {
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_cut_1, p1);
      remove(TabMsfragger.TAB_PREFIX + "search_enzyme_cutafter");
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_sense_1, "C");
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_allowed_missed_cleavage_1, getProperty(TabMsfragger.TAB_PREFIX + "allowed_missed_cleavage"));
      remove(TabMsfragger.TAB_PREFIX + "allowed_missed_cleavage");
      setProperty(TabMsfragger.TAB_PREFIX + TabMsfragger.PROP_misc_fragger_enzyme_dropdown_1, getProperty(TabMsfragger.TAB_PREFIX + "misc.fragger.enzyme-dropdown"));
      remove(TabMsfragger.TAB_PREFIX + "misc.fragger.enzyme-dropdown");
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_name_1, getProperty(TabMsfragger.TAB_PREFIX + "search_enzyme_name"));

      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_cut_2, "");
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_nocut_2, "");
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_name_2, "null");
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_allowed_missed_cleavage_2, "2");
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_sense_2, "C");
      setProperty(TabMsfragger.TAB_PREFIX + TabMsfragger.PROP_misc_fragger_enzyme_dropdown_2, "null");
    }

    final String p2 = getProperty(TabMsfragger.TAB_PREFIX + "search_enzyme_butnotafter");
    if (p2 != null) {
      setProperty(TabMsfragger.TAB_PREFIX + MsfraggerParams.PROP_search_enzyme_nocut_1, p2);
      remove(TabMsfragger.TAB_PREFIX + "search_enzyme_butnotafter");
    }

    final String p3 = getProperty("database.db-path");
    if (p3 != null) {
      String s = "";
      try {
        s = Paths.get(p3).toAbsolutePath().normalize().toString();
      } catch (Exception e) {}
      setProperty("database.db-path", s);
    }
  }

  public void save() throws IOException {
    PathUtils.createDirs(path.toAbsolutePath().getParent());
    save(Files.newOutputStream(path));
  }

  public void save(OutputStream os) throws IOException {
    PropertiesUtils.storeSorted(this, os, comments, true);
    os.flush();
  }

  public Path getPath() {
    return path;
  }

  public void setPath(Path path) {
    this.path = path;
  }
}
