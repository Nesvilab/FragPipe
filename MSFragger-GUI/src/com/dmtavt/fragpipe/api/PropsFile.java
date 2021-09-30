package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tools.fragger.MsfraggerParams;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.PropertiesUtils;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PropsFile extends Properties {
  private static final Logger log = LoggerFactory.getLogger(PropsFile.class);
  private Path path;
  private final String comments;

  public PropsFile(Path path, String comments) {
    super();
    Objects.requireNonNull(path);
    this.path = path;
    this.comments = comments;
  }

  public String getComments() {
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
  }

  public void save() throws IOException {
    log.debug("Writing PropsFile to: {}", path.toString());
    try {
      PathUtils.createDirs(path.getParent());
    } catch (IOException ex) {
      log.error("Could not create directory structure to save properties");
      throw(ex);
    }
    try (OutputStream os = Files.newOutputStream(path)) {
      //store(os, cacheComments());
      PropertiesUtils.storeSorted(this, os, comments, true);
      os.flush();
    } catch (IOException ex) {
      log.error("Could not save properties to: " + path.toString(), ex);
      throw(ex);
    }
  }

  public void save(OutputStream os) throws IOException {
    log.debug("Writing properties to stream");
    try (BufferedOutputStream bos = new BufferedOutputStream(os)) {
      //store(os, cacheComments());
      PropertiesUtils.storeSorted(this, os, comments, true);
      os.flush();
    } catch (IOException ex) {
      log.error("Error writing properties to stream", ex);
      throw(ex);
    }
  }

  public Path getPath() {
    return path;
  }

  public void setPath(Path path) {
    this.path = path;
  }
}
