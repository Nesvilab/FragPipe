package com.dmtavt.fragpipe.api;

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
      log.debug("Backing file not exists, not loading anything: {}", path.toString());
    } else {
      try (BufferedReader br = Files.newBufferedReader(path)) {
        this.load(br);
      }
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
