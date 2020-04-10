package com.dmtavt.fragpipe.api;

import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.PropertiesUtils;
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

  public void load() {
    log.debug("Loading properties from: {}", path);
  }

  public void save() {
    try {
      PathUtils.createDirs(path.getParent());
    } catch (IOException e) {
      log.error("Could not create directory structure to save properties");
      return;
    }
    try (OutputStream os = Files.newOutputStream(path)) {
      //store(os, cacheComments());
      PropertiesUtils.storeSorted(this, os, comments, true);
      os.flush();
    } catch (IOException ex) {
      log.error("Could not save properties to: " + path.toString(), ex);
    }
  }

  public Path getPath() {
    return path;
  }

  public void setPath(Path path) {
    this.path = path;
  }
}
