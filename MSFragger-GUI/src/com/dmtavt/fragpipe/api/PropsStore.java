package com.dmtavt.fragpipe.api;

import com.github.chhh.utils.PropertiesUtils;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Properties;
import java.util.function.Supplier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PropsStore extends Properties {
  private static final Logger log = LoggerFactory.getLogger(PropsFile.class);
  private final Supplier<InputStream> load;
  private Supplier<OutputStream> save;
  private final String comments;

  public PropsStore(Supplier<OutputStream> save, Supplier<InputStream> load, String comments) {
    super();
    Objects.requireNonNull(save);
    this.save = save;
    Objects.requireNonNull(load);
    this.load = load;
    this.comments = comments;
  }

  public String getComments() {
    return comments;
  }

  public void load() throws IOException {
    log.debug("Loading properties from storage");
    try (BufferedReader br = new BufferedReader(new InputStreamReader(load.get(),
        StandardCharsets.UTF_8))) {
      this.load(br);
    }
  }

  public void save() throws IOException {
    log.debug("Writing properties to storage");
    try (BufferedOutputStream bos = new BufferedOutputStream(save.get())) {
      //store(os, cacheComments());
      PropertiesUtils.storeSorted(this, bos, comments, true);
      bos.flush();
    } catch (IOException ex) {
      log.error("Error saving properties to storage", ex);
      throw(ex);
    }
  }
}
