package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;
import java.util.List;

public class MissingAssetsException extends Throwable {

  private final List<Path> notExisting;

  public MissingAssetsException(List<Path> notExisting) {
    this.notExisting = notExisting;
  }

  public List<Path> getNotExisting() {
    return notExisting;
  }
}
