package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;

public class MessageManifestSave {

  public final Path path;

  public MessageManifestSave() {
    path = null;
  }

  public MessageManifestSave(Path path) {
    this.path = path;
  }
}
