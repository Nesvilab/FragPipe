package com.dmtavt.fragpipe.messages;

import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.TimeUtils;
import java.nio.file.Files;
import java.nio.file.Path;

public class MessageSaveLog {
  public final Path workDir;

  public MessageSaveLog(Path filePath) {
    this.workDir = filePath;
  }

  public static MessageSaveLog saveInDir(Path dir) {
    Path existing = PathUtils.existing(dir.toString());
    if (existing == null) {
      throw new IllegalArgumentException("Given dir must exist");
    }
    if (!Files.isDirectory(existing)) {
      throw new IllegalArgumentException("Given mpath must be a dir");
    }
    return new MessageSaveLog(existing.resolve(String.format("log_%s.txt", TimeUtils.dateTimeNoSpaces())));
  }
}
