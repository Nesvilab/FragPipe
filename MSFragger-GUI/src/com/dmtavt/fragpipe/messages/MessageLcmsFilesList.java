package com.dmtavt.fragpipe.messages;

import java.util.List;
import java.util.StringJoiner;
import com.dmtavt.fragpipe.api.InputLcmsFile;

public class MessageLcmsFilesList {
  public final MessageType type;
  public final List<InputLcmsFile> files;

  public MessageLcmsFilesList(MessageType type, List<InputLcmsFile> files) {
    this.type = type;
    this.files = files;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageLcmsFilesList.class.getSimpleName() + "[", "]")
        .add("type=" + type)
        .add("file count=" + (files == null ? null : Integer.toString(files.size())))
        .toString();
  }
}
