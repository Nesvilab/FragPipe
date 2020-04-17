package com.dmtavt.fragpipe.messages;

import java.util.List;
import com.dmtavt.fragpipe.api.InputLcmsFile;

public class MessageLcmsFilesTableUpdated {
  public final List<InputLcmsFile> files;
  public MessageLcmsFilesTableUpdated(List<InputLcmsFile> files) {
    this.files = files;
  }
}
