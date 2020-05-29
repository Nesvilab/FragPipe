package com.dmtavt.fragpipe.messages;

import java.util.List;

public class MessageUpdatePackagesAvailable {

  final List<String> toDownload;

  public MessageUpdatePackagesAvailable(List<String> toDownload) {
    this.toDownload = toDownload;
  }
}
