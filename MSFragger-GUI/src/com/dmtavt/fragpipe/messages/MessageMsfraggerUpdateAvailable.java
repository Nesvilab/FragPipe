package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class MessageMsfraggerUpdateAvailable {

  public final String newVersion;
  public final String manualDownloadUrl;

  public MessageMsfraggerUpdateAvailable(String newVersion, String manualDownloadUrl) {
    this.newVersion = newVersion;
    this.manualDownloadUrl = manualDownloadUrl;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ",
        MessageMsfraggerUpdateAvailable.class.getSimpleName() + "[", "]")
        .add("newVersion='" + newVersion + "'")
        .add("manualDownloadUrl='" + manualDownloadUrl + "'")
        .toString();
  }
}
