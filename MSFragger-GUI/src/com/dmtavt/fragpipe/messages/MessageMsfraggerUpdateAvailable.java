package com.dmtavt.fragpipe.messages;

public class MessageMsfraggerUpdateAvailable {

  public final String newVersion;
  public final String manualDownloadUrl;

  public MessageMsfraggerUpdateAvailable(String newVersion, String manualDownloadUrl) {
    this.newVersion = newVersion;
    this.manualDownloadUrl = manualDownloadUrl;
  }
}
