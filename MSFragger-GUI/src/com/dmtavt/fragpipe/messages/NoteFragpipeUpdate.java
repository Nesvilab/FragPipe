package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteFragpipeUpdate {

  public final String releaseVer;
  public final String downloadUrl;
  public final String announcement;

  public NoteFragpipeUpdate(String releaseVer, String downloadUrl, String announcement) {
    this.releaseVer = releaseVer;
    this.downloadUrl = downloadUrl;
    this.announcement = announcement;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteFragpipeUpdate.class.getSimpleName() + "[", "]")
        .add("releaseVer='" + releaseVer + "'")
        .add("downloadUrl='" + downloadUrl + "'")
        .add("announcement='" + announcement + "'")
        .toString();
  }
}
