package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class NoteFragpipeUpdate {

  public final String releaseVer;
  public final String downloadUrl;

  public NoteFragpipeUpdate(String releaseVer, String downloadUrl) {
    this.releaseVer = releaseVer;
    this.downloadUrl = downloadUrl;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", NoteFragpipeUpdate.class.getSimpleName() + "[", "]")
        .add("releaseVer='" + releaseVer + "'")
        .add("downloadUrl='" + downloadUrl + "'")
        .toString();
  }
}
