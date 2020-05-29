package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class MessageDlProgress {
  public final long downloaded;
  public final long totalSize;
  public final String busTopic;

  public MessageDlProgress(long downloaded, long totalSize, String busTopic) {
    this.downloaded = downloaded;
    this.totalSize = totalSize;
    this.busTopic = busTopic;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessageDlProgress.class.getSimpleName() + "[", "]")
        .add("busTopic=" + busTopic)
        .add("downloaded=" + downloaded)
        .add("totalSize=" + totalSize)
        .toString();
  }
}
