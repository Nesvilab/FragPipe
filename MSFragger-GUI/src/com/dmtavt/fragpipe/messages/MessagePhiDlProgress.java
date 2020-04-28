package com.dmtavt.fragpipe.messages;

import java.util.StringJoiner;

public class MessagePhiDlProgress {
  public final long downloaded;
  public final long totalSize;

  public MessagePhiDlProgress(long downloaded, long totalSize) {
    this.downloaded = downloaded;
    this.totalSize = totalSize;
  }

  @Override
  public String toString() {
    return new StringJoiner(", ", MessagePhiDlProgress.class.getSimpleName() + "[", "]")
        .add("downloaded=" + downloaded)
        .add("totalSize=" + totalSize)
        .toString();
  }
}
