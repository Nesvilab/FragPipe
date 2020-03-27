package com.dmtavt.fragpipe.messages;

import static com.dmtavt.fragpipe.Fragpipe.UI_STATE_CACHE_FN;

import com.github.chhh.utils.CacheUtils;
import java.nio.file.Path;

public class MessageLoadUiState {
  public final Path path;

  public MessageLoadUiState(Path path) {
    this.path = path;
  }

  public static MessageLoadUiState newForCache() {
    Path formCachePath = CacheUtils.getTempFile(UI_STATE_CACHE_FN);
    return new MessageLoadUiState(formCachePath);
  }
}
