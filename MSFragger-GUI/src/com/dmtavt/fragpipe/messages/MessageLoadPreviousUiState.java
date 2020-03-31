package com.dmtavt.fragpipe.messages;

import static com.dmtavt.fragpipe.Fragpipe.UI_STATE_CACHE_FN;

import com.github.chhh.utils.CacheUtils;
import java.nio.file.Path;

public class MessageLoadPreviousUiState {
  public final Path path;

  public MessageLoadPreviousUiState(Path path) {
    this.path = path;
  }

  public static MessageLoadPreviousUiState newForCache() {
    Path formCachePath = CacheUtils.getTempFile(UI_STATE_CACHE_FN);
    return new MessageLoadPreviousUiState(formCachePath);
  }
}
