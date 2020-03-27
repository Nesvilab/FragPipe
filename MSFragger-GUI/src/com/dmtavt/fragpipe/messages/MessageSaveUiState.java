package com.dmtavt.fragpipe.messages;

import com.dmtavt.fragpipe.Fragpipe;
import java.nio.file.Path;
import com.github.chhh.utils.CacheUtils;

public class MessageSaveUiState {

  public final Path path;

  public MessageSaveUiState(Path path) {
    this.path = path;
  }

  public static MessageSaveUiState newForCache() {
    Path formCachePath = CacheUtils.getTempFile(Fragpipe.UI_STATE_CACHE_FN);
    return new MessageSaveUiState(formCachePath);
  }
}
