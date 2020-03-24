package com.dmtavt.fragpipe.messages;

import static com.dmtavt.fragpipe.messages.MessageSaveAllForms.FORMS_CACHE_FN;

import java.nio.file.Path;
import com.github.chhh.utils.CacheUtils;

public class MessageLoadAllForms {

  public final Path path;

  public MessageLoadAllForms(Path path) {
    this.path = path;
  }

  public static MessageLoadAllForms forCaching() {
    Path formCachePath = CacheUtils.getTempFile(FORMS_CACHE_FN);
    return new MessageLoadAllForms(formCachePath);
  }
}
