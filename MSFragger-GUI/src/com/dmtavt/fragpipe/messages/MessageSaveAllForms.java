package com.dmtavt.fragpipe.messages;

import java.nio.file.Path;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.CacheUtils;

public class MessageSaveAllForms {
  public static final String FORMS_CACHE_FN = "fragpipe-forms" + ThisAppProps.TEMP_FILE_EXT;
  public final Path path;

  public MessageSaveAllForms(Path path) {
    this.path = path;
  }

  public static MessageSaveAllForms forCaching() {
    Path formCachePath = CacheUtils.getTempFile(FORMS_CACHE_FN);
    return new MessageSaveAllForms(formCachePath);
  }
}
