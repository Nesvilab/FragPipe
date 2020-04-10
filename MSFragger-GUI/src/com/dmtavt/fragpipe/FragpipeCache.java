package com.dmtavt.fragpipe;

import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.StringUtils;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeCache {
  private static final Logger log = LoggerFactory.getLogger(FragpipeCache.class);
  public static final String FILE_CACHE_UI = "../cache/fragpipe-ui.cache";
  public static final String FILE_CACHE_RUNTIME = "../cache/fragpipe-runtime.cache";

  private static class Holder {
    public static String fragpipeJar;
    static {
      fragpipeJar = JarUtils.getCurrentJarPath();
      log.debug("Caching class determined fargpipe jar file is at: {}", Holder.fragpipeJar);
      if (StringUtils.isBlank(fragpipeJar)) {
        log.error("Could not figure fragpipe jar location at runtime");
      } else {
        log.debug("This makes cache file paths:\n{}",
            Stream.of(getPathRuntimeCache(), getPathUiCache()).map(Path::toString)
                .collect(Collectors.joining("\n")));
      }
    }
  }

  private static Path getFragpipeJarPath() {
    if (Holder.fragpipeJar == null) {
      throw new IllegalStateException("Could not figure fragpipe jar file location automatically");
    }
    return Paths.get(Holder.fragpipeJar);
  }

  public static Path getPathUiCache() {
    return getFragpipeJarPath().resolve(FILE_CACHE_UI).normalize().toAbsolutePath();
  }

  public static Path getPathRuntimeCache() {
    return getFragpipeJarPath().resolve(FILE_CACHE_RUNTIME).normalize().toAbsolutePath();
  }
}
