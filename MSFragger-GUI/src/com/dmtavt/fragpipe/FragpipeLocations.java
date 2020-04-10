package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.PropsFile;
import com.dmtavt.fragpipe.messages.NoteFragpipeCache;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeLocations {
  private static final Logger log = LoggerFactory.getLogger(FragpipeLocations.class);
  public static final String FN_CACHE_UI = "fragpipe-ui.cache";
  public static final String FN_CACHE_RUNTIME = "fragpipe-runtime.cache";

  public final Path jar;
  public final Path cache;
  public final Path tools;
  public final Path lib;

  private FragpipeLocations(Path jar, Path cache, Path tools, Path lib) {
    this.jar = jar;
    this.cache = cache;
    this.tools = tools;
    this.lib = lib;
  }

  public static FragpipeLocations init() {
    Path jar = Paths.get(Objects.requireNonNull(JarUtils.getCurrentJarPath()));
    Path cache = jar.resolve(Paths.get("../cache"));
    Path tools = jar.resolve(Paths.get("../tools"));
    Path lib = jar.resolve(Paths.get("../lib"));
    Stream<Path> paths = Stream.of(jar, cache, tools, lib);
    log.debug("Fragpipe locations:\n\t{}",
        paths.map(Path::toString).collect(Collectors.joining("\n\t")));
    for (Path path : paths.collect(Collectors.toList())) {
      try {
        PathUtils.createDirs(path);
      } catch (IOException e) {
        log.error("Error initializing fragpipe locations",e );
        throw new IllegalStateException("Error initializing fragpipe locations", e);
      }
    }
    return new FragpipeLocations(jar, cache, tools, lib);
  }

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
    return getFragpipeJarPath().resolve(FN_CACHE_UI).normalize().toAbsolutePath();
  }

  public static Path getPathRuntimeCache() {
    return getFragpipeJarPath().resolve(FN_CACHE_RUNTIME).normalize().toAbsolutePath();
  }

  public static NoteFragpipeCache loadCache() {
    PropsFile propsUi = new PropsFile(FragpipeLocations.getPathUiCache(),
        Version.version(true) + " ui state cache");
    PropsFile propsRuntime = new PropsFile(FragpipeLocations.getPathRuntimeCache(),
        Version.version(true) + " runtime cache");

    try {
      propsUi.load();
    } catch (IOException e) {
      log.error("Error while loading ui cache");
    }
    try {
      propsRuntime.load();
    } catch (IOException e) {
      log.error("Error while loading runtime cache");
    }

    return new NoteFragpipeCache(propsRuntime, propsUi);
  }
}
