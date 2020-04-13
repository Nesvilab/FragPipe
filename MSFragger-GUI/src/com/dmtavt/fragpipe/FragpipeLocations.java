package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.PropsFile;
import com.dmtavt.fragpipe.messages.NoteFragpipeCache;
import com.github.chhh.utils.CacheUtils;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeLocations {
  private static volatile FragpipeLocations INSTANCE = null;
  private static final Logger log = LoggerFactory.getLogger(FragpipeLocations.class);
  public static final String FN_CACHE_UI = "fragpipe-ui.cache";
  public static final String FN_CACHE_RUNTIME = "fragpipe-runtime.cache";

  public final Path jarPath;
  public final Path cache;
  public final Path tools;
  public final Path lib;

  private FragpipeLocations(Path jarPath, Path cache, Path tools, Path lib) {
    this.jarPath = jarPath;
    this.cache = cache;
    this.tools = tools;
    this.lib = lib;
  }

  private static class Holder {
    public static String fragpipeJar;
    public static FragpipeLocations locations;
    static {
      fragpipeJar = JarUtils.getCurrentJarPath();
      log.debug("Caching class determined fargpipe jar file is at: {}", Holder.fragpipeJar);
      if (StringUtils.isBlank(fragpipeJar)) {
        log.error("Could not figure fragpipe jar location at runtime");
        throw new IllegalStateException("Could not figure fragpipe jar location at runtime");
      }
      Path jarPath = Paths.get(fragpipeJar);
      Path dir = Files.isDirectory(jarPath) ? jarPath : jarPath.getParent();
      Path cache = dir.resolve(Paths.get("../cache"));
      Path tools = dir.resolve(Paths.get("../tools"));
      Path lib = dir.resolve(Paths.get("../lib"));
      List<Path> paths = Arrays.asList(jarPath, dir, cache, tools, lib);
      log.debug("Fragpipe locations:\n\t{}",
          paths.stream().map(Path::toString).collect(Collectors.joining("\n\t")));
      for (Path path : paths) {
        try {
          PathUtils.createDirs(path);
        } catch (IOException e) {
          log.error("Error initializing fragpipe locations",e );
          throw new IllegalStateException("Error initializing fragpipe locations", e);
        }
      }
      locations = new FragpipeLocations(jarPath, cache, tools, lib);
    }
  }

  public List<Path> getCachePaths() {
    List<Path> paths = new ArrayList<>();
    paths.add(getPathRuntimeCache(false));
    paths.add(getPathRuntimeCache(true));
    paths.add(getPathUiCache(false));
    paths.add(getPathUiCache(true));
    return paths;
  }

  public void delete(List<Path> paths) throws IOException {
    for (Path path : paths) {
      Files.deleteIfExists(path);
    }
  }

  public static FragpipeLocations get() {
    return Holder.locations;
  }

  private String createCacheComment(String cacheType) {
    return Version.version(true) + cacheType + " cache";
  }

  public NoteFragpipeCache loadCache() {
    boolean[] order = {false, true};
    Function<Boolean, String> f = (b) -> b ? "global" : "local";

    PropsFile pfRuntime = null, pfUi = null;
    for (boolean isGlobal : order) {
      Path p = getPathUiCache(isGlobal);
      log.debug("Trying to load {} ui state file: {}", f.apply(isGlobal), p);
      if (!Files.exists(p)) {
        log.debug("{} ui state file not exists, skipping: {}", f.apply(isGlobal), p);
        continue;
      }
      pfUi = tryLoadSilently(p, createCacheComment("ui state"));
    }
    if (pfUi == null) {
      pfUi = new PropsFile(getPathUiCache(false), createCacheComment("ui state"));
    }

    for (boolean isGlobal : order) {
      Path p = getPathRuntimeCache(isGlobal);
      log.debug("Trying to load {} runtime file: {}", f.apply(isGlobal), p);
      if (!Files.exists(p)) {
        log.debug("{} runtime file not exists, skipping: {}", f.apply(isGlobal), p);
        continue;
      }
      pfRuntime = tryLoadSilently(p, createCacheComment("ui state"));
    }
    if (pfRuntime == null) {
      pfRuntime = new PropsFile(getPathRuntimeCache(false), createCacheComment("runtime"));
    }
    Objects.requireNonNull(pfRuntime, "Runtime props file");
    Objects.requireNonNull(pfUi, "Ui state props file");
    return new NoteFragpipeCache(pfRuntime, pfUi);
  }

  public PropsFile tryLoadSilently(Path path, String comments) {
    log.debug("Trying to load file: {}", path.toString());
    PropsFile pf = new PropsFile(path, comments);
    try {
      pf.load();
    } catch (IOException e) {
      log.error("Error while trying to load", e);
    }
    return pf;
  }

  public Path getPathRuntimeCache() {
    return getPathRuntimeCache(false);
  }

  public Path getPathRuntimeCache(boolean isGlobal) {
    return isGlobal ? CacheUtils.getTempFile(FN_CACHE_RUNTIME) : get().cache.resolve(FN_CACHE_RUNTIME);
  }

  public Path getPathUiCache() {
    return getPathUiCache(false);
  }

  public Path getPathUiCache(boolean isGlobal) {
    return isGlobal ? CacheUtils.getTempFile(FN_CACHE_UI) : get().cache.resolve(FN_CACHE_UI);
  }

}
