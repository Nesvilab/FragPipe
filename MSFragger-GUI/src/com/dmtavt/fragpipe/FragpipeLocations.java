package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.PropsFile;
import com.dmtavt.fragpipe.messages.MissingAssetsException;
import com.dmtavt.fragpipe.messages.NoteFragpipeCache;
import com.github.chhh.utils.CacheUtils;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeLocations {
  private static volatile FragpipeLocations INSTANCE = null;
  private static final Logger log = LoggerFactory.getLogger(FragpipeLocations.class);
  public static final String FN_CACHE_UI = "fragpipe-ui.cache";
  public static final String FN_CACHE_RUNTIME = "fragpipe-runtime.cache";

  private final Path jarPath;
  private final Path cache;
  private final Path tools;
  private final Path lib;
  private final Path workflows;

  private FragpipeLocations(Path jarPath, Path cache, Path tools, Path lib, Path workflows) {
    this.jarPath = jarPath;
    this.cache = cache;
    this.tools = tools;
    this.lib = lib;
    this.workflows = workflows;
  }

  public static class Holder {
    public static String fragpipeJar;
    public static FragpipeLocations locations;
    private static void printDebug(AtomicInteger a) {
      System.err.printf("============================ %03d\n", a.incrementAndGet());
    }
    static {
      //AtomicInteger a = new AtomicInteger(0);
      fragpipeJar = JarUtils.getCurrentJarPath();
      log.debug("Caching class determined fargpipe jar file is at: {}", Holder.fragpipeJar);
      if (StringUtils.isBlank(fragpipeJar)) {
        log.error("Could not figure fragpipe jar location at runtime");
        throw new IllegalStateException("Could not figure fragpipe jar location at runtime");
      }
      Path jarPath = Paths.get(fragpipeJar);
      log.debug("Jar path: {}", jarPath);
      Path dir = Files.isDirectory(jarPath) ? jarPath : jarPath.getParent();
      log.debug("Jar dir: {}", dir);

      Path cache = dir.resolve(Paths.get("../cache"));

      Path tools;
      final String debugClassLoc = "build/classes/java/main";
      final String debugParentDirName = "MSFragger-GUI";
      if (dir.toString().toLowerCase().replaceAll("\\\\", "/").contains(debugClassLoc)) {
        Path debugDir = dir;
        while (!debugDir.getFileName().toString().equals(debugParentDirName) && debugDir.getParent() != null) {
          debugDir = debugDir.getParent();
        }
        if (!debugDir.getFileName().toString().equals(debugParentDirName)) {
          throw new IllegalStateException("Could not set up tools location for debug session");
        }
        tools = debugDir.resolve(Paths.get("tools"));
      } else {
        tools = dir.resolve(Paths.get("../tools"));
      }

      Path lib = dir.resolve(Paths.get("../lib"));
      Path workflows = dir.resolve("../workflows");

      // create locations if they don't yet exist
      List<Path> paths = Arrays.asList(dir, cache, tools, lib, workflows);
      log.debug("Fragpipe locations:\n\t{}",
          paths.stream().map(Path::toString).collect(Collectors.joining("\n\t")));
      for (Path path : paths) {
        try {
          if (Files.exists(path)) {
            continue;
          }
          PathUtils.createDirs(path);
        } catch (Exception e) {
          log.error("Error initializing fragpipe locations", e);
          //throw new IllegalStateException("Error initializing fragpipe locations", e);
        }
      }

      locations = new FragpipeLocations(jarPath, cache, tools, lib, workflows);
    }
  }

  public Path getDirCache() {
    return cache;
  }

  public Path getDirTools() {
    return tools;
  }

  public Path getDirApp() {
    return getJarPath().getParent().getParent();
  }

  public Path getDirWorkflows() {
    return workflows;
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

  public Path getJarPath() {
    return this.jarPath;
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


  public static List<Path> createToolsPaths(Stream<String> assets)
      throws MissingAssetsException {
    List<Path> paths = assets.map(asset -> FragpipeLocations.get().getDirTools().resolve(asset))
        .collect(Collectors.toList());
    List<Path> notExisting = Seq.seq(paths).filter(Files::notExists).toList();
    if (!notExisting.isEmpty()) {
      throw new MissingAssetsException(notExisting);
    }
    return paths;
  }

  /**
   * Checks if any assets are missing and shows an error dialog saying which are, returns NULL
   * in this case. Otherwise fills the array with locations of actual assets.
   *
   * @param assets          Locations relative to tools/ subdir.
   * @return null in case of errors or missing files.
   */
  public static List<Path> checkToolsMissing(Stream<String> assets) {
    try {
      return createToolsPaths(assets);
    } catch (MissingAssetsException e) {
      String fileList = Seq.seq(e.getNotExisting()).map(Path::toString).toString("\n");
      log.error("Missing assets: {}", fileList);
      SwingUtils.showErrorDialog(null, "Missing assets:\n"+ fileList, "Missing assets");
    }
    return null;
  }

  /**
   * Checks if an asset is missing and shows an error dialog saying which are. Otherwise shows UI
   * with error and returns false.
   *
   * @param asset           Location relative to tools/ subdir.
   * @return null if asset is missing and error message was show.
   */
  public static Path checkToolMissing(String asset) {
    try {
      return createToolsPaths(Stream.of(asset)).get(0);
    } catch (MissingAssetsException e) {
      String fileList = Seq.seq(e.getNotExisting()).map(Path::toString).toString("\n");
      SwingUtils.showErrorDialog(null, "Missing assets:\n"+ fileList, "Missing assets");
    }
    return null;
  }
}
