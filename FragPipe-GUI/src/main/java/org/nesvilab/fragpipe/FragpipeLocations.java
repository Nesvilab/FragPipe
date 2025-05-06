/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe;

import static org.nesvilab.fragpipe.Version.PROGRAM_TITLE;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.api.PropsFile;
import org.nesvilab.fragpipe.messages.MissingAssetsException;
import org.nesvilab.fragpipe.messages.NoteFragpipeCache;
import org.nesvilab.utils.*;
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
  private final Path workflows;
  private final Path databases;
  private final Path longTermStorage;
  private final Path jobs;

  private FragpipeLocations(Path jarPath, Path cache, Path tools, Path workflows,
      Path databases, Path jobs, Path longTermStorage) {
    this.jarPath = jarPath;
    this.cache = cache;
    this.tools = tools;
    this.workflows = workflows;
    this.databases = databases;
    this.longTermStorage = longTermStorage;
    this.jobs = jobs;
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
      Path jarPath = Paths.get(fragpipeJar).toAbsolutePath();
      log.debug("Jar path: {}", jarPath);
      final Path jarDir = Files.isDirectory(jarPath) ? jarPath : jarPath.getParent();
      log.debug("Jar dir: {}", jarDir);
//      final Path dir = OsUtils.isWindows() ? jarDir : Paths.get(CacheUtils.XDG_CACHE_HOME, "FragPipe", "bin");
      final Path dir = jarDir;

      Path cache = dir.resolve(Paths.get("../cache"));

      Path tools;
      final String debugClassLoc = "build/classes/java/main";
      final String debugParentDirName = "FragPipe-GUI";
      if (dir.toString().toLowerCase().replaceAll("\\\\", "/").contains(debugClassLoc)) {
        Path debugDir = dir;
        while (!debugDir.getFileName().toString().equals(debugParentDirName) && debugDir.getParent() != null) {
          debugDir = debugDir.getParent();
        }
        if (!debugDir.getFileName().toString().equals(debugParentDirName)) {
          throw new IllegalStateException("Could not set up tools location for debug session");
        }
        tools = debugDir.resolve(Paths.get("../tools"));
      } else {
        tools = dir.resolve(Paths.get("../tools"));
      }

      Path workflows = dir.resolve("../workflows");
      Path databases = dir.resolve("../databases");
      Path longTermStorage = CacheUtils.getTempDir();
      Path jobs = dir.resolve("../jobs");
      // create locations if they don't yet exist
      List<Path> paths = Arrays.asList(dir, cache, tools, workflows, databases, jobs, longTermStorage);
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

      locations = new FragpipeLocations(jarPath, cache, tools, workflows, databases, jobs, longTermStorage);
    }
  }

  public Path getDirCache() {
    return cache;
  }

  public Path getDirTools() {
    return tools;
  }

  public Path getDirJobs() {
      return jobs;
  }
  
  public Path getDirDatabases() {
    return databases;
  }

  public Path getDirFragpipeRoot() {
    return getJarPath().toAbsolutePath().getParent().getParent();
  }

  public Path getOrMakeDirInRoot(String dir) {
    Path path = getDirFragpipeRoot().resolve(dir);
    try {
      return PathUtils.createDirs(path);
    } catch (IOException e) {
      log.warn("Couldn't create sub directory {} inside " + PROGRAM_TITLE + " folder.", dir);
      return null;
    }
  }

  public Path getDirWorkflows() {
    return workflows;
  }

  public List<Path> getCachePaths() {
    List<Path> paths = new ArrayList<>();
    paths.add(getPathRuntimeCache(true));
    paths.add(getPathUiCache(true));
    paths.add(getWorkflowsCache(true));
    return paths;
  }

  public void delete(List<Path> paths) throws IOException {
    for (Path path : paths) {
      if (Files.isDirectory(path)) {
        try (Stream<Path> walk = Files.walk(path)) {
          for (Path p : walk.sorted(Comparator.reverseOrder()).collect(Collectors.toList())) {
            Files.deleteIfExists(p);
          }
        }
      } else {
        Files.deleteIfExists(path);
      }
    }
  }

  public static FragpipeLocations get() {
    return Holder.locations;
  }

  private String createCacheComment(String cacheType) {
    return Version.version(true) + cacheType + " cache";
  }

  public NoteFragpipeCache loadCache() {
    PropsFile pfRuntime, pfUi;

    Path p = getPathUiCache(true);
    if (Files.exists(p)) {
      pfUi = tryLoadSilently(p, createCacheComment("ui state"));
    } else {
      pfUi = new PropsFile(getPathUiCache(true), createCacheComment("ui state"));
    }

    p = getPathRuntimeCache(true);
    if (Files.exists(p)) {
      pfRuntime = tryLoadSilently(p, createCacheComment("runtime"));
    } else {
      pfRuntime = new PropsFile(getPathRuntimeCache(true), createCacheComment("runtime"));
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

  public Path getFragpipeBin() {
    Path binDir = getDirFragpipeRoot().resolve("bin");
    Path p;
    if (OsUtils.isWindows()) {
      p = binDir.resolve("fragpipe.bat");
    } else {
      p = binDir.resolve("fragpipe");
    }
    Path exists = PathUtils.existing(p.toString());
    if (exists != null) {
      return exists;
    }
    return p;
  }

  public Path getJarPath() {
    return this.jarPath;
  }

  public Path getPathRuntimeCache(boolean isSystemCache) {
    return isSystemCache ? CacheUtils.getTempFile(FN_CACHE_RUNTIME) : get().cache.resolve(FN_CACHE_RUNTIME);
  }

  public Path getPathLongTermStorage() {
    return longTermStorage;
  }

  public Path getPathUiCache(boolean isSystemCache) {
    return isSystemCache ? CacheUtils.getTempFile(FN_CACHE_UI) : get().cache.resolve(FN_CACHE_UI);
  }

  public Path getWorkflowsCache(boolean isSystemCache) {
    return isSystemCache ? CacheUtils.getTempFile(FN_CACHE_UI).resolveSibling("workflows") : get().cache.resolve(FN_CACHE_UI).resolveSibling("workflows");
  }

  private static Path tryLocateAsset(Path path) {
    return PathUtils.existing(path);
  }

  private static Path tryLocateBestAlternative(Path path) {
    String fn = path.getFileName().toString();
    String lessExt = StringUtils.upToLastDot(fn);
    Pattern re = Pattern.compile("(.+?)(\\d+[.\\d-]*)$");

    Matcher m = re.matcher(lessExt);
    if (!m.matches()) {
      // this doesn't look like a versioned file name
      return null;
    }
    final String fnBaseLo = m.group(1).toLowerCase();
    try {
      final Path dirTools = FragpipeLocations.get().getDirTools();
      List<Path> matching = Files.walk(dirTools)
          .filter(f -> {
            String fnLessExt = StringUtils.upToLastDot(f.getFileName().toString());
            Matcher ma = re.matcher(fnLessExt);
            return f.getFileName().toString().toLowerCase().startsWith(fnBaseLo)
                && ma.matches()
                && Files.isRegularFile(f);
          })
          .collect(Collectors.toList());
      if (matching.isEmpty())
        return null;
      final VersionComparator vc = new VersionComparator();
      matching.sort((o2, o1) -> vc.compare(o1.getFileName().toString(), o2.getFileName().toString()));
      return matching.get(0);
    } catch (IOException e) {
      log.warn("Something happened while searching for tool replacements with different version", e);
      return null;
    }
  }

  public static List<Path> tryLocateTools(Stream<String> assets)
      throws MissingAssetsException {
    final Path dirTools = FragpipeLocations.get().getDirTools();
    List<Path> toCheck = assets.map(dirTools::resolve).collect(Collectors.toList());
    List<Path> found = new ArrayList<>();

    for (Path path : toCheck) {
      Path located = tryLocateBestAlternative(path);
      if (located == null) {
        located = tryLocateAsset(path);
      } else {
        log.debug("Located better alternative: {} -> {}", path, located);
      }
      if (located == null) {
        throw new MissingAssetsException(Collections.singletonList(path));
      }
      found.add(located.normalize());
    }
    return found;
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
      return tryLocateTools(assets);
    } catch (MissingAssetsException e) {
      String fileList = Seq.seq(e.getNotExisting()).map(Path::toString).toString("\n");
      log.error("Missing assets: {}", fileList);
      SwingUtils.showErrorDialog(null, "Missing assets:\n"+ fileList, "Missing assets");
    }
    return null;
  }

  public static Path locateLicense() {
    Path license = null;
    try {
      Path[] dirs = {FragpipeLocations.get().tools.resolve("../"), FragpipeLocations.get().tools.resolve("../../")};
      for (Path dir : dirs) {
        license = Files.list(dir)
          .filter(Files::isRegularFile)
          .filter(p -> p.getFileName().toString().toLowerCase().startsWith("license") && p.getFileName().toString().toLowerCase().endsWith(".dat"))
          .findFirst()
          .orElse(null);
        if (license != null) {
          break;
        }
      }
    } catch (IOException e) {
      throw new RuntimeException("Error locating license", e);
    }
    return license;
  }
}
