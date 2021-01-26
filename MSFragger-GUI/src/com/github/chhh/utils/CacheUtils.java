package com.github.chhh.utils;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.params.ThisAppProps;

public class CacheUtils {
  private static final Logger log = LoggerFactory.getLogger(CacheUtils.class);
  public static final String SYS_TEMP_DIR = System.getProperty("java.io.tmpdir");

  public static final String XDG_CONFIG_HOME = ((Supplier<String>) () -> {
    if (OsUtils.isUnix()) {
      if (System.getenv("XDG_CONFIG_HOME") == null || System.getenv("XDG_CONFIG_HOME").isEmpty())
        return System.getProperty("user.home") + "/.config";
      else
        return System.getenv("XDG_CONFIG_HOME");
    }
    return null;
  }).get();
  public static final String XDG_CACHE_HOME = ((Supplier<String>) () -> {
    if (OsUtils.isUnix()) {
      if (System.getenv("XDG_CACHE_HOME") == null || System.getenv("XDG_CACHE_HOME").isEmpty())
        return System.getProperty("user.home") + "/.cache";
      else
        return System.getenv("XDG_CACHE_HOME");
    }
    return null;
  }).get();

  private CacheUtils() {}

  public static Path getTempDir() {
    return getSystemTempDir().resolve(ThisAppProps.APP_TEMP_DIR);
  }

  private static Path locateTempFile(Path tempDir, String fn) throws FileNotFoundException {
    Path file = tempDir.resolve(fn);
    if (!Files.exists(file))
      throw new FileNotFoundException("File '" + fn + "' not found in: " + tempDir.toString());
    if (!Files.isRegularFile(file) || !Files.isReadable(file))
      throw new FileNotFoundException("File '" + file.toString() + "' is not a regular readable file.");
    return file;
  }

  /**
   * Search for an existing file in known (new and old) locations.
   */
  public static Path locateTempFile(String fn) throws FileNotFoundException {
    return locateTempFile(getTempDir(), fn);
  }

  /**
   * @param fn File-name for the temp file. Location is predetermined.
   */
  public static Path getTempFile(String fn) {
    Path p = getTempDir().resolve(fn);
    if (!Files.exists(p.getParent())) {
      try {
        Files.createDirectories(p.getParent());
      } catch (IOException e) {
        throw new IllegalStateException("Could not create directory structure for a temporary file: " + p.toString());
      }
    }
    return p;
  }

  /**
   * System-wide temporary directory.
   * @return
   */
  public static Path getSystemTempDir() {
    final String dir = OsUtils.isUnix() ? XDG_CONFIG_HOME + "/FrapPipe" : SYS_TEMP_DIR;
    if (dir == null || dir.isEmpty())
      throw new IllegalStateException("Could not locate system-wide temporary directory");
    return Paths.get(dir);
  }

}
