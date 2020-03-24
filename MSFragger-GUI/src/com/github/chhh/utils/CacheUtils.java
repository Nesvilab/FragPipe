package com.github.chhh.utils;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.ThisAppProps;

public class CacheUtils {
  private static final Logger log = LoggerFactory.getLogger(CacheUtils.class);
  public static final String SYS_TEMP_DIR = System.getProperty("java.io.tmpdir");

  private CacheUtils() {}

  public static Path getTempDir() {
    return getSystemTempDir().resolve(ThisAppProps.APP_TEMP_DIR);
  }

  public static Path getTempDirOld() {
    return Paths.get(SYS_TEMP_DIR);
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
    try {
      return locateTempFile(getTempDir(), fn);
    } catch (FileNotFoundException ex) {
      return locateTempFile(getTempDirOld(), fn);
    }
  }

  /**
   * Search for all existing files in known (new and old) locations.
   */
  public static List<Path> locateTempFiles(String fn) {
    final List<Path> paths = new ArrayList<>();
    final List<Path> dirs = Arrays.asList(getTempDir(), getTempDirOld());
    for (Path dir : dirs) {
      try {
        Path path = locateTempFile(fn);
        if (path != null) {
          paths.add(path);
        }
      } catch (Exception ignored) {}
    }
    return paths;
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
    if (SYS_TEMP_DIR == null || SYS_TEMP_DIR.isEmpty())
      throw new IllegalStateException("Could not locate system-wide temporary directory");
    return Paths.get(SYS_TEMP_DIR);
  }

}
