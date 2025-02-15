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

package org.nesvilab.utils;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.fragpipe.params.ThisAppProps;

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
    if (!Files.exists(p.toAbsolutePath().getParent())) {
      try {
        Files.createDirectories(p.toAbsolutePath().getParent());
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
    final String dir = OsUtils.isUnix() ? XDG_CONFIG_HOME + "/FragPipe" : SYS_TEMP_DIR;
    if (dir == null || dir.isEmpty())
      throw new IllegalStateException("Could not locate system-wide temporary directory");
    return Paths.get(dir);
  }

}
