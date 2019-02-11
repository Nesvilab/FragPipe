package umich.msfragger.util;

import java.io.FileNotFoundException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import umich.msfragger.params.ThisAppProps;

public class CacheUtils {
  private CacheUtils() {}

  public static Path getTempDir() {
    return Paths.get(ThisAppProps.SYS_TEMP_DIR).resolve(ThisAppProps.APP_TEMP_DIR);
  }

  public static Path getTempDirOld() {
    return Paths.get(ThisAppProps.SYS_TEMP_DIR);
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
   * @param fn File-name for the temp file. Location is predetermined.
   */
  public static Path getTempFile(String fn) {
    return getTempDir().resolve(fn);
  }
}
