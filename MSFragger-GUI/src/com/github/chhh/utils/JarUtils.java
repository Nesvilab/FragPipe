package com.github.chhh.utils;

import static umich.msfragger.params.ThisAppProps.JAR_FILE_AS_RESOURCE_EXT;

import java.io.IOException;
import java.io.InputStream;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.security.CodeSource;
import java.util.Set;

import java.util.logging.Level;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JarUtils {
  private static final Logger log = LoggerFactory.getLogger(JarUtils.class);
  private JarUtils() {}

  /**
   * Unpack and possibly rename a file from this jar to a temp dir.
   */
  public static Path unpackFromJar(String resourceLocation, boolean randomizeName,
      boolean scheduleForDeletion) throws IOException {

    return unpackFromJar(JarUtils.class, resourceLocation, randomizeName, scheduleForDeletion);
  }

  /**
   * Unpack and possibly rename a file from a jar where a class is loaded from to a temp dir.
   * @param clazz The jar from which to unpack is determined by this class.
   * @param resourceLocation Location of the resource within the jar.
   * @param randomizeName Make the name unique, even if there are similarly named files in the
   * temp directory.
   * @param scheduleForDeletion The file will be scheduled for deletion before JVM stops.
   * @return Path to unpacked file.
   */
  public static Path unpackFromJar(Class<?> clazz, String resourceLocation,
      boolean randomizeName, boolean scheduleForDeletion) throws IOException {

    try (InputStream in = clazz.getResourceAsStream(resourceLocation)) {
      final String resourceNameDest = computeFinalResourceName(resourceLocation);

      Path tempFile = randomizeName
          ? Files.createTempFile("fragpipe-", "-" + resourceNameDest)
          : Paths.get(CacheUtils.SYS_TEMP_DIR, resourceNameDest);

      Files.copy(in, tempFile, StandardCopyOption.REPLACE_EXISTING);
      if (scheduleForDeletion) {
        tempFile.toFile().deleteOnExit();
      }
      log.debug("Unpacked temp file (delete on exit = {}): {}", scheduleForDeletion, tempFile.toString());
      return tempFile;
    }

  }

  /**
   * Unpack and possibly rename a file from a jar where a class is loaded from to a temp dir.
   * @param clazz The jar from which to unpack is determined by this class.
   * @param resourceLocation Location of the resource within the jar.
   * @param locationInTemp Additional nested directories inside the system temp dir.
   * @param maintainRelLoc Recreate the original directory structure of the resource location
   * in temp. That will be on top of {@code locationInTemp}.
   * @param scheduleForDeletion The file will be scheduled for deletion before JVM stops.
   * @return Path to unpacked file.
   * @throws IOException
   */
  public static Path unpackFromJar(Class<?> clazz, String resourceLocation, Path locationInTemp,
      boolean maintainRelLoc, boolean scheduleForDeletion) throws IOException {

    try (InputStream in = clazz.getResourceAsStream(resourceLocation)) {
      final String resourceNameDest = computeFinalResourceName(resourceLocation);

      final Path tempDir = Paths.get(CacheUtils.SYS_TEMP_DIR);
      Path destDir = tempDir;
      if (locationInTemp != null)
        destDir = destDir.resolve(locationInTemp);
      if (maintainRelLoc) {
        String loc = computeResourceParent(resourceLocation);
        if (!StringUtils.isNullOrWhitespace(loc))
          destDir = destDir.resolve(Paths.get(loc));
      }
      Path tempFile = destDir.resolve(resourceNameDest);

      if (Files.notExists(destDir)) {
        destDir = Files.createDirectories(destDir);
        if (scheduleForDeletion) {
          destDir.toFile().deleteOnExit();
        }
      }

      Files.copy(in, tempFile, StandardCopyOption.REPLACE_EXISTING);
      if (!OsUtils.isWindows()) {
        final Set<PosixFilePermission> permissions = Files.readAttributes(tempFile, PosixFileAttributes.class).permissions();
        permissions.add(PosixFilePermission.OWNER_EXECUTE);
        permissions.add(PosixFilePermission.GROUP_EXECUTE);
        permissions.add(PosixFilePermission.OTHERS_EXECUTE);
        Files.setAttribute(tempFile, "posix:permissions", permissions);
      }
      if (scheduleForDeletion) {
        tempFile.toFile().deleteOnExit();
      }
      log.debug("Unpacked temp file (delete on exit = {}): {}", scheduleForDeletion, tempFile.toString());
      return tempFile;
    }
  }

  private static String computeResourceParent(String resourceLocation) {
    String s = StringUtils.upToLastChar(resourceLocation, '/', true);
    return s.startsWith("/") ? s.substring(1) : s;
  }

  private static String computeFinalResourceName(String resourceLocation) {
    final String resourceNameOrig = StringUtils.afterLastChar(resourceLocation, '/', false);
    return resourceNameOrig.toLowerCase().endsWith(JAR_FILE_AS_RESOURCE_EXT)
        ? StringUtils.upToLastDot(resourceNameOrig) + ".jar"
        : resourceNameOrig;
  }

  public static String getCurrentJarPath() {
      URI uri = getCurrentJarUri();
      if (uri == null)
          return null;
      String mainPath = extractMainFilePath(uri);
      if (mainPath == null)
          return null;
      try {
          return mainPath;
      } catch (Exception ignore) {
          return null;
      }
  }

  public static URI getCurrentJarUri() {
      try {
          CodeSource codeSource = OsUtils.class.getProtectionDomain().getCodeSource();
          URL location = codeSource.getLocation();
          return location.toURI();
      } catch (URISyntaxException ex) {
          java.util.logging.Logger.getLogger(OsUtils.class.getName()).log(Level.SEVERE, null, ex);
      }
      return null;
  }

  /**
   * Extract the outermost JAR file path from a URI possibly containing resource locations
   * within a JAR. Such URIs contain '!' to indicate locations within a JAR and that breaks
   * normal path parsing utilities, such as {@link Paths#get(URI)}.
   */
  public static String extractMainFilePath(URI uri) {
      String scheme = uri.getScheme();
      URL uriUrl;
      try {
          uriUrl = uri.toURL();
      } catch (MalformedURLException e) {
          return null;
      }

      switch (scheme) {
          case "jar":
          case "jar:file":
              final JarURLConnection conJar;
              try {
                  conJar = (JarURLConnection) uriUrl.openConnection();
              } catch (IOException e) {
                  return null;
              }
              final URL url = conJar.getJarFileURL();
              try {
                  URI uri1 = url.toURI();
                  Path path = Paths.get(uri1);
                  return path.toAbsolutePath().toString();
              } catch (Exception e) {
                  return null;
              }


          case "file":
              return Paths.get(uri).toAbsolutePath().toString();

          default:
              return null;
      }
  }
}
