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

import java.io.IOException;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.FileSystem;
import java.nio.file.FileSystemAlreadyExistsException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.CodeSource;
import java.util.Collections;
import java.util.Iterator;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JarUtils {
  private static final Logger log = LoggerFactory.getLogger(JarUtils.class);
  private JarUtils() {}

  public static void walkResources(String resourcesDir, Consumer<Path> callback) throws IOException {
    URI uri = null;
    try {
      uri = JarUtils.class.getResource(resourcesDir).toURI();
    } catch (URISyntaxException e) {
      throw new IllegalArgumentException("Not a valid URI", e);
    }
    Path myPath;
    if (uri.getScheme().equals("jar")) {
      FileSystem fileSystem;
      try {
        fileSystem = FileSystems.newFileSystem(uri, Collections.emptyMap());
      } catch (FileSystemAlreadyExistsException ex) {
        fileSystem = FileSystems.getFileSystem(uri);
        System.err.println("File system " + uri + " already exist. Using the existing one.");
      }
      myPath = fileSystem.getPath(resourcesDir);
    } else {
      myPath = Paths.get(uri);
    }
    Stream<Path> walk = Files.walk(myPath, 1);
    for (Iterator<Path> it = walk.iterator(); it.hasNext();){
      Path next = it.next();
      callback.accept(next);
    }
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

  public static boolean isInIDE() {
      String jarPath = getCurrentJarPath();
      return jarPath != null && jarPath.toLowerCase().replaceAll("\\\\", "/").contains("build/classes/java/main");
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
                  return path.toAbsolutePath().normalize().toString();
              } catch (Exception e) {
                  return null;
              }


          case "file":
              return Paths.get(uri).toAbsolutePath().normalize().toString();

          default:
              return null;
      }
  }
}
