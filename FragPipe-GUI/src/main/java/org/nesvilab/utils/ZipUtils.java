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

import static org.nesvilab.utils.PosixFileAttributes.permissions;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ZipUtils {
  private static final Logger log = LoggerFactory.getLogger(ZipUtils.class);

  @Deprecated
  public static void unzip_deprecated(Path fileZip, Path destDir) throws IOException {
    if (!Files.isDirectory(destDir)) {
      throw new IOException("Destination not a directory");
    }
    byte[] buffer = new byte[8192];
    ZipInputStream zis = new ZipInputStream(new FileInputStream(fileZip.toFile()));
    ZipEntry zipEntry = zis.getNextEntry();
    while (zipEntry != null) {
      File newFile = newFile(destDir.toFile(), zipEntry);
      boolean isELF = false;
      try (FileOutputStream fos = new FileOutputStream(newFile)) {
        int len;
        boolean first = true;
        while ((len = zis.read(buffer)) > 0) {
          if (first) {
            isELF = new String(buffer, 0, 4).equals("\u007fELF");
            first = false;
          }
          fos.write(buffer, 0, len);
        }
      }
      // if unzipping ELF files on Unix, add execute bit
      // shell scripts need execute bit too, but is currently omitted
      if (OsUtils.isUnix() && isELF) {
        final Set<PosixFilePermission> permissions = Files.readAttributes(newFile.toPath(), PosixFileAttributes.class).permissions();
        permissions.add(PosixFilePermission.OWNER_EXECUTE);
        permissions.add(PosixFilePermission.GROUP_EXECUTE);
        permissions.add(PosixFilePermission.OTHERS_EXECUTE);
        Files.setPosixFilePermissions(newFile.toPath(), permissions);
      }
      zipEntry = zis.getNextEntry();
    }
    zis.closeEntry();
    zis.close();
  }

  public static void unzip(Path fileZip, Path destDir) throws IOException {
    if (!Files.isDirectory(destDir)) {
      throw new IOException("Destination not a directory");
    }
    try(final org.apache.commons.compress.archivers.zip.ZipFile zipFile = org.apache.commons.compress.archivers.zip.ZipFile.builder().setFile(fileZip.toFile()).get()) {
      for (final ZipArchiveEntry zipEntry : Collections.list(zipFile.getEntries())) {
        final File newFile = newFile(destDir.toFile(), zipEntry);
        try (final FileOutputStream fos = new FileOutputStream(newFile)) {
          IOUtils.copy(zipFile.getInputStream(zipEntry), fos);
        }
        // unzip with Posix file permissions when possible
        try {
          Files.setPosixFilePermissions(newFile.toPath(), permissions(zipEntry.getUnixMode()));
        } catch (UnsupportedOperationException ignored) {
        }
      }
    }
  }

  public static File newFile(File destinationDir, ZipEntry zipEntry) throws IOException {
    File destFile = new File(destinationDir, zipEntry.getName());
    log.debug("Creating new file: {}", destFile);

    String destDirPath = destinationDir.getCanonicalPath();
    String destFilePath = destFile.getCanonicalPath();

    if (!destFilePath.startsWith(destDirPath + File.separator)) {
      throw new IOException("Entry is outside of the target dir: " + zipEntry.getName());
    }

    return destFile;
  }

  public static void unzipWithSubfolders(Path zipPath, Path destDir) throws IOException {
    try (ZipFile zip = new ZipFile(zipPath.toFile())) {

      Enumeration<? extends ZipEntry> entries = zip.entries();
      byte[] buffer = new byte[8192];

      //Iterate over entries
      while (entries.hasMoreElements()) {
        ZipEntry zipEntry = entries.nextElement();

        if (zipEntry.isDirectory()) {
          // If directory then create a new directory in uncompressed folder
          Path unzippedDir = destDir.resolve(zipEntry.getName());
          if (!unzippedDir.normalize().startsWith(destDir.normalize())) {
            throw new IOException("Entry is outside of target dir: " + zipEntry.getName() + "; unzipped dir: " + unzippedDir.normalize().toString() + "; dest dir: " + destDir.normalize().toString());
          }
          log.debug("Possibly creating unzipped direcotry: {}", unzippedDir);
          PathUtils.createDirs(unzippedDir);

        } else {
          // Else create the file and copy contents
          File newFile = newFile(destDir.toFile(), zipEntry);
          log.debug("Writing unzipped file: {}", newFile);
          try (FileOutputStream fos = new FileOutputStream(newFile)) {
            try (BufferedInputStream bis = new BufferedInputStream(zip.getInputStream(zipEntry))) {
              int read;
              while ((read = bis.read(buffer)) > 0) {
                fos.write(buffer, 0, read);
              }
            }
          }
        }
      }
    }
  }
}
