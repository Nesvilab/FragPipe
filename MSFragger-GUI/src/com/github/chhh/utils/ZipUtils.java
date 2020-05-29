package com.github.chhh.utils;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ZipUtils {
  private static final Logger log = LoggerFactory.getLogger(ZipUtils.class);

  public static void unzip(Path fileZip, Path destDir) throws IOException {
    if (!Files.isDirectory(destDir)) {
      throw new IOException("Destination not a directory");
    }
    byte[] buffer = new byte[8192];
    ZipInputStream zis = new ZipInputStream(new FileInputStream(fileZip.toFile()));
    ZipEntry zipEntry = zis.getNextEntry();
    while (zipEntry != null) {
      File newFile = newFile(destDir.toFile(), zipEntry);
      try (FileOutputStream fos = new FileOutputStream(newFile)) {
        int len;
        while ((len = zis.read(buffer)) > 0) {
          fos.write(buffer, 0, len);
        }
      }
      zipEntry = zis.getNextEntry();
    }
    zis.closeEntry();
    zis.close();
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
    ZipFile zip = new ZipFile(zipPath.toFile());
    FileSystem fileSystem = FileSystems.getDefault();
    Enumeration<? extends ZipEntry> entries = zip.entries();
    byte[] buffer = new byte[8192];
    ZipInputStream zis = new ZipInputStream(new FileInputStream(zipPath.toFile()));

    //Iterate over entries
    while (entries.hasMoreElements())
    {
      ZipEntry zipEntry = entries.nextElement();

      if (zipEntry.isDirectory()) {
        // If directory then create a new directory in uncompressed folder
        Path unzippedDir = destDir.resolve(zipEntry.getName());
        if (!unzippedDir.normalize().startsWith(destDir)) {
          throw new IOException("Entry is outside of target dir: " + zipEntry.getName());
        }
        log.debug("Possibly creating unzipped direcotry: {}", unzippedDir);
        PathUtils.createDirs(unzippedDir);

      } else {
        // Else create the file
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
