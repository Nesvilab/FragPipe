package com.github.chhh.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
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
}
