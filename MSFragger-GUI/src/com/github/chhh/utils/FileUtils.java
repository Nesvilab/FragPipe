/*
 * Copyright (c) 2016 Dmitry Avtonomov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.chhh.utils;

import com.github.chhh.utils.FileSizeUnit;
import java.nio.file.Path;
import java.text.DecimalFormat;
import org.jetbrains.annotations.NotNull;

/**
 * @author Dmitry Avtonomov
 */
public class FileUtils {

  private FileUtils() {
  }

  /**
   * Get file size in desired units.
   */
  public static double fileSize(long sizeInBytes, FileSizeUnit unit) {
    double fileSize = (double)sizeInBytes;
    switch (unit) {
      case B:
        return fileSize;
      case kB:
        return fileSize / FileSizeUnit.kB.numBytes;
      case MB:
        return fileSize / FileSizeUnit.MB.numBytes;
      case GB:
        return fileSize / FileSizeUnit.GB.numBytes;
      case TB:
        return fileSize / FileSizeUnit.TB.numBytes;
      case PB:
        return fileSize / FileSizeUnit.PB.numBytes;
      default:
        throw new IllegalArgumentException("Unit not supported");
    }
  }

  /**
   * Returns file size in the most appropriate units.
   */
  public static FileSize fileSize(long sizeInBytes) {
    double size = (double)sizeInBytes;
    return getFileSize(size);
  }

  @NotNull
  private static FileSize getFileSize(double size) {
    final FileSizeUnit[] u = FileSizeUnit.values();
    for (FileSizeUnit fileSizeUnit : u) {
      double val = size / fileSizeUnit.numBytes;
      if (val <= 950) {
        return new FileSize(val, fileSizeUnit);
      }
    }
    return new FileSize(size / FileSizeUnit.PB.numBytes, FileSizeUnit.PB);
  }

  /**
   * Get file size in desired units.
   */
  public static double fileSize(Path path, FileSizeUnit unit) {
    return fileSize(path.toFile().length(), unit);
  }

  /**
   * Returns file size in the most appropriate units.
   *
   * @param path Path to file.
   * @return File size.
   */
  public static FileSize fileSize(Path path) {
    double size = path.toFile().length();

    return getFileSize(size);
  }

  public static class FileSize {
    public static final DecimalFormat FORMAT = new DecimalFormat("0.##");
    public static final DecimalFormat FORMAT_B = new DecimalFormat("0");
    public final double size;
    public final FileSizeUnit unit;

    public FileSize(double size, FileSizeUnit unit) {
      this.size = size;
      this.unit = unit;
    }

    @Override
    public String toString() {
      switch (unit) {
        case B:
          return FORMAT_B.format(size) + " " + unit.name();
        default:
          return FORMAT.format(size) + " " + unit.name();
      }
    }
  }
}
