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
