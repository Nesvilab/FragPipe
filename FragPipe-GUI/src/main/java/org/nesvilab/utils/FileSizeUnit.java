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

/**
 * @author Dmitry Avtonomov
 */
public enum FileSizeUnit {
  B(1L),
  kB(pow(1024L, 1)),
  MB(pow(1024L, 2)),
  GB(pow(1024L, 3)),
  TB(pow(1024L, 4)),
  PB(pow(1024L, 5));
  //EB (pow(1024L, 6)),
  //ZB (pow(1024L, 7)),
  //YB (pow(1024L, 8));

  public final long numBytes;

  FileSizeUnit(long numBytes) {
    this.numBytes = numBytes;
  }

  private static long pow(final long value, final int power) {
    long result = 1L;
    for (int i = 0; i < power; i++) {
      long prev = result;
      result = prev * value;
      if (prev != 0 && value > Long.MAX_VALUE / prev) {
        throw new ArithmeticException("Overflow when calculating power(long, int)");
      }
    }
    return result;
  }
}
