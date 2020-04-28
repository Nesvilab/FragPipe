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
