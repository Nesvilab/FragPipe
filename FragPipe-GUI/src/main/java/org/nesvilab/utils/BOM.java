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
 * Type safe enumeration class that describes the different types of Unicode
 * BOMs.
 */
public enum BOM {
  /** NONE. */
  NONE(new byte[]{}, "NONE"),

  /** UTF-8 BOM (EF BB BF). */
  UTF_8(new byte[]{
      (byte) 0xEF,
      (byte) 0xBB,
      (byte) 0xBF},
      "UTF-8"),

  /** UTF-16, little-endian (FF FE). */
  UTF_16_LE(new byte[]{
      (byte) 0xFF,
      (byte) 0xFE},
      "UTF-16LE"),

  /** UTF-16, big-endian (FE FF). */
  UTF_16_BE(new byte[]{
      (byte) 0xFE,
      (byte) 0xFF},
      "UTF_16BE"),

  /** * UTF-32, little-endian (FF FE 00 00). */
  UTF_32_LE(new byte[]{
      (byte) 0xFF,
      (byte) 0xFE,
      (byte) 0x00,
      (byte) 0x00},
      "UTF-32LE"),

  /** UTF-32, big-endian (00 00 FE FF). */
  UTF_32_BE(new byte[]{
      (byte) 0x00,
      (byte) 0x00,
      (byte) 0xFE,
      (byte) 0xFF},
      "UTF-32BE");

  final byte bytes[];
  final String alias;


  BOM(final byte bom[], final String alias) {
    assert (bom != null) : "invalid BOM: null is not allowed";
    assert (alias != null) : "invalid alias: null is not allowed";
    assert (alias.length() != 0) : "invalid alias: empty string is not allowed";

    this.bytes = bom;
    this.alias = alias;
  }

  /**
   * Returns a <code>String</code> representation of this <code>BOM</code>
   * value.
   */
  public final String toString() {
    return alias;
  }

  /**
   * Returns the bytes corresponding to this <code>BOM</code> value.
   */
  public final byte[] getBytes() {
    final int length = bytes.length;
    final byte[] result = new byte[length];

    // Make a defensive copy
    System.arraycopy(bytes, 0, result, 0, length);

    return result;
  }

}
