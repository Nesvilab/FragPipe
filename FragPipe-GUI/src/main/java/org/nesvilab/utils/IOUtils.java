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

import org.nesvilab.utils.okio.SourceMarker;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import okio.Buffer;
import okio.BufferedSource;
import okio.ByteString;
import okio.Okio;
import okio.Source;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Dmitry Avtonomov
 */
public class IOUtils {

  private static final Logger log = LoggerFactory.getLogger(IOUtils.class);

  private IOUtils() {
  }

  /**
   * Like {@link Files#readAllLines(java.nio.file.Path, java.nio.charset.Charset) } method, but
   * works with streams.
   *
   * @param is The stream is closed after reading. It's up to the user to make sure it's not an
   *           endless stream.
   */
  public static List<String> readAllLines(InputStream is) throws IOException {
    return readAllLines(is, StandardCharsets.UTF_8);
  }

  /**
   * Like {@link Files#readAllLines(java.nio.file.Path, java.nio.charset.Charset) } method, but
   * works with streams.
   *
   * @param is The stream is closed after reading. It's up to the user to make sure it's not an
   *           endless stream.
   * @param cs Charset for decoder.
   */
  public static List<String> readAllLines(InputStream is, Charset cs) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(is, cs))) {
      List<String> result = new ArrayList<>();
      for (; ; ) {
        String line = reader.readLine();
        if (line == null) {
          break;
        }
        result.add(line);
      }
      return result;
    }
  }

  /** Read the object from Base64 string. */
  private static Object fromString(String s) throws IOException, ClassNotFoundException {
    byte[] data = Base64.getDecoder().decode(s);
    ObjectInputStream ois = new ObjectInputStream(
        new ByteArrayInputStream(data));
    Object o = ois.readObject();
    ois.close();
    return o;
  }

  /** Write the object to a Base64 string. */
  private static String toString(Serializable o) throws IOException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ObjectOutputStream oos = new ObjectOutputStream(baos);
    oos.writeObject(o);
    oos.close();
    return Base64.getEncoder().encodeToString(baos.toByteArray());
  }

  private static int[] toUtf8CodePoints(String s) throws IOException {
    ByteArrayInputStream bais = new ByteArrayInputStream(s.getBytes());
    int[] codes = new int[s.length() * 4]; // max num bytes for this string
    try (Source src = Okio.source(bais); BufferedSource buf = Okio.buffer(src)) {
      int code;
      int ptr = -1;
      while (!buf.exhausted()) {
        code = buf.readUtf8CodePoint();
        System.out.println("Code: " + code);
        codes[++ptr] = code;
      }
      return Arrays.copyOf(codes, ptr + 1);
    }
  }

  public static BOM detectBom(BufferedSource bufferedSource) throws IOException {
    int bomMaxLen = Arrays.stream(BOM.values())
        .mapToInt(bom -> bom.getBytes().length).max().orElse(0);
    final byte[] readBuf = new byte[bomMaxLen];
    final int read;
    try (BufferedSource peek = bufferedSource.peek()) {
      read = peek.read(readBuf);
    }
    for (BOM ref : BOM.values()) {
      if (ref == BOM.NONE || ref.bytes.length > read) {
        continue;
      }
      boolean match = true;
      for (int i = 0; i < ref.bytes.length; i++) {
        if (ref.bytes[i] != readBuf[i]) {
          match = false;
          break;
        }
      }
      if (match) {
        return ref;
      }
    }
    return BOM.NONE;
  }

  public static int codePointSizeBytes(byte[] bytes) {
    byte b0 = bytes[0];
    int codePoint;
    int byteCount;
    int min;

    if ((b0 & 0x80) == 0) {
      // 0xxxxxxx.
      codePoint = b0 & 0x7f;
      byteCount = 1; // 7 bits (ASCII).
      min = 0x0;
    } else if ((b0 & 0xe0) == 0xc0) {
      // 0x110xxxxx
      codePoint = b0 & 0x1f;
      byteCount = 2; // 11 bits (5 + 6).
      min = 0x80;
    } else if ((b0 & 0xf0) == 0xe0) {
      // 0x1110xxxx
      codePoint = b0 & 0x0f;
      byteCount = 3; // 16 bits (4 + 6 + 6).
      min = 0x800;
    } else if ((b0 & 0xf8) == 0xf0) {
      // 0x11110xxx
      codePoint = b0 & 0x07;
      byteCount = 4; // 21 bits (3 + 6 + 6 + 6).
      min = 0x10000;
    } else {
      throw new UnsupportedOperationException("Don't know how to treat this codepoint");
    }
    return byteCount;
  }

  public static int codePointSizeBytes(int codePoint) {
    log.debug("Code point dec[{}], oct[{}], hex[{}], bin[{}]", codePoint,
        Integer.toOctalString(codePoint), Integer.toHexString(codePoint),
        Integer.toBinaryString(codePoint));
    if (codePoint >= 0 && codePoint <= 0x007F) {
      return 1;
    }
    if (codePoint <= 0x07FF) {
      return 2;
    }
    if (codePoint <= 0xFFFF) {
      return 3;
    }
    if (codePoint <= 0x10FFFF) {
      return 4;
    }
    throw new UnsupportedOperationException("Code point out of range");
  }

  public static void tokenize(InputStream is, String start, String end) throws IOException {
    Buffer b = new Buffer();
    ByteString needle = new ByteString(start.getBytes(StandardCharsets.UTF_8));
    int size = 16;
    ByteBuffer bb = ByteBuffer.allocate(size);
    try (Source src = Okio.source(is); BufferedSource buf = Okio.buffer(src)) {
      long read, total = 0, totalPrev = -1;
      read = buf.read(bb);
      bb.flip();
      while ((read = buf.read(b, size)) > 0) {
        totalPrev = total;
        total += read;
        log.debug("Read {}/{} (={}) bytes. b.completeSegmentByteCount()={}", read, size, total,
            b.completeSegmentByteCount());
        long from = -1;
        long pos;
        while ((pos = b.indexOf(needle, ++from)) >= 0) {
          log.debug("{} @ {}", start, totalPrev + pos);
        }

      }
    }
  }

  public static void tokenize1(InputStream is, String start, String end) throws IOException {
    ByteString needle = new ByteString(start.getBytes(StandardCharsets.UTF_8));
    byte[] bytes = needle.toByteArray();
    int size = 8192;
    ByteBuffer bb = ByteBuffer.allocate(size);
    try (Source src = Okio.source(is); BufferedSource buf = Okio.buffer(src)) {
      long read, total = 0;
      long lo = -1, hi = -1;
      int ptr = 0;
      ArrayList<Long> locs = new ArrayList<>();
      while ((read = buf.read(bb)) > 0) {
        bb.flip();
        while (bb.position() < bb.limit()) {
          byte b = bb.get();
          if (b == bytes[ptr]) {
            //log.debug("Found '{}' @ {}", needle.utf8(), total + bb.position() - 1);
            if (ptr == bytes.length - 1) {
              // found match
            }

            long pos = total + bb.position() - 1;
            locs.add(pos);
            if (locs.size() % 20 == 0) {
              //log.debug("{} @ {}", needle.utf8(), locs);
              //System.out.printf("'%s' @ %s\n", needle.utf8(), locs.toString());
              locs.clear();
            }
          }
        }
        total += read;
        bb.flip();
      }
      System.out.println("Done");
    }
  }

  public static class Tracker {

    public byte[] seqLo;
    public byte[] seqMi;
    public byte[] seqHi;
    public int posLo = -1;
    public int posMi = -1;
    public int posHi = -1;

    public void reset() {
      posLo = 0;
      posMi = 0;
      posHi = 0;
    }
  }

  public static class Needle {
    public final byte[] bytes;
    public int pos = 0;

    public Needle(byte[] bytes) {
      this.bytes = bytes;
    }
    boolean extendMatch(byte b) {
      if (b == bytes[pos]) {
        pos += 1;
        if (pos == bytes.length) {
          pos = 0;
          return true;
        }
      } else {
        pos = 0;
      }
      return false;
    }
  }

  private static long find(BufferedSource source, byte[] seq) throws IOException {
    int pos = 0;
    long read = 0;
    while (true) {
      byte b = source.readByte();
      read += 1;
      if (b == seq[pos]) {
        pos += 1;
        if (pos == seq.length) { // found
          return read - seq.length;
        }
      } else {
        pos = 0;
      }
    }
  }

  public static void tokenize2(InputStream is, java.lang.String start, java.lang.String end)
      throws IOException {
    Tracker t = new Tracker();
    t.seqLo = "<spectrum".getBytes(StandardCharsets.UTF_8);
    t.seqHi = "</spectrum".getBytes(StandardCharsets.UTF_8);

    SourceMarker marker = new SourceMarker(Okio.source(is));
    BufferedSource source = marker.source();

    Buffer buf = new Buffer();

    long total = 0;
    byte b;
    byte[] bb = new byte[1];
    ArrayList<Long> locs = new ArrayList<>();
    Needle needleLo1 = new Needle(t.seqLo);
    Needle needleLo2 = new Needle(t.seqLo);
    Needle needleHi = new Needle(t.seqHi);
    while (true) {
      try {
        b = source.readByte();
      } catch (EOFException e) {
        break;
      }
      total += 1;
      if (!needleLo1.extendMatch(b)) {
        continue;
      }
      locs.add(total - needleLo1.bytes.length);
      buf.write(needleLo1.bytes);
      while (true) {
        try {
          b = source.readByte();
          bb[0] = b;
          buf.write(bb);
        } catch (EOFException e) {
          break;
        }
        total += 1;
        if (needleHi.extendMatch(b)) {
          locs.add(total - needleLo2.bytes.length);
          buf.write(needleLo2.bytes);
          break;
        }
      }
    }

    for (int i = 0; i < locs.size(); i += 2) {
      System.out.printf("loc: %d - %d\n", locs.get(i), locs.get(i+1));
    }

    log.debug("Done reading");
  }

//  public static void findOffsets(InputStream is, String target) throws IOException {
//    Buffer b = new Buffer();
//    ByteString needle = new ByteString(target.getBytes(StandardCharsets.UTF_8));
//    try (Source src = Okio.source(is); BufferedSource buf = Okio.buffer(src)) {
//      long read, total = 0;
//      while ((read = buf.read(b, size)) > 0) {
//        totalPrev = total;
//        total += read;
//        log.debug("Read {}/{} (={}) bytes. b.completeSegmentByteCount()={}", read, size, total, b.completeSegmentByteCount());
//        long from = -1;
//        long pos;
//        while ((pos = b.indexOf(needle, ++from)) >= 0) {
//          log.debug("{} @ {}", start, totalPrev + pos);
//        }
//
//      }
//    }
//  }
}
