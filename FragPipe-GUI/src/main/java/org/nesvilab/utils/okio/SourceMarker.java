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

package org.nesvilab.utils.okio;

import java.io.IOException;
import okio.Buffer;
import okio.BufferedSource;
import okio.ForwardingSource;
import okio.Okio;
import okio.Source;

/**
 * Builds a buffered source that can rewind to a marked position earlier in the stream.
 *
 * <p>Mark potential positions to rewind back to with {@link #mark}; rewind back to these positions
 * with {@link #reset}. Both operations apply to the position in the {@linkplain #source() buffered
 * source}; resetting will impact the buffer.
 *
 * <p>When marking it is necessary to specify how much data to retain. Once you advance above this
 * limit, the mark is discarded and resetting is not permitted. This may be used to lookahead a
 * fixed number of bytes without loading an entire stream into memory. To reset an arbitrary
 * number of bytes use {@code mark(Long#MAX_VALUE)}.
 */
public final class SourceMarker {

  /*
   * This class wraps the underlying source in a MarkSource to support mark and reset. It creates a
   * BufferedSource for the caller so that it can track its offsets and manipulate its buffer.
   */

  /**
   * The offset into the underlying source. To compute the user's offset start with this and
   * subtract userBuffer.size().
   */
  long offset;

  /** The offset of the earliest mark, or -1 for no mark. */
  long mark = -1L;

  /** The offset of the latest readLimit, or -1 for no mark. */
  long limit = -1L;

  boolean closed;

  final MarkSource markSource;
  final BufferedSource userSource;

  /** A copy of the underlying source's data beginning at {@code mark}. */
  final Buffer markBuffer;

  /** Just the userSource's buffer. */
  final Buffer userBuffer;

  public SourceMarker(Source source) {
    this.markSource = new MarkSource(source);
    this.markBuffer = new Buffer();
    this.userSource = Okio.buffer(markSource);
    this.userBuffer = userSource.getBuffer();
  }

  public BufferedSource source() {
    return userSource;
  }

  /**
   * Marks the current position in the stream as one to potentially return back to. Returns the
   * offset of this position. Call {@link #reset(long)} with this position to return to it later. It
   * is an error to call {@link #reset(long)} after consuming more than {@code readLimit} bytes from
   * {@linkplain #source() the source}.
   */
  public long mark(long readLimit) throws IOException {
    if (readLimit < 0L) {
      throw new IllegalArgumentException("readLimit < 0: " + readLimit);
    }

    if (closed) {
      throw new IllegalStateException("closed");
    }

    // Mark the current position in the buffered source.
    long userOffset = offset - userBuffer.size();

    // If this is a new mark promote userBuffer data into the markBuffer.
    if (mark == -1L) {
      markBuffer.writeAll(userBuffer);
      mark = userOffset;
      offset = userOffset;
    }

    // Grow the limit if necessary.
    long newMarkBufferLimit = userOffset + readLimit;
    if (newMarkBufferLimit < 0) newMarkBufferLimit = Long.MAX_VALUE; // Long overflow!
    limit = Math.max(limit, newMarkBufferLimit);

    return userOffset;
  }

  /** Resets {@linkplain #source() the source} to {@code userOffset}. */
  public void reset(long userOffset) throws IOException {
    if (closed) {
      throw new IllegalStateException("closed");
    }

    if (userOffset < mark // userOffset is before mark.
        || userOffset > limit // userOffset is beyond limit.
        || userOffset > mark + markBuffer.size() // userOffset is in the future.
        || offset - userBuffer.size() > limit) { // Stream advanced beyond limit.
      throw new IOException("cannot reset to " + userOffset + ": out of range");
    }

    // Clear userBuffer to cause data at 'offset' to be returned by the next read.
    offset = userOffset;
    userBuffer.clear();
  }

  final class MarkSource extends ForwardingSource {
    MarkSource(Source source) {
      super(source);
    }

    @Override public long read(Buffer sink, long byteCount) throws IOException {
      if (closed) {
        throw new IllegalStateException("closed");
      }

      // If there's no mark, go to the underlying source.
      if (mark == -1L) {
        long result = super.read(sink, byteCount);
        if (result == -1L) return -1L;
        offset += result;
        return result;
      }

      // If we can read from markBuffer, do that.
      if (offset < mark + markBuffer.size()) {
        long posInBuffer = offset - mark;
        long result = Math.min(byteCount, markBuffer.size() - posInBuffer);
        markBuffer.copyTo(sink, posInBuffer, result);
        offset += result;
        return result;
      }

      // If we can write to markBuffer, do that.
      if (offset < limit) {
        long byteCountBeforeLimit = limit - (mark + markBuffer.size());
        long result = super.read(markBuffer, Math.min(byteCount, byteCountBeforeLimit));
        if (result == -1L) return -1L;
        markBuffer.copyTo(sink, markBuffer.size() - result, result);
        offset += result;
        return result;
      }

      // Attempt to read past the limit. Data will not be saved.
      long result = super.read(sink, byteCount);
      if (result == -1L) return -1L;

      // We read past the limit. Discard marked data.
      markBuffer.clear();
      mark = -1L;
      limit = -1L;
      return result;
    }

    @Override public void close() throws IOException {
      if (closed) return;

      closed = true;
      markBuffer.clear();
      super.close();
    }
  }
}

