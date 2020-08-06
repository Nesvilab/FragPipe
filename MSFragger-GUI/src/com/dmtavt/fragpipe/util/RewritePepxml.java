package com.dmtavt.fragpipe.util;

import com.github.chhh.utils.StringUtils;
import java.io.EOFException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import okio.Buffer;
import okio.BufferedSource;
import okio.Okio;
import okio.Sink;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RewritePepxml {
  private static final Logger log = LoggerFactory.getLogger(RewritePepxml.class);

  public static void main(String[] args) throws IOException {
    Optional<Path> notExists = Arrays.stream(args).map(Paths::get).filter(Files::notExists).findFirst();
    if (notExists.isPresent()) {
      System.err.printf("Not all given paths exist: %s\n", notExists.toString());
      System.exit(1);
    }
    Path pepxml = Paths.get(args[0]);
    String[] replacements = new String[Math.max(0, args.length-1)];
    for (int i = 1; i < args.length; i++) {
      replacements[i-1] = args[i];
    }
    System.out.printf("Fixing pepxml: %s\n", pepxml);
    rewriteRawPath(pepxml, replacements);
  }

  public static void rewriteRawPath(Path pepxml, String... replacement) throws IOException {
    log.debug("Rewriting pepxml: {}", pepxml);
    Path dir = pepxml.getParent();
    Path fn = pepxml.getFileName();
    Path temp = Files.createTempFile(dir, fn.toString(), ".temp-rewrite");
    log.debug("Temp file chosen to rewrite pepxml: {}", temp);

    // look for:
    // <msms_run_summary base_name="D:\data\20171007_LUMOS_f01"aw_data_type="mzML" raw_data="mzML">
    // and rewrite with correct path or just the file name

    final byte[] bytesLo = "<msms_run_summary".getBytes();
    final byte[] bytesHi = ">".getBytes();
    final int overlap = 2 << 10;
    final int bufsz = 2 << 16;
//    final int overlap = 256;
//    final int bufsz = 512;

    Sink sink = Okio.sink(temp.toFile(), false);
    Buffer buf = new Buffer();
    int foundCount = 0;
    try (BufferedSource bs = Okio.buffer(Okio.source(pepxml))) {
      try {
        FindResult fr = new FindResult();
        Pattern re = Pattern.compile("base_name=\"([^\"]+)\"");
        while (true) {
          BufferedSource peek = bs.peek();
          if (!find(peek, bufsz, bytesLo, fr)) {
            long toDump = fr.bytesRead - overlap;
            if (toDump <= 0) {
              throw new IllegalStateException("Weird situation, is it the end of the file? Why was it found? Shouldn't happen, I think.");
            }
            buf.write(bs, toDump);
            sink.write(buf, buf.size());
          } else { // found
            ++foundCount;
//            if (foundCount > 1) {
//              throw new IllegalStateException("More than one element to be replaced found. Don't know how to handle this situation.");
//            }
            long offset = fr.bytesRead - bytesLo.length;
            if (!find(peek, overlap, bytesHi, fr)) {
              throw new IllegalStateException("Didn't find closing tag bracket with the search limit");
            }
            long len = fr.bytesRead + bytesLo.length;
            buf.write(bs, offset);
            sink.write(buf, buf.size());

            buf.write(bs, len);
            String originalMsmsRunSummary = buf.readUtf8();
            log.debug("Original msms_run_summary in the file was: {}", originalMsmsRunSummary);

            Matcher m = re.matcher(originalMsmsRunSummary);
            if (!m.find()) {
              throw new IllegalStateException("Didn't find base_name attribute inside msms_run_summary");
            }
            String origPath = m.group(1);
            Path origPathFn = Paths.get(origPath).getFileName();

            String rewrite;
            if (replacement != null && replacement.length > 0) {
              // try to match to what we have
              Map<String, Path> mapFnLessExtToFull = Seq.of(replacement).map(Paths::get)
                  .toMap(path -> StringUtils.upToLastDot(path.getFileName().toString()), path -> path);
              Path correctRaw = mapFnLessExtToFull.get(origPathFn.toString());
              if (correctRaw == null) {
                System.err.printf("Didn't find correct mapping for raw file path in pepxml: %s", origPath);
                System.exit(1);
              }
              String ext = StringUtils.afterLastDot(correctRaw.getFileName().toString());
              rewrite = String.format(
                  "<msms_run_summary base_name=\"%s\" raw_data_type=\"%s\" raw_data=\"%s\">",
                  StringUtils.upToLastDot(correctRaw.toString()), ext, ext);

            } else {
              rewrite = re.matcher(originalMsmsRunSummary).replaceFirst(String.format("base_name=\"%s\"", origPathFn));
            }
            log.debug("Rewritten tag: {}", rewrite);
            buf.write(rewrite.getBytes(StandardCharsets.UTF_8));
            sink.write(buf, buf.size());
          }
        }
      } catch (EOFException eof) {
        log.debug("Got to end of file");
        buf.writeAll(bs);
      }
    } finally {
      if (sink != null) {
        sink.flush();
        sink.close();
      }
    }

    // rewriting done
    // delete original, rename temp file
    log.debug("Deleting file: {}", pepxml);
    Files.deleteIfExists(pepxml);
    log.debug("Moving file: [{}] -> [{}]", temp, pepxml);
    Files.move(temp, pepxml);

  }

  private static class FindResult {
    public boolean isFound;
    public long bytesRead;

    private FindResult() {}

    private FindResult(boolean isFound, long bytesRead) {
      this.isFound = isFound;
      this.bytesRead = bytesRead;
    }
  }

  private static boolean find(BufferedSource source, int limit, byte[] seq, final FindResult result) throws IOException {
    int pos = 0;
    long read = 0;
    while (true) {
      if (read >= limit) {
        result.isFound = false;
        result.bytesRead = read;
        return false;
      }

      byte b = source.readByte();
      read += 1;
      if (b == seq[pos]) {
        pos += 1;
        if (pos == seq.length) { // found
          result.isFound = true;
          result.bytesRead = read;
          return true;
        }
      } else {
        pos = 0;
      }
    }
  }
}
