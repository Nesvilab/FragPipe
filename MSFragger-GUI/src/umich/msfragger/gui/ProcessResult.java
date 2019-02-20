/*
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package umich.msfragger.gui;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.util.StringUtils;

public class ProcessResult implements AutoCloseable {

  private final ProcessBuilderInfo pbi;

  private boolean started;
  private StringBuilder output = new StringBuilder();
  private Integer exitCode;
  private Process proc;
  private InputStream stdOut;
  private InputStream stdErr;
  private BufferedOutputStream stdErrRedirect;
  private BufferedOutputStream stdOutRedirect;

  public ProcessResult(ProcessBuilderInfo pbi) {
    this.pbi = pbi;
  }

  public Process start() throws IOException {
    stdOutRedirect = redirectToFile(pbi.pb, pbi.fnStdOut);
    if (pbi.fnStdErr != null && pbi.fnStdErr.equals(pbi.fnStdOut)) {
      stdErrRedirect = stdOutRedirect;
    } else {
      stdErrRedirect = redirectToFile(pbi.pb, pbi.fnStdErr);
    }

    proc = pbi.pb.start();
    stdOut = proc.getInputStream();
    stdErr = proc.getErrorStream();
    started = true;
    return proc;
  }

  @Override
  public void close() throws Exception {
    if (stdOutRedirect != null) {
      stdOutRedirect.close();
    }
    if (stdErrRedirect != null) {
      stdErrRedirect.close();
    }
  }

  public Process getProcess() {
    return proc;
  }

  public byte[] pollStdOut() throws IOException {
    return poll(stdOut);
  }

  public byte[] pollStdErr() throws IOException {
    return poll(stdErr);
  }

  private static byte[] poll(InputStream is) throws IOException {
    if (is == null) {
      return null;
    }
    int available = is.available();
    if (available > 0) {
      byte[] bytes = new byte[available];
      int read = is.read(bytes);
      return bytes;
    }
    return null;
  }

  /**
   * Creates a new file output stream to the
   */
  private static BufferedOutputStream redirectToFile(ProcessBuilder pb, String fn) throws IOException {
    if (pb == null || pb.directory() == null || StringUtils.isNullOrWhitespace(fn)) {
      return null;
    }
    final Path pathLogOut = pb.directory().toPath().resolve(fn);
    if (!Files.exists(pathLogOut.getParent())) {
      Files.createDirectories(pathLogOut);
    }
    return new BufferedOutputStream(Files
        .newOutputStream(pathLogOut, StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE));
  }

  public ProcessBuilder getProcessBuilder() {
    return pbi.pb;
  }

  public ProcessBuilderInfo getProcessBuilderInfo() {
    return pbi;
  }

  public boolean isStarted() {
    return started;
  }

  public StringBuilder getOutput() {
    return output;
  }

  public String appendOut(byte[] bytes) throws IOException {
    return append(bytes, stdOutRedirect);
  }

  public String appendErr(byte[] bytes) throws IOException {
    return append(bytes, stdErrRedirect);
  }

  private String append(byte[] bytes, BufferedOutputStream bos) throws IOException {
    if (bytes == null || bytes.length == 0) {
      return null;
    }
    String s = new String(bytes, UTF_8);
    output.append(s);
    if (bos != null) {
      bos.write(bytes);
      bos.flush();
    }
    return s;
  }

  public Integer getExitCode() {
    return exitCode;
  }

  public void setExitCode(Integer exitCode) {
    this.exitCode = exitCode;
  }
}
