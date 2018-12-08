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

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.util.StringUtils;

class ProcessResult implements AutoCloseable {

  private ProcessBuilderInfo pbi;

  public ProcessResult(ProcessBuilderInfo pbi) {
    this.pbi = pbi;
  }

  private boolean started;
  private Path workingDir;
  private StringBuilder output = new StringBuilder();
  private Integer exitCode;
  private Process proc;
  private InputStream stdOut;
  private InputStream stdErr;
  private OutputStream stdErrRedirect;
  private OutputStream stdOutRedirect;

  public Process start() throws IOException {
    stdErrRedirect = createStdErrRedirect();
    stdOutRedirect = createStdOutRedirect();
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

  public String pollStdOut() throws IOException {
    return poll(stdOut);
  }

  public String pollStdErr() throws IOException {
    return poll(stdErr);
  }

  private static String poll(InputStream is) throws IOException {
    if (is == null) {
      return null;
    }
    int available = is.available();
    if (available > 0) {
      byte[] bytes = new byte[available];
      int read = is.read(bytes);
      return new String(bytes);
    }
    return null;
  }

  private OutputStream createStdOutRedirect() throws IOException {
    return createOutputStream(pbi.pb, pbi.fnStdOut);
  }

  private OutputStream createStdErrRedirect() throws IOException {
    return createOutputStream(pbi.pb, pbi.fnStdErr);
  }

  /**
   * Creates a new file output stream to the
   */
  public static OutputStream createOutputStream(ProcessBuilder pb, String fn) throws IOException {
    if (!StringUtils.isNullOrWhitespace(fn) && pb.directory() != null) {
      final Path pathLogOut = pb.directory().toPath().resolve(fn);
      if (!Files.exists(pathLogOut.getParent())) {
        Files.createDirectories(pathLogOut);
      }
      return new BufferedOutputStream(Files
          .newOutputStream(pathLogOut, StandardOpenOption.CREATE,
              StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE));
    }
    return null;
  }

  public ProcessBuilder getPb() {
    return pbi.pb;
  }

  public boolean isStarted() {
    return started;
  }

  public void setStarted(boolean started) {
    this.started = started;
  }

  public StringBuilder getOutput() {
    return output;
  }

  public void append(String s) {
    output.append(s);

  }

  public Integer getExitCode() {
    return exitCode;
  }

  public void setExitCode(Integer exitCode) {
    this.exitCode = exitCode;
  }
}
