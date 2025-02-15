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

import org.nesvilab.fragpipe.exceptions.UnexpectedException;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProcessUtils {
  private static final Logger log = LoggerFactory.getLogger(ProcessUtils.class);

  private ProcessUtils() {}

  /** Coalesces output lines with '\n' characters. */
  public static String captureOutput(ProcessBuilder pb) throws UnexpectedException {
    return String.join("\n", captureOutputLines(pb));
  }

  public static List<String> captureOutputLines(ProcessBuilder pb) throws UnexpectedException {
    pb.redirectErrorStream(true);
    List<String> lines = new ArrayList<>();
    log.debug("Starting process to capture output: {}", String.join(" ", pb.command()));
    int code;
    try {
      Process p = pb.start();
      try (BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
        String line;
        while ((line = br.readLine()) != null) {
          lines.add(line);
        }
      } catch (IOException e) {
        throw new UnexpectedException(e);
      }
      code = p.waitFor();
    } catch (InterruptedException | IOException e) {
      throw new UnexpectedException(e);
    }
    log.debug("Got return code {} from process: {}", code, String.join(" ", pb.command()));
    return lines;
  }

  /**
   * @param callback Called for each line read from output of the process. If this callback returns
   *                 false - processing stops.
   */
  public static void consumeLines(ProcessBuilder pb, Function<String, Boolean> callback) throws UnexpectedException {
    pb.redirectErrorStream(true);
    log.debug("Starting process to capture output: {}", String.join(" ", pb.command()));
    Process p = null;
    try {
      p = pb.start();
      try (BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
        String line;
        Boolean doContinue = true;
        while ( doContinue && ((line = br.readLine()) != null) ) {
           doContinue = callback.apply(line);
           if (!doContinue) {
             log.debug("Got a no-go from the callback, stoping line consumption. The process will still run in the background until finished.");
           }
        }
      } catch (IOException e) {
        throw new UnexpectedException(e);
      }
    } catch (IOException e) {
      throw new UnexpectedException(e);
    } finally {
      if (p != null) {
        try {
          p.destroyForcibly();
        } catch (Exception ex) {
          log.warn("Something happened while forcibly destroying process that was no longer needed for line consumption", ex);
        }
      }
    }
  }
}
