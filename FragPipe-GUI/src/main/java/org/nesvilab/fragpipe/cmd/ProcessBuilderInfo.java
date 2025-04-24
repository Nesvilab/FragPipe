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

package org.nesvilab.fragpipe.cmd;

import static org.nesvilab.fragpipe.messages.MessagePrintToConsole.toConsole;

import java.awt.Color;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Locale;
import java.util.function.BiConsumer;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.messages.MessageKillAll;
import org.nesvilab.fragpipe.messages.MessageKillAll.REASON;
import org.nesvilab.fragpipe.messages.MessageSaveLog;
import org.nesvilab.fragpipe.process.ProcessResult;
import org.nesvilab.utils.swing.TextConsole;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProcessBuilderInfo {

  private static final Logger log = LoggerFactory.getLogger(ProcessBuilderInfo.class);
  public final ProcessBuilder pb;
  public final String name;
  public final String fnStdout;
  public final String fnStderr;
  public final String parallelGroup;
  public static final String GROUP_SEQUENTIAL = "SEQUENTIAL EXECUTION";

  public ProcessBuilderInfo(ProcessBuilder pb, String name, String fnStdout,
      String fnStderr, String parallelGroup) {
    this.pb = pb;
    this.name = name;
    this.fnStdout = fnStdout;
    this.fnStderr = fnStderr;
    this.parallelGroup = parallelGroup;
  }

  public static Runnable toRunnable(final ProcessBuilderInfo pbi, final Path wdPath, BiConsumer<ProcessBuilderInfo, TextConsole> pbiPrinter, TextConsole console, boolean isDownstream) {
    return () -> {
      final ProcessResult pr = new ProcessResult(pbi);
      Process started = null;
      try {
        log.debug("Starting: {}", pbi.name);
        if (pbiPrinter != null) {
          pbiPrinter.accept(pbi, console);
        }
        started = pr.start();
        log.debug("Started: {}", pbi.name);
      } catch (IOException e) {
        log.error("Error while starting process: " + pbi.name + ", stopping", e);
        Bus.post(new MessageKillAll(REASON.CANT_START_PROCESS, console));

        if (Fragpipe.headless) {
          System.exit(1);
        } else {
          return;
        }
      }

      // main loop reading process' output
      try {
        if ((pbi.name.equalsIgnoreCase("peptideprophet") || pbi.name.equalsIgnoreCase("ptmprophet")) || pbi.name.equalsIgnoreCase("writesubmzml")) {
          toConsole(Fragpipe.COLOR_BLACK, "Please wait. " + pbi.name + " is running. It can take a long time.", true, console);
        }

        StringBuilder sbBuffer = new StringBuilder();
        while (true) {
          Thread.sleep(200L);
          final byte[] pollErr = pr.pollStdErr();
          if (pollErr != null && pollErr.length > 0) {
            if (pbi.name.equalsIgnoreCase("peptideprophet")) {
              String errStr = (new String(pollErr, StandardCharsets.UTF_8)).replaceAll("WARNING: CANNOT correct data file[^\r\n]+[\r\n]+", "").replaceAll("WARNING: cannot open data file[^\r\n]+[\r\n]+", "");
              sbBuffer.append(errStr);
            } else if (pbi.name.equalsIgnoreCase("ptmprophet")) {
              String errStr = (new String(pollErr, StandardCharsets.UTF_8)).replaceAll("\"chmod [0-9]{3} [^\\r\\n]+\" failed: Operation not permitted[\\r\\n]*", "");
              sbBuffer.append(errStr);
            } else if (pbi.name.equalsIgnoreCase("writesubmzml")) {
              sbBuffer.append(new String(pollErr, StandardCharsets.UTF_8));
            } else {
              String errStr = pr.appendErr(pollErr);
              toConsole(null, errStr, false, console);
            }
          }

          final byte[] pollOut = pr.pollStdOut();
          if (pollOut != null && pollOut.length > 0) {
            if (pbi.name.equalsIgnoreCase("peptideprophet") || pbi.name.equalsIgnoreCase("ptmprophet") || pbi.name.equalsIgnoreCase("writesubmzml")) {
              sbBuffer.append(new String(pollOut, StandardCharsets.UTF_8));
            } else {
              String outStr = pr.appendOut(pollOut);
              toConsole(null, outStr, false, console);
            }
          }

          if (started.isAlive()) {
            continue;
          }

          if (pbi.name.equalsIgnoreCase("peptideprophet") || pbi.name.equalsIgnoreCase("ptmprophet") || pbi.name.equalsIgnoreCase("writesubmzml")) {
            pr.appendOut(sbBuffer.toString().getBytes(StandardCharsets.UTF_8));
            toConsole(null, sbBuffer.toString(), false, console);
          }

          try {
            log.debug("Checking exit value: {}", pbi.name);
            final int exitValue = started.exitValue();
            log.debug("Exit value '{}': {}", exitValue, pbi.name);
            Color c = exitValue == 0 ? Fragpipe.COLOR_GREEN_DARKER : Fragpipe.COLOR_RED;
            String msg = String.format(Locale.ROOT, "Process '%s' finished, exit code: %d\n", pbi.name, exitValue);
            toConsole(c, msg, false, console);
            if (exitValue != 0) {
              log.debug("Exit value not zero, killing all processes");
              toConsole(Fragpipe.COLOR_RED, "Process returned non-zero exit code, stopping", true, console);
              Bus.post(new MessageKillAll(REASON.NON_ZERO_RETURN_FROM_PROCESS, console));
              Bus.post(MessageSaveLog.saveInDir(wdPath, console));
              break;
            }
          } catch (IllegalThreadStateException ex) {
            log.warn("Checking for exit value when subprocess was not alive threw exception.");
          }
          break;
        }
      } catch (IOException e) {
        log.error("Error while starting process " + pbi.name, e);
      } catch (InterruptedException e) {
        // graceful stop request
        String msg = "Processing interrupted, stopping " + pbi.name;
        log.debug(msg, e);
        toConsole(Fragpipe.COLOR_RED_DARKEST, msg, true, console);
        // all the cleanup is done in the final block
      } finally {
        // grab the exit code of the process to use for System.exit() if operating in headless mode
        int overallExitCode = 0;
        try {
          if (started != null && !started.isAlive() && started.exitValue() != 0) {
            overallExitCode = started.exitValue();
          }
        } catch (Exception ex) {
          log.debug("Could not get the exit code: {}", ex.getMessage());
        }
        // in the end whatever happens always try to kill the process
        if (started != null && started.isAlive()) {
          log.debug("Killing underlying external process");
          started.destroyForcibly();
          overallExitCode = 1;
        }
        try {
          pr.close();
        } catch (Exception e) {
          log.error("Error closing redirected std/err streams from external process", e);
        }
        if (Fragpipe.headless && overallExitCode != 0) {
          System.exit(overallExitCode);
        }
      }
    };
  }
}
