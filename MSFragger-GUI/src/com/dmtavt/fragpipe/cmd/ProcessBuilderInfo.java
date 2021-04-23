package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageExternalProcessOutput;
import com.dmtavt.fragpipe.messages.MessageKillAll;
import com.dmtavt.fragpipe.messages.MessageKillAll.REASON;
import com.dmtavt.fragpipe.messages.MessagePrintToConsole;
import com.dmtavt.fragpipe.messages.MessageSaveLog;
import com.dmtavt.fragpipe.process.ProcessResult;
import java.awt.Color;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Locale;
import java.util.function.Consumer;
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

  public static Runnable toRunnable(final ProcessBuilderInfo pbi, final Path wdPath,
      Consumer<ProcessBuilderInfo> pbiPrinter) {
    return () -> {

      final ProcessResult pr = new ProcessResult(pbi);
      Process started;
      try {
        log.debug("Starting: {}", pbi.name);
        if (pbiPrinter != null) {
          pbiPrinter.accept(pbi);
        }
        started = pr.start();
        log.debug("Started: {}", pbi.name);
      } catch (IOException e) {
        log.error("Error while starting process: " + pbi.name + ", stopping", e);
        Bus.post(new MessageKillAll(REASON.CANT_START_PROCESS));
        return;
      }

      // main loop reading process' output
      try {
        while (true) {

          Thread.sleep(200L);
          final byte[] pollErr = pr.pollStdErr();
          String errStr = pr.appendErr(pollErr);
          if (errStr != null) {
            if (pbi.name.toLowerCase().contentEquals("peptideprophet")) {
              errStr = errStr.replaceAll("WARNING: CANNOT correct data file[^\r\n]+[\r\n]+", "").replaceAll("WARNING: cannot open data file[^\r\n]+[\r\n]+", "");
            }
            Bus.post(new MessageExternalProcessOutput(true, errStr,
                pbi.name));
          }
          final byte[] pollOut = pr.pollStdOut();
          final String outStr = pr.appendOut(pollOut);
          if (outStr != null) {
            Bus.post(new MessageExternalProcessOutput(false, outStr,
                pbi.name));
          }
          if (started.isAlive()) {
            continue;
          }

          try {
            log.debug("Checking exit value: {}", pbi.name);
            final int exitValue = started.exitValue();
            log.debug("Exit value '{}': {}", exitValue, pbi.name);
            Color c = exitValue == 0
                ? Fragpipe.COLOR_GREEN_DARKER
                : Fragpipe.COLOR_RED;
            String msg = String.format(Locale.ROOT,
                "Process '%s' finished, exit code: %d\n", pbi.name, exitValue);
            Bus.post(new MessagePrintToConsole(c, msg, false));
            if (exitValue != 0) {
              log.debug("Exit value not zero, killing all processes");
              Bus.post(new MessagePrintToConsole(
                  Fragpipe.COLOR_RED, "Process returned non-zero exit code, stopping", true));
              Bus.post(new MessageKillAll(REASON.NON_ZERO_RETURN_FROM_PROCESS));
              Bus.post(MessageSaveLog.saveInDir(wdPath));
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
        Bus.post(new MessagePrintToConsole(Fragpipe.COLOR_RED_DARKEST, msg, true));
        // all the cleanup is done in the finally block

      } finally {
        // in the end whatever happens always try to kill the process
        if (started != null && started.isAlive()) {
          log.debug("Killing underlying external process");
          started.destroyForcibly();
        }
        try {
          pr.close();
        } catch (Exception e) {
          log.error("Error closing redirected std/err streams from external process", e);
        }
      }
    };
  }
}
