package umich.msfragger.gui;

import java.awt.Color;
import java.io.IOException;
import java.util.Locale;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.messages.MessageAppendToConsole;
import umich.msfragger.messages.MessageExternalProcessOutput;
import umich.msfragger.messages.MessageKillAll;
import umich.msfragger.messages.MessageStartProcess;
import umich.msfragger.util.FileDrop.Event;
import umich.msfragger.util.LogUtils;
import umich.msfragger.util.StringUtils;

public class ProcessManager {
  private static final Logger log = LoggerFactory.getLogger(ProcessManager.class);

  private static final ProcessManager instance = new ProcessManager();
  private final Object lock = new Object();
  private ConcurrentLinkedQueue<Future<?>> procs = new ConcurrentLinkedQueue<>();
  private ExecutorService exec = Executors.newFixedThreadPool(1);

  private ProcessManager() {

    EventBus.getDefault().register(this);
  }

  public static ProcessManager get() {
    return instance;
  }

  public void init() {
    log.debug("Initializing Process Manager: init()");
  }

  private void stop() {
    synchronized (lock) {
      try {
        long done = procs.stream().filter(Future::isDone).count();
        long total = procs.size();
        String msg = String.format("Stopping %d/%d processes", total - done, total);
        EventBus.getDefault()
            .post(new MessageAppendToConsole(msg, MsfraggerGuiFrame.COLOR_RED_DARKEST));
        for (Future<?> proc : procs) {
          proc.cancel(true);
        }

      } finally {
        // whatever happens, kill the old executor service, and start a new one
        try {
          final ExecutorService old = exec;
          old.shutdownNow();
          try {
            old.awaitTermination(5L, TimeUnit.SECONDS);
          } catch (InterruptedException ignore) {}
        } finally {
          exec = Executors.newFixedThreadPool(1);
        }
      }

      procs.clear();
    }
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onStartProcess(MessageStartProcess m) {
    synchronized (lock) {

      for (final ProcessBuilderInfo pbi : m.pbis) {

        log.debug("Received start request to start: {}", pbi.name);
        final ProcessResult pr = new ProcessResult(pbi);

        Runnable runnable = () -> {

          Process started;
          try {
            log.debug("Starting: {}", pbi.name);
            started = pr.start();
            log.debug("Started: {}", pbi.name);
          } catch (IOException e) {
            log.error("Error while starting process: " + pbi.name + ", stopping", e);
            stop();
            return;
          }

          // main loop reading process' output
          try {
            while (true) {

              Thread.sleep(200L);
              byte[] pollErr = pr.pollStdErr();
              String errStr = pr.appendErr(pollErr);
              EventBus.getDefault().post(new MessageExternalProcessOutput(true, errStr));
              byte[] pollOut = pr.pollStdOut();
              String outStr = pr.appendOut(pollOut);
              EventBus.getDefault().post(new MessageExternalProcessOutput(false, errStr));
              if (started.isAlive()) {
                continue;
              }

              try {
                log.debug("Checking exit value: {}", pbi.name);
                final int exitValue = started.exitValue();
                Color c = exitValue == 0 ? MsfraggerGuiFrame.COLOR_GREEN_DARKER
                    : MsfraggerGuiFrame.COLOR_RED;
                String msg = String.format(Locale.ROOT,
                    "Process '%s' finished, exit code: %d\n", pbi.name, exitValue);
                EventBus.getDefault().post(new MessageAppendToConsole(msg, c));
              } catch (IllegalThreadStateException ex) {
                log.warn("Checking for exit value when subprocess was not alive threw exception.");
              }
              break;
            }

          } catch (IOException e) {
            log.error("Error while starting process " + pbi.name, e);

          } catch (InterruptedException e) {
            // graceful stop request
            String msg = "Processing interrupted, stopping %s" + pbi.name;
            log.debug(msg, e);
            EventBus.getDefault()
                .post(new MessageAppendToConsole(msg, MsfraggerGuiFrame.COLOR_RED_DARKEST));
            // all the cleanup is done in the finally block

          } finally {
            // in the end whatever happens always try to kill the process
            if (started != null && started.isAlive()) {
              started.destroyForcibly();
            }
            try {
              pr.close();
            } catch (Exception e) {
              log.error("Error closing redirected std/err streams from external process", e);
            }
          }
        };

        // start the process
        procs.add(exec.submit(runnable));
      } // END: iter over ProcessBuilderInfos
    } // END: sync

  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onKillAll(MessageKillAll m) {
    stop();
  }
}
