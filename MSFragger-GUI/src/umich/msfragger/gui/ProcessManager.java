package umich.msfragger.gui;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.messages.MessageAppendToConsole;
import umich.msfragger.messages.MessageKillAll;
import umich.msfragger.messages.MessageStartProcesses;

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
    init0();
  }

  private void init0() {
    if (exec != null) {
      try {
        exec.shutdownNow();
        exec.awaitTermination(5, TimeUnit.SECONDS);
      } catch (InterruptedException ex) {
        log.error("Interrupted while waiting for excutor shutdown", ex);
      }
    }
    exec = Executors.newFixedThreadPool(1);
    procs.clear();
  }

  private void stop() {
    synchronized (lock) {
      try {
        long done = procs.stream().filter(Future::isDone).count();
        long total = procs.size();
        String msg = String.format("\n~~~~~~~~~~~~~~~~~~~~\nStopping %d/%d processes (%d already finished)", total - done, total, done);
        EventBus.getDefault()
            .post(new MessageAppendToConsole(msg, MsfraggerGuiFrame.COLOR_RED_DARKEST));
        for (Future<?> proc : procs) {
          proc.cancel(true);
        }

      } finally {
        // whatever happens, kill the old executor service, and start a new one
        init0();
      }
    }
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onStartProcess(MessageStartProcesses m) {
    synchronized (lock) {
      for (RunnableDescription runDesc : m.runDescs) {
        Future<?> future = exec.submit(runDesc.runnable);
        procs.add(future);
      }
    } // END: sync

  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onKillAll(MessageKillAll m) {
    stop();
  }
}
