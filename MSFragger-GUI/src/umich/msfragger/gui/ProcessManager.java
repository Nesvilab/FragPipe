package umich.msfragger.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
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
  private final ConcurrentLinkedQueue<RunnableDescription> tasks = new ConcurrentLinkedQueue<>();
  private final ConcurrentLinkedQueue<Future<?>> procs = new ConcurrentLinkedQueue<>();

  private volatile CompletableFuture<Void> cf = CompletableFuture.completedFuture(null);
  private ExecutorService execSingle;
  private ExecutorService execMulti;

  private ProcessManager() {
    init0();
    EventBus.getDefault().register(this);
  }

  public static ProcessManager get() {
    return instance;
  }

  public void init() {
    log.debug("Initializing Process Manager: init()");
    init0();
  }

  private ExecutorService newSingleExecutor() {
    return Executors.newFixedThreadPool(1);
  }

  private ExecutorService newMultiExecutor() {
    return Executors.newWorkStealingPool(Math.max(1, Runtime.getRuntime().availableProcessors() - 1));
  }

  private void init0() {
    synchronized (lock) {
      procs.clear();
      tasks.clear();
      cf.cancel(true);
      cf = CompletableFuture.completedFuture(null);

      if (execSingle != null) {
        try {
          execSingle.shutdownNow();
          execSingle.awaitTermination(5, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
          log.error("Interrupted while waiting for excutor shutdown", ex);
        }
      }
      execSingle = newSingleExecutor();
      execMulti = newMultiExecutor();
    }
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
        // whatever happens, kill the old executor service, start a new one, clear queues
        init0();
      }
    }
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onStartProcess(MessageStartProcesses m) {
    if (m.runDescs.isEmpty())
      return;
    synchronized (lock) {
      tasks.addAll(m.runDescs);

      Iterator<RunnableDescription> it = tasks.iterator();
      final List<RunnableDescription> group = new ArrayList<>();
      while (it.hasNext()) {
        RunnableDescription next = it.next();

        // are we starting a new group?
        if (!group.isEmpty()) {
          RunnableDescription last = group.get(group.size() - 1);
          if (last.parallelGroup.equals(next.parallelGroup)) {
            group.add(next);
            continue;
          } else {
            processGroup(group);
          }
        }
        // next is either a null group element or a new group element
        group.add(next);
        if (next.parallelGroup == null) {
          processGroup(group);
        }
      }

//      for (RunnableDescription runDesc : m.runDescs) {
//        Future<?> future = execSingle.submit(runDesc.runnable);
//        procs.add(future);
//      }
    } // END: sync

  }

  private void processGroup(List<RunnableDescription> group) {
    synchronized (lock) {
      final List<RunnableDescription> copy = new ArrayList<>(group);

      if (group.size() == 1) {
        cf = cf.whenCompleteAsync((aVoid, throwable) -> {
          if (throwable != null) {
            log.error("Error occurred at some stage of pipeline", throwable);
            return;
          }
          copy.get(0).runnable.run();
        }, execSingle);
        procs.add(cf);

      } else {
        // group of several processes to be run in parallel

        cf = cf.whenCompleteAsync((aVoid, throwable) -> {
          if (throwable != null) {
            log.error("Error occurred at some stage of pipeline", throwable);
            return;
          }
          List<CompletableFuture<Void>> groupCfs = new ArrayList<>();
          for (RunnableDescription rd : copy) {
            CompletableFuture<Void> groupCf = CompletableFuture.runAsync(rd.runnable, execMulti);
            procs.add(groupCf);
            groupCfs.add(groupCf);
            cf = CompletableFuture.allOf(groupCfs.toArray(new CompletableFuture[0]));
          }
        }, execSingle);
        procs.add(cf);
      }
      group.clear();
    }
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onKillAll(MessageKillAll m) {
    stop();
  }
}
