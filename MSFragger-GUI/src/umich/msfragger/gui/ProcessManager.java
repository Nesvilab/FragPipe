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
import java.util.stream.Collectors;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.messages.MessageAppendToConsole;
import umich.msfragger.messages.MessageKillAll;
import umich.msfragger.messages.MessageStartProcesses;

public class ProcessManager {
  private static final Logger log = LoggerFactory.getLogger(ProcessManager.class);

  private static final ProcessManager instance = new ProcessManager();
  private final Object lock = new Object();
  private final ConcurrentLinkedQueue<RunnableDescription> tasks = new ConcurrentLinkedQueue<>();
  private final ConcurrentLinkedQueue<List<RunnableDescription>> groups = new ConcurrentLinkedQueue<>();
  private final ConcurrentLinkedQueue<CompletableFuture<?>> procs = new ConcurrentLinkedQueue<>();

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
    return Executors.newFixedThreadPool(Math.max(1, Runtime.getRuntime().availableProcessors() - 1));
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
          log.error("Interrupted while waiting for sequential excutor shutdown", ex);
        }
      }
      if (execMulti != null) {
        try {
          execMulti.shutdownNow();
          execMulti.awaitTermination(5, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
          log.error("Interrupted while waiting for parallel excutor shutdown", ex);
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
    synchronized (lock) {

      if (m.runDescs.isEmpty())
        return;

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
        // next is either a null/sequential group element or a new group element
        group.add(next);
        if (next.parallelGroup == null || next.parallelGroup.equals(ProcessBuilderInfo.GROUP_SEQUENTIAL)) {
          processGroup(group);
        }
      }

      if (groups.isEmpty()) {
        log.error("No runnable groups found");
        return;
      }
      submit();

    } // END: sync

  }

//  private CompletableFuture<Void> submit() {
  private void submit() {
    synchronized (lock) {
      List<RunnableDescription> rds = groups.poll();
      if (rds == null) {
        log.debug("No more groups to process");
        return;
      }

      if (rds.size() == 1) {
        RunnableDescription rd = rds.get(0);
        log.debug("Submitting for serial execution: [{}] {}", rd.description.name,
            rd.description.command);
        cf = CompletableFuture.runAsync(rd.runnable, execSingle)
            .thenRunAsync(this::submit, execSingle);

      } else {
        String groupName = rds.stream().map(rd -> rd.parallelGroup).distinct()
            .collect(Collectors.joining(", "));
        String cmds = rds.stream().map(rd -> rd.description.command)
            .collect(Collectors.joining("\n\t"));
        log.debug("Submitting for parallel execution: [{}] {} commands:\n\t{}", groupName,
            rds.size(), cmds);
        List<CompletableFuture<Void>> cfs = new ArrayList<>();
        for (RunnableDescription rd : rds) {
          CompletableFuture<Void> f = CompletableFuture.runAsync(rd.runnable, execMulti);
          cfs.add(f);
        }
        final CompletableFuture<Void> f = CompletableFuture
            .allOf(cfs.toArray(new CompletableFuture[0]));
        cf = f.thenRunAsync(this::submit, execSingle);
      }
    }
  }

  private void processGroup(List<RunnableDescription> group) {
    final List<RunnableDescription> copy = new ArrayList<>(group);
    if (group.size() == 1) {
      RunnableDescription rd = copy.get(0);
      log.debug("Scheduling for serial execution: [{}] {}", rd.description.name, rd.description.command);
    } else {
      // group of several processes to be run in parallel
      String groupName = group.stream().map(rd -> rd.parallelGroup).distinct()
          .collect(Collectors.joining(", "));
      String cmds = group.stream().map(rd -> rd.description.command)
          .collect(Collectors.joining("\n\t"));
      log.debug("Scheduling for parallel execution: [{}] {} commands:\n\t{}", groupName,
          group.size(), cmds);
    }

    groups.add(copy);
    group.clear();
  }

  @Subscribe(threadMode = ThreadMode.BACKGROUND)
  public void onKillAll(MessageKillAll m) {
    stop();
  }
}
