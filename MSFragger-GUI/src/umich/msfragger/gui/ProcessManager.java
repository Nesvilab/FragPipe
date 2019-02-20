package umich.msfragger.gui;

import java.util.concurrent.ConcurrentLinkedQueue;
import org.greenrobot.eventbus.Subscribe;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.ProcessBuilderInfo;
import umich.msfragger.messages.MessageKillAll;
import umich.msfragger.messages.MessageProcessStarted;

public class ProcessManager {

  private static final Logger log = LoggerFactory.getLogger(ProcessManager.class);
  private final Object lock = new Object();
  private ConcurrentLinkedQueue<ProcessResult> procs = new ConcurrentLinkedQueue<>();

  private static class Holder {

    static ProcessManager man = new ProcessManager();
  }

  public static ProcessManager get() {
    return Holder.man;
  }

  @Subscribe
  public void submit(MessageProcessStarted m) {
    synchronized (lock) {
      procs.add(m.pr);
    }
  }

  @Subscribe
  public void killAll(MessageKillAll m) {
    synchronized (lock) {
      ProcessResult pr;
      while ((pr = procs.poll()) != null) {
        ProcessBuilderInfo pbi = pr.getProcessBuilderInfo();
        ProcessBuilder pb = pr.getProcessBuilder();
        Process p = pr.getProcess();
        if (p.isAlive()) {
          log.debug("ProcMan#killAll: Killing process '{}': {}",
              pbi.name, String.join(" ", pb.command()));
          try {
            p.destroyForcibly();
          } catch (Exception e) {
            log.error("Error while trying to terminate a child process '{}': {}",
                pbi.name, String.join(" ", pb.command()));
            // I guess we can't do much about it
          }
        } else {
          log.debug("ProcMan#killAll: Process '{}' already finished execution with code {}: {}",
              pbi.name, p.exitValue(), String.join(" ", pb.command()));
        }
      }
      // at this point the queue must be empty
    }
  }
}
