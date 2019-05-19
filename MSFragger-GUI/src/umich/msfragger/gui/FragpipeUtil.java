package umich.msfragger.gui;

import static java.nio.file.StandardWatchEventKinds.ENTRY_CREATE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY;
import static java.nio.file.StandardWatchEventKinds.OVERFLOW;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Event;
import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.WatchEvent;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.cmd.CmdDatabaseDownload;
import umich.msfragger.cmd.CmdPhilosopherWorkspaceCleanInit;
import umich.msfragger.cmd.ProcessBuildersDescriptor;
import umich.msfragger.gui.dialogs.DbUniprotIdPanel;
import umich.msfragger.messages.MessageDbUpdate;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.UsageTrigger;

public class FragpipeUtil {
  private static final Logger log = LoggerFactory.getLogger(FragpipeUtil.class);
  private FragpipeUtil() {}

  /**
   * Call from EDT only.
   *
   * @param binPhi Philosopher binary path.
   */
  public static void downloadDb(Component parent, String binPhi) throws Exception {
    Set<String> searchPaths = new LinkedHashSet<>();
    searchPaths.add(".");
    searchPaths.addAll(PathUtils.getClasspathDirs());
    Path jarPath = PathUtils.getCurrentJarPath();
    if (jarPath != null) {
      searchPaths.add(jarPath.toString());
    }
    String[] paths = searchPaths.toArray(new String[0]);
    String phi = PathUtils.testBinaryPath(binPhi, paths);
    if (phi == null) {
      throw new IllegalStateException("Philosopher binary not found");
    }

    JFileChooser fc = new JFileChooser();
    String load = ThisAppProps.load(ThisAppProps.PROP_DB_SAVE_PATH);
    if (load != null) {
      fc.setCurrentDirectory(new File(load));
    }
    fc.setMultiSelectionEnabled(false);
    fc.setAcceptAllFileFilterUsed(true);
    fc.setApproveButtonText("Select directory");
    fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    fc.setDialogTitle("Download location");
    if (fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
      Path dir = fc.getSelectedFile().toPath();

      ThisAppProps.save(ThisAppProps.PROP_DB_SAVE_PATH, dir.toAbsolutePath().normalize().toString());

      // download db
      //DbIdDialog dialog = new DbIdDialog();
      //dialog.setVisible(true);
      DbUniprotIdPanel dbUniprotIdPanel = new DbUniprotIdPanel();
      int confirmation = SwingUtils.showConfirmDialog(parent, dbUniprotIdPanel);
      if (JOptionPane.OK_OPTION == confirmation) {
        final String uniprotId = dbUniprotIdPanel.getSelectedUniprotId();
        log.info("Database for download ID: {}", uniprotId);
        // philosopher workspace --init
        // philosopher database --reviewed --contam --id UP000005640
        CmdPhilosopherWorkspaceCleanInit cmdCleanInit = new CmdPhilosopherWorkspaceCleanInit(
            true, dir);
        UsageTrigger usePhi = new UsageTrigger(binPhi, "philosopher binary");
        if (!cmdCleanInit.configure(usePhi, false)) {
          log.error("configuration of philosopher clean/init not successful");
          return;
        }
        CmdDatabaseDownload cmdDownload = new CmdDatabaseDownload(true, dir);
        cmdDownload.configure(parent, usePhi, uniprotId);


        final List<ProcessBuildersDescriptor> pbDescs = new ArrayList<>();
        pbDescs.add(cmdCleanInit.builders());
        pbDescs.add(cmdDownload.builders());

        List<ProcessBuilder> pbs = Stream.of(cmdCleanInit, cmdDownload)
            .flatMap(cmdBase -> cmdBase.builders().pbs.stream())
            .collect(Collectors.toList());

        WatchService watch = FileSystems.getDefault().newWatchService();
        dir.register(watch, ENTRY_CREATE, ENTRY_MODIFY);

        try {
          JFrame frame = SwingUtils.findParentFrame(parent);
          final JDialog dlg = new JDialog(frame, "Downloading database", true);
          JProgressBar bar = new JProgressBar(0, 100);
          bar.setIndeterminate(true);
          Dimension d = new Dimension(300, 75);
          bar.setMinimumSize(d);
          bar.setSize(d);
          dlg.add(bar, BorderLayout.CENTER);
          dlg.setSize(d);
          dlg.setLocationRelativeTo(parent);

          Thread updateThread = new Thread(() -> {
            try {

              for (ProcessBuilder pb : pbs) {
                final String cmd = String.join(" ", pb.command());
                log.info("Executing: " + cmd);

                Process proc = pb.start();

                proc.waitFor(1, TimeUnit.MINUTES);
                if (proc.exitValue() != 0) {
                  throw new IllegalStateException("Process returned non zero value");
                }
              }

            } catch (Exception ex) {
              throw new IllegalStateException("Something happened during database download", ex);
            } finally {
              dlg.setVisible(false);
            }

             // TODO: show notification if download successful
          });
          updateThread.start();

          // show the dialog, this blocks until dlg.setVisible(false) is called
          // so this call is made in the finally block
          dlg.setVisible(true);

        } catch (Exception e) {
          log.error("Error while trying to download database", e);
        } finally {
          WatchKey poll = watch.poll();
          if (poll.isValid() && !poll.pollEvents().isEmpty()) {
            for (WatchEvent<?> event : poll.pollEvents()) {
              Kind<?> kind = event.kind();
              if (kind == OVERFLOW) {
                continue;
              }
              WatchEvent<Path> ev = (WatchEvent<Path>)event;
              final Path filename = ev.context();
              log.info("Detected new or changed file: " + filename.toString());
              final String fn = filename.getFileName().toString();
              int lastIndexOf = fn.lastIndexOf('.');
              String ext = lastIndexOf < 0 ? fn : fn.substring(lastIndexOf + 1);
              if (ext.startsWith(".fa") || ext.startsWith("fa")) {
                // most likely a fasta file
                final Path fullDbPath = dir.resolve(filename);
                EventBus.getDefault().post(new MessageDbUpdate(fullDbPath.toString()));
              }
            }
          }
        }



//        final List<ProcessBuilderInfo> pbis = pbDescs.stream()
//            .flatMap(pbd -> pbd.pbs.stream()
//                .map(pb -> new ProcessBuilderInfo(pb, pbd.name,pbd.fileCaptureStdout, pbd.fileCaptureStderr)))
//            .collect(Collectors.toList());
//
//        List<RunnableDescription> toRun = new ArrayList<>();
//        for (ProcessBuilderInfo pbi : pbis) {
//          final String command = String.join(" ", pbi.pb.command());
//
////          Runnable runnable = MsfraggerGuiFrame.pbiToRunnable(pbi, dir, info -> {
////            log.info("Running: {}", command);
////          });
////
////
//          Runnable runnable = () -> {
//            log.info("Fake runnable for command: {}", command);
//          };
//
//          ProcessDescription pd = new Builder().setCommand(command)
//              .setWorkDir(dir.toString()).setName(pbi.name).create();
//          RunnableDescription rd = new RunnableDescription(pd, runnable);
//          toRun.add(rd);
//        }
//        ProcessDescription pdFinalizer = new Builder().setCommand("EventBus.getDefault().post(new MessageDecoyTag(\"rev_\"));")
//            .setWorkDir(dir.toString()).setName("Finalizer - update decoy tag").create();
//        toRun.add(new RunnableDescription(pdFinalizer, () -> {
//          // update decoy tag
//          log.info("Running finalizer task: {}", pdFinalizer.command);
//          EventBus.getDefault().post(new MessageDecoyTag("rev_"));
//        }));
//
//        EventBus.getDefault().post(new MessageStartProcesses(toRun));
      }
    }
  }

}
