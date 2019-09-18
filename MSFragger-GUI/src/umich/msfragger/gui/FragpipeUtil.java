package umich.msfragger.gui;

import static java.nio.file.StandardWatchEventKinds.ENTRY_CREATE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY;
import static java.nio.file.StandardWatchEventKinds.OVERFLOW;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.WatchEvent;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
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
import umich.msfragger.cmd.ProcessBuilderInfo;
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
        final boolean isReviewed = dbUniprotIdPanel.isReviewed();
        final boolean isAddContaminants = dbUniprotIdPanel.isAddContaminants();
        final boolean isAddIsoforms = dbUniprotIdPanel.isAddIsoforms();

        // philosopher workspace --init
        // philosopher database --reviewed --contam --id UP000005640
        UsageTrigger usePhi = new UsageTrigger(binPhi, "philosopher binary");
        CmdPhilosopherWorkspaceCleanInit cmdCleanInit = new CmdPhilosopherWorkspaceCleanInit(
            true, dir);
        if (!cmdCleanInit.configure(usePhi, false)) {
          log.error("configuration of philosopher clean/init not successful");
          return;
        }
        CmdDatabaseDownload cmdDownload = new CmdDatabaseDownload(true, dir);
        cmdDownload.configure(parent, usePhi, uniprotId, isReviewed, isAddContaminants, isAddIsoforms);

        List<ProcessBuilder> pbs = Stream.of(cmdCleanInit, cmdDownload)
            .flatMap(cmdBase -> cmdBase.getBuilderDescriptor().pbs.stream())
            .collect(Collectors.toList());

        WatchService watch = FileSystems.getDefault().newWatchService();
        if (watch != null) {
          dir.register(watch, ENTRY_CREATE, ENTRY_MODIFY);
        }

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

                ProcessBuilderInfo pbi = new ProcessBuilderInfo(pb, pb.toString(),
                    null, null, null);
                ProcessResult pr = new ProcessResult(pbi);
                pr.start().waitFor(5, TimeUnit.MINUTES);
                log.info("Process output: {}", pr.getOutput().toString());
                final int exitValue = pr.getProcess().exitValue();
                if (!cmd.toLowerCase().contains("workspace") && exitValue != 0) {
                  throw new IllegalStateException("Process returned non zero value");
                }
              }

            } catch (Exception ex) {
              throw new IllegalStateException("Something happened during database download", ex);
            } finally {
              dlg.setVisible(false);
              dlg.dispose();
            }

          });
          updateThread.start();

          // show the dialog, this blocks until dlg.setVisible(false) is called
          // so this call is made in the finally block
          dlg.setVisible(true);

        } catch (Exception e) {
          log.error("Error while trying to download database", e);
        } finally {

          if (watch != null) {
            for (;;) {
              // retrieve key
              WatchKey key;
              try {
                key = watch.poll();
              } catch (Exception e) {
                log.warn("Something happened wihle waiting for WatcherSerice.poll().", e);
                break;
              }
              if (key == null) {
                log.info("No more file events when downloading database");
                break;
              }

              // process events
              for (WatchEvent<?> event: key.pollEvents()) {
                Kind<?> kind = event.kind();
                if (OVERFLOW.equals(kind)) {
                  continue;
                } else if (ENTRY_CREATE.equals(kind) || ENTRY_MODIFY.equals(kind)) {
                  Object context = event.context();
                  if (context != null) {
                    if (context instanceof Path) {
                      Path path = (Path) event.context();
                      log.info("Detected new or changed file: " + path.toString());
                      final String fn = path.getFileName().toString();
                      int lastIndexOf = fn.lastIndexOf('.');
                      String ext = lastIndexOf < 0 ? fn : fn.substring(lastIndexOf + 1);
                      if (ext.startsWith(".fa") || ext.startsWith("fa")) {
                        // most likely a fasta file
                        final Path fullDbPath = dir.resolve(path);
                        log.info("Sending new MessageDbUpdate: " + fullDbPath.toString());
                        JOptionPane.showMessageDialog(parent,
                            "<html>Downloaded new file:<br/>" + fullDbPath.toString(),
                            "Download complete", JOptionPane.INFORMATION_MESSAGE);
                        EventBus.getDefault().post(new MessageDbUpdate(fullDbPath.toString()));
                        break;
                      }
                    }
                  }
                } else {
                  log.error("unknown event kind: " + kind.toString());
                }
              }

              // reset the key
              boolean valid = key.reset();
              if (!valid) {
                // object no longer registered
              }
            }

            watch.close();
          }
        }
      }
    }
  }

}
