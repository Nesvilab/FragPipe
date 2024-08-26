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

package com.dmtavt.fragpipe.api;

import static java.nio.file.StandardWatchEventKinds.ENTRY_CREATE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY;
import static java.nio.file.StandardWatchEventKinds.OVERFLOW;
import static javax.swing.JOptionPane.OK_CANCEL_OPTION;

import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.cmd.CmdDatabaseDownload;
import com.dmtavt.fragpipe.cmd.CmdDatabaseUpdate;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherWorkspaceClean;
import com.dmtavt.fragpipe.cmd.CmdPhilosopherWorkspaceCleanInit;
import com.dmtavt.fragpipe.cmd.PbiBuilder;
import com.dmtavt.fragpipe.cmd.ProcessBuilderInfo;
import com.dmtavt.fragpipe.dialogs.DbUniprotIdPanel;
import com.dmtavt.fragpipe.messages.MessageDbNewPath;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.dmtavt.fragpipe.process.ProcessResult;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.UsageTrigger;
import com.github.chhh.utils.swing.FileChooserUtils;
import com.github.chhh.utils.swing.FileChooserUtils.FcMode;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.WatchEvent;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DownloadDbHelper {

  private static final Logger log = LoggerFactory.getLogger(DownloadDbHelper.class);

  private DownloadDbHelper() {
  }

  /**
   * Call from EDT only.
   *
   * @param binPhi           Philosopher binary path.
   */
  public static void downloadDb(Component parent, String binPhi) throws Exception {
    DbUniprotIdPanel p = new DbUniprotIdPanel();
    final int confirmation = SwingUtils.showConfirmDialog2(parent, p, "Download options", OK_CANCEL_OPTION);
    if (JOptionPane.OK_OPTION == confirmation) {
      final Path addSpikeInFasta = p.addSpikeIn();
      final String uniprotId = p.getSelectedUniprotId();
      log.info("Database for download ID: {}", uniprotId);
      final boolean isReviewed = p.isReviewed();
      final boolean isAddContaminants = p.isAddContaminants();
      final boolean isAddIsoforms = p.isAddIsoforms();
      final boolean isAddDecoys = p.isAddDecoys();
      final boolean isAddIrt = p.isAddIrt();
      UsageTrigger usePhi = new UsageTrigger(binPhi, "philosopher binary");

      if (addSpikeInFasta != null) {
        if (!Files.exists(addSpikeInFasta)) {
          SwingUtils.showDialog(p, new JLabel("<html>Could not find spike in fasta file:<br/>" + addSpikeInFasta), "Error preparing for DB download", JOptionPane.ERROR_MESSAGE);
          return;
        }
      }

      JFileChooser fc = FileChooserUtils.create("Download to directory", "Select directory", false, FcMode.DIRS_ONLY, true);
      FileChooserUtils.setPath(fc, ThisAppProps.load(ThisAppProps.PROP_DB_SAVE_PATH));
      if (fc.showOpenDialog(parent) == JFileChooser.APPROVE_OPTION) {
        Path dir = fc.getSelectedFile().toPath();
        ThisAppProps.save(ThisAppProps.PROP_DB_SAVE_PATH, dir.toAbsolutePath().normalize().toString());

        CmdPhilosopherWorkspaceCleanInit cmdCleanInit = new CmdPhilosopherWorkspaceCleanInit(true, dir);
        if (!cmdCleanInit.configure(usePhi, false)) {
          log.error("configuration of philosopher clean/init not successful");
          return;
        }

        // unpack iRT fasta
        Path pathIrt = null;
        if (isAddIrt) {
          pathIrt = FragpipeLocations.get().getDirTools().resolve("fasta/irtfusion.fasta");
          if (!Files.exists(pathIrt)) {
            SwingUtils.showDialog(p, new JLabel("<html>Could not find iRT fasta file:<br/>" + pathIrt), "Error preparing for DB download", JOptionPane.ERROR_MESSAGE);
            return;
          }
        }

        CmdDatabaseDownload cmdDownload = new CmdDatabaseDownload(true, dir);
        cmdDownload.configure(parent, usePhi, uniprotId, isReviewed, isAddContaminants, isAddIsoforms, isAddDecoys, pathIrt, addSpikeInFasta);
        CmdPhilosopherWorkspaceClean cmdClean = new CmdPhilosopherWorkspaceClean(true, dir);
        cmdClean.configure(usePhi);

        List<ProcessBuilder> pbInit = Stream.of(cmdCleanInit).flatMap(cmdBase -> cmdBase.getBuilderDescriptor().pbis.stream().map(pbi -> pbi.pb)).collect(Collectors.toList());
        runProcesses(pbInit, 1);

        List<ProcessBuilder> pbs = Stream.of(cmdDownload, cmdClean).flatMap(cmdBase -> cmdBase.getBuilderDescriptor().pbis.stream().map(pbi -> pbi.pb)).collect(Collectors.toList());

        WatchService watch = FileSystems.getDefault().newWatchService();
        if (watch != null) {
          log.info("Monitoring directory for file changes: {}", dir);
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
              runProcesses(pbs, 10);

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
            stopWatch:
            for (; ; ) {
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
              for (WatchEvent<?> event : key.pollEvents()) {
                Kind<?> kind = event.kind();
                if (OVERFLOW.equals(kind)) {
                  continue;
                } else if (ENTRY_CREATE.equals(kind) || ENTRY_MODIFY.equals(kind)) {
                  Object context = event.context();
                  if (context != null) {
                    if (context instanceof Path) {
                      Path path = (Path) event.context();
                      log.info("Detected new or changed file: " + path.toString());
                      String ext = StringUtils.afterLastDot(path.getFileName().toString()).toLowerCase();
                      log.info("Extension: [{}]", ext);

                      boolean isFile = !Files.isDirectory(path);
                      boolean isNotInsideMeta = Arrays
                          .stream(path.toAbsolutePath().normalize().toString().split("[\\\\/]"))
                          .noneMatch(".meta"::equalsIgnoreCase);
                      boolean isExtFa = ext.startsWith("fa");
                      log.info("Is regular file: {}; Is not inside .meta folder: {}; Extension starts with .fa: {}",
                          isFile, isNotInsideMeta, isExtFa);

                      // remove extension check? philosopher broke it
                      if (isFile && (isNotInsideMeta || isExtFa)) {
                        // most likely a fasta file
                        if (addSpikeInFasta != null) {
                          final String fastaFileName = path.getFileName().toString();
                          final String newFastaFileName = fastaFileName.substring(0, fastaFileName.length() - ".fas".length()) + "-spikein.fas";
                          Files.move(dir.resolve(path), dir.resolve(newFastaFileName));
                          path = Paths.get(newFastaFileName);
                        }
                        final Path fullDbPath = dir.resolve(path);
                        log.debug("Sending new MessageDbUpdate: " + fullDbPath.toString());
                        JOptionPane.showMessageDialog(parent,
                            "<html>Downloaded new file:<br/>" + fullDbPath.toString(),
                            "Download complete", JOptionPane.INFORMATION_MESSAGE);
                        Bus.post(new MessageDbNewPath(fullDbPath.toString()));
                        break stopWatch;
                      }
                    }
                  }
                } else {
                  log.error("DB Download: Unknown event kind: " + kind.toString());
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

  private static void runProcesses(List<ProcessBuilder> pbInit, int timeoutMinutes)
      throws InterruptedException, IOException {
    for (ProcessBuilder pb : pbInit) {
      final String cmd = String.join(" ", pb.command());
      log.info("Executing: " + cmd);

      ProcessBuilderInfo pbi = new PbiBuilder().setPb(pb)
          .setName(pb.toString()).setFnStdOut(null).setFnStdErr(null)
          .setParallelGroup(null).create();
      ProcessResult pr = new ProcessResult(pbi);
      if (pr.start().waitFor(timeoutMinutes, TimeUnit.MINUTES)) {
        log.info("Process output: {}", pr.getOutput().toString());
        final int exitValue = pr.getProcess().exitValue();
        if (!cmd.toLowerCase().contains("workspace") && exitValue != 0) {
          throw new IllegalStateException("Process returned non zero value");
        }
      } else {
        throw new InterruptedException(pb + " took more than " + timeoutMinutes + " minutes. Interrupted the process.");
      }
    }
  }

  public static String findPhiBinStrict(String binPhi) {
    Set<String> searchPaths = new LinkedHashSet<>();
    searchPaths.add(".");
    searchPaths.addAll(PathUtils.getClasspathDirs());
    String jarPath = JarUtils.getCurrentJarPath();
    if (jarPath != null) {
      searchPaths.add(jarPath);
    }
    String[] paths = searchPaths.toArray(new String[0]);
    String phi = PathUtils.testBinaryPath(binPhi, paths);
    if (phi == null) {
      throw new IllegalStateException("Philosopher binary not found");
    }
    return phi;
  }

  public static void updateDb(Component parent, String binPhi, Path fasta, boolean isAddContam) throws Exception {
    UsageTrigger usePhi = new UsageTrigger(binPhi, "philosopher binary");
    Path dir = fasta.toAbsolutePath().getParent();
    CmdPhilosopherWorkspaceCleanInit cmdCleanInit = new CmdPhilosopherWorkspaceCleanInit(true, dir);

    CmdDatabaseUpdate cmdDbUpdate = new CmdDatabaseUpdate(true, dir);

    CmdPhilosopherWorkspaceClean cmdClean = new CmdPhilosopherWorkspaceClean(true, dir);
    cmdClean.configure(usePhi);

    if (!cmdCleanInit.configure(usePhi, false)) {
      log.error("configuration of philosopher clean/init not successful");
      return;
    }
    if (!cmdDbUpdate.configure(parent, usePhi, fasta, isAddContam)) {
      log.error("configuration of phi db update failed");
      return;
    }
    if (!cmdClean.configure(usePhi)) {
      log.error("configuration of phi clean failed");
      return;
    }

    List<ProcessBuilder> pbs = Stream.of(cmdCleanInit, cmdDbUpdate, cmdClean)
        .flatMap(cmdBase -> cmdBase.getBuilderDescriptor().pbis.stream().map(pbi -> pbi.pb))
        .collect(Collectors.toList());

    WatchService watch = FileSystems.getDefault().newWatchService();
    if (watch != null) {
      dir.register(watch, ENTRY_CREATE, ENTRY_MODIFY);
    }

    try {
      JFrame frame = SwingUtils.findParentFrame(parent);
      final JDialog dlg = new JDialog(frame, "Updating database", true);
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
          runProcesses(pbs, 120);

        } catch (Exception ex) {
          log.error("Something happened during database update", ex);
          throw new IllegalStateException("Something happened during database update", ex);
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
        for (; ; ) {
          // retrieve key
          WatchKey key;
          try {
            key = watch.poll();
          } catch (Exception e) {
            log.warn("Something happened while waiting for WatcherSerice.poll().", e);
            break;
          }
          if (key == null) {
            log.info("No more file events when updating database");
            break;
          }

          // process events
          for (WatchEvent<?> event : key.pollEvents()) {
            Kind<?> kind = event.kind();
            if (OVERFLOW.equals(kind)) {
              continue;
            } else if (ENTRY_CREATE.equals(kind) || ENTRY_MODIFY.equals(kind)) {
              Object context = event.context();
              if (context != null) {
                if (context instanceof Path) {
                  Path path = (Path) event.context();
                  log.info("Detected new or changed file: " + path.toString());
                  String ext = StringUtils.afterLastDot(path.getFileName().toString()).toLowerCase();

                  boolean isFile = !Files.isDirectory(path);
                  boolean isNotInsideMeta = Arrays
                      .stream(path.toAbsolutePath().normalize().toString().split("[\\\\/]"))
                      .noneMatch(".meta"::equalsIgnoreCase);
                  boolean isStartsWithFa = ext.startsWith("fa");

                  // remove extension check? philosopher broke it
                  if (isFile && (isNotInsideMeta || isStartsWithFa)) {
                    // most likely a fasta file
                    final Path fullDbPath = dir.resolve(path);
                    log.debug("Sending new MessageDbUpdate: " + fullDbPath.toString());
                    JOptionPane.showMessageDialog(parent,
                        "<html>Updated database, new file:<br/>" + fullDbPath.toString(),
                        "Update complete", JOptionPane.INFORMATION_MESSAGE);
                    Bus.post(new MessageDbNewPath(fullDbPath.toString()));
                    break;
                  }
                }
              }
            } else {
              log.error("DB Update: Unknown event kind: " + kind.toString());
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
