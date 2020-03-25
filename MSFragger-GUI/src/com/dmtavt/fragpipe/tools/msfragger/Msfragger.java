package com.dmtavt.fragpipe.tools.msfragger;

import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.NoteConfigMsfragger;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import net.java.balloontip.BalloonTip;
import net.java.balloontip.styles.RoundedBalloonStyle;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.MsfraggerGuiFrame;
import umich.msfragger.gui.api.VersionFetcher;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherGithub;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherLocal;
import umich.msfragger.params.fragger.MsfraggerVersionFetcherServer;

public class Msfragger {
  private static final Logger log = LoggerFactory.getLogger(Msfragger.class);

  public static String version(Path jar) throws ValidationException {
    // only validate Fragger version if the current Java version is 1.8 or higher
    if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_8)) {
      // we can't test fragger binary verison when java version is less than 1.8
      throw new ValidationException("MSFragger requires Java 8+, can't check version without it.");
    }
    // get the vesrion reported by the current executable
    FraggerRunResult test = testJar(jar.toString());
    String localVer = test.isVersionPrintedAtAll ? test.version : "Can't get version";

  }


  public static void checkUpdates(NoteConfigMsfragger m) {

    // update the version label
    guiFrame.fraggerVer = StringUtils.isNullOrWhitespace(localVer) ? MsfraggerGuiFrame.UNKNOWN_VERSION : localVer;
    guiFrame.getLblFraggerJavaVer().setText(String.format(
        "MSFragger version: %s. %s", guiFrame.fraggerVer, ));

    // Now check the versions on remotes.
    final MsfraggerVersionComparator vc = new MsfraggerVersionComparator();
    Thread t = new Thread(() -> {

      MsfraggerVersionFetcherServer vfServer = new MsfraggerVersionFetcherServer();
      MsfraggerVersionFetcherGithub vfGithub = new MsfraggerVersionFetcherGithub();
//      MsfraggerVersionFetcherServer vfServer = null;
//      MsfraggerVersionFetcherGithub vfGithub = null;
      MsfraggerVersionFetcherLocal vfLocal = new MsfraggerVersionFetcherLocal();
      List<VersionFetcher> verFetchers = Arrays.asList(vfServer, vfGithub, vfLocal);
      for (final VersionFetcher vf : verFetchers) {
        if (vf == null) {
          continue;
        }
        try {
          final String updateVer = vf.fetchVersion();
          if (StringUtils.isNullOrWhitespace(updateVer)) {
            continue;
          }
          // we got a non-empty version from some version fetcher
          if (vc.compare(localVer, updateVer) < 0) {
            // local versin is older, than the fetched version
            // show balloon popup, must be done on EDT
            String url = vf.getDownloadUrl();
            final String manualDownloadUrl = StringUtils.isNullOrWhitespace(url)
                ? vfLocal.getDownloadUrl() : url;
            SwingUtilities.invokeLater(() -> {
              if (guiFrame.balloonMsfragger != null) {
                guiFrame.balloonMsfragger.closeBalloon();
              }

              StringBuilder sb = new StringBuilder();
              if (test.isVersionPrintedAtAll) {
                sb.append(String.format("Your version is [%s]<br>\n"
                        + "There is a newer version of MSFragger available [%s].<br>\n",
                    localVer, updateVer));
              } else {
                sb.append(
                    String.format("<b>This version is not supported anymore</b><br>\n"
                        + "Get a new version of MSFragger [%s].<br>\n", updateVer));
              }
              if (vf.canAutoUpdate()) {
                sb.append("<br>If you choose to auto-update a new version will be downloaded<br>\n"
                    + "and placed in the same folder as the old one. The old one will be kept.");
              }
              JEditorPane ep = SwingUtils.createClickableHtml(sb.toString(),
                  guiFrame.balloonBgColor);

              JPanel panel = new JPanel();
              panel.setBackground(ep.getBackground());
              panel.setLayout(new BorderLayout());

              JPanel panelButtons = new JPanel();
              panelButtons.setBackground(ep.getBackground());
              panelButtons.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));

              if (vf.canAutoUpdate()) {
                JButton btnAutoUpdate = new JButton("Auto-update");
                btnAutoUpdate.addActionListener(e -> {
                  if (guiFrame.balloonMsfragger == null) {
                    return;
                  }
                  guiFrame.balloonMsfragger.setVisible(false);
                  guiFrame.balloonMsfragger = null;

                  final JDialog dlg = new JDialog(guiFrame, "Updating MSFragger",
                      true);
                  JProgressBar pb = new JProgressBar(0, 100);
                  pb.setIndeterminate(true);
                  Dimension d = new Dimension(300, 75);
                  pb.setMinimumSize(d);
                  pb.setSize(d);
                  dlg.add(pb, BorderLayout.CENTER);
                  dlg.setSize(d);
                  dlg.setLocationRelativeTo(guiFrame);

                  Thread updateThread = new Thread(() -> {
                    try {

                      Path updated = vf.autoUpdate(Paths.get(jarPath));
                      validateAndSaveMsfraggerPath(guiFrame, updated.toAbsolutePath().toString());

                    } catch (Exception ex) {
                      throw new IllegalStateException(
                          "Something happened during MSFragger auto-update", ex);
                    } finally {
                      dlg.setVisible(false);
                    }
                  });
                  updateThread.start();

                  // show the dialog, this blocks until dlg.setVisible(false) is called
                  // so this call is made in the finally block
                  dlg.setVisible(true);
                });
                panelButtons.add(btnAutoUpdate);
              }

              if (!StringUtils.isNullOrWhitespace(manualDownloadUrl)) {
                JButton btnManualUpdate = new JButton("Manual update");
                btnManualUpdate.addActionListener(e -> {
                  try {
                    SwingUtils.openBrowserOrThrow(new URI(manualDownloadUrl));
                  } catch (URISyntaxException ex) {
                    throw new IllegalStateException("Incorrect url/uri", ex);
                  }
                });
                panelButtons.add(btnManualUpdate);
              }

              JButton btnClose = new JButton("Close");
              btnClose.addActionListener(e -> {
                if (guiFrame.balloonMsfragger == null) {
                  return;
                }
                guiFrame.balloonMsfragger.setVisible(false);
                guiFrame.balloonMsfragger = null;
              });

              panel.add(ep, BorderLayout.CENTER);
              panelButtons.add(btnClose);
              panel.add(panelButtons, BorderLayout.SOUTH);

              guiFrame.balloonMsfragger = new BalloonTip(guiFrame.getTextBinMsfragger(), panel,
                  new RoundedBalloonStyle(5, 5, guiFrame.balloonBgColor, Color.BLACK), true);
              guiFrame.balloonMsfragger.setVisible(true);
            });
          }
          return; // stop iterations, we've found that there is no better version than the current

        } catch (Exception ex) {
          // no biggie
          continue;
        }
      }
    });
    t.start();

    return true;
  }

  public static FraggerRunResult testJar(String jarPath) {
      String verStr = null;
      boolean isVersionPrintedAtAll = false;
      try {
          ProcessBuilder pb = new ProcessBuilder("java", "-jar", jarPath);
          List<Pattern> regexs = Arrays.asList(MsfraggerVersionComparator.regexOldScheme1, MsfraggerVersionComparator.regexNewScheme1);
          pb.redirectErrorStream(true);
          Process pr = pb.start();
          try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
              String line;
              while ((line = in.readLine()) != null) {
                  for (Pattern re : regexs) {
                      Matcher m = re.matcher(line);
                      if (m.find()) {
                          isVersionPrintedAtAll = true;
                          verStr = m.group(2);
                      }
                  }
              }
              pr.waitFor();
          }
      } catch (IOException | InterruptedException e) {
          throw new IllegalStateException("Error while creating a java process for MSFragger test.");
      }
      return new FraggerRunResult(isVersionPrintedAtAll, verStr);
  }

  public static void validateJar(String path) throws ValidationException {
    try {
      Path p = Paths.get(path);
      if (!Files.exists(p))
        throw new ValidationException("Given path not exists: " + path);
      if (!path.toLowerCase().endsWith(".jar"))
        throw new ValidationException("Given path not a jar file: " + path);
      if (!validateMsfraggerJarContents(p))
        throw new ValidationException("Not an MSFragger jar file: " + path);
    } catch (Exception e) {
      if (e instanceof ValidationException)
        throw e;
      throw new ValidationException("Invalid MSFragger jar", e);
    }
  }

  private static boolean validateMsfraggerJarContents(Path p) {
    final boolean[] found = {false};
    try (FileSystem fs = FileSystems.newFileSystem(p, null)) {
      for (Path root : fs.getRootDirectories()) {
        Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
          final Pattern regex = Pattern.compile("msfragger.*\\.jar", Pattern.CASE_INSENSITIVE);

          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            String fileName = file.getFileName().toString();
            if ("MSFragger.class".equalsIgnoreCase(fileName)) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            } else if (regex.matcher(fileName).find()) {
              found[0] = true;
              return FileVisitResult.TERMINATE;
            }
            return FileVisitResult.CONTINUE;
          }
        });
      }
    } catch (IOException ex) {
      log.warn("Error while checking MSFragger jar contents", ex);
    }

    return found[0];
  }

  public static class FraggerRunResult {
      final public boolean isVersionPrintedAtAll;
      final public String version;

      public FraggerRunResult(boolean isVersionPrintedAtAll, String version) {
          this.isVersionPrintedAtAll = isVersionPrintedAtAll;
          this.version = version;
      }
  }
}
