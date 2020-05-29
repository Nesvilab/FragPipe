package com.dmtavt.fragpipe;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.api.DownloadProgress;
import com.dmtavt.fragpipe.api.UpdatePackage;
import com.dmtavt.fragpipe.messages.MessageDlProgress;
import com.dmtavt.fragpipe.messages.MessageUpdatePackagesDownload;
import com.dmtavt.fragpipe.messages.MessageUpdateWorkflows;
import com.github.chhh.utils.Holder;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import com.github.chhh.utils.ZipUtils;
import com.github.chhh.utils.swing.MigUtils;
import com.github.chhh.utils.swing.UiCheck;
import com.github.chhh.utils.swing.UiUtils;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSink;
import okio.ForwardingSource;
import okio.Okio;
import okio.Source;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;
import org.jetbrains.annotations.NotNull;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FragpipeUpdater {
  private static final Logger log = LoggerFactory.getLogger(FragpipeUpdater.class);
  private static final String PROP_IGNORE_UPDATES = "ignore-updates";

  @Subscribe(threadMode = ThreadMode.ASYNC)
  public void on(MessageUpdatePackagesDownload m) {
    try {
      for (UpdatePackage update : m.updates) {
        try {
          Path download = downloadToUpdatesDir(update.downloadUrl);
          if (download == null) {
            log.warn("Download did not complete: {}", update.downloadUrl);
            return;
          }
          log.debug("Download complete to: {}", download);

          if (download.getFileName().toString().toLowerCase().endsWith(".zip")) {
            log.debug("Update is zipped, unpacking: {}", download);
            unzipToRootDir(download);
          }

        } catch (IOException e) {
          log.warn("Something happened with download: {}", update.downloadUrl);
        }
      }
    } finally {
      Bus.post(new MessageUpdateWorkflows()); // in case any workflows were overwritten or new ones created
    }
  }

  public static void unzipToRootDir(Path zip) throws IOException {
    Path unzipTo = FragpipeLocations.get().getDirFragpipeRoot();
    ZipUtils.unzipWithSubfolders(zip, unzipTo);
  }

  /**
   * @return Path of the downloaded file
   */
  private static Path downloadToUpdatesDir(String link) throws IOException {
    Path dlDir = FragpipeLocations.get().getOrMakeDirInRoot("updates");
    String fn = Seq.of(link.split("/")).findLast().orElseThrow(
        () -> new IllegalStateException("Could not get the last part of download URL"));
    Path dlLocation = dlDir.resolve(fn);
    log.debug("Downloading to file: {}", dlLocation);
    if (!download(link, dlLocation)) {
      log.warn("Download didn't finish: {}", link);
      return null;
    }
    return dlLocation;
  }

  public static boolean download(String link, Path dlLocation) throws IOException {
    boolean isDlSuccess = false;
    final String fn = Seq.of(link.split("/")).findLast().orElseThrow(
        () -> new IllegalStateException("Could not get the last part of download URL"));
    final OkHttpClient client = new OkHttpClient();
    final Holder<DownloadProgress> dlProgress = new Holder<>();
    final AtomicBoolean downloadProcessEnded = new AtomicBoolean(false);

    try (Response response = client.newCall(new Request.Builder().url(link).build()).execute()) {
      if (!response.isSuccessful()) {
        throw new IllegalStateException("Request unsuccessful: " + link);
      }
      try (ResponseBody body = response.body()) {
        if (body == null) {
          throw new IllegalStateException("Null response body during download");
        }
        final long totalDlSize = body.contentLength();
        log.debug("Got response, code: {}", response.code());
        SwingUtilities.invokeLater(() -> {
          synchronized (downloadProcessEnded) {
            if (downloadProcessEnded.get()) {
              return;
            }
            dlProgress.obj = new DownloadProgress(link, fn);
            Bus.registerQuietly(dlProgress.obj);
          }
        });
        try (BufferedSink sink = Okio.buffer(Okio.sink(dlLocation))) {
          final AtomicLong received = new AtomicLong(0);
          Source fwd = new ForwardingSource(body.source()) {
            @Override
            public long read(@NotNull Buffer sink, long byteCount) throws IOException {
              long read = super.read(sink, byteCount);
              long totalRead = received.addAndGet(read);
              Bus.post(new MessageDlProgress(totalRead, totalDlSize, fn));
              //log.debug("read {}", FileUtils.fileSize(totalRead));
              return read;
            }
          };
          log.debug("Start writing to file: {}", dlLocation);

          long read = 0;
          while (read >= 0) {
            read = fwd.read(sink.getBuffer(), 8192);
            if (dlProgress.obj != null && dlProgress.obj.isCancel) {
              log.debug("Download cancelled");
            }
          }
          isDlSuccess = true;
        } finally {
          synchronized (downloadProcessEnded) {
            downloadProcessEnded.set(true);
            log.debug("Download process finished");
          }
        }
      }
    } finally {
      synchronized (downloadProcessEnded) {
        downloadProcessEnded.set(true);
        if (dlProgress.obj != null) {
          Bus.unregister(dlProgress.obj);
          dlProgress.obj.close();
        }
      }
    }
    downloadProcessEnded.set(true);
    return isDlSuccess;
  }

  public static List<UpdatePackage> checkNewUpdatePackages(Properties props) {
    Map<String, String> updates = Seq.seq(props.stringPropertyNames())
        .filter(prop -> prop.startsWith("update-package."))
        .filter(prop -> !prop.contains(".desc"))
        .toMap(s -> s, s -> props.getProperty(s));

    List<UpdatePackage> toDownload = new ArrayList<>();
    for (Entry<String, String> kv : updates.entrySet()) {
      String propName = kv.getKey();
      String url = kv.getValue();
      String[] split = url.split("/");
      if (split.length < 2) {
        log.warn("Likely error in remote updates file: " + propName);
        continue;
      }
      String fn = split[split.length - 1];
      Path dest = FragpipeLocations.get().getOrMakeDirInRoot("updates");
      Optional<Path> ours = PathUtils
          .findFilesQuietly(dest, path -> path.getFileName().toString().equals(fn)).findAny();
      if (ours.isPresent()) {
        log.debug("We already have file from remote: {}", dest.relativize(ours.get()));
      } else {
        String desc = props.getProperty(propName + ".desc", "");
        Pattern re = Pattern.compile("\\.ver\\[([^],]+?),([^]]*?)]");
        String minVer = "";
        String maxVer = "";
        Matcher m = re.matcher(propName);
        boolean isWithinRange = true;
        if (m.find()) {
          minVer = m.group(1);
          maxVer = m.group(2);
          // check if current version is within range
          VersionComparator vc = new VersionComparator();
          if (isWithinRange && !StringUtils.isBlank(minVer)) {
            isWithinRange = vc.compare(Version.version(), minVer) >= 0;
          }
          if (isWithinRange && !StringUtils.isBlank(maxVer)) {
            isWithinRange = vc.compare(Version.version(), maxVer) <= 0;
          }
        }
        if (isWithinRange) {
          toDownload.add(new UpdatePackage(kv.getValue(), propName, desc, minVer, maxVer));
        } else {
          log.debug("Update package '{}' is out of version range, cur ver '{}'", propName, Version.version());
        }
      }
    }
    if (toDownload.isEmpty()) {
      log.debug("No update packages to download");
    } else {
      log.debug("Need to download update packages:\n\t{}", Seq.seq(toDownload).toString("\n\t"));
    }
    return toDownload;
  }

  public static void askToDownloadUpdates(Component parent, List<UpdatePackage> updates) {
    if (updates == null || updates.isEmpty()) {
      return;
    }

    updates = filterPreviouslyIgnoredUpdates(updates);
    if (updates.isEmpty()) {
      return;
    }

    String text = "FragPipe update packages available, do you want to download and install?\n" +
        "A restart of FragPipe might be required for all changes to take effect.\n\nUpdates:\n - " +
        Seq.seq(updates).map(UpdatePackage::getDescriptionOrName).toString("\n - ");
    MigUtils mu = MigUtils.get();
    JPanel p = mu.newPanel(mu.lcFillXNoInsetsTopBottom());
    final UiCheck check = UiUtils.createUiCheck("Don't show these updates again", false);

    mu.add(p, new JLabel(SwingUtils.makeHtml(text))).wrap();
    mu.add(p, check).wrap();

    int confirmation = SwingUtils.showConfirmDialog(parent, p, "Updates available");
    if (check.isSelected()) {
      addIgnoredUpdates(updates);
    }

    if (JOptionPane.OK_OPTION == confirmation) {
      Bus.post(new MessageUpdatePackagesDownload(updates));
    }
  }



  private static List<UpdatePackage> filterPreviouslyIgnoredUpdates(List<UpdatePackage> updates) {
    String[] ignored = Fragpipe.propsVar().getProperty(PROP_IGNORE_UPDATES, "").split(";;");
    return Seq.seq(updates)
        .filter(update -> Arrays.stream(ignored).noneMatch(update.propertyName::equals))
        .toList();
  }

  private static void addIgnoredUpdates(List<UpdatePackage> updates) {
    String s = Seq.of(Fragpipe.propsVar().getProperty(PROP_IGNORE_UPDATES, "").split(";;"))
        .append(updates.stream().map(u -> u.propertyName))
        .filter(StringUtils::isNotBlank)
        .distinct().toString(";;");
    Fragpipe.propsVarSet(PROP_IGNORE_UPDATES, s);
  }
}
