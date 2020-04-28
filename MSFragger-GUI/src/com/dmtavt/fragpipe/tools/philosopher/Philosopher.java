package com.dmtavt.fragpipe.tools.philosopher;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.dmtavt.fragpipe.messages.MessagePhiDlProgress;
import com.dmtavt.fragpipe.messages.MessagePhilosopherNewBin;
import com.github.chhh.utils.Holder;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.SwingUtilities;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.Buffer;
import okio.BufferedSink;
import okio.ForwardingSource;
import okio.Okio;
import okio.Segment;
import okio.Source;
import org.jetbrains.annotations.NotNull;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Philosopher {

  private static final Logger log = LoggerFactory.getLogger(Philosopher.class);
  public static final String DOWNLOAD_GITHUB_PAGE_URL = "https://github.com/Nesvilab/philosopher/releases/latest";

  public static void downloadPhilosopherManually() {
    try {
      Desktop.getDesktop()
          .browse(URI.create(DOWNLOAD_GITHUB_PAGE_URL));
    } catch (IOException ex) {
      log.error("Error opening browser to download philosopher", ex);
      SwingUtils.showErrorDialogWithStacktrace(ex, null);
    }
  }

  public static Version validate(String path) throws ValidationException, UnexpectedException {

    String binPath = PathUtils.testBinaryPath(path);
    if (binPath == null) {
      throw new ValidationException("Does not appear to be an executable file");
    }
    // get the vesrion reported by the current executable
    // if we couldn't download remote properties, try using local ones
    // if we have some philosopher properties (local or better remote)
    // then check if this version is known to be compatible
    ProcessBuilder pb = new ProcessBuilder(binPath, "version");
    pb.redirectErrorStream(true);

    // get the vesrion reported by the current executable
    final Pattern reVer = Pattern
        .compile(".*version[^=]*?=\\s*v?\\.?([^\\s,;]+).*", Pattern.CASE_INSENSITIVE);
    final Pattern reBuild = Pattern
        .compile(".*build[^=]*?=\\s*([^\\s,;]+).*", Pattern.CASE_INSENSITIVE);
    final StringBuilder sbVer = new StringBuilder();
    final StringBuilder sbBuild = new StringBuilder();
    ProcessUtils.consumeLines(pb, line -> {
      Matcher mVer = reVer.matcher(line);
      if (mVer.matches()) {
        log.debug("Detected philosopher version: {}", mVer.group(1));
        sbVer.append(mVer.group(1));
      }
      Matcher mBuild = reBuild.matcher(line);
      if (mBuild.matches()) {
        log.debug("Detected philosopher build: {}", mBuild.group(1));
        sbBuild.append(mBuild.group(1));
      }
      return sbVer.length() == 0;
    });

    if (sbVer.length() == 0) {
      throw new ValidationException(
          "Version string not found in output of: " + String.join(" ", pb.command()));
    }

    final String fragpipeVerMajor = com.dmtavt.fragpipe.Version.version().split("[-_]+")[0];
    final Properties p = Fragpipe.propsFix();
    log.debug("Validating philosopher version compatibility with FragPipe major version: {}",
        fragpipeVerMajor);
    String minPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name
            .startsWith(PhilosopherProps.PROP_LOWEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);
    String maxPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name
            .startsWith(PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);
    log.debug("Figured fragpipe {} requires philosopher versions in range [{}, {}]",
        fragpipeVerMajor, minPhiVer, maxPhiVer);

    String link = p.getProperty(PhilosopherProps.PROP_DOWNLOAD_URL,
        "https://github.com/Nesvilab/philosopher/releases");

    final String version = sbVer.toString();
    final String build = sbBuild.toString();
    log.debug("Phi validation, proceeding with Version: {}, Build: {}", version, build);

    if (StringUtils.isNotBlank(version)) {
      StringBuilder sb = new StringBuilder();
      if (minPhiVer != null && VersionComparator.cmp(version, minPhiVer) < 0) {
        sb.append("This Philosopher version is no longer supported by FragPipe.\n");
        sb.append("Minimum required version: ").append(minPhiVer).append("\n");
      }
      if (maxPhiVer != null && VersionComparator.cmp(version, maxPhiVer) > 0) {
        sb.append("Latest known compatible version: ").append(maxPhiVer).append("\n");
      }
      if (sb.length() > 0) {
        sb.append("<a href=\"").append(link).append("\">Click here</a> to download a newer one.");
        throw new ValidationException(SwingUtils.makeHtml(sb.toString()));
      }
    }

    return new Version(version, StringUtils.isBlank(build) ? "" : build, false, link);
  }

  public static UpdateInfo checkUpdates(String path)
      throws ValidationException, UnexpectedException {
    String binPath = PathUtils.testBinaryPath(path);
    if (binPath == null) {
      throw new ValidationException("Does not appear to be an executable file");
    }
    try {
      ProcessBuilder pb = new ProcessBuilder(binPath, "version");
      pb.redirectErrorStream(true);

      final Pattern reNewVer = Pattern
          .compile(".*new\\s+version.*available.*", Pattern.CASE_INSENSITIVE);
      final AtomicBoolean isUpdateFound = new AtomicBoolean(false);
      ProcessUtils.consumeLines(pb, line -> {
        Matcher mVer = reNewVer.matcher(line);
        if (mVer.matches()) {
          log.debug("Detected philosopher has an update");
          isUpdateFound.set(true);
          return false;
        }
        return true;
      });

      final Properties p = Fragpipe.propsFix();
      String link = p.getProperty(PhilosopherProps.PROP_DOWNLOAD_URL,
          "https://github.com/Nesvilab/philosopher/releases");
      return new UpdateInfo(isUpdateFound.get(), link);
    } catch (Exception e) {
      throw new UnexpectedException(e);
    }
  }

  public static void downloadPhilosopherAutomatically() throws IOException {
    final Pattern re;
    if (OsUtils.isWindows()) {
      re = Pattern.compile("(/Nesvilab/philosopher/releases/download/v[\\d.]+/philosopher_v[\\d.]+_windows_amd64.zip)");
    } else if (OsUtils.isUnix()) {
      re = Pattern.compile("(/Nesvilab/philosopher/releases/download/v[\\d.]+/philosopher_v[\\d.]+_linux_amd64.zip)");
    } else {
      SwingUtils.showInfoDialog(null, "Automatic downloads for Mac not supported", "Error");
      return;
    }

    OkHttpClient client = new OkHttpClient();
    String html;
    Request request = new Request.Builder().url(DOWNLOAD_GITHUB_PAGE_URL).build();
    try (Response response = client.newCall(request).execute()) {
      ResponseBody body = response.body();
      if (body == null) {
        throw new IllegalStateException("Response body was null");
      }
      html = body.string();
    }

    Matcher m = re.matcher(html);
    if (!m.find()) {
      throw new IllegalStateException("Did not find supported download links on the page");
    }
    String link = m.group(1);
    log.debug("Found download link: {}", link);
    if (!link.toLowerCase().startsWith("https://github.com")) {
      link = StringUtils.prependOnce(link, "/");
      link = StringUtils.prependOnce(link, "https://github.com");
      log.debug("Assuming full download link is: {}", link);
    }

    Path dirCache = FragpipeLocations.get().getDirCache();
    String fn = Seq.of(link.split("/")).findLast().orElseThrow(
        () -> new IllegalStateException("Could not get the last part of download URL"));
    Path dlLocation = dirCache.resolve(fn);
    log.debug("Downloading to file: {}", dlLocation);

    try (Response response = client.newCall(new Request.Builder().url(link).build()).execute()) {
      if (!response.isSuccessful())
        throw new IllegalStateException("Request unsuccessful");
      try (ResponseBody body = response.body()) {
        if (body == null) {
          throw new IllegalStateException("Null response body during download");
        }
        final long totalDlSize = body.contentLength();
        log.debug("Got response, code: {}", response.code());

        final Holder<PhiDownloadProgress> dlProgress = new Holder<>();
        SwingUtilities.invokeLater(() -> {
          dlProgress.obj = new PhiDownloadProgress();
          Bus.registerQuietly(dlProgress.obj);
        });

        try (BufferedSink sink = Okio.buffer(Okio.sink(dlLocation))) {
          AtomicLong received = new AtomicLong(0);
          Source fwd = new ForwardingSource(body.source()) {
            @Override
            public long read(@NotNull Buffer sink, long byteCount) throws IOException {
              long read = super.read(sink, byteCount);
              long totalRead = received.addAndGet(read);
              Bus.post(new MessagePhiDlProgress(totalRead, totalDlSize));
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
              return;
            }
          }
          //sink.writeAll(fwd);

          log.debug("Completed saving philosopher download to file: {}", dlLocation);
          Bus.post(new MessagePhilosopherNewBin(dlLocation.toString()));
        } finally {
          log.debug("Download process finished");
          if (dlProgress.obj != null) {
            Bus.unregister(dlProgress.obj);
            dlProgress.obj.close();
          }
        }
      }
    }
  }

  public static class Version {

    public final String version;
    public final String build;
    public final boolean isNewVersionFound;
    public final String downloadUrl;

    public Version(String version, String build, boolean isNewVersionFound,
        String downloadUrl) {
      this.version = version;
      this.build = build;
      this.isNewVersionFound = isNewVersionFound;
      this.downloadUrl = downloadUrl;
    }
  }

  public static class UpdateInfo {

    public final boolean isUpdateAvailable;
    public final String downloadUrl;

    public UpdateInfo(boolean isUpdateAvailable, String downloadUrl) {
      this.isUpdateAvailable = isUpdateAvailable;
      this.downloadUrl = downloadUrl;
    }
  }
}
