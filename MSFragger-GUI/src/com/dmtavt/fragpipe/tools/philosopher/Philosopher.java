package com.dmtavt.fragpipe.tools.philosopher;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.philosopher.PhilosopherProps;

public class Philosopher {

  private static final Logger log = LoggerFactory.getLogger(Philosopher.class);

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
    final Pattern reVer = Pattern.compile(".*version[^=]*?=\\s*v?\\.?([^\\s,;]+).*", Pattern.CASE_INSENSITIVE);
    final Pattern reBuild = Pattern.compile(".*build[^=]*?=\\s*([^\\s,;]+).*", Pattern.CASE_INSENSITIVE);
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
      throw new ValidationException("Version string not found in output of: " + String.join(" ", pb.command()));
    }

    final String fragpipeVerMajor = com.dmtavt.fragpipe.Version.version().split("[-_]+")[0];
    final Properties p = Fragpipe.propsFix();
    log.debug("Validating philosopher version compatibility with FragPipe major version: {}", fragpipeVerMajor);
    String minPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name.startsWith(PhilosopherProps.PROP_LOWEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);
    String maxPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name.startsWith(PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);
    log.debug("Figured fragpipe {} requires philosopher versions in range [{}, {}]", fragpipeVerMajor, minPhiVer, maxPhiVer);

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


  public static UpdateInfo checkUpdates(String path) throws ValidationException, UnexpectedException {
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

  public static class UpdateInfo {
    public final boolean isUpdateAvailable;
    public final String downloadUrl;

    public UpdateInfo(boolean isUpdateAvailable, String downloadUrl) {
      this.isUpdateAvailable = isUpdateAvailable;
      this.downloadUrl = downloadUrl;
    }
  }
}
