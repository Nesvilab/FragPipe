package com.dmtavt.fragpipe.tools.philosopher;

import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Properties;
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
    final VersionComparator vc = new VersionComparator();

    // get the vesrion reported by the current executable
    // if we couldn't download remote properties, try using local ones
    // if we have some philosopher properties (local or better remote)
    // then check if this version is known to be compatible

    ProcessBuilder pb = new ProcessBuilder(binPath, "version");
    pb.redirectErrorStream(true);

    boolean isNewVersionStringFound = false;
    String version = null;
    String build = null;

    // get the vesrion reported by the current executable
    String oldUnusedDownloadLink = null;
    int returnCode = 0;
    String printed = ProcessUtils.captureOutput(pb);
    log.debug("philosopher version printed: {}", printed);
    Matcher mNewVer = Pattern.compile("new\\s+version.*available.*?:\\s*(\\S+)", Pattern.CASE_INSENSITIVE).matcher(printed);
    if (mNewVer.find()) {
      isNewVersionStringFound = true;
      log.debug("Philosopher found newer update version");
      oldUnusedDownloadLink = mNewVer.group(1);
    }

    final Matcher mVer = Pattern.compile(".*version[^=]*?=\\s*v?\\.?([^\\s,;]+).*", Pattern.CASE_INSENSITIVE).matcher(printed);
    if (mVer.matches()) {
      version = mVer.group(1);
      log.debug("Detected philosopher version: {}", version);
    }
    final Matcher mBuild =  Pattern.compile(".*build[^=]*?=\\s*([^\\s,;]+).*", Pattern.CASE_INSENSITIVE).matcher(printed);
    if (mBuild.matches()) {
      build = mBuild.group(1);
      log.debug("Detected philosopher build: {}", build);
    }

    final String fragpipeVerMajor = com.dmtavt.fragpipe.Version.version().split("[-_]+")[0];
    final Properties props = PhilosopherProps.getProperties();
    String minPhiVer = props.stringPropertyNames().stream()
        .filter(name -> name.startsWith(PhilosopherProps.PROP_LOWEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(props::getProperty).orElse(null);
    String maxPhiVer = props.stringPropertyNames().stream()
        .filter(name -> name.startsWith(PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(props::getProperty).orElse(null);

    String link = PhilosopherProps.getProperties().getProperty(PhilosopherProps.PROP_DOWNLOAD_URL,
        "https://github.com/Nesvilab/philosopher/releases");



    if (StringUtils.isBlank(version) || returnCode != 0) {
      StringBuilder sb = new StringBuilder(
          "This Philosopher version is no longer supported by FragPipe.<br/>\n");
      if (minPhiVer != null) {
        sb.append("Minimum required version: ").append(minPhiVer).append("<br/>\n");
      }
      if (maxPhiVer != null) {
        sb.append("Latest known compatible version: ").append(maxPhiVer).append("<br/>\n");
      }
      sb.append("<a href=\"").append(link).append("\">Click here</a> to download a newer one.");
      throw new ValidationException(sb.toString());

    }


     if (minPhiVer != null && vc.compare(build, minPhiVer) < 0) {
      // doesn't meet min version requirement
      StringBuilder sb = new StringBuilder("Philosopher version ")
          .append(build).append(" is no longer supported by FragPipe.<br/>\n");
      sb.append("Minimum required version: ").append(minPhiVer).append("<br/>\n");
      if (maxPhiVer != null) {
        sb.append("Latest known compatible version: ").append(maxPhiVer).append("<br/>\n");
      }
      sb.append("<a href=\"").append(link).append("\">Click here</a> to download a newer one.");
      throw new ValidationException(sb.toString());

    } else if (isNewVersionStringFound) {
      return new Version(version, StringUtils.isBlank(build) ? "" : build, true, link);
    }
    return new Version(version, StringUtils.isBlank(build) ? "" : build, false, link);
  }
}
