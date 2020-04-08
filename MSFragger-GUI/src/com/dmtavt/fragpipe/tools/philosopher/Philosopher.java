package com.dmtavt.fragpipe.tools.philosopher;

import com.dmtavt.fragpipe.Fragpipe;
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

    // get the vesrion reported by the current executable
    // if we couldn't download remote properties, try using local ones
    // if we have some philosopher properties (local or better remote)
    // then check if this version is known to be compatible

    ProcessBuilder pb = new ProcessBuilder(binPath, "version");
    pb.redirectErrorStream(true);

    boolean isNewVersionStringFound = false;

    // get the vesrion reported by the current executable
    String oldUnusedDownloadLink = null;
    int returnCode = 0;

    final Pattern reNewVer = Pattern.compile("new\\s+version.*available.*?:\\s*(\\S+)", Pattern.CASE_INSENSITIVE);
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
    final Properties p = Fragpipe.props();
    String minPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name.startsWith(PhilosopherProps.PROP_LOWEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);
    String maxPhiVer = p.stringPropertyNames().stream()
        .filter(name -> name.startsWith(PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + fragpipeVerMajor))
        .findFirst().map(p::getProperty).orElse(null);

    String link = p.getProperty(PhilosopherProps.PROP_DOWNLOAD_URL,
        "https://github.com/Nesvilab/philosopher/releases");


    final String version = sbVer.toString();
    final String build = sbBuild.toString();
    log.debug("Phi validation, proceeding with Version: {}, Build: {}", version, build);

    if (StringUtils.isBlank(version)) {
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


//    final VersionComparator vc = new VersionComparator();
//     if (minPhiVer != null && vc.compare(build, minPhiVer) < 0) {
//      // doesn't meet min version requirement
//      StringBuilder sb = new StringBuilder("Philosopher version ")
//          .append(build).append(" is no longer supported by FragPipe.<br/>\n");
//      sb.append("Minimum required version: ").append(minPhiVer).append("<br/>\n");
//      if (maxPhiVer != null) {
//        sb.append("Latest known compatible version: ").append(maxPhiVer).append("<br/>\n");
//      }
//      sb.append("<a href=\"").append(link).append("\">Click here</a> to download a newer one.");
//      throw new ValidationException(sb.toString());
//
//    } else if (isNewVersionStringFound) {
//      return new Version(version, StringUtils.isBlank(build) ? "" : build, true, link);
//    }
    return new Version(version, StringUtils.isBlank(build) ? "" : build, false, link);
  }


}
