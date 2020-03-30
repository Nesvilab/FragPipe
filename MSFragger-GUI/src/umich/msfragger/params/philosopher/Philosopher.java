package umich.msfragger.params.philosopher;

import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.VersionComparator;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Optional;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Philosopher {

  private static final Logger log = LoggerFactory.getLogger(Philosopher.class);

  public static class Version {

    public final String version;

    public Version(String version) {
      this.version = version;
    }
  }

  public static Version validate(String path) throws ValidationException, UnexpectedException {
    String binPath = PathUtils.testBinaryPath(path);

    final Pattern regexNewerVerFound = Pattern
        .compile("new version.*available.*?:\\s*(\\S+)", Pattern.CASE_INSENSITIVE);
    final Pattern regexVersion = Pattern
        .compile("build.*?=(?<build>\\S+).*version.*?=v?\\.?(?<version>\\S+)",
            Pattern.CASE_INSENSITIVE);
    final Pattern regexOldPhiVer = Pattern.compile("\\d{6,}");
    final VersionComparator vc = new VersionComparator();

    // get the vesrion reported by the current executable
    // if we couldn't download remote properties, try using local ones
    // if we have some philosopher properties (local or better remote)
    // then check if this version is known to be compatible

    ProcessBuilder pb = new ProcessBuilder(binPath, "version");
    pb.redirectErrorStream(true);

    boolean isNewVersionStringFound = false;
    String curVersionAndBuild = null;
    String curPhiVer = null;

    // get the vesrion reported by the current executable
    String oldUnusedDownloadLink = null;
    try {
      Process pr = pb.start();
      BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()));
      String line;
      while ((line = in.readLine()) != null) {
        Matcher m = regexNewerVerFound.matcher(line);
        if (m.find()) {
          isNewVersionStringFound = true;
          log.debug("Philosopher found newer update version");
          oldUnusedDownloadLink = m.group(1);
        }
        Matcher mVer = regexVersion.matcher(line);
        if (mVer.find()) {
          curVersionAndBuild = mVer.group("version") + " (build " + mVer.group("build") + ")";
          curPhiVer = mVer.group("version");
          log.debug("Detected philosopher version: {}", curPhiVer);
        }
      }

      int returnCode = pr.waitFor();

      String vCurMajor = com.dmtavt.fragpipe.Version.version().split("[-_]+")[0];
      final Properties props = PhilosopherProps.getProperties();
      String propKeyStubMin = PhilosopherProps.PROP_LOWEST_COMPATIBLE_VERSION + "." + vCurMajor;
      Optional<String> propKeyMin = props.stringPropertyNames().stream()
          .filter(name -> name.startsWith(propKeyStubMin)).findFirst();
      String minPhiVer = propKeyMin.map(props::getProperty).orElse(null);
      String propKeyStubMax = PhilosopherProps.PROP_LATEST_COMPATIBLE_VERSION + "." + vCurMajor;
      Optional<String> propKeyMax = props.stringPropertyNames().stream()
          .filter(name -> name.startsWith(propKeyStubMax)).findFirst();
      String maxPhiVer = propKeyMax.map(props::getProperty).orElse(null);

      String link = PhilosopherProps.getProperties().getProperty(PhilosopherProps.PROP_DOWNLOAD_URL,
          "https://github.com/Nesvilab/philosopher/releases");

      boolean isOldVersionScheme = curPhiVer != null && regexOldPhiVer.matcher(curPhiVer).find();
      if (isOldVersionScheme) {
        log.warn("Old philosopher versioning scheme detected");
      }

      if (returnCode != 0 || isOldVersionScheme || curPhiVer == null) {
        StringBuilder sb = new StringBuilder(
            "This Philosopher version is no longer supported by FragPipe.<br/>\n");
        if (minPhiVer != null) {
          sb.append("Minimum required version: ").append(minPhiVer).append("<br/>\n");
        }
        if (maxPhiVer != null) {
          sb.append("Latest known compatible version: ").append(maxPhiVer).append("<br/>\n");
        }
        sb.append("Please <a href=\"").append(link)
            .append("\">click here</a> to download a newer one.");
        ep = SwingUtils.createClickableHtml(sb.toString());

      } else {

        if (minPhiVer != null && vc.compare(curPhiVer, minPhiVer) < 0) {
          // doesn't meet min version requirement
          StringBuilder sb = new StringBuilder("Philosopher version ")
              .append(curPhiVer).append(" is no longer supported by FragPipe.<br/>\n");
          sb.append("Minimum required version: ").append(minPhiVer).append("<br/>\n");
          if (maxPhiVer != null) {
            sb.append("Latest known compatible version: ").append(maxPhiVer).append("<br/>\n");
          }
          sb.append("Please <a href=\"").append(link)
              .append("\">click here</a> to download a newer one.");
          ep = SwingUtils.createClickableHtml(sb.toString());

        } else if (isNewVersionStringFound) {
          StringBuilder sb = new StringBuilder();
          sb.append("Newer version of Philosopher available.<br/>\n");
          sb.append("<a href=\"").append(link).append("\">Click here</a> to download.<br/>\n");

          if (maxPhiVer == null) {
            sb.append(
                "<br>\nHowever, we have not yet checked if it's fully compatible with this version of ")
                .append(com.dmtavt.fragpipe.Version.PROGRAM_TITLE).append(".");
          } else { // max ver != null
            int cmp = vc.compare(curPhiVer, maxPhiVer);
            if (cmp == 0) {
              sb.append(
                  "<br>\nHowever, <b>you currently have the latest known tested version</b>.");
            } else if (cmp < 0) {
              sb.append("<br>\nThe latest known tested version is<br>\n")
                  .append("<b>Philosopher ").append(maxPhiVer).append("</b>.<br/>\n");
              sb.append(
                  "It is not recommended to upgrade to newer versions unless they are tested.");
            } else if (cmp > 0) {
              sb.append("<br>\nYour current version is higher than the last known tested version.");
            }
          }
          ep = SwingUtils.createClickableHtml(sb.toString());
        }
      }

    } catch (IOException | InterruptedException e) {
      throw new IllegalStateException(
          "Error while creating a java process for Philosopher test.");
    }
  }
}
