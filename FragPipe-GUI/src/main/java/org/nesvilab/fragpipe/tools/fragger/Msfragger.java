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

package org.nesvilab.fragpipe.tools.fragger;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.VersionFetcher;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.messages.MessageMsfraggerUpdateAvailable;
import org.nesvilab.fragpipe.messages.NoteConfigMsfragger;
import org.nesvilab.utils.StringUtils;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Msfragger {

  private static final Logger log = LoggerFactory.getLogger(Msfragger.class);
  public static final Pattern patternExpiryDate = Pattern.compile("Expiry date: (\\d{4}-\\d{2}-\\d{2}).");
  public static final Pattern patternCustomer = Pattern.compile("Customer: ([^.]+).");
  public static final Pattern patternMode = Pattern.compile("Mode: ([^.]+).");

  public static Version getVersion(Path jar) throws Exception {
    // only validate Fragger version if the current Java version is 1.9 or higher
    if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9)) {
      // we can't test fragger binary version when java version is less than 1.9
      throw new ValidationException("MSFragger requires Java 9+, can't check version without it.");
    }

    // get the version reported by the current executable
    return testJar(jar.toString());
  }


  public static void checkUpdates(NoteConfigMsfragger m) {
    final MsfraggerVerCmp vc = new MsfraggerVerCmp();
    final String verLocal = m.version;
    Thread t = new Thread(() -> {

      MsfraggerVersionFetcherServer vfServer = new MsfraggerVersionFetcherServer();
      MsfraggerVersionFetcherLocal vfLocal = new MsfraggerVersionFetcherLocal();
      List<VersionFetcher> verFetchers = Arrays.asList(vfServer, vfLocal);
      for (final VersionFetcher vf : verFetchers) {
        if (vf == null) {
          continue;
        }
        try {
          final String verUpdated = vf.fetchVersion();
          if (StringUtils.isNullOrWhitespace(verUpdated)) {
            continue;
          }
          // we got a non-empty version from some version fetcher
          if (vc.compare(verLocal, verUpdated) >= 0) {
            continue; // our version is same or newer
          }
          // local version is older than the fetched version
          String url = vf.getDownloadUrl();
          final String manualDownloadUrl = !StringUtils.isNullOrWhitespace(url) ? url : vfLocal.getDownloadUrl();
          Bus.post(new MessageMsfraggerUpdateAvailable(verUpdated, manualDownloadUrl));
          break;
        } catch (Exception ex) {
          // no biggie
          log.warn("Something happened while checking for MSFragger updates");
        }
      }
    });
    t.start();
  }

  private static Version testJar(String jarPath) throws Exception {

    String verStr = null;
    boolean isVersionParsed = false;
    String license = "Academic";
    String customer = "N/A";
    String mode = "N/A";
    String expiryDate = "N/A";
    boolean isValid = true;

    if (!jarPath.contains("-Commercial-")) {
      Matcher m = MsfraggerVerCmp.regex.matcher(jarPath);
      if (m.find()) {
        isVersionParsed = true;
        verStr = m.group(2);
      }
    }

    if (!isVersionParsed) {
      Path licensePath = FragpipeLocations.locateLicense();
      ProcessBuilder pb;
      if (licensePath == null) {
        pb = new ProcessBuilder(Fragpipe.getBinJava(), "-jar", jarPath);
      } else {
        pb = new ProcessBuilder(Fragpipe.getBinJava(), "-jar", jarPath, "--license", licensePath.toAbsolutePath().normalize().toString());
      }
      pb.redirectErrorStream(true);
      Process pr = pb.start();
      pr.waitFor(5, TimeUnit.SECONDS);
      try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
        String line;
        while ((line = in.readLine()) != null) {
          if (line.startsWith("The license ")) {
            license = "Commercial";
            if (line.endsWith(" is corrupted.") || line.endsWith(" is not valid for this product.") || line.contains(" has expired.")) {
              isValid = false;
            } else {
              isValid = true;
            }
            Matcher m1 = patternExpiryDate.matcher(line);
            Matcher m2 = patternCustomer.matcher(line);
            Matcher m3 = patternMode.matcher(line);
            if (m1.find()) {
              expiryDate = m1.group(1);
            }
            if (m2.find()) {
              customer = m2.group(1);
            }
            if (m3.find()) {
              mode = m3.group(1);
            }
          } else if (line.startsWith("No license file found.")) {
            isValid = false;
          }
         
          Matcher m = MsfraggerVerCmp.regex2.matcher(line);
          if (m.find()) {
            isVersionParsed = true;
            verStr = m.group(2);
          }
        }
      }
    }

    return new Version(isVersionParsed, verStr, license, customer, mode, expiryDate, isValid);
  }

  public static class Version {

    public final boolean isVersionParsed;
    public final DefaultArtifactVersion version;
    public final String license;
    public final String customer;
    public final String mode;
    public final String expiryDate;
    public final boolean isValid;

    public Version(boolean isVersionParsed, String version, String license, String customer, String mode, String expiryDate, boolean isValid) {
      this.isVersionParsed = isVersionParsed;
      if (version != null) {
        this.version = new DefaultArtifactVersion(version);
      } else {
        this.version = new DefaultArtifactVersion("N/A");
      }
      this.license = license;
      this.customer = customer;
      this.mode = mode;
      this.expiryDate = expiryDate;
      this.isValid = isValid;
    }
  }
}
