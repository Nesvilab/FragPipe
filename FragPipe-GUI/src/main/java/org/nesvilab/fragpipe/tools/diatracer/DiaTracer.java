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

package org.nesvilab.fragpipe.tools.diatracer;

import static org.nesvilab.fragpipe.tools.fragger.Msfragger.patternCustomer;
import static org.nesvilab.fragpipe.tools.fragger.Msfragger.patternExpiryDate;
import static org.nesvilab.fragpipe.tools.fragger.Msfragger.patternMode;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.Bus;
import org.nesvilab.fragpipe.api.VersionFetcher;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.messages.MessageDiaTracerUpdateAvailable;
import org.nesvilab.fragpipe.messages.NoteConfigDiaTracer;
import org.nesvilab.fragpipe.tools.fragger.Msfragger.Version;
import org.nesvilab.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DiaTracer {

  private static final Logger log = LoggerFactory.getLogger(DiaTracer.class);
  private static final Pattern re = Pattern.compile("diatracer-(Commercial-)?(\\d+\\.\\d+\\.\\d+[\\w-]*).jar", Pattern.CASE_INSENSITIVE);
  private static final Pattern re2 = Pattern.compile("diatracer-(Commercial-)?(\\d+\\.\\d+\\.\\d+[\\w-]*)", Pattern.CASE_INSENSITIVE);

  public static Version getVersion(Path jar) throws Exception {
    Version test;
    if (!SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_9)) {
      throw new ValidationException("diaTracer requires Java 9+, can't check version without it.");
    }

    test = testJar(jar.toString());
    if (!test.isVersionParsed) {
      throw new ValidationException("Could not get version info with given jar: " + jar);
    }
    return test;
  }


  public static void checkUpdates(NoteConfigDiaTracer m) {
    final DefaultArtifactVersion verLocal = new DefaultArtifactVersion(m.version);
    Thread t = new Thread(() -> {
      DiaTracerVersionFetcherServer vfServer = new DiaTracerVersionFetcherServer();
      List<VersionFetcher> verFetchers = List.of(vfServer);
      for (final VersionFetcher vf : verFetchers) {
        if (vf == null) {
          continue;
        }
        try {
          final String verUpdated = vf.fetchVersion();
          if (StringUtils.isNullOrWhitespace(verUpdated)) {
            continue;
          }
          if (verLocal.compareTo(new DefaultArtifactVersion(verUpdated)) >= 0) {
            continue;
          }
          String url = vf.getDownloadUrl();
          Bus.post(new MessageDiaTracerUpdateAvailable(verUpdated, url));
          break;
        } catch (Exception ex) {
          log.warn("Something happened while checking for diatracer updates");
        }
      }
    });
    t.start();
  }

  private static Version testJar(String jarPath) throws Exception {

    String verStr = null;
    boolean isVersionParsed = false;
    String license = "N/A";
    String customer = "N/A";
    String mode = "N/A";
    String expiryDate = "N/A";
    boolean isValid = true;
    
    if (!jarPath.contains("-Commercial-")) {
      license = "Academic";
      Matcher m = re.matcher(jarPath);
      if (m.find()) {
        isVersionParsed = true;
        verStr = m.group(2);
      }
    }

    if (!isVersionParsed) {
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      if (Fragpipe.headless) {
        cmd.add("-Djava.awt.headless=true"); // In some rare case, the server does not have X11 but DISPLAY env var is set, which crashes the headless mode. Setting the headless env to true to prevent the crash.
      }
      cmd.add("-jar");
      cmd.add(jarPath);
      Path licensePath = FragpipeLocations.locateLicense();
      if (licensePath != null) {
        cmd.add("--license");
        cmd.add(licensePath.toAbsolutePath().normalize().toString());
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
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
            }
          }  else if (line.startsWith("No license file found.")) {
            isValid = false;
          }

          Matcher m = re2.matcher(line);
          if (m.find()) {
            isVersionParsed = true;
            verStr = m.group(2);
          }
        }
      }
    }

    return new Version(isVersionParsed, verStr, license, customer, mode, expiryDate, isValid);
  }
}
