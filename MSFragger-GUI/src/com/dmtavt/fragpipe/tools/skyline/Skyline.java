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

package com.dmtavt.fragpipe.tools.skyline;

import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.jooq.lambda.Seq;

public class Skyline {

  private static final String DOWNLOAD_URL = "https://skyline.ms/project/home/software/Skyline/begin.view";
  private static final Pattern versionWithSpacesPattern = Pattern.compile(" ([\\d.]+) ?");
  private static final Pattern versionNoSpacesPattern = Pattern.compile("([\\d.]+)");

  public static DefaultArtifactVersion skylineVersionT = new DefaultArtifactVersion("23.1.0.380");
  private static DefaultArtifactVersion skylineVersion = null;
  private static String skylineRunnerPath = null;

  public static String getSkylineRunnerPath() {
    if (skylineRunnerPath != null) {
      return skylineRunnerPath;
    }
    try {
      sub();
    } catch (Exception e) {
      e.printStackTrace();
    }
    return skylineRunnerPath;
  }

  public static DefaultArtifactVersion getSkylineVersion() {
    if (skylineVersion != null) {
      return skylineVersion;
    }
    try {
      sub();
    } catch (Exception e) {
      e.printStackTrace();
    }
    return skylineVersion;
  }

  private static void sub() throws Exception {
    for (String s : new String[]{"SkylineRunner.exe", "SkylineDailyRunner.exe"}) {
      List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(s));
      if (classpathJars != null && !classpathJars.isEmpty()) {
        Version version = validate(classpathJars.get(0).toAbsolutePath().toString());
        if (version != null) {
          skylineVersion = new DefaultArtifactVersion(version.strippedVersion);
          skylineRunnerPath = classpathJars.get(0).toAbsolutePath().toString();
        }
      }
    }
  }

  public static Version validate(String path) throws ValidationException, UnexpectedException {
    String binPath = PathUtils.testBinaryPath(path);
    if (binPath == null) {
      throw new ValidationException("Does not appear to be an executable file: \"" + path + "\"");
    }

    StringBuilder sbVer = new StringBuilder();

    ProcessBuilder pb = new ProcessBuilder(binPath, "--version");
    pb.redirectErrorStream(true);
    ProcessUtils.consumeLines(pb, line -> {
      sbVer.append(line);
      return sbVer.length() == 0;
    });

    if (sbVer.toString().startsWith("Error")) {
      return null;
    }

    if (sbVer.length() == 0) {
      throw new ValidationException("Version string not found for Skyline from \"" + binPath + "\"");
    }

    return new Version(sbVer.toString(), false, DOWNLOAD_URL);
  }

  public static class Version {

    public final String versionStr;
    public final String strippedVersion;
    public final boolean isNewVersionFound;
    public final String downloadUrl;

    public Version(String versionStr, boolean isNewVersionFound, String downloadUrl) {
      this.versionStr = versionStr;
      this.isNewVersionFound = isNewVersionFound;
      this.downloadUrl = downloadUrl;
      this.strippedVersion = getStrippedVersion(versionStr);
    }

    private String getStrippedVersion(String version) {
      Matcher match;
      if (version.contains(" ")) {
        match = versionWithSpacesPattern.matcher(version);
      } else {
        match = versionNoSpacesPattern.matcher(version);
      }
      if (match.find()) {
        return match.group(1);
      } else {
        return version;
      }
    }
  }
}
