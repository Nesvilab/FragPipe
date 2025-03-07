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

package org.nesvilab.fragpipe.tools.diann;

import static org.nesvilab.utils.OsUtils.isUnix;
import static org.nesvilab.utils.OsUtils.isWindows;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.jooq.lambda.Seq;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.exceptions.UnexpectedException;
import org.nesvilab.fragpipe.exceptions.ValidationException;
import org.nesvilab.fragpipe.util.GitHubJson;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.ProcessUtils;
import org.nesvilab.utils.SwingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Diann {

  private static final Logger log = LoggerFactory.getLogger(Diann.class);
  private static final String DOWNLOAD_GITHUB_PAGE_URL = "https://github.com/vdemichev/DiaNN/releases/latest";
  private static final String GITHUB_RELEASE_LATEST_API_URL = "https://api.github.com/repos/vdemichev/DiaNN/releases/latest";
  private static final Pattern reVer = Pattern.compile("DIA-NN ([\\w\\s.-]+) \\(Data-Independent Acquisition by Neural Networks\\)", Pattern.CASE_INSENSITIVE);

  private static final String[] DIANN_SO_DEPS = {"diann_so/libm.so.6", "diann_so/libstdc++.so.6"};
  private static final String[] DIANN_SO_DEPS_libgomp = {"diann_so/libgomp.so.1.0.0"};
  public static final String DIANN_WIN = "diann/1.8.2_beta_8/win/DiaNN.exe";
  public static final String DIANN_LINUX = "diann/1.8.2_beta_8/linux/diann-1.8.1.8";
  public static final String fallBackDiannVersion = "1.8.2 Beta 8";
  public static String LD_PRELOAD_str;
  public static String fallbackDiannPath;

  static {
    List<Path> diannPath = null;
    if (isWindows()) {
      diannPath = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_WIN));
    } else if (isUnix()) {
      diannPath = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_LINUX));
    } else {
      SwingUtils.showErrorDialog(null, "DIA-NN only works in Windows and Linux.", "DIA-NN not supported in this OS");
      fallbackDiannPath = null;
    }

    if (diannPath == null || diannPath.isEmpty()) {
      SwingUtils.showErrorDialog(null, "Cannot find DIA-NN executable file.", "DIA-NN not found");
      fallbackDiannPath = null;
    }

    if (diannPath != null) {
      if (diannPath.size() > 1) {
        System.err.print("There are more than one DIA-NN executable file: ");
        for (Path p : diannPath) {
          System.err.print(p.toAbsolutePath() + "; ");
        }
        System.err.println();
        fallbackDiannPath = null;
      } else if (diannPath.size() == 1) {
        LD_PRELOAD_str = getLDPRELOAD(diannPath);
        fallbackDiannPath = diannPath.get(0).toAbsolutePath().normalize().toString();
      }
    }
  }

  public static void downloadDiannManually() {
    try {
      Desktop.getDesktop().browse(URI.create(DOWNLOAD_GITHUB_PAGE_URL));
    } catch (IOException ex) {
      log.error("Error opening browser to download DIA-NN", ex);
      SwingUtils.showErrorDialogWithStacktrace(ex, null);
    }
  }

  public static Version validate(String path) throws ValidationException, UnexpectedException {
    String binPath = PathUtils.testBinaryPath(path);
    if (binPath == null) {
      throw new ValidationException("Does not appear to be an executable file: \"" + path + "\"");
    }
    if (binPath.toLowerCase().endsWith(".exe") && !isWindows()) {
      throw new ValidationException("The DIA-NN binary file is a windows version");
    }
    if (!binPath.toLowerCase().endsWith(".exe") && !binPath.toLowerCase().endsWith(".exe\"") && isWindows()) {
      throw new ValidationException("The DIA-NN binary file is not a windows version");
    }

    StringBuilder sbVer = new StringBuilder();

    ProcessBuilder pb = new ProcessBuilder(binPath, "--version");
    pb.redirectErrorStream(true);
    ProcessUtils.consumeLines(pb, line -> {
      Matcher mVer = reVer.matcher(line.trim());
      if (mVer.matches()) {
        sbVer.append(mVer.group(1));
      }
      return sbVer.length() == 0;
    });

    if (sbVer.length() == 0) {
      throw new ValidationException("Version string not found for DIA-NN from \"" + binPath + "\"");
    }

    return new Version(sbVer.toString(), false, DOWNLOAD_GITHUB_PAGE_URL);
  }

  public static UpdateInfo checkUpdates(String path) {
    try {
      Version version = validate(path);
      String latestVersion = fetchLatestVersion();
      if (!latestVersion.equalsIgnoreCase("n/a")) {
        DefaultArtifactVersion v1 = new DefaultArtifactVersion(version.version);
        DefaultArtifactVersion v2 = new DefaultArtifactVersion(latestVersion);
        if (v1.compareTo(v2) < 0) {
          return new UpdateInfo(true, DOWNLOAD_GITHUB_PAGE_URL);
        }
      }
    } catch (Exception ignored) {}

    return new UpdateInfo(false, DOWNLOAD_GITHUB_PAGE_URL);
  }

  private static String fetchLatestVersion() {
    try {
      String response = org.apache.commons.io.IOUtils.toString(new URL(GITHUB_RELEASE_LATEST_API_URL), StandardCharsets.UTF_8);
      Gson gson = new GsonBuilder().create();
      GitHubJson gitHubJsons = gson.fromJson(response, new TypeToken<GitHubJson>() {}.getType());
      return gitHubJsons.getTagName();
    } catch (Exception ex) {
      return "N/A";
    }
  }

  private static String getLDPRELOAD(List<Path> diannPath) {
    String LD_PRELOAD_str = null;
    if (isUnix()) {
      try {
        final ProcessBuilder pb = new ProcessBuilder(diannPath.get(0).toAbsolutePath().normalize().toString());
        if (checkExitCode(pb) != 0) {
          final List<Path> diann_so_path = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_SO_DEPS));
          if (diann_so_path == null || diann_so_path.size() != DIANN_SO_DEPS.length) {
            System.err.print(".so files missing");
            return null;
          }
          LD_PRELOAD_str = diann_so_path.get(0).toString() + ":" + diann_so_path.get(1).toString();
          final ProcessBuilder pb2 = new ProcessBuilder(diannPath.get(0).toAbsolutePath().normalize().toString());
          pb2.environment().put("LD_PRELOAD", LD_PRELOAD_str);
          if (checkExitCode(pb2) != 0) {
            final List<Path> diann_so_path2 = FragpipeLocations.checkToolsMissing(Seq.of(DIANN_SO_DEPS_libgomp));
            if (diann_so_path2 == null || diann_so_path2.size() != DIANN_SO_DEPS_libgomp.length) {
              System.err.print(".so files missing");
              return null;
            }
            LD_PRELOAD_str += ":" + diann_so_path2.get(0).toString();
          }
        }
      } catch (Exception ex) {
        System.err.println("Failed in checking " + diannPath.get(0).toAbsolutePath());
        ex.printStackTrace();
        return null;
      }
    }
    return LD_PRELOAD_str;
  }

  private static int checkExitCode(final ProcessBuilder pb) throws Exception {
    final Process proc;
    proc = pb.start();
    return proc.waitFor();
  }

  public static class Version {

    public final String version;
    public final boolean isNewVersionFound;
    public final String downloadUrl;

    public Version(String version, boolean isNewVersionFound, String downloadUrl) {
      this.version = version;
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
