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

package org.nesvilab.fragpipe;

import static org.nesvilab.utils.OsUtils.isUnix;
import static org.nesvilab.utils.OsUtils.isWindows;

import java.util.Locale;
import org.nesvilab.utils.SwingUtils;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;

public class FragPipeMain {

  public static String PHILOSOPHER_VERSION = "0.0.0";
  private static final Pattern patternWin = Pattern.compile("philosopher-v(.+)\\.exe");
  private static final Pattern patternUnix = Pattern.compile("philosopher-v((?!.*\\.exe$).+)");

  public static void main(String[] args) {
    Locale.setDefault(Locale.US);
    if (args.length == 1 && (args[0].equalsIgnoreCase("--help") || args[0].equalsIgnoreCase("-h"))) {
      System.out.print(Fragpipe.help());
      System.exit(1);
    } else if (args.length > 0) {
      for (int i = 0; i < args.length; ++i) {
        if (args[i].equalsIgnoreCase("--headless")) {
          System.setProperty("java.awt.headless", "true"); // In some rare case, the server does not have X11 but DISPLAY env var is set, which crashes the headless mode. Setting the headless env to true to prevent the crash.
          Fragpipe.headless = true;
        } else if (args[i].equalsIgnoreCase("--dry-run")) {
          Fragpipe.dryRun = true;
        } else if (args[i].equalsIgnoreCase("--workflow")) {
          Fragpipe.workflowFile = Paths.get(args[++i]);
        } else if (args[i].equalsIgnoreCase("--manifest")) {
          Fragpipe.manifestFile = Paths.get(args[++i]);
        } else if (args[i].equalsIgnoreCase("--ram")) {
          Fragpipe.ram = Integer.parseInt(args[++i]);
        } else if (args[i].equalsIgnoreCase("--threads")) {
          Fragpipe.nThreadsHeadlessOnly = Integer.parseInt(args[++i]);
        } else if (args[i].equalsIgnoreCase("--workdir")) {
          Fragpipe.workdir = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-tools-folder")) {
          Fragpipe.toolsFolderPath = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-diann")) {
          Fragpipe.diannBinPath = args[++i].trim();
        } else if (args[i].equalsIgnoreCase("--config-python")) {
          Fragpipe.pythonBinPath = args[++i].trim();
        } else {
          System.err.println("Cannot recognize the argument " + args[i]);
          System.exit(1);
        }
      }
    }

    getPhilosopherBin();

    Fragpipe.main0();
  }

  private static void getPhilosopherBin() {
    Path p = FragpipeLocations.get().getDirTools().resolve("Philosopher");
    DefaultArtifactVersion latestVersionWin = new DefaultArtifactVersion(PHILOSOPHER_VERSION);
    DefaultArtifactVersion latestVersionUnix = new DefaultArtifactVersion(PHILOSOPHER_VERSION);
    Path pWin = null;
    Path pUnix = null;

    try (DirectoryStream<Path> stream = Files.newDirectoryStream(p)) {
      for (Path entry : stream) {
        Matcher mWin = patternWin.matcher(entry.getFileName().toString());
        Matcher mUnix = patternUnix.matcher(entry.getFileName().toString());
        if (mWin.matches()) {
          DefaultArtifactVersion v = new DefaultArtifactVersion(mWin.group(1));
          if (latestVersionWin.compareTo(v) < 0) {
            latestVersionWin = v;
            pWin = entry;
          }
        }
        if (mUnix.matches()) {
          DefaultArtifactVersion v = new DefaultArtifactVersion(mUnix.group(1));
          if (latestVersionUnix.compareTo(v) < 0) {
            latestVersionUnix = v;
            pUnix = entry;
          }
        }
      }
    } catch (IOException e) {
      SwingUtils.showErrorDialogWithStacktrace(e, null);
      Fragpipe.philosopherBinPath = null;
      PHILOSOPHER_VERSION = "NA";
      return;
    }

    if (isWindows()) {
      if (pWin != null) {
        Fragpipe.philosopherBinPath = pWin.normalize().toAbsolutePath().normalize().toString();
        PHILOSOPHER_VERSION = latestVersionWin.toString();
      } else {
        SwingUtils.showErrorDialog(null, "Philosopher binary not found at " + p, "Philosopher not found");
        Fragpipe.philosopherBinPath = null;
        PHILOSOPHER_VERSION = "NA";
      }
    } else if (isUnix()) {
      if (pUnix != null) {
        Fragpipe.philosopherBinPath = pUnix.normalize().toAbsolutePath().normalize().toString();
        PHILOSOPHER_VERSION = latestVersionUnix.toString();
      } else {
        Fragpipe.philosopherBinPath = null;
        PHILOSOPHER_VERSION = "NA";
      }
    } else {
      SwingUtils.showErrorDialog(null, "Philosopher only supports Windows and Unix systems", "Philosopher not supported in this OS");
      Fragpipe.philosopherBinPath = null;
      PHILOSOPHER_VERSION = "NA";
    }
  }
}
