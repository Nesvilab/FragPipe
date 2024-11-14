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

package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.Installed;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.PythonModule;
import com.github.chhh.utils.RegQuery;
import com.github.chhh.utils.StringUtils;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.jooq.lambda.Seq;
import org.slf4j.LoggerFactory;

public class PyInfo {
  private static final org.slf4j.Logger log = LoggerFactory.getLogger(PyInfo.class);
  private String command;
  private String version;
  private DefaultArtifactVersion fullVersion;
  private Map<PythonModule, Installed> modules = new HashMap<>();

  @Override
  public String toString() {
    return new StringJoiner(", ", PyInfo.class.getSimpleName() + "[", "]")
        .add("command='" + command + "'")
        .add("version='" + version + "'")
        .add("fullVersion=" + fullVersion)
        .toString();
  }

  public static final String pythonWinPath = "python_Windows/python/python.exe";
  public static final String pythonLinuxPath = "python_Linux/python/bin/python";

  private PyInfo() {
  }

  public static PyInfo fromCommand(String command) throws ValidationException, UnexpectedException {
    PyInfo pi = new PyInfo();
    pi.trySetPythonCommand(command);
    return pi;
  }

  /** @param command The command to start python interpreter. */
  private void trySetPythonCommand(String command) throws ValidationException, UnexpectedException {
    this.command = command;
    this.version = tryGetVersion(this.command);

    Matcher m = Pattern.compile("python\\s+([0-9.]+)", Pattern.CASE_INSENSITIVE).matcher(this.version);
    if (m.find()) {
      this.fullVersion = new DefaultArtifactVersion(m.group(1).trim());
    } else {
      throw new ValidationException("Could not detect python version.");
    }
  }

  public String getCommand() {
    return command;
  }

  public String getVersion() {
    return version;
  }

  public DefaultArtifactVersion getFullVersion() {
    return fullVersion;
  }

  public Map<PythonModule, Installed> getModules() {
    return Collections.unmodifiableMap(modules);
  }

  private static String tryGetVersion(String cmd) throws UnexpectedException, ValidationException {
    ProcessBuilder pb = new ProcessBuilder(cmd, "--version");
    pb.redirectErrorStream(true);

    String printed;
    log.debug("Trying to check python version: {}", String.join(" ", pb.command()));
    printed = ProcessUtils.captureOutput(pb);
    log.debug("Python version command printed: {}", printed);

    Matcher m = Pattern.compile("(python\\s+[0-9.]+)", Pattern.CASE_INSENSITIVE).matcher(printed);
    if (m.find()) {
      String version = m.group(1);
      log.debug("Found python version string in output: {}", version);
      return version;
    } else {
      log.debug("Did not find python version string in output");
      throw new ValidationException("Could not detect python version");
    }
  }

  /**
   * Checks that a registry location can be converted to {@link java.nio.file.Path}
   * via {@link Paths#get(String, String...)} method.
   */
  private static boolean vetRegistryLocation(String loc) {
    try {
      Paths.get(loc);
    } catch (Exception ignored) {
      return false;
    }
    return true;
  }

  public static PyInfo findSystemPython() throws UnexpectedException {
    return findSystemPython(null);
  }

  public static PyInfo findSystemPython(DefaultArtifactVersion minFullVersion) throws UnexpectedException {
    // try to query the registry on Windows
    if (OsUtils.isWindows()) {
      return findPythonWindows(minFullVersion);
    } else {
      return findPythonOtherOs(minFullVersion);
    }
  }

  private static PyInfo findPythonOtherOs(DefaultArtifactVersion minFullVersion) {
    final String[] commands = {"python", "python3"};
    // try the Python commands searching PATH env-var
    for (String cmd : commands) {
      try {
        PyInfo pi = com.dmtavt.fragpipe.api.PyInfo.fromCommand(cmd);
        if (minFullVersion == null || pi.getFullVersion().compareTo(minFullVersion) >= 0) {
          return pi;
        }
      } catch (Exception ignore) {}
    }
    return null;
  }

  private static PyInfo findPythonWindows(DefaultArtifactVersion minFullVersion) throws UnexpectedException {
    final String[] commands = {"python", "python3"};
    final String[] roots = {"HKCU", "HKU", "HKLM", "HKCR", "HKCC"};
    final String[] locations = {
        "\\Software\\Python\\PythonCore"
    };
    List<String> potentialLocs = new ArrayList<>();
    for (String root : roots) {
      for (String loc : locations) {
        List<String> query = RegQuery.query(root + loc);
        potentialLocs.addAll(query.stream()
            .filter(com.dmtavt.fragpipe.api.PyInfo::vetRegistryLocation)
            .collect(Collectors.toList()));
      }
    }

    List<String> possiblePython3InstallPaths = potentialLocs.stream()
        .filter(loc -> Paths.get(loc).getFileName().toString().startsWith("3."))
        .sorted(Comparator.reverseOrder())
        .collect(Collectors.toList());
    for (String possiblePython3InstallPath : possiblePython3InstallPaths) {
      final List<String> qureyInstallPath;
      try {
        qureyInstallPath = RegQuery.query(possiblePython3InstallPath + "\\InstallPath", "");
      } catch (Exception ignored) {
        continue;
      }
      for (String installPath : qureyInstallPath) {
        for (String cmd : commands) {
          try {
            final String rVal = RegQuery.getTokenValue(RegQuery.TOKEN_REGSZ, installPath);
            final String pythonBinPath = Paths.get(rVal, cmd).toString();
            PyInfo pi = com.dmtavt.fragpipe.api.PyInfo.fromCommand(pythonBinPath);
            if (minFullVersion == null || pi.getFullVersion().compareTo(minFullVersion) >= 0) {
              return pi;
            }

          } catch (Exception ignored) {
            // on Windows the registry might be dirty, those paths don't mean much
          }
        }
      }
    }

    // as a backup try just running the commands
    return findPythonOtherOs(minFullVersion);
  }


  public static void modifyEnvironmentVariablesForPythonSubprocesses(final ProcessBuilder pb) {
    final String command = pb.command().get(0);
    final Map<String, String> env = pb.environment();
    modifyEnvironmentVariablesForPythonSubprocesses(command, env);
  }

  /** Modify environment variables for Anaconda Python on Windows. */
  public static void modifyEnvironmentVariablesForPythonSubprocesses(final String command, final Map<String, String> env) {
    Path p;
    try {
      p = Paths.get(command);
    } catch (Exception e) {
      throw new IllegalStateException("Could not parse python command as path");
    }

    if (OsUtils.isWindows()) {

      if (!p.isAbsolute()) {
        // Windows + Not absolute python bin path. Try to get the first location reported by "WHERE <cmd>"
        List<String> cmdWhere = Arrays.asList("where", p.toString());
        List<String> lines;
        try {
          lines = ProcessUtils.captureOutputLines(new ProcessBuilder(cmdWhere));
          log.debug("Searching for actual python bin path using WHERE on Windows yielded:\n\t{}", lines);
          if (lines.isEmpty() || lines.stream().anyMatch(line -> line.toLowerCase().contains("could not find"))) {
            throw new IllegalStateException("Windows' WHERE command could not find any python binary matching: " + p.toString());
          }
          p = Paths.get(lines.get(0)); // update p
          log.debug("Using WHERE output, determined python bin full path: {}", p);
        } catch (UnexpectedException e) {
          throw new IllegalStateException("Error running WHERE command to search for actual python location: " + String.join(" ", cmdWhere));
        }
      } else {
        log.debug("Python bin path is already absolute, should be ok: {}", p.toString());
      }

      final Path root = p.toAbsolutePath().getParent();
      if (root == null) {
        throw new IllegalStateException("Could not get parent path for python command");
      }

      final String[] pathVarNames = {"path", "PATH", "Path"};
      for (String pathVarName : pathVarNames) {
        log.debug("Env map contains variable named [{}]: {}", pathVarName, env.containsKey(pathVarName));
      }

      Seq<String> envPath = Seq
          .of(root.toString()) // main python dir
          .append( // for Anaconda Python
              Seq.of("Library\\mingw-w64\\bin", "Library\\usr\\bin", "Library\\bin", "Scripts", "bin")
                  .map(rel -> root.resolve(rel).toString()))
          .append( // for Python programs invoking Java programs
              Seq.of(System.getProperty("java.home")).filter(StringUtils::isNotBlank)
              .map(Paths::get).map(javaHomePath -> javaHomePath.resolve("bin").toString()))
          .append( // append old existing entries in PATH (split by platform-dependent separator)
              Seq.of("path", "PATH", "Path").map(env::get).filter(StringUtils::isNotBlank)
              .flatMap(oldPath -> Arrays.stream(oldPath.split(File.pathSeparator)))
              .map(String::trim).filter(StringUtils::isNotBlank));

      env.put("Path", envPath.toString(File.pathSeparator));

      log.debug("python env map: {}", env);
    }
  }

  public String validateCalFile(final Path path) {
    ProcessBuilder pb = new ProcessBuilder(command, "-c", "import pandas as pd; import sys\n" +
        "rt_referencefile = sys.argv[1]\n" +
        "rt_reference_run = pd.read_csv(rt_referencefile, index_col=False, sep='\\t')\n" +
        "if not {'modified_peptide', 'precursor_charge', 'irt'}.issubset(rt_reference_run.columns):\n" +
        "  print('Reference iRT file has wrong format. Requires columns modified_peptide, precursor_charge and irt.')\n" +
        "  print(f'{rt_reference_run.columns}')\n" +
        "  print(f'{rt_reference_run}')\n" +
        "  sys.exit(1)\n" +
        "print('ok')",
        path.toString());
    PyInfo.modifyEnvironmentVariablesForPythonSubprocesses(pb);
    pb.environment().put("PYTHONIOENCODING", "utf-8");
    final String s;
    try {
      Process proc = pb.redirectErrorStream(true).start();
      java.io.InputStream inputStream = proc.getInputStream();
      s = new java.util.Scanner(inputStream).useDelimiter("\\A").next();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    return s;
  }

  public String validateIMCalFile(final Path path) {
    ProcessBuilder pb = new ProcessBuilder(command, "-c", "import pandas as pd; import sys\n" +
        "rt_referencefile = sys.argv[1]\n" +
        "rt_reference_run = pd.read_csv(rt_referencefile, index_col=False, sep='\\t')\n" +
        "if not {'modified_peptide', 'precursor_charge', 'im'}.issubset(rt_reference_run.columns):\n" +
        "  print('Reference iRT file has wrong format. Requires columns modified_peptide, precursor_charge and im.')\n" +
        "  print(f'{rt_reference_run.columns}')\n" +
        "  print(f'{rt_reference_run}')\n" +
        "  sys.exit(1)\n" +
        "print('ok')",
        path.toString());
    PyInfo.modifyEnvironmentVariablesForPythonSubprocesses(pb);
    pb.environment().put("PYTHONIOENCODING", "utf-8");
    final String s;
    try {
      Process proc = pb.redirectErrorStream(true).start();
      java.io.InputStream inputStream = proc.getInputStream();
      s = new java.util.Scanner(inputStream).useDelimiter("\\A").next();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    return s;
  }

  /**
   * Check if a specific package is installed in a python environment.
   *
   * @param module The name of one of the packages specified in {@code setup.py}
   *      {@code packages = [...]} array.
   * @return UNKNOWN if some errors occur while trying to start the interpreter.
   */
  public Installed checkModuleInstalled(PythonModule module) {
    Installed cached = modules.get(module);
    if (cached != null)
      return cached;

    if (command == null)
      throw new IllegalStateException("Call to checkModuleInstalled() before setPythonCommand()");

    Installed installed = Installed.UNKNOWN;
    ProcessBuilder pb = new ProcessBuilder(command,
        "-c", String.format(
        "try:\n" +
            "    import %s\n" +
            "except ModuleNotFoundError:\n" +
            "    print('ModuleNotFoundError')\n" +
            "except ImportError:\n" +
            "    print('Installed with ImportError')\n" +
            "else:\n" +
            "    print('Installed and imported with no error')",
        module.someImportName));
    modifyEnvironmentVariablesForPythonSubprocesses(pb);
    Process pr = null;
    try {
      pr = pb.start();
    } catch (IOException ex) {
      log.error("Could not start python " + module.installName + " check process", ex);
    }
    if (pr != null) {
      try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
        String line;
        while ((line = in.readLine()) != null) {
          if ("Installed and imported with no error".equals(line))
            installed = Installed.YES;
          else if ("ModuleNotFoundError".equals(line))
            installed = Installed.NO;
          else if ("Installed with ImportError".equals(line))
            installed = Installed.INSTALLED_WITH_IMPORTERROR;
        }
      } catch (IOException ex) {
        log.error("Could not read python " + module.installName + " check output", ex);
      }
      try {
        pr.waitFor();
      } catch (InterruptedException ex) {
        log.error("Error while waiting for python " + module.installName + " check process to finish", ex);
      }
    }

    modules.put(module, installed);
    return installed;
  }

  public List<PythonModule> modulesOfStatus(Installed installedStatus, List<PythonModule> modules) {
    return modules.stream()
        .filter(pm -> installedStatus.equals(checkModuleInstalled(pm)))
        .collect(Collectors.toList());
  }

  public Map<Installed, List<PythonModule>> modulesByStatus(List<PythonModule> modules) {
    return modules.stream()
        .collect(Collectors.groupingBy(this::checkModuleInstalled));
  }
}
