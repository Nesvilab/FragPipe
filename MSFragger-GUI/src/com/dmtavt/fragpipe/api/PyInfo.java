package com.dmtavt.fragpipe.api;

import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.Installed;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.PythonInfo;
import com.github.chhh.utils.PythonModule;
import com.github.chhh.utils.RegQuery;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.slf4j.LoggerFactory;

public class PyInfo {
  private static final org.slf4j.Logger log = LoggerFactory.getLogger(PyInfo.class);
  private String command;
  private String version;
  private int majorVersion;
  private Map<PythonModule, Installed> modules;

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
    this.version = tryGetVersion(command);
    Matcher m = Pattern.compile("python\\s+([0-9]+)", Pattern.CASE_INSENSITIVE)
        .matcher(this.version);
    if (m.find()) {
      final String majorVer = m.group(1);
      this.majorVersion = Integer.parseInt(majorVer);
    } else {
      throw new ValidationException("Could not detect python major version");
    }
  }

  public String getCommand() {
    return command;
  }

  public String getVersion() {
    return version;
  }

  public int getMajorVersion() {
    return majorVersion;
  }

  public Map<PythonModule, Installed> getModules() {
    return Collections.unmodifiableMap(modules);
  }

  private boolean isMajorVersion(int majorVersion) {
    return majorVersion == this.majorVersion;
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

  public static PyInfo findSystemPython(Integer minMajorVersion) throws UnexpectedException {
    String[] commands = {"python", "python3"};

    // try to query the registry on Windows
    if (OsUtils.isWindows()) {
      return findPythonWindows(minMajorVersion);
    } else {
      return findPythonOtherOs(minMajorVersion);
    }
  }

  private static PyInfo findPythonOtherOs(Integer minMajorVersion) {
    final String[] commands = {"python", "python3"};
    // try the Python commands searching PATH env-var
    for (String cmd : commands) {
      try {
        PyInfo pi = PyInfo.fromCommand(cmd);
        if (minMajorVersion == null || minMajorVersion <= pi.getMajorVersion()) {
          return pi;
        }
      } catch (Exception ignore) {}
    }
    return null;
  }

  private static PyInfo findPythonWindows(Integer minMajorVersion) throws UnexpectedException {
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
            .filter(PyInfo::vetRegistryLocation)
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
            PyInfo pi = PyInfo.fromCommand(pythonBinPath);
            if (minMajorVersion == null || minMajorVersion <= pi.getMajorVersion()) {
              return pi;
            }

          } catch (Exception ignored) {
            // on Windows the registry might be dirty, those paths don't mean much
          }
        }
      }
    }
    return null;
  }

  /**
   * modify environment variables for Anaconda Python on Windows
   */
  public static void modifyEnvironmentVariablesForPythonSubprocesses(final ProcessBuilder pb) {
    final String command = pb.command().get(0);
    if (Paths.get(command).isAbsolute() && OsUtils.isWindows()) {
      final String root = Paths.get(command).getParent().toString();
      final Map<String, String> env = pb.environment();
      env.put("Path", String.join(";",
          root,
          // for Anaconda Python
          Paths.get(root, "Library\\mingw-w64\\bin").toString(),
          Paths.get(root, "Library\\usr\\bin").toString(),
          Paths.get(root, "Library\\bin").toString(),
          Paths.get(root, "Scripts").toString(),
          Paths.get(root, "bin").toString(),
          // for Python programs invoking Java programs
          Paths.get(System.getProperty("java.home"), "bin").toString(),
          env.get("Path")));
    }
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
      Logger.getLogger(PythonInfo.class.getName()).log(Level.SEVERE,
          "Could not start python " + module.installName + " check process", ex);
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
        Logger.getLogger(PythonInfo.class.getName()).log(Level.SEVERE,
            "Could not read python " + module.installName + " check output", ex);
      }
      try {
        pr.waitFor();
      } catch (InterruptedException ex) {
        Logger.getLogger(PythonInfo.class.getName()).log(Level.SEVERE,
            "Error while waiting for python " + module.installName + " check process to finish", ex);
      }
    }

    modules.put(module, installed);
    return installed;
  }


}
