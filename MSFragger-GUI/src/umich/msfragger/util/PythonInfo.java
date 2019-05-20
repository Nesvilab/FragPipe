/*
 * Copyright 2018 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.greenrobot.eventbus.EventBus;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PythonInfo {
  private static PythonInfo instance = new PythonInfo();
  private final Object initLock = new Object();
  public static PythonInfo get() {
    return instance;
  }

  private String command;
  private String version;
  private int majorVersion;
  private Map<PythonModule, Installed> modules;

  private PythonInfo() {
    reset(false);
  }

  private void reset(boolean notify) {
    final boolean unchanged = command == null && version == null && majorVersion == -1;
    command = null;
    version = null;
    majorVersion = -1;
    modules = new HashMap<>();
    if (notify && !unchanged)
      notifyInfoChanged();
  }

  private void notifyInfoChanged() {
    EventBus.getDefault().postSticky(new MessageInfoChanged());
  }

  public static class MessageInfoChanged {
  }

  /**
   * @param command The command to start python interpreter.
   * @return If the provided command works.
   */
  private boolean trySetPythonCommand(String command) throws Exception {
    synchronized (initLock) {
      String version = null;
      try {
        version = tryPythonCommandVersion(command);
      } catch (Exception e) {
        reset(true);
        throw e;
      }
      if (version == null) {
        reset(true);
        return false;
      }
      boolean notify = false;
      notify |= !Objects.equals(this.command, command);
      if (!Objects.equals(this.command, command))
        reset(false);
      this.command = command;
      notify |= !Objects.equals(this.version, version);
      this.version = version;
      Pattern verRe = Pattern.compile("python\\s+([0-9]+)", Pattern.CASE_INSENSITIVE);
      Matcher m1 = verRe.matcher(version);
      if (m1.find()) {
        final String pythonMajorVer = m1.group(1);
        notify |= !Objects.equals(this.majorVersion, Integer.valueOf(pythonMajorVer));
        this.majorVersion = Integer.valueOf(pythonMajorVer);
      }

      if (notify) {
        notifyInfoChanged();
      }
      return true;
    }
  }

  public boolean isInitialized() {
    synchronized (initLock) {
      return !StringUtils.isNullOrWhitespace(command);
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

  private static String tryPythonCommandVersion(String cmd) throws Exception {
    ProcessBuilder pb = new ProcessBuilder(cmd, "--version");
    pb.redirectErrorStream(true);

    Process pr;
    try {
      pr = pb.start();
    } catch (IOException ex) {
      throw new Exception("Could not start the python/python3 process.");
    }
    String version = null;
    try {
      // reading process output
      try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
        Pattern pythonVersionRe = Pattern.compile("(python\\s+[0-9\\.]+)", Pattern.CASE_INSENSITIVE);
        String line;
        while ((line = in.readLine()) != null) {
          Matcher m = pythonVersionRe.matcher(line);
          if (m.find()) {
            version = m.group(1);
          }
        }
      }
      int exitCode = pr.waitFor();
      if (exitCode == 0) {
        return version;
      }
    } catch (InterruptedException ex) {
      throw new Exception("Error waiting for python/python3 process to finish.");
    }

    return null;
  }

  public boolean setPythonCommand(String command) throws Exception {
    synchronized (initLock) {
      return trySetPythonCommand(command);
    }
  }

  /**
   * Checks that a registry location can be converted to {@link java.nio.file.Path}
   * via {@link Paths#get(String, String...)} method.
   */
  private boolean vetRegistryLocation(String loc) {
    try {
      Paths.get(loc);
    } catch (Exception ignored) {
      return false;
    }
    return true;
  }

  public void findPythonCommand() throws Exception {
    synchronized (initLock) {
      String[] commands = {"python", "python3"};

      // try to query the registry on Windows
      if (OsUtils.isWindows()) {
        final String[] roots = {"HKCU", "HKU", "HKLM", "HKCR", "HKCC"};
        final String[] locations = {
            "\\Software\\Python\\PythonCore"
        };
        List<String> potentialLocs = new ArrayList<>();
        for (String root : roots) {
          for (String loc : locations) {
            List<String> query = RegQuery.query(root + loc);
            potentialLocs.addAll(query.stream()
                .filter(this::vetRegistryLocation)
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
                if (trySetPythonCommand(pythonBinPath))
                  return;
              } catch (Exception ignored) {
                // on Windows the registry might be dirty, those paths don't mean much
              }
            }
          }
        }
      }

      // try the Python commands searching PATH env-var
      for (String cmd : commands) {
        if (trySetPythonCommand(cmd))
          return;
      }
    }
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
