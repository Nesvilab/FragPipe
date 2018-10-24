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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.commons.lang3.NotImplementedException;
import umich.msfragger.gui.MsfraggerGuiFrame;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PythonInfo {
  private static PythonInfo instance = new PythonInfo();
  public static PythonInfo get() {
    return instance;
  }

  private String command;
  private String version;
  private int majorVersion;
  private Map<PythonModule, Installed> modules;

  private PythonInfo() {
    command = null;
    version = null;
    majorVersion = -1;
    modules = new HashMap<>();
  }

  /**
   * @param command The command to start python interpreter.
   * @return If the provided command works.
   */
  private synchronized boolean trySetPythonCommand(String command) throws Exception {
    String version = tryPythonCommandVersion(command);
    if (version == null)
      return false;
    this.command = command;
    this.version = version;
    Pattern verRe = Pattern.compile("python\\s+([0-9]+)", Pattern.CASE_INSENSITIVE);
    Matcher m1 = verRe.matcher(version);
    if (m1.find()) {
      final String pythonMajorVer = m1.group(1);
      this.majorVersion = Integer.valueOf(pythonMajorVer);
    }
    return true;
  }

  public boolean isAvailable() {
    return !StringUtils.isNullOrWhitespace(command);
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

  private String tryPythonCommandVersion(String cmd) throws Exception {
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

    return "Not recognized";
  }

  public void setPythonCommand(String command) throws Exception {
      trySetPythonCommand(command);
  }
  
  public void findPythonCommand() throws Exception {
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
          potentialLocs.addAll(RegQuery.query(root + loc));
        }
      }

      List<String> possiblePython3InstallPaths = potentialLocs.stream()
          .filter(loc -> Paths.get(loc).getFileName().toString().startsWith("3."))
          .sorted(Comparator.reverseOrder())
          .collect(Collectors.toList());
      for (String possiblePython3InstallPath : possiblePython3InstallPaths) {
        List<String> res = RegQuery.query(possiblePython3InstallPath + "\\InstallPath", "");
        for (String r : res) {
          for(String cmd : commands) {
            try {
              final String rVal = RegQuery.getTokenValue(RegQuery.TOKEN_REGSZ, r);
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
    for(String cmd : commands) {
      if (trySetPythonCommand(cmd))
        return;
    }
  }

  /**
   * Check if a specific package is installed in a python environment.
   *
   * @param module The name of one of the packages specified in {@code setup.py}
   *      {@code packages = [...]} array.
   * @return UNKNOWN if some errors occur while trying to start the interpreter.
   */
  public synchronized Installed checkModuleInstalled(PythonModule module) {
    Installed cached = modules.get(module);
    if (cached != null)
      return cached;

    if (command == null)
      throw new IllegalStateException("Call to checkModuleInstalled() before setPythonCommand()");

    Installed installed = Installed.UNKNOWN;
    ProcessBuilder pb = new ProcessBuilder(command,
        "-c", "import pkgutil; print(1 if pkgutil.find_loader('" + module.someImportName + "') else 0)");
    Process pr = null;
    try {
      pr = pb.start();
    } catch (IOException ex) {
      Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE,
          "Could not start python " + module.installName + " check process", ex);
    }
    if (pr != null) {
      try (BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()))) {
        String line;
        while ((line = in.readLine()) != null) {
          if ("1".equals(line))
            installed = Installed.YES;
          else if ("0".equals(line))
            installed = Installed.NO;
        }
      } catch (IOException ex) {
        Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE,
            "Could not read python " + module.installName + " check output", ex);
      }
      try {
        pr.waitFor();
      } catch (InterruptedException ex) {
        Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE,
            "Error while waiting for python " + module.installName + " check process to finish", ex);
      }
    }

    modules.put(module, installed);
    return installed;
  }


}
