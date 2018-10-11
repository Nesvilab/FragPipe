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
import java.util.logging.Level;
import java.util.logging.Logger;
import umich.msfragger.gui.MsfraggerGuiFrame;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PythonInfo {

    private PythonInfo() {}

  /**
   * Check if a specific package is installed in a python environment.
   * @param pythonCmd The command to start python interpreter.
   * @param pkgImportName The name of one of the packages specified in {@code setup.py}
   *      {@code packages = [...]} array.
   * @return UNKNOWN if some errors occur while trying to start the interpreter.
   */
  public static Installed checkPythonPackageAvailability(String pythonCmd, String pkgImportName) {
      Installed installed = Installed.UNKNOWN;


      ProcessBuilder pb = new ProcessBuilder(pythonCmd,
              "-c", "import pkgutil; print(1 if pkgutil.find_loader('" + pkgImportName + "') else 0)");
      Process pr = null;
      try {
          pr = pb.start();
      } catch (IOException ex) {
          Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE,
                  "Could not start python " + pkgImportName + " check process", ex);
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
                      "Could not read python " + pkgImportName + " check output", ex);
          }
          try {
              pr.waitFor();
          } catch (InterruptedException ex) {
              Logger.getLogger(MsfraggerGuiFrame.class.getName()).log(Level.SEVERE,
                      "Error while waiting for python " + pkgImportName + " check process to finish", ex);
          }
      }

      return installed;
  }
}
