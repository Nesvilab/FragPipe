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

package org.nesvilab.utils;

import org.nesvilab.fragpipe.exceptions.UnexpectedException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class RegQuery {

  private static final String REG = "reg";
  private static final String QUERY = "query";
  public static final String TOKEN_REGSZ = "REG_SZ";
  public static final String TOKEN_REGDWORD = "REG_DWORD";

  // The following is the command and the result
  //
  // $> reg query "HKCU\Software\Python\PythonCore"
  // >HKEY_CURRENT_USER\Software\Python\PythonCore\3.7
  //
  // In case of an error a message is displayed like this:
  // >ERROR: The system was unable to find the specified registry key or value.
  /**
   * Query a registry path for sub-folders.
   * @param path Path in registry to query.
   * @return
   */
  public static List<String> query(String path) throws UnexpectedException {
    if (path == null)
      throw new NullPointerException("No nulls");
    List<String> cmd = new ArrayList<>();
    cmd.add("reg");
    cmd.add("query");
    cmd.add(path);
    return ProcessUtils.captureOutputLines(new ProcessBuilder(cmd));
  }

  /**
   * Query a regsitry path for keys. Key can be null or empty.
   * @param path Path in registry.
   * @param key Can be null or empty to query for (Default) empty-name registry key.
   * @return
   * @throws Exception
   */
  public static List<String> query(String path, String key) throws Exception {
    if (path == null)
      throw new NullPointerException("No nulls");
    List<String> cmd = new ArrayList<>();
    cmd.add("reg");
    cmd.add("query");
    cmd.add(path);
    if (key == null || key.trim().length() == 0) {
      cmd.add("/ve");
    } else {
      cmd.add("/v");
      cmd.add(key);
    }
    ProcessBuilder pb = new ProcessBuilder(cmd);
    Process pr = pb.start();
    BufferedReader in = new BufferedReader(new InputStreamReader(pr.getInputStream()));
    String line;
    List<String> results = new ArrayList<>();
    while ((line = in.readLine()) != null) {
      if (line.trim().length() == 0)
        continue;
      results.add(line);
    }
    return results;
  }

  public static String getTokenValue(String tokenType, String registryValue) {
    int loc = registryValue.indexOf(tokenType);
    if (loc < 0)
      throw new IllegalStateException(String.format(
          "Given value of wrong type. Expected %s, but value '%s'", tokenType, registryValue));
    return registryValue.substring(loc + tokenType.length()).trim();
  }
}
