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

package org.nesvilab.fragpipe.util;

import java.util.List;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Checks that the OS-level regional settings use '.' as the decimal separator.
 * FragPipe spawns external tools (MSFragger, IonQuant, Philosopher, ...) which
 * inherit the OS locale (LC_NUMERIC on Unix, the regional format on Windows).
 * If the decimal separator is ',', those tools may parse or emit numbers in a
 * format that downstream steps cannot read.
 */
public class LocaleCheck {

  private static final Logger log = LoggerFactory.getLogger(LocaleCheck.class);
  public static final String REQUIRED_DECIMAL_SEPARATOR = ".";

  private LocaleCheck() {}

  /**
   * @return the decimal separator the OS would use for non-Java tools, or
   *         {@code null} if it could not be determined.
   */
  public static String detectSystemDecimalSeparator() {
    try {
      if (OsUtils.isWindows()) {
        return detectWindowsDecimalSeparator();
      } else if (OsUtils.isUnix() || OsUtils.isMac()) {
        return detectUnixDecimalSeparator();
      }
    } catch (Exception e) {
      log.warn("Could not detect system decimal separator", e);
    }
    return null;
  }

  /**
   * @return an error message describing the problem, or {@code null} if the
   *         regional settings are acceptable (or undetectable).
   */
  public static String validateDecimalSeparator() {
    String sep = detectSystemDecimalSeparator();
    if (sep == null || REQUIRED_DECIMAL_SEPARATOR.equals(sep)) {
      return null;
    }
    StringBuilder sb = new StringBuilder();
    sb.append("FragPipe requires '.' (period) as the decimal separator in the system regional settings.\n");
    sb.append("The current decimal separator is '").append(sep).append("'.\n\n");
    if (OsUtils.isWindows()) {
      sb.append("To fix this on Windows:\n");
      sb.append("  1. Open Settings -> Time & Language -> Region (or Control Panel -> Region).\n");
      sb.append("  2. Click 'Additional date, time, & regional settings' -> 'Change date, time, or number formats'.\n");
      sb.append("  3. Click 'Additional settings...' and set 'Decimal symbol' to '.' (period).\n");
      sb.append("  4. Click OK / Apply, then restart FragPipe.");
    } else {
      sb.append("To fix this on Linux/macOS, set a locale that uses '.' as the decimal separator before launching FragPipe, for example:\n");
      sb.append("  export LC_NUMERIC=C\n");
      sb.append("  export LC_ALL=C\n");
      sb.append("Then restart FragPipe from the same terminal.");
    }
    return sb.toString();
  }

  private static String detectWindowsDecimalSeparator() throws Exception {
    ProcessBuilder pb = new ProcessBuilder("reg", "query", "HKCU\\Control Panel\\International", "/v", "sDecimal");
    List<String> lines = ProcessUtils.captureOutputLines(pb);
    for (String line : lines) {
      String trimmed = line.trim();
      if (trimmed.startsWith("sDecimal")) {
        // Output format: "    sDecimal    REG_SZ    ."
        String[] parts = trimmed.split("\\s+", 3);
        if (parts.length >= 3) {
          return parts[2].trim();
        }
      }
    }
    return null;
  }

  private static String detectUnixDecimalSeparator() throws Exception {
    ProcessBuilder pb = new ProcessBuilder("locale", "-k", "LC_NUMERIC");
    List<String> lines = ProcessUtils.captureOutputLines(pb);
    for (String line : lines) {
      if (line.startsWith("decimal_point=")) {
        String value = line.substring("decimal_point=".length()).trim();
        if (value.length() >= 2 && value.startsWith("\"") && value.endsWith("\"")) {
          value = value.substring(1, value.length() - 1);
        }
        return value;
      }
    }
    return null;
  }
}
