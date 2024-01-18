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

import com.dmtavt.fragpipe.exceptions.UnexpectedException;
import com.dmtavt.fragpipe.exceptions.ValidationException;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.ProcessUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Skyline {

  private static final Logger log = LoggerFactory.getLogger(Skyline.class);
  private static final String DOWNLOAD_URL = "https://skyline.ms/project/home/software/Skyline/begin.view";

  public static void downloadSkylineManually() {
    try {
      Desktop.getDesktop().browse(URI.create(DOWNLOAD_URL));
    } catch (IOException ex) {
      log.error("Error opening browser to download Skyline", ex);
      SwingUtils.showErrorDialogWithStacktrace(ex, null);
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

    if (sbVer.length() == 0) {
      throw new ValidationException("Version string not found for Skyline from \"" + binPath + "\"");
    }

    return new Version(sbVer.toString(), false, DOWNLOAD_URL);
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
}
