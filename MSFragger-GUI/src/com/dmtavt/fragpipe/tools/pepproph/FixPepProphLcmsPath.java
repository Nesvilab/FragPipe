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

package com.dmtavt.fragpipe.tools.pepproph;

import com.dmtavt.fragpipe.api.InputLcmsFile;
import java.nio.file.Path;
import org.apache.commons.lang3.NotImplementedException;

public class FixPepProphLcmsPath {

  /**
   * @param pepxmlPath Path to pepxml file after PeptideProphet.
   * @param lcmsFile The original LCMS file used for search.
   * @param workDir The 'output directory' specified in FragPipe Run tab.
   */
  public static void fixPathInplace(Path pepxmlPath, InputLcmsFile lcmsFile, Path workDir) {
    // TODO: Fengchao offered to implement
    throw new NotImplementedException("Fengchao offered to implement");
  }
}
