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

import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.awt.Desktop;
import java.io.IOException;
import java.nio.file.Path;

public class FragpipeUtils {

  public static void openInExplorer(Component parent, String path) {
    if (StringUtils.isBlank(path)) {
      SwingUtils.showInfoDialog(parent, "Empty path", "Does not exist");
      return;
    }
    Path existing = PathUtils.existing(path);
    if (existing == null) {
      SwingUtils
          .showInfoDialog(parent, "Path:\n'" + path + "'\nDoes not exist", "Not exists");
      return;
    }
    try {
      Desktop.getDesktop().open(existing.toFile());
    } catch (IOException ex) {
      SwingUtils
          .showErrorDialog(parent, "Could not open path in system file browser.", "Error");
    }
  }
}
