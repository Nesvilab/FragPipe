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

package org.nesvilab.utils.swing;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;
import org.nesvilab.utils.PathUtils;
import org.nesvilab.utils.SwingUtils;

public class FileChooserUtils {

  public static final String MULTI_FILE_DELIMITER = "; ";

  private FileChooserUtils() {
  }

  public static JFileChooser create(String title, boolean multiSelection,
    FcMode selectionMode, FileFilter... filters) {
    return create(title, "Select", multiSelection, selectionMode, true,
        filters);
  }

  public static Builder builder(String title) {
    return new Builder(title);
  }

  public static class Builder {
    String title;
    String approveButton = "Select";
    boolean multiSelection = false;
    FcMode selectionMode = FcMode.ANY;
    boolean isAcceptAllUsed = true;
    List<FileFilter> filters = Collections.emptyList();
    Stream<String> possiblePaths = null;

    public Builder(String title) {
      this.title = title;
    }

    public JFileChooser create() {
      JFileChooser fc = FileChooserUtils
          .create(title, approveButton, multiSelection, selectionMode, isAcceptAllUsed,
              filters.toArray(new FileFilter[0]));
      if (possiblePaths != null) {
        FileChooserUtils.setPath(fc, possiblePaths);
      }
      return fc;
    }

    public Builder title(String title) {
      this.title = title;
      return this;
    }

    public Builder approveButton(String approveButton) {
      this.approveButton = approveButton;
      return this;
    }

    public Builder multi(boolean multiSelection) {
      this.multiSelection = multiSelection;
      return this;
    }

    public Builder mode(FcMode selectionMode) {
      this.selectionMode = selectionMode;
      return this;
    }

    public Builder acceptAll(boolean acceptAllUsed) {
      isAcceptAllUsed = acceptAllUsed;
      return this;
    }

    public Builder filters(List<FileFilter> filters) {
      this.filters = filters;
      return this;
    }

    public Builder paths(Stream<String> possiblePaths) {
      this.possiblePaths = possiblePaths;
      return this;
    }
  }

  public static JFileChooser create(String title, String approveButton, boolean multiSelection,
                                    FcMode selectionMode, boolean isAcceptAllUsed, javax.swing.filechooser.FileFilter... filters) {
    return create(null, title, approveButton, multiSelection, selectionMode, isAcceptAllUsed, filters);
  }

  public static JFileChooser create(JFileChooser fc, String title, String approveButton, boolean multiSelection,
                                    FcMode selectionMode, boolean isAcceptAllUsed, javax.swing.filechooser.FileFilter... filters) {
    if (fc == null)
      fc = new JFileChooser();
    fc.setDialogTitle(title);
    fc.setApproveButtonText(approveButton);
    fc.setMultiSelectionEnabled(multiSelection);
    fc.setFileSelectionMode(selectionMode.fileChooserConstant);
    fc.setAcceptAllFileFilterUsed(isAcceptAllUsed);
    for (javax.swing.filechooser.FileFilter filter : filters) {
      fc.addChoosableFileFilter(filter);
    }
    return fc;
  }

  /**
   * Sets current direcotry of a file chooser to the first non-null, non-empty and existing
   * path.
   */
  public static void setPath(JFileChooser fc, Stream<String> possiblePaths) {
    File f = possiblePaths
        .map(PathUtils::existing)
        .filter(Objects::nonNull)
        .map(Path::toFile)
        .findFirst().orElse(null);
    fc.setCurrentDirectory(f);
    if (f != null && f.isFile()) {
      fc.setSelectedFile(f);
    }
  }

  /**
   * Tries to set file chooser directory to an existing path, bubbling up the file system
   * looking for existing locations.
   */
  public static void setPath(JFileChooser fc, Path path) {
    try {
      if (Files.exists(path)) {
        if (Files.isDirectory(path)) {
          fc.setCurrentDirectory(path.toAbsolutePath().getParent().toFile());
          fc.setSelectedFile(path.toFile());
        } else { // Files.exists(path) && !Files.isDirectory(path)
          fc.setCurrentDirectory(path.toFile());
        }
      } else { // !Files.exists(path)
        Path existing = SwingUtils.findExistingUpstreamPath(path);
        fc.setCurrentDirectory(existing == null ? null : existing.toFile());
      }
    } catch (Exception ignored) {
      fc.setCurrentDirectory(null);
    }
  }

  public static void setPath(JFileChooser fc, String path) {
    try {
      Path p = Paths.get(path);
      setPath(fc, p);
    } catch (Exception ignored) {
      fc.setCurrentDirectory(null);
    }
  }

  public enum FcMode {
    FILES_ONLY(JFileChooser.FILES_ONLY),
    DIRS_ONLY(JFileChooser.DIRECTORIES_ONLY),
    ANY(JFileChooser.FILES_AND_DIRECTORIES);
    public final int fileChooserConstant;

    FcMode(int fileChooserConstant) {
      this.fileChooserConstant = fileChooserConstant;
    }
  }
}
