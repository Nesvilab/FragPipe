package com.github.chhh.utils.swing;

import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.SwingUtils;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.stream.Stream;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

public class FileChooserUtils {

  public static final String MULTI_FILE_DELIMITER = "; ";

  private FileChooserUtils() {
  }

  public static JFileChooser create(String title, boolean multiSelection,
    FcMode selectionMode, FileFilter... filters) {
    return create(title, "Select", multiSelection, selectionMode, true,
        filters);
  }

  public static JFileChooser create(String title, String approveButton, boolean multiSelection,
      FcMode selectionMode, boolean isAcceptAllUsed, javax.swing.filechooser.FileFilter... filters) {
    JFileChooser fc = new JFileChooser();
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
  }

  /**
   * Tries to set file chooser directory to an existing path, bubbling up the file system
   * looking for existing locations.
   */
  public static void setPath(JFileChooser fc, Path path) {
    try {
      if (Files.exists(path)) {
        if (Files.isDirectory(path)) {
          fc.setCurrentDirectory(path.getParent().toFile());
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
