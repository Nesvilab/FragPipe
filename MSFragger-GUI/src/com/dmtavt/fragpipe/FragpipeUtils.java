package com.dmtavt.fragpipe;

import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Component;
import java.awt.Desktop;
import java.io.IOException;
import java.nio.file.Path;

public class FragpipeUtils {

  public static void openInExplorer(Component parent, String path) {
    if (StringUtils.isBlank(path)) {
      SwingUtils.showInfoDialog(parent, "Empty path", "Not exists");
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
