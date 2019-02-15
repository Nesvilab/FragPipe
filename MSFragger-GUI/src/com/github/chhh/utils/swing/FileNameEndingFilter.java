package com.github.chhh.utils.swing;

import java.io.File;
import java.util.Arrays;
import java.util.Locale;
import javax.swing.filechooser.FileFilter;

public class FileNameEndingFilter extends FileFilter {
  private final String desc;
  private final String[] exts;
  private final String[] extsDotLoCase;

public FileNameEndingFilter(String desc, String... exts) {
    if (exts == null || exts.length == 0) {
      throw new IllegalArgumentException(
          "Extensions must be non-null and not empty");
    }
    this.desc = desc;
    this.exts = new String[exts.length];
    this.extsDotLoCase = new String[exts.length];
    for (int i = 0; i < exts.length; i++) {
      if (exts[i] == null || exts[i].length() == 0) {
        throw new IllegalArgumentException(
            "Each extension must be non-null and not empty");
      }
      this.exts[i] = exts[i];
      this.extsDotLoCase[i] = exts[i].toLowerCase(Locale.ENGLISH);
      if (!this.extsDotLoCase[i].startsWith(".")) {
        this.extsDotLoCase[i] = "." + this.extsDotLoCase[i];
      }
    }
  }

  public boolean accept(File f) {
    if (f != null) {
      if (f.isDirectory()) {
        return true;
      }
      final String fileName = f.getName().toLowerCase(Locale.ENGLISH);
      for (String ext : extsDotLoCase) {
        if (fileName.endsWith(ext)) {
          return true;
        }
      }
    }
    return false;
  }

  public String getDescription() {
    return desc;
  }

  public String[] getExts() {
    String[] result = new String[exts.length];
    System.arraycopy(exts, 0, result, 0, exts.length);
    return result;
  }

  public String toString() {
    return super.toString() + "[description=" + getDescription() +
        " exts=" + Arrays.asList(getExts()) + "]";
  }

}
