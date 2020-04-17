package com.dmtavt.fragpipe.params.umpire;

import com.github.chhh.utils.StringUtils;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class UmpireSeGarbageFiles {
  public static final List<String> filesToMove = Arrays.asList("diaumpire_se.log");
  public static final List<String> fileNameSuffixesToMove = Arrays.asList(
      "_Peak", ".DIAWindowsFS", ".RTidxFS",
      ".ScanClusterMapping_Q1", ".ScanClusterMapping_Q2", ".ScanClusterMapping_Q3",
      ".ScanidxFS", ".ScanPosFS", ".ScanRTFS", "_diasetting.ser", "_params.ser",
      "_Q1.mgf", "_Q2.mgf", "_Q3.mgf");
  private UmpireSeGarbageFiles() {}

  public static List<Path> getGarbageFiles(Path lcmsFilePath) {
    if (!lcmsFilePath.getFileName().toString().toLowerCase().endsWith(".mzxml"))
      throw new IllegalArgumentException("Can only accept file paths ending with .mzxml");

    List<Path> toMove = new ArrayList<>();
    String fnLessExt = StringUtils.upToLastDot(lcmsFilePath.getFileName().toString());
    Path filePath = lcmsFilePath.getParent();

    for (String fileToMove : UmpireSeGarbageFiles.filesToMove) {
      toMove.add(filePath.resolve(fileToMove));
    }

    for (String suffix : UmpireSeGarbageFiles.fileNameSuffixesToMove) {
      String filenameToMove = fnLessExt + suffix;
      toMove.add(filePath.resolve(filenameToMove));
    }
    return toMove;
  }
}
