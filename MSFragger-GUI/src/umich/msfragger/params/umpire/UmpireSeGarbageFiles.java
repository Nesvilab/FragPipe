package umich.msfragger.params.umpire;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import umich.msfragger.util.StringUtils;

public class UmpireSeGarbageFiles {
  public static final List<String> filesToMove = Arrays.asList("diaumpire_se.log");
  public static final List<String> fileNameSuffixesToMove = Arrays.asList(
      "_Peak", ".DIAWindowsFS", ".RTidxFS",
      ".ScanClusterMapping_Q1", ".ScanClusterMapping_Q2", ".ScanClusterMapping_Q3",
      ".ScanidxFS", ".ScanPosFS", ".ScanRTFS", "_diasetting.ser", "_params.ser",
      "_Q1.mgf", "_Q2.mgf", "_Q3.mgf");
  public List<String> toMove = new ArrayList<>();

  private UmpireSeGarbageFiles() {}

  public static UmpireSeGarbageFiles create(Path lcmsFilePath) {
    if (!lcmsFilePath.getFileName().toString().toLowerCase().endsWith(".mzxml"))
      throw new IllegalArgumentException("Can only accept file paths ending with .mzxml");

    UmpireSeGarbageFiles garbage = new UmpireSeGarbageFiles();
    String fnLessExt = StringUtils.upToLastDot(lcmsFilePath.getFileName().toString());
    Path filePath = lcmsFilePath.getParent();

    for (String fileToMove : UmpireSeGarbageFiles.filesToMove) {
      garbage.toMove.add(filePath.resolve(fileToMove).toString());
    }

    for (String suffix : UmpireSeGarbageFiles.fileNameSuffixesToMove) {
      String filenameToMove = fnLessExt + suffix;
      String file = filePath.resolve(filenameToMove).toString();
      garbage.toMove.add(file);
    }
    return garbage;
  }
}
