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

package org.nesvilab.fragpipe.tools.umpire;

import org.nesvilab.utils.StringUtils;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class UmpireSeGarbageFiles {
  public static final List<String> filesToMoveSansLog = Collections.emptyList();
  public static final List<String> logFile = Collections.singletonList("diaumpire_se.log");
  public static final List<String> filesToMove = Stream.concat(logFile.stream(), filesToMoveSansLog.stream())
          .collect(Collectors.toList());
  public static final List<String> fileNameSuffixesToMoveMzML = Arrays.asList("_Q1.mzML", "_Q2.mzML", "_Q3.mzML");
  public static final List<String> fileNameSuffixesToMove = Arrays.asList(
      "_Peak", ".DIAWindowsFS", ".RTidxFS",
      ".ScanClusterMapping_Q1", ".ScanClusterMapping_Q2", ".ScanClusterMapping_Q3",
      ".ScanidxFS", ".ScanPosFS", ".ScanRTFS", "_diasetting.ser", "_params.ser",
      "_Q1.mgf", "_Q2.mgf", "_Q3.mgf");
  private UmpireSeGarbageFiles() {}

  public static List<Path> getGarbageFiles(Path lcmsFilePath, final boolean withLog, final boolean withMzML) {
    List<Path> toMove = new ArrayList<>();
    String fnLessExt = StringUtils.upToLastDot(lcmsFilePath.getFileName().toString());
    Path filePath = lcmsFilePath.toAbsolutePath().getParent();

    for (String fileToMove : withLog ? filesToMove : filesToMoveSansLog) {
      toMove.add(filePath.resolve(fileToMove));
    }

    for (String suffix : UmpireSeGarbageFiles.fileNameSuffixesToMove) {
      String filenameToMove = fnLessExt + suffix;
      toMove.add(filePath.resolve(filenameToMove));
    }

    if(withMzML)
      for (String suffix : UmpireSeGarbageFiles.fileNameSuffixesToMoveMzML)
        toMove.add(filePath.resolve(fnLessExt + suffix));

    return toMove;
  }
}
