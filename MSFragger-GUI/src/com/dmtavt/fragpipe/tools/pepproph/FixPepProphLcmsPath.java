package com.dmtavt.fragpipe.tools.pepproph;

import com.dmtavt.fragpipe.api.InputLcmsFile;
import java.nio.file.Path;
import org.apache.commons.lang3.NotImplementedException;

public class FixPepProphLcmsPath {

  /**
   * @param pepxmlPath Path to pepxml file after peptide prophet.
   * @param lcmsFile The original LCMS file used for search.
   * @param workDir The 'output directory' specified in FragPipe Run tab.
   */
  public static void fixPathInplace(Path pepxmlPath, InputLcmsFile lcmsFile, Path workDir) {
    // TODO: Fengchao offered to implement
    throw new NotImplementedException("Fengchao offered to implement");
  }
}
