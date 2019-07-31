package umich.msfragger.params.ptmshepherd;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.util.StringUtils;

public class PtmshepherdParams {
  private static final Logger log = LoggerFactory.getLogger(PtmshepherdParams.class);
  public static final String PROP_DATABASE = "database";
  /**
   * Dataset string format is `dataset = (set-name) (path-to-psm.tsv) (path-to-mzml-folder)`
   */
  public static final String PROP_DATASET = "dataset";
  public static final String DEFAULT_DATASET_NAME = "default-ptmshepherd-dataset";

  private Path workDir;
  private Path db;
  private Map<LcmsFileGroup, Path> groups;

  public PtmshepherdParams(Path workDir, Path db, Map<LcmsFileGroup, Path> groups) {
    this.workDir = workDir;
    this.db = db;
    this.groups = groups;
  }

  public String createConfig() {
    StringBuilder sb = new StringBuilder();
    sb.append(PROP_DATABASE).append(" = ").append(db.toAbsolutePath().normalize().toString()).append("\n");
    for (Entry<LcmsFileGroup, Path> e : groups.entrySet()) {
      LcmsFileGroup g = e.getKey();
      List<Path> lcmsPathsForGroup = g.lcmsFiles.stream().map(inputLcmsFile -> inputLcmsFile.path.getParent())
          .distinct().collect(Collectors.toList());
      if (lcmsPathsForGroup.size() != 1) {
        String msg = "PTM Shepherd config only works when LCMS files in a group are in one directory.";
        log.error(msg);
        throw new IllegalArgumentException(msg);
      }
      Path lcmsFilesDir = lcmsPathsForGroup.get(0);
      Path p = e.getValue();
      Path psmTsv = g.outputDir(workDir).resolve("psm.tsv");
      final String datasetName = StringUtils.isNullOrWhitespace(g.name) ? DEFAULT_DATASET_NAME : g.name;
      sb.append(PROP_DATASET).append(" = ")
          .append(datasetName).append(" ")
          .append(psmTsv.toString()).append(" ")
          .append(lcmsFilesDir.toString()).append("\n");
    }
    return sb.toString();
  }
}
