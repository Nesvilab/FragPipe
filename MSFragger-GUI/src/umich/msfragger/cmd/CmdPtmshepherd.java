package umich.msfragger.cmd;


import java.awt.Component;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import org.apache.commons.codec.Charsets;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcProps;
import umich.msfragger.params.ptmshepherd.PtmshepherdParams;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.SwingUtils;


public class CmdPtmshepherd extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPtmshepherd.class);
  public static final String NAME = "PTMShepherd";
  public static final String CONFIG_FN = "shepherd.config";
  public static final String JAR_SHEPHERD_NAME = "PTMShepherd-20180820_2.jazz";
  /** Fully qualified name, such as one you'd use for `java -cp my.jar com.example.MyClass`. */
  public static final String JAR_SHEPHERD_MAIN_CLASS = "edu.umich.andykong.ptmshepherd.PTMShepherd";
  public static final String JAR_COMMON_DEPS = "common-deps-1.0.3.jazz";
  public static final String FN_CAPTURE_STDOUT = "ptm-shepherd.log";
  public static final String FN_CAPTURE_STDERR = "ptm-shepherd.log";

  public CmdPtmshepherd(boolean isRun, Path workDir) {
    super(isRun, workDir, FN_CAPTURE_STDOUT, FN_CAPTURE_STDERR);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, boolean isDryRun, int ramGb,
      Path db, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

    final long numGroups = mapGroupsToProtxml.keySet().stream()
        .map(group -> group.name).distinct().count();
    pbs.clear();
    Set<Path> groupWds = mapGroupsToProtxml.keySet().stream().map(g -> g.outputDir(wd))
        .collect(Collectors.toSet());

    // check that each group only has lcms files in one directory
    for (LcmsFileGroup g : mapGroupsToProtxml.keySet()) {
      List<Path> lcmsPathsForGroup = g.lcmsFiles.stream().map(inputLcmsFile -> inputLcmsFile.path.getParent())
          .distinct().collect(Collectors.toList());
      if (lcmsPathsForGroup.size() != 1) {
        String msg = "PTM Shepherd requires all LCMS files in a group/experiment to be in one directory.\n<br/><br/>"
            + "<b>Check 'Select LC/MS Files' tab.</b>";
        SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
        log.error(msg);
        return false;
      }
    }

    Path jarShepherd;
    Path jarDeps;
    try {
      // common deps
      jarDeps = JarUtils
          .unpackFromJar(ToolingUtils.class, "/" + JAR_COMMON_DEPS,
              ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);
      // Shepherd jar
      jarShepherd = JarUtils.unpackFromJar(ToolingUtils.class, "/" + JAR_SHEPHERD_NAME,
          ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);

    } catch (IOException | NullPointerException ex) {
      log.error("Could not unpack Shepherd to temp dir", ex);
      JOptionPane.showMessageDialog(comp,
          "Could not unpack tools to a temporary directory.\n"
              + "Disable Shepherd.", "Can't unpack", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    PtmshepherdParams params = new PtmshepherdParams(wd, db, mapGroupsToProtxml);
    String config;
    try {
      config = params.createConfig();
    } catch (Exception e) {
      String msg = "Could not configure PTM Shepherd.\n<br/><br/>Error message:" + e.getMessage();
      SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
      return false;
    }

    // write config file
    Path pathConfig = wd.resolve(CONFIG_FN);
    if (true || !isDryRun) {
      log.debug("Writing {} config to file: {}", NAME, pathConfig.toString());
      try (BufferedWriter bw = Files
          .newBufferedWriter(pathConfig, Charsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.WRITE,
              StandardOpenOption.TRUNCATE_EXISTING)) {
        bw.write(config);
        bw.flush();
      } catch (IOException e) {
        log.error("Error writing Shepherd config to file", e);
        String msg =
            "Error writing Shepherd config to file.\n<br/><br/>Error message:" + e.getMessage();
        SwingUtils
            .showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error",
                JOptionPane.WARNING_MESSAGE);
        return false;
      }
    }

    // builders
    List<String> cmd = new ArrayList<>();
    cmd.add("java");
    if (ramGb > 0) {
      cmd.add("-Xmx" + ramGb + "G");
    }
    cmd.add("-cp");
    List<String> toJoin = new ArrayList<>();
    // TODO: uncomment when we use small Shepherd jar without dependencies
    //toJoin.add(jarDeps.toAbsolutePath().normalize().toString());
    toJoin.add(jarShepherd.toAbsolutePath().normalize().toString());
    final String sep = System.getProperties().getProperty("path.separator");
    final String classpath = org.apache.commons.lang3.StringUtils.join(toJoin, sep);
    if (OsUtils.isWindows())
      cmd.add("\"" + classpath + "\"");
    else
      cmd.add(classpath);
    cmd.add(JAR_SHEPHERD_MAIN_CLASS);
    cmd.add(pathConfig.toString());
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbs.add(pb);

    isConfigured = true;
    return true;
  }
}
