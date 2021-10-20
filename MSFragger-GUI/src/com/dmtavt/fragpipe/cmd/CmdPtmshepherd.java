package com.dmtavt.fragpipe.cmd;


import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.ptmshepherd.PtmshepherdParams;
import com.github.chhh.utils.PathUtils;
import com.github.chhh.utils.SwingUtils;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CmdPtmshepherd extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPtmshepherd.class);
  public static final String NAME = "PTMShepherd";
  public static final String CONFIG_FN = "shepherd.config";
  public static final String JAR_SHEPHERD_NAME = "ptmshepherd-1.2.5.jar";
  /** Fully qualified name, such as one you'd use for `java -cp my.jar com.example.MyClass`. */
  public static final String JAR_SHEPHERD_MAIN_CLASS = "edu.umich.andykong.ptmshepherd.PTMShepherd";
  public static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR, "commons-math3-3.6.1.jar"};
  public static final String FN_CAPTURE_STDOUT = "ptm-shepherd.log";
  public static final String FN_CAPTURE_STDERR = "ptm-shepherd.log";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");
  private static final String THERMO_RAW_EXT = "RAW";

  public CmdPtmshepherd(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml, List<String> supportedExts) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, supportedExts);
    if (!notSupportedExts.isEmpty()) {
      JOptionPane.showMessageDialog(comp, String.format(
          "<html>%s doesn't support '.%s' files.<br/>"
              + "Either remove them from input or disable %s<br/>"
              + "You can convert files using <i>msconvert</i> from ProteoWizard.", NAME, String.join(", ", notSupportedExts), NAME),
          NAME + " error", JOptionPane.WARNING_MESSAGE);
      return false;
    }
    return true;
  }

  public boolean configure(Component comp, boolean isDryRun, Path binFragger, int ramGb,
      Path db, Map<LcmsFileGroup, Path> mapGroupsToProtxml, Map<String, String> additionalProps) {

    initPreConfig();

    final Path extLibsThermo = CmdMsfragger.searchExtLibsThermo(Collections.singletonList(binFragger.getParent()));
    ArrayList<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
    if (extLibsThermo != null) {
      sup.add(THERMO_RAW_EXT);
    }

    final long numGroups = mapGroupsToProtxml.keySet().stream()
        .map(group -> group.name).distinct().count();
    pbis.clear();
    Set<Path> groupWds = mapGroupsToProtxml.keySet().stream().map(g -> g.outputDir(wd))
        .collect(Collectors.toSet());

    // check that each group only has lcms files in one directory
    for (LcmsFileGroup g : mapGroupsToProtxml.keySet()) {
      List<Path> lcmsPathsForGroup = g.lcmsFiles.stream().map(inputLcmsFile -> inputLcmsFile
          .getPath().getParent())
          .distinct().collect(Collectors.toList());
      if (lcmsPathsForGroup.size() != 1) {
        String msg = "PTM Shepherd requires all LCMS files in a group/experiment to be in one directory.\n<br/><br/>"
            + "<b>Check 'Workflows' tab, 'Input LCMS files' section.</b>";
        SwingUtils.showDialog(comp, SwingUtils.createClickableHtml(msg), NAME + " configuration error", JOptionPane.WARNING_MESSAGE);
        log.error(msg);
        return false;
      }
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_SHEPHERD_NAME).concat(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    PtmshepherdParams params = new PtmshepherdParams(wd, db, mapGroupsToProtxml, additionalProps);
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

    if (!isDryRun) {
      log.debug("Writing {} config to file: {}", NAME, pathConfig.toString());
      try {
        Files.deleteIfExists(pathConfig);
      } catch (IOException e) {
        SwingUtils.showDialog(comp, SwingUtils.createClickableHtml("Could not delete existing config file:<br/>\n"
                + pathConfig.toString()), NAME + " configuration error",
                JOptionPane.WARNING_MESSAGE);
        return false;
      }
      try (BufferedWriter bw = Files.newBufferedWriter(pathConfig, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
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
    cmd.add(Fragpipe.getBinJava());
    if (ramGb > 0) {
      cmd.add("-Xmx" + ramGb + "G");
    }
    if (extLibsThermo != null) {
      cmd.add(createJavaDParamString("batmass.io.libs.thermo.dir", extLibsThermo.toString()));
    }
    cmd.add("-cp");
    cmd.add(constructClasspathString(classpathJars));
    cmd.add(JAR_SHEPHERD_MAIN_CLASS);
    cmd.add(PathUtils.quotePath(pathConfig.toString(), false));
    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }
}
