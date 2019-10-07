package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.util.OsUtils;

public class CmdImquant extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdImquant.class);

  public static final String NAME = "IMQuant";
  public static final String JAR_IMQUANT_NAME = "imquant-1.0.0.jazz";
  public static final String JAR_MSFTBX_NAME = "batmass-io-1.16.6.jazz";
  public static final String JAR_IMQUANT_MAIN_CLASS = "imquant.IMQuant";
  private static String[] JAR_DEPS = {JAR_MSFTBX_NAME};
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "mzXML");
  private static List<String> RESOURCE_LOCATIONS = new ArrayList<>();

  static {
    RESOURCE_LOCATIONS.add("ext/bruker/libtimsdata-2-4-4.so");
    RESOURCE_LOCATIONS.add("ext/bruker/timsdata-2-4-4.dll");
  }

  private static final String UNPACK_SUBDIR_IN_TEMP = "fragpipe";

  public CmdImquant(boolean isRun, Path workDir, String fileCaptureStdout,
      String fileCaptureStderr) {
    super(isRun, workDir, fileCaptureStdout, fileCaptureStderr);
  }

  public CmdImquant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }


  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, Path binFragger, int ramGb,
      Map<String, String> uiCompsRepresentation,
      Map<InputLcmsFile, Path> lcmsToFraggerPepxml,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

//    Usage:
//    java -jar IMQuant.jar <options> <.d/.mzML/.mzXML/.pepXML/_quant.csv files>
//        Options:
//        --mztol <float>        # MS1 tolerance in PPM. Default: 20.0
//        --imtol <float>        # 1/K0 tolerance. Default: 0.1
//        --plot 0/1             # Plot traced features or not. Default: 0
//        --psm <string>         # Path to Philosopher's psm.tsv. Optional.
//        --multidir <string>    # Output dir for the multi experimental result. Optional.


    final Path extLibsBruker = CmdMsfragger.searchExtLibsBruker(Collections.singletonList(binFragger.getParent()));
    ArrayList<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
    if (extLibsBruker != null) {
      sup.add("d");
    }
    if (!checkCompatibleFormats(comp, lcmsToFraggerPepxml, sup)) {
      return false;
    }

    List<String> jars = Stream.concat(Arrays.stream(JAR_DEPS), Stream.of(JAR_IMQUANT_NAME))
        .collect(Collectors.toList());
    final List<Path> unpacked = new ArrayList<>();
    if (!unpackJars(jars, unpacked, NAME)) {
      return false;
    }

    if (!unpackJars(RESOURCE_LOCATIONS, new ArrayList<>(), NAME)) { // copy timsdata library to temp directory
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add("java");
    if (ramGb > 0) {
      cmd.add("-Xmx" + ramGb + "G");
    }
    cmd.add("-cp");
    cmd.add(constructClasspathString(unpacked));
    cmd.add(JAR_IMQUANT_MAIN_CLASS);
    cmd.add("--mztol");
    cmd.add(getOrThrow(uiCompsRepresentation, "ui.imquant.mz-tol"));
    cmd.add("--imtol");
    cmd.add(getOrThrow(uiCompsRepresentation, "ui.imquant.im-tol"));
    cmd.add("--plot");
    cmd.add(getOrThrow(uiCompsRepresentation, "ui.imquant.is-plot").contentEquals("true") ? "1" : "0");

    for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      LcmsFileGroup group = e.getKey();
      Path psmTsv = group.outputDir(wd).resolve("psm.tsv");
      cmd.add("--psm");
      cmd.add(psmTsv.toString());
    }

    if (mapGroupsToProtxml.size() > 1) {
      cmd.add("--multidir");
      cmd.add(wd.toString());
    }

    for (Entry<InputLcmsFile, Path> e : lcmsToFraggerPepxml.entrySet()) {
      InputLcmsFile lcms = e.getKey();
      Path pepxml = e.getValue();
      cmd.add(lcms.path.toString());
      cmd.add(wd.relativize(pepxml).toString());
    }

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  private String getOrThrow(Map<String, String> m, String key) {
    String s = m.get(key);
    if (s == null)
      throw new IllegalStateException("Could not get key: " + key);
    return s;
  }

  public static String constructClasspathString(List<Path> jarDepsPaths, Path ... additionalJars) {
    List<String> toJoin = new ArrayList<>();
    final Function<Path, String> pathMapping = (Path p) -> p.toAbsolutePath().normalize().toString();
    toJoin.addAll(jarDepsPaths.stream().map(pathMapping).collect(Collectors.toList()));
    toJoin.addAll(Arrays.stream(additionalJars).map(pathMapping).collect(Collectors.toList()));
    final String sep = System.getProperties().getProperty("path.separator");
    final String classpath = org.apache.commons.lang3.StringUtils.join(toJoin, sep);
    return OsUtils.isWindows() ? "\"" + classpath + "\"" : classpath;
  }

  private boolean checkCompatibleFormats(Component comp,  Map<InputLcmsFile, Path> lcmsToPepxml, List<String> supportedFormats) {
    List<String> notSupportedExts = getNotSupportedExts1(lcmsToPepxml, supportedFormats);
    if (!notSupportedExts.isEmpty()) {
      StringBuilder sb = new StringBuilder();
      sb.append(String.format("<html>%s can't work with '.%s' files.<br/>", NAME, String.join(", ", notSupportedExts)));
      if (notSupportedExts.contains(".d") || notSupportedExts.contains("d")) {
        sb.append("Support for Bruker files requires 'ext' folder with 'bruker' sub-folder<br/>\n")
            .append("to be next to your MSFragger.jar. It is shipped with MSFragger.zip distribution.<br/>\n");
      }
      sb.append(String.format("Compatible formats are: %s<br/>", String.join(", ", supportedFormats)));
      sb.append(String.format("Either remove files from input or disable %s<br/>", NAME));
      sb.append("You can also convert files using <i>msconvert</i> from ProteoWizard.");

      JOptionPane.showMessageDialog(comp, sb.toString(), NAME + " error", JOptionPane.WARNING_MESSAGE);
      return false;
    }
    return true;
  }
}
