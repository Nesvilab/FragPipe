package umich.msfragger.cmd;

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
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.tmtintegrator.TmtiPanel;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdTmtIntegrator extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdTmtIntegrator.class);

  public static final String NAME = "TmtIntegrator";
  public static final String JAR_NAME = "tmt-integrator-1.1.4.jazz";
  public static final String JAR_MAIN = "TMTIntegrator";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML");
  public static final String CONFIG_FN = "tmt-integrator-conf.yaml";

  public CmdTmtIntegrator(boolean isRun, Path workDir, String fileCaptureStdout,
      String fileCaptureStderr) {
    super(isRun, workDir, fileCaptureStdout, fileCaptureStderr);
  }

  public CmdTmtIntegrator(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml, List<String> supportedFormats) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, supportedFormats);
    if (!notSupportedExts.isEmpty()) {
      JOptionPane.showMessageDialog(comp, String.format(
          "<html>%s can't work with '.%s' files.<br/>"
              + "Compatible formats are: %s<br/>"
              + "Either remove files from input or disable %s<br/>"
              + "You can also convert files using <i>msconvert</i> from ProteoWizard.",
          NAME, String.join(", ", notSupportedExts), String.join(", ", supportedFormats), NAME),
          NAME + " error", JOptionPane.WARNING_MESSAGE);
      return false;
    }
    return true;
  }

  public boolean configure(TmtiPanel panel, Path binFragger, UsageTrigger phi,
      int ramGb, String pathFasta,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    isConfigured = false;

    // unpack
    List<String> jars = Stream.of(JAR_NAME).collect(Collectors.toList());
    final List<Path> unpacked = new ArrayList<>();
    if (!unpackJars(jars, unpacked, NAME)) {
      return false;
    }
    Path jarTmti = unpacked.get(0);

//    // check Thermo compatibility
//    final List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
//    final Path extLibsThermo = CmdMsfragger.searchExtLibsThermo(Collections.singletonList(binFragger.getParent()));
//    if (extLibsThermo != null) {
//      sup.add("RAW");
//    }

    // see if input files match compatibility table
    if (!checkCompatibleFormats(panel, mapGroupsToProtxml, SUPPORTED_FORMATS)) {
      return false;
    }

    // write and check TMT-I config file
    Path pathConf;
    try {
      pathConf = wd.resolve(CONFIG_FN);
      Files.deleteIfExists(pathConf);
      log.debug("Writing {} config file", NAME);
      try (BufferedWriter bw = Files.newBufferedWriter(pathConf, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
        panel.formToConfig(bw, ramGb, jarTmti.toString(), pathFasta.toString(), wd.toString());
      }
      List<String> cmdCheck = new ArrayList<>();
      cmdCheck.add("java");
      cmdCheck.add("-cp");
      cmdCheck.add(constructClasspathString(unpacked));
      cmdCheck.add(JAR_MAIN);
      cmdCheck.add(pathConf.toString());
      cmdCheck.add("--ValParam");
      ProcessBuilder pb = new ProcessBuilder(cmdCheck);
      pb.directory(wd.toFile());
      Process p = pb.start();
      int checkExitCode = p.waitFor();
      if (checkExitCode != 0) {
        JOptionPane.showMessageDialog(panel, "Invalid TMT Integrator config file, exit code " + checkExitCode,
            "TMT Integrator configuration", JOptionPane.ERROR_MESSAGE);
        return false;
      }
    } catch (IOException e) {
      log.error("Error while configuring TMT-Integrator", e);
      return false;
    } catch (InterruptedException e) {
      log.error("Error checking TMT Integrator written config file validity", e);
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add("java");
//    if (extLibsThermo != null) {
//      cmd.add("-Dbatmass.io.libs.thermo.dir=\"" + extLibsThermo.toString() + "\"");
//    }
    if (ramGb > 0) {
      cmd.add("-Xmx" + ramGb + "G");
    }
    cmd.add("-cp");
    cmd.add(constructClasspathString(unpacked));
    cmd.add(JAR_MAIN);
    cmd.add(pathConf.toString());
    mapGroupsToProtxml.keySet().forEach(g -> {
      cmd.add(g.outputDir(wd).resolve("psm.tsv").toString());
    });

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 200;
  }
}
