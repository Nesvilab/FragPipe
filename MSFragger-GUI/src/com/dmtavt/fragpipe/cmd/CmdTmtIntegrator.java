package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiConfProps;
import com.github.chhh.utils.StringUtils;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.tmtintegrator.QuantLabelAnnotation;
import com.dmtavt.fragpipe.tools.tmtintegrator.TmtiPanel;
import com.github.chhh.utils.FileDelete;
import com.github.chhh.utils.SwingUtils;

public class CmdTmtIntegrator extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdTmtIntegrator.class);

  public static final String NAME = "TmtIntegrator";
  public static final String JAR_NAME = "tmt-integrator-3.2.1.jar";
  public static final String JAR_MAIN = "TMTIntegrator";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "raw");
  public static final String CONFIG_FN = "tmt-integrator-conf.yml";

  public CmdTmtIntegrator(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, SUPPORTED_FORMATS);
    if (!notSupportedExts.isEmpty()) {
      if (Fragpipe.headless) {
        log.error(String.format("%s can't work with '.%s' files. You can also convert files using msconvert from ProteoWizard.", NAME, String.join(", ", notSupportedExts)));
      } else {
        JOptionPane.showMessageDialog(comp, String.format(
                "<html>%s can't work with '.%s' files.<br/>"
                    + "Compatible formats are: %s<br/>"
                    + "Either remove files from input or disable %s<br/>"
                    + "You can also convert files using <i>msconvert</i> from ProteoWizard.",
                NAME, String.join(", ", notSupportedExts), String.join(", ", SUPPORTED_FORMATS), NAME),
            NAME + " error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }
    return true;
  }

  public boolean configure(TmtiPanel panel, boolean isDryRun,
      int ramGb, String pathFasta,
      Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    isConfigured = false;

    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Stream.of(JAR_NAME));
    if (classpathJars == null) {
      return false;
    }

//    // check Thermo compatibility
//    final List<String> sup = new ArrayList<>(SUPPORTED_FORMATS);
//    final Path extLibsThermo = CmdMsfragger.searchExtLibsThermo(Collections.singletonList(binFragger.getParent()));
//    if (extLibsThermo != null) {
//      sup.add("RAW");
//    }

    // see if input files match compatibility table
    if (!checkCompatibleFormats(panel, mapGroupsToProtxml)) {
      return false;
    }

    // write and check TMT-I config file
    Path pathConf;
    final String tmtOutDirName = "tmt-report";
    final Path outDir = getWd().resolve(tmtOutDirName);
    try {
      pathConf = wd.resolve(CONFIG_FN);
      Files.deleteIfExists(pathConf);
      if (!isDryRun) {
        FileDelete.deleteFileOrFolder(outDir);
        Files.createDirectories(outDir);
      }
      log.debug("Writing {} config file", NAME);
      if (!Files.exists(pathConf.getParent())) {
        log.debug(NAME + " config required presence of output work dir, creating: {}", pathConf.getParent());
        Files.createDirectories(pathConf.getParent());
      }
      Map<String, String> conf = panel.formToConfig(ramGb, classpathJars.get(0).toString(), pathFasta, outDir.toString());

      // check that there is a reference channel set in each annotation file
      String refTag = conf.get("ref_tag");
      if (StringUtils.isNullOrWhitespace(refTag)) {
        SwingUtils.showWarningDialog(panel, "Reference channel tag can't be left empty\n"
            + "in " + NAME + " config, unless selecting Virtual reference option.", NAME + " Config");
      }

      List<Path> filesWithoutRefChannel = new ArrayList<>();
      // only check for presence of reference channels if "Define Reference is set to "Reference Sample"
      if (TmtiConfProps.COMBO_ADD_REF_CHANNEL.equalsIgnoreCase(panel.getDefineReference())) {
        for (Path path : panel.getAnnotations().values()) {
          List<QuantLabelAnnotation> annotations = TmtiPanel
              .parseTmtAnnotationFile(path.toFile());
          if (annotations.stream().noneMatch(a -> a.getSample().contains(refTag))) {
            filesWithoutRefChannel.add(path);
          }
        }
      }
      if (!filesWithoutRefChannel.isEmpty()) {
        String files = filesWithoutRefChannel.stream().map(Path::toString)
            .collect(Collectors.joining("\n"));
        SwingUtils.showWarningDialog(panel, "Found annotation files without reference channel\n"
            + "specified:\n" + files
            + "\n\nOne sample name in each annotation file must start with\n"
            + "the reference tag you set. Currently it is set to: '" + refTag + "'.\n"
                + "You can change that in Quant tab, [Ref Sample Tag] text field.",

            NAME + " Config");
        return false;
      }


      try (BufferedWriter bw = Files.newBufferedWriter(pathConf, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
        panel.writeConfig(bw, conf);
      }
      List<String> cmdCheck = new ArrayList<>();
      cmdCheck.add(Fragpipe.getBinJava());
      cmdCheck.add("-cp");
      cmdCheck.add(constructClasspathString(classpathJars));
      cmdCheck.add(JAR_MAIN);
      cmdCheck.add(pathConf.toString());
      cmdCheck.add("--ValParam");
      ProcessBuilder pb = new ProcessBuilder(cmdCheck);
      pb.directory(wd.toFile());
      log.debug("Running TMT-I config file check: {}", String.join(", ", pb.command()));
      Process p = pb.start();
      int checkExitCode = p.waitFor();
      if (checkExitCode != 0) {
        if (Fragpipe.headless) {
          log.error("Invalid TMT Integrator config file, exit code " + checkExitCode);
        } else {
          JOptionPane.showMessageDialog(panel, "Invalid TMT Integrator config file, exit code " + checkExitCode, "TMT Integrator configuration", JOptionPane.ERROR_MESSAGE);
        }
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
    cmd.add(Fragpipe.getBinJava());
//    if (extLibsThermo != null) {
//      cmd.add("-Dbatmass.io.libs.thermo.dir=\"" + extLibsThermo.toString() + "\"");
//    }
    if (ramGb > 0) {
      cmd.add("-Xmx" + ramGb + "G");
    }
    cmd.add("-cp");
    cmd.add(constructClasspathString(classpathJars));
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
}
