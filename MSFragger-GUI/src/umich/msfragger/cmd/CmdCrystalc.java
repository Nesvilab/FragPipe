package umich.msfragger.cmd;

import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import umich.msfragger.gui.FraggerPanel;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.ToolingUtils;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.crystalc.CrystalcProps;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.StringUtils;

public class CmdCrystalc extends CmdBase {

  public CmdCrystalc(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  private String getModifiedPepxmlFn(String pepxmlFn, String pepxmlExtFragger) {
    return StringUtils.upToLastDot(pepxmlFn) + "_c." + StringUtils.afterLastDot(pepxmlFn);
  }

  public boolean configure(Component comp,
      FraggerPanel fp, boolean isDryRun,
      CrystalcParams ccParams, String fastaPath, Map<InputLcmsFile, Path> pepxmlFiles) {

    Path jarCystalc;
    Path jarDeps;
    try {
      // common deps
      jarDeps = JarUtils
          .unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_COMMON_DEPS,
              ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);
      // msadjuster jar
      jarCystalc = JarUtils.unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_CRYSTALC_NAME,
          ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);

    } catch (IOException | NullPointerException ex) {
      JOptionPane.showMessageDialog(comp,
          "Could not unpack tools to a temporary directory.\n"
              + "Disable Crystal-C.",
          "Can't unpack", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    final Set<String> lcmsExts = pepxmlFiles.keySet().stream()
        .map(f -> StringUtils.afterLastDot(f.path.getFileName().toString()))
        .collect(Collectors.toSet());
    boolean anyMatch = lcmsExts.stream().map(String::toLowerCase)
        .anyMatch(ext -> !("mzml".equals(ext) || "mzxml".equals(ext)));
    if (lcmsExts.isEmpty() || anyMatch) {
      String foundExts = String.join(", ", lcmsExts);
      JOptionPane.showMessageDialog(comp,
          "Crystal-C only supports mzML and mzXML input files.\n" +
              "The following LCMS file extensions found: " + foundExts + ".\n"
              + "Disable Crystal-C.",
          "Unsupported by Crystal-C", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    final String pepxmlExt = fp.getOutputFileExt();
    if (!"pepxml".equals(pepxmlExt.toLowerCase())) {
      JOptionPane.showMessageDialog(comp,
          "Crystal-C only accepts pepXML file extension.\n"
              + "Switch to pepXML in MSFragger options or disable Crystal-C :\\",
          "Unsupported by Crystal-C", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    final int ramGb = fp.getRamGb();
    final String ccParamsFilePrefix = "crystalc";
    final String ccParamsFileSuffix = ".params";

    // multiple raw file extensions or multiple lcms file locaitons
    // issue a separate command for each pepxml file
    int index = -1;
    for (Map.Entry<InputLcmsFile, Path> kv : pepxmlFiles.entrySet()) {
      final InputLcmsFile lcms = kv.getKey();
      final String lcmsFn = lcms.path.getFileName().toString();
      final Path pepxml = kv.getValue();
      final String pepxmlFn = pepxml.getFileName().toString();

      CrystalcParams p;
      Path ccParamsPath = lcms.outputDir(wd).resolve(ccParamsFilePrefix + "-" + (++index) + "-" + pepxmlFn + ccParamsFileSuffix);
      try {
        p = ccParams;
        String ext = StringUtils.afterLastDot(lcmsFn);
        p.setRawDirectory(lcms.path.getParent().toString());
        p.setRawFileExt(ext);
        p.setOutputFolder(lcms.outputDir(wd).toString());
        p.setFasta(fastaPath);
        if (!isDryRun) {
          Files.deleteIfExists(ccParamsPath);
          p.save(Files.newOutputStream(ccParamsPath, StandardOpenOption.CREATE));
        }
      } catch (IOException ex) {
        JOptionPane.showMessageDialog(comp,
            "Could not create Crystal-C parameter file.",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }

      List<String> cmd = new ArrayList<>();
      cmd.add("java");
      if (ramGb > 0) {
        cmd.add("-Xmx" + ramGb + "G");
      }
      cmd.add("-cp");
      List<String> toJoin = new ArrayList<>();
      toJoin.add(jarDeps.toAbsolutePath().normalize().toString());
      toJoin.add(jarCystalc.toAbsolutePath().normalize().toString());
      final String sep = System.getProperties().getProperty("path.separator");
      cmd.add("\"" + org.apache.commons.lang3.StringUtils.join(toJoin, sep) + "\"");
      cmd.add(CrystalcProps.JAR_CRYSTALC_MAIN_CLASS);
      cmd.add(ccParamsPath.toString());
      cmd.add(pepxml.toString());

      pbs.add(new ProcessBuilder(cmd));
    }

    return true;
  }
}
