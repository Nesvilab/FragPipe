package umich.msfragger.cmd;

import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.MsfraggerGuiFrame;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.crystalc.CrystalcProps;
import umich.msfragger.params.fragger.FraggerMigPanel;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.StringUtils;

public class CmdCrystalc extends CmdBase {

  public static final String NAME = "Crystal-C";

  public CmdCrystalc(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  /**
   * @param pepxmlExtFragger Need to provide the extension, because there can be multiple dots
   * in the extension, so can't guess what the extension is.
   */
  private String getModifiedPepxmlFn(String pepxmlFn, String pepxmlExtFragger) {

    //throw new NotImplementedException("TODO: this calc of filename doesn't work right, there are extra letters."); // TODO: Not implemented
    // example
    //[23:20:59.444] Executing command [PeptideProphet] from working dir: D:\Gygi_data\Subset\Test11\B
    //$> C:\Users\nesvi\Desktop\MSFragger-20180316\philosopher_windows_amd64.exe peptideprophet --decoy rev_ --nonparam --expectscore --decoyprobs --masswidth 1000.0 --clevel -2 --database C:\Users\nesvi\Desktop\AATC\DIA-full\2017-12-09-td-up000005640.fasta b1931_293T_proteinID_11A_QE3_122212.p_c.pepXML

    int lastIndexOf = pepxmlFn.toLowerCase().lastIndexOf(pepxmlExtFragger.toLowerCase());
    if (lastIndexOf < 0) {
      throw new IllegalArgumentException("Pepxml file name must end with the extension from Fragger config");
    }

    return pepxmlFn.substring(0, lastIndexOf - 1) + "_c." + StringUtils.afterLastDot(pepxmlFn);
  }

  /**
   * @param inputs Pepxml files after search engine, but before Peptide Prophet.
   */
  public Map<InputLcmsFile, Path> outputs(Map<InputLcmsFile, Path> inputs, String pepxmlExtFragger) {
    Map<InputLcmsFile, Path> m = new HashMap<>();
    for (Entry<InputLcmsFile, Path> e : inputs.entrySet()) {
      Path dir = e.getValue().getParent();
      String pepxmlFn = e.getValue().getFileName().toString();
      m.put(e.getKey(), dir.resolve(getModifiedPepxmlFn(pepxmlFn, pepxmlExtFragger)));
    }
    return m;
  }

  /**
   * @param ccParams Get these by calling {@link MsfraggerGuiFrame#crystalcFormToParams()}.
   */
  public boolean configure(Component comp,
      FraggerMigPanel fp, boolean isDryRun,
      CrystalcParams ccParams, String fastaPath, Map<InputLcmsFile, Path> pepxmlFiles) {
    pbs.clear();
    if (StringUtils.isNullOrWhitespace(fastaPath)) {
      JOptionPane.showMessageDialog(comp, "Fasta file [Crystal-C] path can't be empty.",
          "Warning", JOptionPane.WARNING_MESSAGE);
      return false;
    }

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
      final Path outDir = lcms.outputDir(wd);

      CrystalcParams ccp;
      Path ccParamsPath = lcms.outputDir(wd).resolve(ccParamsFilePrefix + "-" + (++index) + "-" + pepxmlFn + ccParamsFileSuffix);
      try {
        ccp = ccParams;
        String ext = StringUtils.afterLastDot(lcmsFn);
        ccp.setRawDirectory(lcms.path.getParent().toString());
        ccp.setRawFileExt(ext);
        ccp.setOutputFolder(outDir.toString());
        ccp.setFasta(fastaPath);
        if (!isDryRun) {
          Files.deleteIfExists(ccParamsPath);
          ccp.save(Files.newOutputStream(ccParamsPath, StandardOpenOption.CREATE));
        }
      } catch (IOException e) {
        JOptionPane.showMessageDialog(comp,
            "Could not create Crystal-C parameter file.\n" + e.getMessage(),
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
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(outDir.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }

  @Override
  public int getPriority() {
    return 80;
  }
}
