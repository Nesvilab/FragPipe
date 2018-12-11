package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.params.pepproph.PeptideProphetParams;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdPeptideProphet extends CmdBase {

  public static final String NAME = "PeptideProphet";

  public CmdPeptideProphet(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  /**
   * @param inputs Either pepxml files after search or after Crystal-C.
   */
  public Map<InputLcmsFile, Path> outputs(Map<InputLcmsFile, Path> inputs, String pepxmlExt) {
    Map<InputLcmsFile, Path> m = new HashMap<>();
    for (Entry<InputLcmsFile, Path> e : inputs.entrySet()) {
      InputLcmsFile lcms = e.getKey();
      final Path pepxml = e.getValue();
      final String cleanFn = pepxml.getFileName().toString();
      final Path cleanDir = pepxml.getParent();

      // getting rid of extension (done like that because of file extensions with
      // with multiple dots in them)
      String[] typicalExts = {pepxmlExt, "pep.xml", "pepxml"};
      String nameWithoutExt = null;
      for (String ext : typicalExts) {
        if (cleanFn.toLowerCase().endsWith(ext)) {
          int lastIndex = cleanFn.toLowerCase().lastIndexOf(ext);
          nameWithoutExt = cleanFn.substring(0, lastIndex);
          break;
        }
      }
      if (nameWithoutExt == null) {
        throw new IllegalStateException(String.format("Could not identify the extension for file: %s", pepxml));
      }

      Path interactXml = cleanDir.resolve("interact-" + nameWithoutExt + "pep.xml").toAbsolutePath();
      m.put(lcms, interactXml);
    }
    return m;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String fastaPath, String decoyTag, String textPepProphCmd, Map<InputLcmsFile, Path> pepxmlFiles) {

    pbs.clear();
    PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
    peptideProphetParams.setCmdLineParams(textPepProphCmd);

    for (Map.Entry<InputLcmsFile, Path> e : pepxmlFiles.entrySet()) {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin());
      cmd.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);

      if (!peptideProphetParams.getCmdLineParams().isEmpty()) {
        String cmdOpts = peptideProphetParams.getCmdLineParams();
        List<String> opts = StringUtils.splitCommandLine(cmdOpts);
        for (String opt : opts) {
          if (!opt.isEmpty()) {
            if (opt.equals(PhilosopherProps.CMD_PEPTIDE_PROPHET)) {
              continue;
            }
            cmd.add(opt);
          }
        }
      }
      cmd.add("--decoy");
      cmd.add(decoyTag);
      cmd.add("--database");
      cmd.add(fastaPath);

      final Path pepxml = e.getValue();
      cmd.add(pepxml.getFileName().toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      // set environment
      pb.directory(pepxml.getParent().toFile());
      pb.environment().putIfAbsent("WEBSERVER_ROOT", "fake-WEBSERVER_ROOT-value");
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
