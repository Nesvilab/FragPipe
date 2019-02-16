package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
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
  public Map<InputLcmsFile, Path> outputs(Map<InputLcmsFile, Path> inputs, String pepxmlExt, boolean combine) {
    Map<InputLcmsFile, Path> m = new HashMap<>();
    for (Entry<InputLcmsFile, Path> e : inputs.entrySet()) {
      InputLcmsFile lcms = e.getKey();
      final Path pepxml = e.getValue();
      final String cleanFn = pepxml.getFileName().toString();
      final Path cleanDir = pepxml.getParent();

      Path interactXml;
      if (!combine) {
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
          throw new IllegalStateException(
              String.format("Could not identify the extension for file: %s", pepxml));
        }
        interactXml = cleanDir.resolve("interact-" + nameWithoutExt + "pep.xml").toAbsolutePath();
      } else {
        // --combine option for peptide prophet
        interactXml = Paths.get("interact.pep.xml");
      }

      m.put(lcms, interactXml);
    }
    return m;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String fastaPath, String decoyTag, String textPepProphCmd, boolean combine,
      Map<InputLcmsFile, Path> pepxmlFiles) {

    pbs.clear();
    PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
    peptideProphetParams.setCmdLineParams(textPepProphCmd);

    if (!combine) {
      for (Map.Entry<InputLcmsFile, Path> e : pepxmlFiles.entrySet()) {
        List<String> cmd = new ArrayList<>();
        cmd.add(usePhilosopher.useBin());
        cmd.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);

        addFreeCommandLineParams(peptideProphetParams, cmd);
        cmd.add("--decoy");
        cmd.add(decoyTag);
        cmd.add("--database");
        cmd.add(fastaPath);

        final Path pepxmlPath = e.getValue();
        cmd.add(pepxmlPath.getFileName().toString());
        ProcessBuilder pb = new ProcessBuilder(cmd);
        setupEnv(pepxmlPath.getParent(), pb);
        pbs.add(pb);
      }
    } else {
      // --combine specified
      Map<String, List<Entry<InputLcmsFile, Path>>> pepxmlByExp = pepxmlFiles.entrySet().stream()
          .collect(Collectors.groupingBy(kv -> kv.getKey().experiment));
      for (List<Entry<InputLcmsFile, Path>> exp : pepxmlByExp.values()) {
        // check that all pepxml files are in one folder
        List<Path> pepxmlDirs = exp.stream().map(e -> e.getValue().getParent()).distinct()
            .collect(Collectors.toList());
        if (pepxmlDirs.size() > 1) {
          String msg = String.format("When 'combine'd PeptideProphet processing requested all files "
              + "for each experiment must be located in the same folder. We found experiment: "
              + "%s with files from %d folders.", exp.get(0).getKey().experiment, pepxmlDirs.size());
          JOptionPane.showMessageDialog(comp, msg, "Experiment/Group files in different folders", JOptionPane.WARNING_MESSAGE);
          return false;
        }

        List<String> cmd = new ArrayList<>();
        cmd.add(usePhilosopher.useBin());
        cmd.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);

        addFreeCommandLineParams(peptideProphetParams, cmd);
        cmd.add("--decoy");
        cmd.add(decoyTag);
        cmd.add("--database");
        cmd.add(fastaPath);
        cmd.add("--combine");
        exp.stream().map(e -> e.getValue().getFileName())
            .forEach(pepxmlFn -> cmd.add(pepxmlFn.toString()));
        final ProcessBuilder pb = new ProcessBuilder(cmd);
        final Path pepxmlDir = pepxmlDirs.get(0);
        setupEnv(pepxmlDir, pb);
        pbs.add(pb);
      }
    }

    isConfigured = true;
    return true;
  }

  private void setupEnv(Path workdir, ProcessBuilder pb) {
    // set environment
    pb.directory(workdir.toFile());
    pb.environment().putIfAbsent("WEBSERVER_ROOT", "fake-WEBSERVER_ROOT-value");
  }

  private void addFreeCommandLineParams(PeptideProphetParams peptideProphetParams,
      List<String> cmd) {
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
  }

  @Override
  public int getPriority() {
    return 92;
  }
}
