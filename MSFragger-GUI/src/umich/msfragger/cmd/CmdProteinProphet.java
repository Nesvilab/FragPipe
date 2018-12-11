package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import umich.msfragger.gui.FraggerPanel;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.protproph.ProteinProphetParams;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdProteinProphet extends CmdBase {

  public static final String NAME = "ProteinProphet";

  private static final String INTERACT_FN = "interact.prot.xml";

  public CmdProteinProphet(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  /**
   * @return Mapping from Experiment/Group name to interact.prot.xml file location.
   */
  public Map<LcmsFileGroup, Path> outputs(Map<InputLcmsFile, Path> pepxmlFiles,
      boolean isProcessGroupsSeparately) {

    Map<String, List<InputLcmsFile>> lcmsByExp = pepxmlFiles.keySet().stream()
        .collect(Collectors.groupingBy(f -> f.experiment));


    Map<LcmsFileGroup, Path> m = new HashMap<>();

    for (Entry<String, List<InputLcmsFile>> e : lcmsByExp.entrySet()) {
      final String groupName = e.getKey();
      final List<InputLcmsFile> lcmsFiles = e.getValue();
      if (lcmsFiles.isEmpty()) {
        throw new IllegalStateException("Empty LCMS file list. This is a bug. Report to "
            + "developers.");
      }
      LcmsFileGroup group = new LcmsFileGroup(groupName, lcmsFiles);
      if (isProcessGroupsSeparately) {
        m.put(group, lcmsFiles.get(0).outputDir(wd).resolve(INTERACT_FN));
      } else {
        m.put(group, wd.resolve(INTERACT_FN));
      }
    }

    if (!isProcessGroupsSeparately) {
      Set<Path> interactProtXmls = new HashSet<>(m.values());
      if (interactProtXmls.size() > 1) {
        throw new IllegalStateException("During combined processing of Experiments/Groups "
            + "only one interact.prot.xml file should be produced. This is probably a bug, report "
            + "to developers.");
      }
    }

    return m;
  }

  public boolean configure(Component comp, FraggerPanel fp, UsageTrigger usePhilosopher,
      String txtProteinProphetCmdLineOpts,
      boolean isProcessGroupsSeparately, Map<InputLcmsFile, Path> pepxmlFiles) {

    pbs.clear();
    ProteinProphetParams proteinProphetParams = new ProteinProphetParams();
    proteinProphetParams.setCmdLineParams(txtProteinProphetCmdLineOpts);

    Map<LcmsFileGroup, Path> groupToProtxml = outputs(pepxmlFiles, isProcessGroupsSeparately);

    if (isProcessGroupsSeparately) {
      for (Entry<LcmsFileGroup, Path> e : groupToProtxml.entrySet()) {
        LcmsFileGroup group = e.getKey();
        Path protxml = e.getValue();
        List<String> pepxmlsPaths = pepxmlFiles.entrySet().stream()
            .filter(pepxml -> pepxml.getKey().experiment.equals(group.name))
            .map(pepxml -> pepxml.getValue().getFileName().toString())
            .collect(Collectors.toList());
        List<String> cmd = createCmdStub(usePhilosopher, protxml.getParent(), proteinProphetParams);
        cmd.addAll(pepxmlsPaths);
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(protxml.getParent().toFile());
        pbs.add(pb);
      }

      // END: isProcessGroupsSeparately
    } else {

      Set<Path> interactProtXmls = new HashSet<>(groupToProtxml.values());
      if (interactProtXmls.size() > 1) {
        JOptionPane.showMessageDialog(comp, "[Protein Prophet]\n"
            + "Report to developers, more than one interact protxml file when\n"
            + "processing experimental groups together.");
        return false;
      }
      Path protxml = interactProtXmls.iterator().next();
      if (!protxml.getParent().equals(wd)) {
        throw new IllegalStateException("Protxml not in global output directory when groups processed together.");
      }
      List<String> pepxmlsPaths = pepxmlFiles.entrySet().stream()
          .map(pepxml -> pepxml.getValue().toString())
          .collect(Collectors.toList());
      List<String> cmd = createCmdStub(usePhilosopher, protxml.getParent(), proteinProphetParams);
      cmd.addAll(pepxmlsPaths);
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(protxml.getParent().toFile());
      pbs.add(pb);

      // END: !isProcessGroupsSeparately
    }


    // by this point each process builder should have its working dir set
    for (ProcessBuilder pb : pbs) {
      Map<String, String> env = pb.environment();

      // add this variable so that TPP didn't try to use webserver stuff
      String ENV_XML_ONLY = "XML_ONLY";
      env.put(ENV_XML_ONLY, "1");

      // collect variables from system
      StringBuilder pathEnv = new StringBuilder();
      Set<String> mergedKeys = new HashSet<>();
      Set<String> envKeys = env.keySet();
      for (String key : envKeys) {
        if (key.toLowerCase().equals("path")) {
          String pathVal = env.get(key);
          pathVal = pathVal.trim();
          pathEnv.append(pathVal);
          if (!pathVal.endsWith(";")) {
            pathEnv.append(";");
          }
          mergedKeys.add(key);
        }
      }
      for (String key : mergedKeys) {
        env.remove(key);
      }

      String ENV_PATH = "PATH";
      final String bin = usePhilosopher.getBin();
      Path binPath = Paths.get(bin);
      String binDir = null;
      if (binPath.isAbsolute()) {
        // the path to the executable was specified as absolute, other needed files must be there as well
        binDir = binPath.toAbsolutePath().getParent().toString();
      } else if (Files.exists(binPath)) {
        binDir = binPath.toAbsolutePath().getParent().toString();
      } else {
        binPath = wd.resolve(bin);
        if (Files.exists(binPath)) {
          binDir = binPath.toAbsolutePath().getParent().toString();
        }
      }
      if (binDir != null) {
        pathEnv.append(";").append(binDir);
      }
      String pathEnvValue = pathEnv.toString();
      env.put(ENV_PATH, pathEnvValue);
    }

    isConfigured = true;
    return true;
  }

  private List<String> createCmdStub(UsageTrigger usePhilosopher, Path protxmlDir,
      ProteinProphetParams proteinProphetParams) {
    List<String> cmd = new ArrayList<>();
    cmd.add(usePhilosopher.useBin(protxmlDir));
    cmd.add(PhilosopherProps.CMD_PROTEIN_PROPHET);

    // for Philosopher command line flags go before files
    String cmdLineOpts = proteinProphetParams.getCmdLineParams().trim();
    if (!StringUtils.isNullOrWhitespace(cmdLineOpts)) {
      List<String> opts = StringUtils.splitCommandLine(cmdLineOpts);
      cmd.addAll(opts);
    }
    return cmd;
  }

  @Override
  public int getPriority() {
    return 96;
  }
}
