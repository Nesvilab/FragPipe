package umich.msfragger.cmd;

import java.awt.BorderLayout;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.protproph.ProteinProphetParams;
import umich.msfragger.util.FileListing;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdProteinProphet extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdProteinProphet.class);

  public static final String NAME = "ProteinProphet";

  private static final String INTERACT_FN = "combined.prot.xml";
  private static final String COMBINED_FN = "combined.prot.xml";

  public CmdProteinProphet(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  /**
   * @return Mapping from Experiment/Group name to interact.prot.xml file location.
   * 'interact' has been renamed to 'combined'.
   */
  public Map<LcmsFileGroup, Path> outputs(Map<InputLcmsFile, ArrayList<Path>> pepxmlFiles,
      boolean isProcessGroupsSeparately, boolean isMultiExperimentReport) {

    Map<String, List<InputLcmsFile>> lcmsByExp = pepxmlFiles.keySet().stream()
        .collect(Collectors.groupingBy(f -> f.getGroup()));

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
        String fn = isMultiExperimentReport ? COMBINED_FN : INTERACT_FN;
        m.put(group, wd.resolve(fn));
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

  private List<Path> findOldFilesForDeletion(List<Path> outputs) {
    Set<Path> outputDirs = outputs.stream().map(Path::getParent).collect(Collectors.toSet());
    final Pattern regex = Pattern.compile(".+?\\.prot\\.xml$", Pattern.CASE_INSENSITIVE);
    final List<Path> toDelete = new ArrayList<>();
    for (Path dir : outputDirs) {
      FileListing fl = new FileListing(dir, regex);
      fl.setRecursive(false);
      fl.setIncludeDirectories(false);
      toDelete.addAll(fl.findFiles());
    }
    return toDelete;
  }

  /**
   * Asks user confirmation before deleting the files.
   * Shows all the file paths to be deleted.
   */
  private boolean deleteFiles(Component comp, List<Path> forDeletion) {
    if (forDeletion == null || forDeletion.isEmpty())
      return true;

    String[][] data = new String[forDeletion.size()][1];
    int index = -1;
    for (Path path : forDeletion) {
      data[++index][0] = path.toString();
    }

    if (!forDeletion.isEmpty()) {
      DefaultTableModel model = new DefaultTableModel(data, new String[] {"To be deleted"});
      JTable table = new JTable(model);
      table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
      JPanel panel = new JPanel(new BorderLayout());
      panel.add(new JLabel("<html>Found " + forDeletion.size() + " old prot-xml files.<br/>"
          + "This might cause problems depending on the selected options.<br/>"
          + "It's recommended to delete the files first.<br/><br/>"
          + "<ul><li><b>Yes</b> - delete files now</li>"
          + "<li><b>No</b> - continue without deleting files</li>"
          + "<li><b>Cancel</b> - stop and don't run anything</li></ul>"
      ), BorderLayout.NORTH);
      panel.add(Box.createVerticalStrut(100), BorderLayout.CENTER);
      panel.add(new JScrollPane(table), BorderLayout.CENTER);

      String[] options = {"Yes - Delete now", "No - Continue as is", "Cancel"};
      int confirmation = JOptionPane
          .showOptionDialog(comp, panel, "Delete the files?",
              JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0]);
      switch (confirmation) {
        case 0:
          for (Path path : forDeletion) {
            try {
              Files.deleteIfExists(path);
            } catch (IOException e) {
              log.error("Error while trying to delete old files: {}", e.getMessage());
              throw new IllegalStateException(e);
            }
          }
          return true;
        case 1:
          return true;
        case 2:
          return false;
      }
    }
    return false;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String txtProteinProphetCmdLineOpts, boolean isMultiExperiment,
      boolean isProcessGroupsSeparately, Map<InputLcmsFile, ArrayList<Path>> pepxmlFiles) {

    pbis.clear();

    // check for existence of old files
    final Map<LcmsFileGroup, Path> outputs = outputs(pepxmlFiles, isProcessGroupsSeparately, isMultiExperiment);
    final List<Path> oldFilesForDeletion = findOldFilesForDeletion(new ArrayList<>(outputs.values()));
    if (!deleteFiles(comp, oldFilesForDeletion)) {
      return false;
    }

    ProteinProphetParams proteinProphetParams = new ProteinProphetParams();
    proteinProphetParams.setCmdLineParams(txtProteinProphetCmdLineOpts);

    Map<LcmsFileGroup, Path> groupToProtxml = outputs(pepxmlFiles, isProcessGroupsSeparately, isMultiExperiment);

    if (isProcessGroupsSeparately) {
      for (Entry<LcmsFileGroup, Path> e : groupToProtxml.entrySet()) {
        LcmsFileGroup group = e.getKey();
        Path protxml = e.getValue();
        List<String> pepxmlFns = pepxmlFiles.entrySet().stream()
            .filter(pepxml -> pepxml.getKey().getGroup().equals(group.name))
            .flatMap(pepxml -> pepxml.getValue().stream()).map(Path::getFileName).map(Path::toString)
            .distinct()
            .collect(Collectors.toList());
        List<String> cmd = createCmdStub(usePhilosopher, protxml.getParent(), proteinProphetParams);
        cmd.addAll(pepxmlFns);
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(protxml.getParent().toFile());
        pbis.add(PbiBuilder.from(pb));
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
          .flatMap(pepxml -> pepxml.getValue().stream()).map(Path::toString)
          .distinct()
          .collect(Collectors.toList());
      List<String> cmd = createCmdStub(usePhilosopher, protxml.getParent(), proteinProphetParams);
      cmd.addAll(pepxmlsPaths);
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(protxml.getParent().toFile());
      pbis.add(PbiBuilder.from(pb));

      // END: !isProcessGroupsSeparately
    }


    // by this point each process builder should have its working dir set
    for (ProcessBuilderInfo pbi : pbis) {
      ProcessBuilder pb = pbi.pb;
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
    if (!cmd.contains("--output")) {
      cmd.addAll(Arrays.asList("--output", "combined"));
    }
    return cmd;
  }

  @Override
  public int getPriority() {
    return 96;
  }
}
