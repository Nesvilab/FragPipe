package umich.msfragger.cmd;

import java.awt.BorderLayout;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
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
import umich.msfragger.gui.ProcessManager;
import umich.msfragger.params.pepproph.PeptideProphetParams;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.util.FileDelete;
import umich.msfragger.util.FileListing;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdPeptideProphet extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPeptideProphet.class);

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
  public Map<InputLcmsFile, List<Path>> outputs(Map<InputLcmsFile, List<Path>> inputs, String pepxmlExt, boolean combine) {
    Map<InputLcmsFile, List<Path>> m = new HashMap<>();
    for (Entry<InputLcmsFile, List<Path>> e : inputs.entrySet()) {
      InputLcmsFile lcms = e.getKey();
      for (Path pepxml : e.getValue()) {
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
          // --combine option for peptide prophet means there's a single interact.pep.xml for each experiment/group
          interactXml = cleanDir.resolve(Paths.get("interact.pep.xml"));
        }

        List<Path> t = m.get(lcms);
        if (t == null) {
          t = new ArrayList<>(e.getValue().size());
          t.add(interactXml);
          m.put(lcms, t);
        } else {
          t.add(interactXml);
        }
      }
    }
    return m;
  }

  private List<Path> findOldFilesForDeletion(Map<InputLcmsFile, List<Path>> outputs) {
//    final Set<Path> outputPaths = pepxmlFiles.keySet().stream()
//        .map(f -> f.outputDir(wd)).collect(Collectors.toSet());
    final Set<Path> outputPaths = outputs.values().stream().flatMap(List::stream)
        .map(Path::getParent).collect(Collectors.toSet());
    final Pattern pepxmlRegex = Pattern.compile(".+?\\.pep\\.xml$", Pattern.CASE_INSENSITIVE);
    final List<Path> pepxmlsToDelete = new ArrayList<>();
    for (Path outputPath : outputPaths) {
      FileListing fl = new FileListing(outputPath, pepxmlRegex);
      fl.setRecursive(false);
      fl.setIncludeDirectories(false);
      pepxmlsToDelete.addAll(fl.findFiles());
    }
    return pepxmlsToDelete;
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
      panel.add(new JLabel("<html>Found " + forDeletion.size() + " old pep-xml files.<br/>"
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

  /**
   * @param pepxmlFiles Either pepxml files after search or after Crystal-C.
   */
  public boolean configure(Component comp, UsageTrigger phi, Path jarFragpipe, boolean isDryRun,
      String fastaPath, String decoyTag, String textPepProphCmd, boolean combine, String enzymeName,
      Map<InputLcmsFile, List<Path>> pepxmlFiles) {

    isConfigured = false;
    pbis.clear();

    final boolean cmdLineContainsCombine = textPepProphCmd.toLowerCase().contains("--combine");
    if (cmdLineContainsCombine && !combine) {
      // command line contained '--combine', but the checkbox was not checked.
      combine = true;
      String msg = String.format(
          "<html>PeptideProphet command line options text field contained '--combine' flag,<br/>"
          + "however the checkbox to combine pepxml files wasn't selected.<br/><br/>"
          + "This is just an information message to bring that to your attention.<br/><br/>"
          + "PeptideProphet will be launched as if the 'combine pepxmls' checkbox was selected.");
      JOptionPane.showMessageDialog(comp, msg, "Inconsistent options for PeptideProphet", JOptionPane.INFORMATION_MESSAGE);
    }
    combine = combine || cmdLineContainsCombine;

    // check for existing pepxml files and delete them
    final Map<InputLcmsFile, List<Path>> outputs = outputs(pepxmlFiles, "pepxml", combine);
    final List<Path> forDeletion = findOldFilesForDeletion(outputs);
    if (!deleteFiles(comp, forDeletion)) {
      return false;
    }

    PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
    peptideProphetParams.setCmdLineParams(textPepProphCmd);

    Set<Path> workspacesToBeCleaned = new HashSet<>();

    if (!combine) {
      LinkedList<ProcessBuilderInfo> pbisPreParallel = new LinkedList<>();
      LinkedList<ProcessBuilderInfo> pbisParallel = new LinkedList<>();
      LinkedList<ProcessBuilderInfo> pbisPostParallel = new LinkedList<>();

      for (Map.Entry<InputLcmsFile, List<Path>> e : pepxmlFiles.entrySet()) {
        for (Path pepxmlPath : e.getValue()) {
          final Path pepxmlDir = pepxmlPath.getParent();
          final String pepxmlFn = pepxmlPath.getFileName().toString();


        // Needed for parallel Peptide Prophet

          // create temp dir to house philosopher's .meta directory, otherwise philosopher breaks
          Path temp = pepxmlDir.resolve("fragpipe-" + pepxmlFn + "-temp");
          if (!isDryRun) {
            try {
              if (Files.exists(temp)) {
                FileDelete.deleteFileOrFolder(temp);
              }
            } catch (IOException ex) {
              log.error("Could not delete old temporary directory for running peptide prophet in parallel", ex);
            }
            try {
              temp = Files.createDirectories(temp);
            } catch (FileAlreadyExistsException ignored) {
              log.error("Temp dir already exists, but we should have tried deleting it first. This is not critical.");
            } catch (IOException ex) {
              log.error("Could not create directory for parallel peptide prophet execution", ex);
              return false;
            }
          }

          // workspace init
          List<String> cmdPhiInit = new ArrayList<>();
          cmdPhiInit.add(phi.useBin());
          cmdPhiInit.addAll(asParts("workspace --init --nocheck"));
          ProcessBuilder pbPhiInit = new ProcessBuilder(cmdPhiInit);
          pbPhiInit.directory(temp.toFile());
          pbisPreParallel.add(new PbiBuilder()
              .setPb(pbPhiInit)
              .setName(getCmdName() + ": Workspace init")
              .setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL).create());

          // peptide prophet itself
          List<String> cmdPp = new ArrayList<>();
          cmdPp.add(phi.useBin());
          cmdPp.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);
          addFreeCommandLineParams(peptideProphetParams, cmdPp, enzymeName);
          cmdPp.add("--decoy");
          cmdPp.add(decoyTag);
          cmdPp.add("--database");
          cmdPp.add(fastaPath);

          cmdPp.add(Paths.get("..", pepxmlPath.getFileName().toString()).toString());
          ProcessBuilder pbPp = new ProcessBuilder(cmdPp);
          setupEnv(temp, pbPp);
          pbisParallel.add(new PbiBuilder()
              .setPb(pbPp)
              .setParallelGroup(getCmdName()).create());

          // delete temp dir
          workspacesToBeCleaned.add(temp);
          List<ProcessBuilder> pbsDeleteTemp = ToolingUtils
              .pbsDeleteFiles(jarFragpipe, Collections.singletonList(temp));
          pbisPostParallel.addAll(pbsDeleteTemp.stream()
              .map(pb -> new PbiBuilder()
                  .setPb(pb)
                  .setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL)
                  .setName(getCmdName() + ": Delete temp").create())
              .collect(Collectors.toList()));
        }
      }
      pbis.addAll(pbisPreParallel);
      pbis.addAll(pbisParallel);
      pbis.addAll(pbisPostParallel);

    } else {
      // --combine specified
      Map<String, List<Entry<InputLcmsFile, List<Path>>>> pepxmlByExp = pepxmlFiles.entrySet().stream()
          .collect(Collectors.groupingBy(kv -> kv.getKey().getGroup()));
      for (List<Entry<InputLcmsFile, List<Path>>> exp : pepxmlByExp.values()) {
        // check that all pepxml files are in one folder
        List<Path> pepxmlDirs = exp.stream().flatMap(e -> e.getValue().stream()).map(Path::getParent).distinct()
            .collect(Collectors.toList());
        if (pepxmlDirs.size() > 1) {
          String msg = String.format("When 'combine'd PeptideProphet processing requested all files "
              + "for each experiment must be located in the same folder. We found experiment: "
              + "%s with files from %d folders.", exp.get(0).getKey().getGroup(), pepxmlDirs.size());
          JOptionPane.showMessageDialog(comp, msg, "Experiment/Group files in different folders", JOptionPane.WARNING_MESSAGE);
          return false;
        }

        List<String> cmd = new ArrayList<>();
        final Path pepxmlDir = pepxmlDirs.get(0);
        cmd.add(phi.useBin(pepxmlDir));
        cmd.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);

        addFreeCommandLineParams(peptideProphetParams, cmd, enzymeName);
        cmd.add("--decoy");
        cmd.add(decoyTag);
        cmd.add("--database");
        cmd.add(fastaPath);
        cmd.add("--combine");

        exp.stream().flatMap(e -> e.getValue().stream()).map(Path::getFileName)
            .forEach(pepxmlFn -> cmd.add(pepxmlFn.toString()));
        final ProcessBuilder pb = new ProcessBuilder(cmd);
        setupEnv(pepxmlDir, pb);
        pbis.add(new PbiBuilder().setPb(pb).create());
      }
    }

    // update global cleanup
    ProcessManager.addFilesToDelete(workspacesToBeCleaned);

    isConfigured = true;
    return true;
  }

  private void setupEnv(Path workdir, ProcessBuilder pb) {
    // set environment
    pb.directory(workdir.toFile());
    pb.environment().putIfAbsent("WEBSERVER_ROOT", "fake-WEBSERVER_ROOT-value");
  }

  private void addFreeCommandLineParams(PeptideProphetParams peptideProphetParams,
      List<String> cmd, String enzymeName) {
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

    final String optNontt = "--nontt";
    final String optEnzyme = "--enzyme";
    final String nonspecific = "nonspecific";
    if (nonspecific.equals(enzymeName)) {
      addToListIfNotThere(cmd, optNontt);
    } else if ("custom".equals(enzymeName)) {
      addToListIfNotThere(cmd, optNontt);
      if (addToListIfNotThere(cmd, optEnzyme)) {
        cmd.add(nonspecific);
      }
    }
  }

  private boolean addToListIfNotThere(List<String> cmd, String opt) {
    if (cmd.contains(opt)) {
      return false;
    }
    for (String s : opt.split("\\s+")) {
      cmd.add(opt);
    }
    return true;
  }

  @Override
  public int getPriority() {
    return 92;
  }

  @Override
  public ProcessBuildersDescriptor getBuilderDescriptor() {
    ProcessBuildersDescriptor b = super.getBuilderDescriptor();
    b.setParallelGroup(getCmdName());
    return b;
  }
}
