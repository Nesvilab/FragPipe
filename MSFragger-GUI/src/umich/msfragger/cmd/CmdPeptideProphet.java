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
        // --combine option for peptide prophet means there's a single interact.pep.xml for each experiment/group
        interactXml = cleanDir.resolve(Paths.get("interact.pep.xml"));
      }

      m.put(lcms, interactXml);
    }
    return m;
  }

  private List<Path> findOldFilesForDeletion(Map<InputLcmsFile, Path> outputs) {
//    final Set<Path> outputPaths = pepxmlFiles.keySet().stream()
//        .map(f -> f.outputDir(wd)).collect(Collectors.toSet());
    final Set<Path> outputPaths = outputs.values().stream()
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
      String fastaPath, String decoyTag, String textPepProphCmd, boolean combine,
      Map<InputLcmsFile, Path> pepxmlFiles) {

    pbs.clear();

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
    final Map<InputLcmsFile, Path> outputs = outputs(pepxmlFiles, "pepxml", combine);
    final List<Path> forDeletion = findOldFilesForDeletion(outputs);
    if (!deleteFiles(comp, forDeletion)) {
      return false;
    }

    PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
    peptideProphetParams.setCmdLineParams(textPepProphCmd);

    if (!combine) {
      for (Map.Entry<InputLcmsFile, Path> e : pepxmlFiles.entrySet()) {
        final Path pepxmlPath = e.getValue();
        final Path pepxmlDir = pepxmlPath.getParent();
        final String pepxmlFn = pepxmlPath.getFileName().toString();


        // Needed for parallel Peptide Prophet

        // create temp dir to move pepxml to, otherwise philosopher breaks
        Path temp = pepxmlDir.resolve("fragpipe-temp-" + pepxmlFn);
        if (!isDryRun) {
          try {
            if (Files.exists(temp)) {
              FileDelete.deleteFileOrFolder(temp);
            }
            Path created = Files.createDirectories(temp);
          } catch (FileAlreadyExistsException ignored) {
            log.debug("Temp dir already exists, no biggie");
          } catch (IOException ex) {
            log.error("Could not create temporary directory for running peptide prophet in parallel", ex);
          }
        }

        // copy original pepxml to temp
        List<ProcessBuilder> pbsCopyToTemp = ToolingUtils
            .pbsCopyFiles(jarFragpipe, temp, false, Collections.singletonList(pepxmlPath));
        pbs.addAll(pbsCopyToTemp);

        // workspace init
        List<String> cmdPhiInit = new ArrayList<>();
        cmdPhiInit.add(phi.useBin());
        cmdPhiInit.add("workspace");
        cmdPhiInit.add("--init");
        ProcessBuilder pbPhiInit = new ProcessBuilder(cmdPhiInit);
        pbPhiInit.directory(temp.toFile());
        pbs.add(pbPhiInit);

        // peptide prophet itself
        List<String> cmdPp = new ArrayList<>();
        cmdPp.add(phi.useBin());
        cmdPp.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);
        addFreeCommandLineParams(peptideProphetParams, cmdPp);
        cmdPp.add("--decoy");
        cmdPp.add(decoyTag);
        cmdPp.add("--database");
        cmdPp.add(fastaPath);

        cmdPp.add(pepxmlPath.getFileName().toString());
        ProcessBuilder pbPp = new ProcessBuilder(cmdPp);
        setupEnv(temp, pbPp);
        pbs.add(pbPp);

        // move pepxml after peptide prophet back to original directory
        HashMap<InputLcmsFile, Path> tempMap = new HashMap<>();
        tempMap.put(e.getKey(), temp.resolve(pepxmlFn));
        Map<InputLcmsFile, Path> outputsInTemp = outputs(tempMap, "pepxml", combine);
        Entry<InputLcmsFile, Path> pepxmlAfterSearch = outputsInTemp.entrySet().iterator().next();
        List<ProcessBuilder> pbsMoveFromTemp = ToolingUtils
            .pbsMoveFiles(jarFragpipe, pepxmlDir, false,
                Collections.singletonList(pepxmlAfterSearch.getValue()));
        pbs.addAll(pbsMoveFromTemp);

        // delete temp dir
        List<ProcessBuilder> pbsDeleteTemp = ToolingUtils
            .pbsDeleteFiles(jarFragpipe, Collections.singletonList(temp));
        pbs.addAll(pbsDeleteTemp);
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
        cmd.add(phi.useBin());
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

  @Override
  public ProcessBuildersDescriptor getBuilderDescriptor() {
    ProcessBuildersDescriptor b = super.getBuilderDescriptor();
    b.setParallelGroup(getCmdName());
    return b;
  }
}
