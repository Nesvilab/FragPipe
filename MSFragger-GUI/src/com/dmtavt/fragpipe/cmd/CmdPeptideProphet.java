package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.exceptions.NoStickyException;
import com.dmtavt.fragpipe.process.ProcessManager;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tools.pepproph.PeptideProphetParams;
import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.dmtavt.fragpipe.util.RewritePepxml;
import com.github.chhh.utils.FileDelete;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
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
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdPeptideProphet extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdPeptideProphet.class);
  private static final Pattern pattern = Pattern.compile("interact-.+\\.pep\\.xml.*");

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
          // --combine option for PeptideProphet means there's a single interact.pep.xml for each experiment/group
          interactXml = cleanDir.resolve(Paths.get("interact.pep.xml"));
        }
        m.computeIfAbsent(lcms, (k) -> new ArrayList<>()).add(interactXml);
      }
    }
    return m;
  }

  public static List<Path> findOldFilesForDeletion(Map<InputLcmsFile, List<Path>> outputs) {
//    final Set<Path> outputPaths = pepxmlFiles.keySet().stream()
//        .map(f -> f.outputDir(wd)).collect(Collectors.toSet());
    final Set<Path> outputPaths = outputs.values().stream().flatMap(List::stream)
        .map(Path::getParent).collect(Collectors.toSet());
    final List<Path> pepxmlsToDelete = new ArrayList<>();
    for (Path outputPath : outputPaths) {
      if (Files.exists(outputPath) && Files.isDirectory(outputPath)) {
        try {
          pepxmlsToDelete.addAll(Files.list(outputPath).filter(Files::isRegularFile).filter(p -> pattern.matcher(p.getFileName().toString()).matches()).collect(Collectors.toList()));
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
    return pepxmlsToDelete;
  }

  /**
   * Asks user confirmation before deleting the files.
   * Shows all the file paths to be deleted.
   */
  public static boolean deleteFiles(Component comp, List<Path> forDeletion, String tool) {
    if (forDeletion == null || forDeletion.isEmpty())
      return true;

    String[][] data = new String[forDeletion.size()][1];
    int index = -1;
    for (Path path : forDeletion) {
      data[++index][0] = path.toString();
    }

    DefaultTableModel model = new DefaultTableModel(data, new String[] {"To be deleted"});
    JTable table = new JTable(model);
    table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    JPanel panel = new JPanel(new BorderLayout());
    panel.add(new JLabel("<html>Found " + forDeletion.size() + " old " + tool + " files.<br/>"
        + "This might cause problems depending on the selected options.<br/>"
        + "It's recommended to delete the files first.<br/><br/>"
        + "<ul><li><b>Yes</b> - delete files now</li>"
        + "<li><b>No</b> - continue without deleting files</li>"
        + "<li><b>Cancel</b> - stop and don't run anything</li></ul>"
        ), BorderLayout.NORTH);
    panel.add(Box.createVerticalStrut(100), BorderLayout.CENTER);
    panel.add(new JScrollPane(table), BorderLayout.CENTER);

    String[] options = {"Yes - Delete now", "No - Continue as is", "Cancel"};
    if (Fragpipe.headless)
      return true;
    final int confirmation = JOptionPane
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
      default:
        return false;
    }
  }

  /**
   * @param pepxmlFiles Either pepxml files after search or after Crystal-C.
   */
  public boolean configure(Component comp, UsageTrigger phi, Path jarFragpipe, boolean isDryRun,
      String fastaPath, String decoyTag, String textPepProphCmd, boolean combine, String enzymeName,
      Map<InputLcmsFile, List<Path>> pepxmlFiles) {

    initPreConfig();

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
    if (!deleteFiles(comp, forDeletion, "pep.xml")) {
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


        // Needed for parallel PeptideProphet

          // create temp dir to house philosopher's .meta directory, otherwise philosopher breaks
          Path temp = pepxmlDir.resolve("fragpipe-" + pepxmlFn + "-temp");
          if (!isDryRun) {
            try {
              if (Files.exists(temp)) {
                FileDelete.deleteFileOrFolder(temp);
              }
            } catch (IOException ex) {
              log.error("Could not delete old temporary directory for running PeptideProphet in parallel", ex);
            }
            try {
              temp = Files.createDirectories(temp);
            } catch (FileAlreadyExistsException ignored) {
              log.error("Temp dir already exists, but we should have tried deleting it first. This is not critical.");
            } catch (IOException ex) {
              log.error("Could not create directory for parallel PeptideProphet execution", ex);
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

          // PeptideProphet itself
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


    // always rewrite pepxml
    Map<InputLcmsFile, List<Path>> pepProphOutputs = outputs(pepxmlFiles, "pepxml", combine);
    HashMap<Path, List<InputLcmsFile>> pepxmlToLcms = new HashMap<>();
    for (Entry<InputLcmsFile, List<Path>> kv : pepProphOutputs.entrySet()) {
      for (Path pepxml : kv.getValue()) {
        pepxmlToLcms.computeIfAbsent(pepxml, path -> new ArrayList<>()).add(kv.getKey());
      }
    }

    for (Entry<Path, List<InputLcmsFile>> kv : pepxmlToLcms.entrySet()) {
      List<Path> lcmsPaths = Seq.seq(kv.getValue()).map(InputLcmsFile::getPath).distinct().toList();
      ProcessBuilder pbRewrite = pbRewritePepxml(jarFragpipe, kv.getKey(), lcmsPaths);
      pbRewrite.directory(kv.getValue().get(0).outputDir(wd).toFile());
      pbis.add(new PbiBuilder().setName("Rewrite pepxml")
          .setPb(pbRewrite).setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL).create());
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
    // return pb;
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
    final TabMsfragger tabMsfragger;
    try {
      tabMsfragger = Fragpipe.getSticky(TabMsfragger.class);
    } catch (NoStickyException e) {
      throw new RuntimeException(e);
    }
    final String optNontt = "--nontt";
    final String optNonmc = "--nonmc";
    final String optEnzyme = "--enzyme";
    final String nonspecific = "nonspecific";
    if (nonspecific.equals(enzymeName) || !StringUtils.isNullOrWhitespace(tabMsfragger.getEnzymeCut2())) {
      addToListIfNotThere(cmd, optNontt);
      addToListIfNotThere(cmd, optNonmc);
    } else if ("custom".equals(enzymeName)) {
      addToListIfNotThere(cmd, optNontt);
      addToListIfNotThere(cmd, optNonmc);
      if (addToListIfNotThere(cmd, optEnzyme)) {
        cmd.add(nonspecific);
      }
    }
  }

  static private boolean addToListIfNotThere(List<String> cmd, String opt) {
    if (cmd.contains(opt))
      return false;
    Collections.addAll(cmd, opt.split("\\s+"));
    return true;
  }

  @Override
  public ProcessBuildersDescriptor getBuilderDescriptor() {
    ProcessBuildersDescriptor b = super.getBuilderDescriptor();
    b.setParallelGroup(getCmdName());
    return b;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }

  private static ProcessBuilder pbRewritePepxml(Path jarFragpipe, Path pepxml, List<Path> lcmsPaths) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }
    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-cp");
    Path root = FragpipeLocations.get().getDirFragpipeRoot();
    String libsDir = root.resolve("lib").toString() + "/*";
    if (Files.isDirectory(jarFragpipe)) {
      libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib").toString() + "/*";
      log.warn("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
    }
    cmd.add(libsDir);
    cmd.add(RewritePepxml.class.getCanonicalName());
    cmd.add(pepxml.toAbsolutePath().normalize().toString());
    for (Path lcms : lcmsPaths) {
      cmd.add(lcms.toString());
    }
    return new ProcessBuilder(cmd);
  }
}
