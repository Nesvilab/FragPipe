package com.dmtavt.fragpipe.cmd;

import static com.dmtavt.fragpipe.cmd.CmdPeptideProphet.deleteFiles;

import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.dmtavt.fragpipe.tools.protproph.ProteinProphetParams;
import com.github.chhh.utils.FileListing;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
  public Map<LcmsFileGroup, Path> outputs(Map<InputLcmsFile, List<Path>> pepxmlFiles, boolean isMultiExperimentReport) {

    Map<String, List<InputLcmsFile>> lcmsByExp = pepxmlFiles.keySet().stream()
        .collect(Collectors.groupingBy(f -> f.getGroup()));

    Map<LcmsFileGroup, Path> m = new TreeMap<>();

    for (Entry<String, List<InputLcmsFile>> e : lcmsByExp.entrySet()) {
      final String groupName = e.getKey();
      final List<InputLcmsFile> lcmsFiles = e.getValue();
      if (lcmsFiles.isEmpty()) {
        throw new IllegalStateException("Empty LCMS file list. This is a bug. Report to "
            + "developers.");
      }
      LcmsFileGroup group = new LcmsFileGroup(groupName, lcmsFiles);
      String fn = isMultiExperimentReport ? COMBINED_FN : INTERACT_FN;
      m.put(group, wd.resolve(fn));
    }

    Set<Path> interactProtXmls = new HashSet<>(m.values());
    if (interactProtXmls.size() > 1) {
      throw new IllegalStateException("During combined processing of Experiments/Groups "
          + "only one interact.prot.xml file should be produced. This is probably a bug, report "
          + "to developers.");
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

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String txtProteinProphetCmdLineOpts, boolean isMultiExperiment, Map<InputLcmsFile, List<Path>> pepxmlFiles) {

    initPreConfig();

    // check for existence of old files
    final Map<LcmsFileGroup, Path> outputs = outputs(pepxmlFiles, isMultiExperiment);
    final List<Path> oldFilesForDeletion = findOldFilesForDeletion(new ArrayList<>(outputs.values()));
    if (!deleteFiles(comp, oldFilesForDeletion, "prot.xml")) {
      return false;
    }

    ProteinProphetParams proteinProphetParams = new ProteinProphetParams();
    proteinProphetParams.setCmdLineParams(txtProteinProphetCmdLineOpts);

    Map<LcmsFileGroup, Path> groupToProtxml = outputs(pepxmlFiles, isMultiExperiment);

    {
      Set<Path> interactProtXmls = new HashSet<>(groupToProtxml.values());
      if (interactProtXmls.size() > 1) {
        JOptionPane.showMessageDialog(comp, "[ProteinProphet]\n"
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

      final Path filelist = wd.resolve("filelist_proteinprophet.txt");

      if (Files.exists(filelist.getParent())) { // Dry run does not make directories, so does not write the file.
        try (BufferedWriter bw = Files.newBufferedWriter(filelist)) {
          for (String f : pepxmlsPaths) {
            bw.write(f);
            bw.newLine();
          }
        } catch (IOException e) {
          throw new UncheckedIOException(e);
        }
      }

      cmd.add(filelist.toString());

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(protxml.getParent().toFile());
      pbis.add(PbiBuilder.from(pb));
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
  public boolean usesPhi() {
    return true;
  }
}
