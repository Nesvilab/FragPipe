/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.cmd;

import static org.nesvilab.fragpipe.cmd.CmdPeptideProphet.deleteFiles;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tools.philosopher.PhilosopherProps;
import org.nesvilab.fragpipe.tools.protproph.ProteinProphetParams;
import org.nesvilab.utils.FileListing;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.UsageTrigger;
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
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdProteinProphet extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdProteinProphet.class);

  public static final String NAME = "ProteinProphet";

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
  public Map<LcmsFileGroup, Path> outputs(Map<InputLcmsFile, List<Path>> pepxmlFiles) {

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
      m.put(group, wd.resolve("combined.prot.xml"));
    }

    Set<Path> interactProtXmls = new HashSet<>(m.values());
    if (interactProtXmls.size() > 1) {
      throw new IllegalStateException("During combined processing of Experiments/Groups "
          + "only one interact.prot.xml file should be produced. This is probably a bug, report "
          + "to developers.");
    }

    return m;
  }

  private Set<Path> findOldFilesForDeletion(List<Path> outputs) {
    Set<Path> outputDirs = outputs.stream().map(Path::getParent).collect(Collectors.toSet());
    final Pattern regex = Pattern.compile(".+?\\.prot\\.xml$", Pattern.CASE_INSENSITIVE);
    final Set<Path> toDelete = new TreeSet<>();
    for (Path dir : outputDirs) {
      FileListing fl = new FileListing(dir, regex);
      fl.setRecursive(false);
      fl.setIncludeDirectories(false);
      toDelete.addAll(fl.findFiles());
    }
    return toDelete;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String txtProteinProphetCmdLineOpts, Map<InputLcmsFile, List<Path>> pepxmlFiles) {

    initPreConfig();

    // check for existence of old files
    final Map<LcmsFileGroup, Path> outputs = outputs(pepxmlFiles);
    final Set<Path> oldFilesForDeletion = findOldFilesForDeletion(new ArrayList<>(outputs.values()));
    if (!deleteFiles(comp, oldFilesForDeletion, "prot.xml")) {
      return false;
    }

    ProteinProphetParams proteinProphetParams = new ProteinProphetParams();
    proteinProphetParams.setCmdLineParams(txtProteinProphetCmdLineOpts);

    Map<LcmsFileGroup, Path> groupToProtxml = outputs(pepxmlFiles);

    {
      Set<Path> interactProtXmls = new HashSet<>(groupToProtxml.values());
      if (interactProtXmls.size() > 1) {
        if (Fragpipe.headless) {
          log.error("[ProteinProphet] Report to developers, more than one interact protxml file when processing experimental groups together.");
        } else {
          JOptionPane.showMessageDialog(comp, "[ProteinProphet]\n"
              + "Report to developers, more than one interact protxml file when\n"
              + "processing experimental groups together.");
        }
        return false;
      } else if (interactProtXmls.isEmpty()) {
        if (Fragpipe.headless) {
          log.error("There are no interact.prot.xml file.");
        } else {
          JOptionPane.showMessageDialog(comp, "There are no interact.prot.xml file.");
        }
        return false;
      }
      Path protxml = interactProtXmls.iterator().next();
      if (!protxml.toAbsolutePath().getParent().equals(wd)) {
        throw new IllegalStateException("Protxml not in global output directory when groups processed together.");
      }
      List<String> pepxmlsPaths = pepxmlFiles.entrySet().stream()
          .flatMap(pepxml -> pepxml.getValue().stream()).map(Path::toString)
          .distinct()
          .collect(Collectors.toList());
      List<String> cmd = createCmdStub(usePhilosopher, protxml.toAbsolutePath().getParent(), proteinProphetParams);

      final Path filelist = wd.resolve("filelist_proteinprophet.txt");

      if (Files.exists(filelist.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
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
      pb.directory(protxml.toAbsolutePath().getParent().toFile());
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
