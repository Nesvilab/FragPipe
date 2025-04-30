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
import static org.nesvilab.fragpipe.cmd.CmdPeptideProphet.findOldFilesForDeletion;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.tabs.TabWorkflow;
import org.nesvilab.fragpipe.tools.msbooster.MSBoosterPanel;
import org.nesvilab.fragpipe.tools.pepproph.PeptideProphetParams;
import org.nesvilab.fragpipe.tools.percolator.PercolatorOutputToPepXML;
import org.nesvilab.fragpipe.tools.percolator.PercolatorPanel;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.StringUtils;
import java.awt.Component;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.commons.io.FilenameUtils;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdPercolator extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPercolator.class);

  public static final String NAME = "Percolator";
  public static final String PERCOLATOR_VERSION = "3.7.1";

  public CmdPercolator(boolean isRun, Path workDir) {
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
        final Path cleanDir = pepxml.toAbsolutePath().getParent();

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
        m.computeIfAbsent(lcms, (k) -> new ArrayList<>()).add(interactXml);
      }
    }
    return m;
  }

  private static String remove_rank_suffix(final String s) {
    final Pattern compile = Pattern.compile("(.+)_rank\\d+\\z");
    final Matcher matcher = compile.matcher(s);
    if (matcher.find())
      return matcher.group(1);
    return s;
  }

  public boolean configure(Component comp, Path jarFragpipe, String percolatorCmd, boolean combine, Map<InputLcmsFile, List<Path>> pepxmlFiles, boolean hasCrystalC, double minProb, String decoyTag, boolean hasCalibratedMzml, boolean writeSubMzml) {
    PeptideProphetParams percolatorParams = new PeptideProphetParams();
    percolatorParams.setCmdLineParams(percolatorCmd);

    final Set<Path> forDeletion = findOldFilesForDeletion(wd);
    if (!deleteFiles(comp, forDeletion, "pep.xml")) {
      return false;
    }

    LinkedList<ProcessBuilderInfo> pbisParallel = new LinkedList<>();
    LinkedList<ProcessBuilderInfo> pbisPostParallel = new LinkedList<>();

    MSBoosterPanel msboosterPanel = Fragpipe.getStickyStrict(MSBoosterPanel.class);

    final Set<String> basenames = new HashSet<>();
    for (Entry<InputLcmsFile, List<Path>> e : pepxmlFiles.entrySet()) {
      InputLcmsFile inputLcmsFile = e.getKey();
      for (Path pepxmlPath : e.getValue()) {
        final Path pepxmlDir = pepxmlPath.toAbsolutePath().getParent();
        final String nameWithoutExt = FilenameUtils.removeExtension(pepxmlPath.getFileName().toString());
        final String basename = remove_rank_suffix(nameWithoutExt);
        if(!basenames.add(basename))
          continue;
        // Percolator
        List<String> cmdPp = new ArrayList<>();
        final String percolator_bin = OsUtils.isUnix() ? "percolator_" + PERCOLATOR_VERSION.replace(".", "_") + "/linux/percolator" :
                OsUtils.isWindows() ? "percolator_" + PERCOLATOR_VERSION.replace(".", "_") + "/windows/percolator.exe" : null;
        cmdPp.add(FragpipeLocations.checkToolsMissing(Seq.of(percolator_bin)).get(0).toString());

        String strippedBaseName;
        if (hasCrystalC) {
          strippedBaseName = basename.replaceFirst("_c$", "");
        } else {
          strippedBaseName = basename;
        }

        addFreeCommandLineParams(percolatorParams, cmdPp);
        TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
        cmdPp.add("--num-threads");
        cmdPp.add("" + tabWorkflow.getThreads());

        boolean onlyPsms = false;
        for(String cmd : cmdPp) {
          if (cmd.contentEquals("--only-psms")) {
            onlyPsms = true;
            break;
          }
        }

        if (onlyPsms) {
          cmdPp.add("--results-psms");
          cmdPp.add(strippedBaseName + "_percolator_target_psms.tsv");
          cmdPp.add("--decoy-results-psms");
          cmdPp.add(strippedBaseName + "_percolator_decoy_psms.tsv");
        } else {
          cmdPp.add("--results-peptides");
          cmdPp.add(strippedBaseName + "_percolator_target_psms.tsv");
          cmdPp.add("--decoy-results-peptides");
          cmdPp.add(strippedBaseName + "_percolator_decoy_psms.tsv");
        }

        cmdPp.add("--protein-decoy-pattern");
        cmdPp.add(decoyTag);

        if (writeSubMzml) { // This is the first-pass and there will be a second-pass. Let Percolator save the weights for the second-pass.
          cmdPp.add("--weights");
          cmdPp.add(strippedBaseName + "_percolator.weights");
        }

        if (inputLcmsFile.getPath().getFileName().toString().endsWith("_sub.mzML")) { // It is likely to be the sub mzML file from the first-pass. Find the weights file.
          String ss = inputLcmsFile.getPath().getFileName().toString().replaceFirst("_sub\\.mzML$", "_percolator.weights");
          try {
            List<Path> pList = Files.walk(inputLcmsFile.getPath().toAbsolutePath().getParent()).filter(p -> {
              String s = p.getFileName().toString();
              return s.endsWith("_percolator.weights") && s.contentEquals(ss);
            }).collect(Collectors.toList());

            if (pList.size() > 1) {
              throw new IllegalStateException("Found more than one weights file: " + pList.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.joining(", ")));
            } else if (pList.size() == 1) {
              cmdPp.add("--init-weights");
              cmdPp.add(pList.get(0).toAbsolutePath().normalize().toString());
              cmdPp.add("--static");
              cmdPp.add("--override");
            }
          } catch (Exception ex) {
            ex.printStackTrace();
            return false;
          }
        }

        if (msboosterPanel.isRun()) {
          cmdPp.add(Paths.get(strippedBaseName + "_edited.pin").toString());
        } else {
          cmdPp.add(Paths.get(strippedBaseName + ".pin").toString());
        }

        ProcessBuilder pbPp = new ProcessBuilder(cmdPp);
        setupEnv(pepxmlDir, pbPp);
        pbisParallel.add(new PbiBuilder()
            .setPb(pbPp)
            .setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL).create());

        String lcmsPath = e.getKey().getPath().toAbsolutePath().normalize().toString();
        if (hasCalibratedMzml) {
          lcmsPath = StringUtils.upToLastDot(lcmsPath) + "_calibrated.mzML";
        } else if (lcmsPath.toLowerCase().endsWith(".raw") || lcmsPath.toLowerCase().endsWith(".d")) {
          lcmsPath = StringUtils.upToLastDot(lcmsPath) + "_uncalibrated.mzML";
        }

        // convert the percolator output tsv to PeptideProphet's pep.xml format
        ProcessBuilder pbRewrite = pbConvertToPepxml(jarFragpipe, "interact-" + basename, strippedBaseName, basename, e.getKey().getDataType().contentEquals("DDA"), minProb, lcmsPath);
        pbRewrite.directory(pepxmlPath.toAbsolutePath().getParent().toFile());
        pbisPostParallel.add(new PbiBuilder().setName("Percolator: Convert to pepxml").setPb(pbRewrite).setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL).create());

        // delete intermediate files
        PercolatorPanel percolatorPanel = Fragpipe.getStickyStrict(PercolatorPanel.class);
        if (!percolatorPanel.isKeepTsvFiles()) {
          final List<Path> temp = new ArrayList<>();
          temp.add(pepxmlDir.resolve(strippedBaseName + "_percolator_target_psms.tsv"));
          temp.add(pepxmlDir.resolve(strippedBaseName + "_percolator_decoy_psms.tsv"));
          List<ProcessBuilder> pbsDeleteTemp = ToolingUtils
                  .pbsDeleteFiles(jarFragpipe, temp);
          pbisPostParallel.addAll(pbsDeleteTemp.stream()
                  .map(pb -> new PbiBuilder()
                          .setPb(pb)
                          .setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL)
                          .setName(getCmdName() + " delete temp").create())
                  .collect(Collectors.toList()));
        }
      }
    }
    pbis.addAll(pbisParallel);
    pbis.addAll(pbisPostParallel);

    isConfigured = true;
    return true;
  }

  private void setupEnv(Path workdir, ProcessBuilder pb) {
    // set environment
    pb.directory(workdir.toFile());
    // return pb;
  }

  private void addFreeCommandLineParams(PeptideProphetParams peptideProphetParams,
      List<String> cmd) {
    if (!peptideProphetParams.getCmdLineParams().isEmpty()) {
      String cmdOpts = peptideProphetParams.getCmdLineParams();
      cmd.addAll(asParts(cmdOpts));
    }
  }

  @Override
  public ProcessBuildersDescriptor getBuilderDescriptor() {
    ProcessBuildersDescriptor b = super.getBuilderDescriptor();
    b.setParallelGroup(getCmdName());
    return b;
  }

  private static ProcessBuilder pbConvertToPepxml(Path jarFragpipe, String outBaseName, String stripedBasename, String basename, boolean isDDA, double minProb, String lcmsPath) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }
    final List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-cp");
    Path root = FragpipeLocations.get().getDirFragpipeRoot();
    String libsDir = root.resolve("lib") + "/*";
    if (Files.isDirectory(jarFragpipe)) {
      libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib") + "/*";
      log.debug("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
    }
    cmd.add(libsDir);
    cmd.add(PercolatorOutputToPepXML.class.getCanonicalName());
    cmd.add(stripedBasename + ".pin");
    cmd.add(basename);
    cmd.add(stripedBasename + "_percolator_target_psms.tsv");
    cmd.add(stripedBasename + "_percolator_decoy_psms.tsv");
    cmd.add(outBaseName);
    cmd.add(isDDA ? "DDA" : "DIA");
    cmd.add(minProb + "");
    cmd.add(lcmsPath);
    return new ProcessBuilder(cmd);
  }

}
