package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.process.ProcessManager;
import com.dmtavt.fragpipe.tools.pepproph.PeptideProphetParams;
import com.dmtavt.fragpipe.util.PercolatorOutputToPepXML;
import com.github.chhh.utils.*;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.Component;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.*;
import java.util.Map.Entry;

public class CmdPercolator extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPercolator.class);

  public static final String NAME = "Percolator";

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
        m.computeIfAbsent(lcms, (k) -> new ArrayList<>()).add(interactXml);
      }
    }
    return m;
  }

  /**
   * @param pepxmlFiles Either pepxml files after search or after Crystal-C.
   */
  public boolean configure(Component comp, UsageTrigger phi, Path jarFragpipe, boolean isDryRun,
      String fastaPath, String decoyTag, String textPepProphCmd, boolean combine, String enzymeName,
      Map<InputLcmsFile, List<Path>> pepxmlFiles) {

    initPreConfig();

    PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
    peptideProphetParams.setCmdLineParams(textPepProphCmd);

    Set<Path> workspacesToBeCleaned = new HashSet<>();

    /*if (!combine)*/ {
      LinkedList<ProcessBuilderInfo> pbisPreParallel = new LinkedList<>();
      LinkedList<ProcessBuilderInfo> pbisParallel = new LinkedList<>();
      LinkedList<ProcessBuilderInfo> pbisPostParallel = new LinkedList<>();

      for (Entry<InputLcmsFile, List<Path>> e : pepxmlFiles.entrySet()) {
        for (Path pepxmlPath : e.getValue()) {
          final Path pepxmlDir = pepxmlPath.getParent();
          final String pepxmlFn = pepxmlPath.getFileName().toString();

          // Percolator
          List<String> cmdPp = new ArrayList<>();
          final String percolator_bin = OsUtils.isUnix() ? "percolator" :
                  OsUtils.isWindows() ? "percolator-v3-05.exe" : null;
          cmdPp.add(FragpipeLocations.checkToolsMissing(Seq.of(percolator_bin)).get(0).toString());

          addFreeCommandLineParams(peptideProphetParams, cmdPp, enzymeName);
          cmdPp.add("--results-psms");
          cmdPp.add(pepxmlPath.getFileName().toString() + "_percolator_results_psms.tsv");
          cmdPp.add("--decoy-results-psms");
          cmdPp.add(pepxmlPath.getFileName().toString() + "_percolator_decoy_results_psms.tsv");
          final String nameWithoutExt = pepxmlFn.substring(0, pepxmlFn.length() - ".pepXML".length());
          cmdPp.add(Paths.get(nameWithoutExt + ".pin").toString());
          ProcessBuilder pbPp = new ProcessBuilder(cmdPp);
//          setupEnv(temp, pbPp);
          setupEnv(pepxmlDir, pbPp);
          pbisParallel.add(new PbiBuilder()
              .setPb(pbPp)
              .setParallelGroup(getCmdName()).create());

          // convert the percolator output tsv to peptideprohept's pep.xml format
          ProcessBuilder pbRewrite = pbConvertToPepxml(jarFragpipe, "interact-" + nameWithoutExt + ".pep.xml", pepxmlPath);
          pbRewrite.directory(pepxmlDir.toFile());
          pbisPostParallel.add(new PbiBuilder().setName("Percolator: Convert to pepxml")
                  .setPb(pbRewrite).setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL).create());

        }
      }
      pbis.addAll(pbisPreParallel);
      pbis.addAll(pbisParallel);
      pbis.addAll(pbisPostParallel);

    }


    // update global cleanup
    ProcessManager.addFilesToDelete(workspacesToBeCleaned);

    isConfigured = true;
    return true;
  }

  private void setupEnv(Path workdir, ProcessBuilder pb) {
    // set environment
    pb.directory(workdir.toFile());
//    pb.environment().putIfAbsent("WEBSERVER_ROOT", "fake-WEBSERVER_ROOT-value");
    // return pb;
  }

  private void addFreeCommandLineParams(PeptideProphetParams peptideProphetParams,
      List<String> cmd, String enzymeName) {
    if (!peptideProphetParams.getCmdLineParams().isEmpty()) {
      String cmdOpts = peptideProphetParams.getCmdLineParams();
      cmd.addAll(asParts(cmdOpts));
    }

//    final String optNontt = "--nontt";
//    final String optEnzyme = "--enzyme";
//    final String nonspecific = "nonspecific";
//    if (nonspecific.equals(enzymeName)) {
//      addToListIfNotThere(cmd, optNontt);
//    } else if ("custom".equals(enzymeName)) {
//      addToListIfNotThere(cmd, optNontt);
//      if (addToListIfNotThere(cmd, optEnzyme)) {
//        cmd.add(nonspecific);
//      }
//    }
  }

  @Override
  public ProcessBuildersDescriptor getBuilderDescriptor() {
    ProcessBuildersDescriptor b = super.getBuilderDescriptor();
    b.setParallelGroup(getCmdName());
    return b;
  }

  @Override
  public boolean usesPhi() {
    return false;
  }

  private static ProcessBuilder pbConvertToPepxml(Path jarFragpipe, String outPepxml, Path fraggerPepxml) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }
    final List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-cp");
    Path root = FragpipeLocations.get().getDirFragpipeRoot();
    String libsDir = root.resolve("lib").toString() + "/*";
    if (Files.isDirectory(jarFragpipe)) {
      libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib").toString() + "/*";
      log.warn("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}", libsDir);
    }
    cmd.add(libsDir);
    cmd.add(PercolatorOutputToPepXML.class.getCanonicalName());
    cmd.add(fraggerPepxml.toString());
    cmd.add(fraggerPepxml.toString() + "_percolator_results_psms.tsv");
    cmd.add(fraggerPepxml.toString() + "_percolator_decoy_results_psms.tsv");
    cmd.add(outPepxml);
    return new ProcessBuilder(cmd);
  }

}
