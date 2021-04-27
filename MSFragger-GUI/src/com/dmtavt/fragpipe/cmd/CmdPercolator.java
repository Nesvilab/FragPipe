package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.tabs.TabMsfragger;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tools.pepproph.PeptideProphetParams;
import com.dmtavt.fragpipe.util.PercolatorOutputToPepXML;
import com.github.chhh.utils.OsUtils;
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

import org.apache.commons.io.FilenameUtils;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

  private static String remove_rank_suffix(final String s) {
    final Pattern compile = Pattern.compile("(.+)_rank\\d+\\z");
    final Matcher matcher = compile.matcher(s);
    if (matcher.find())
      return matcher.group(1);
    return s;
  }

  /**
   * @param pepxmlFiles Either pepxml files after search or after Crystal-C.
   */
  public boolean configure(Path jarFragpipe, String percolatorCmd, boolean combine, Map<InputLcmsFile, List<Path>> pepxmlFiles) {
    PeptideProphetParams percolatorParams = new PeptideProphetParams();
    percolatorParams.setCmdLineParams(percolatorCmd);

    LinkedList<ProcessBuilderInfo> pbisParallel = new LinkedList<>();
    LinkedList<ProcessBuilderInfo> pbisPostParallel = new LinkedList<>();

    final Set<String> basenames = new HashSet<>();
    for (Entry<InputLcmsFile, List<Path>> e : pepxmlFiles.entrySet()) {
      for (Path pepxmlPath : e.getValue()) {
        final Path pepxmlDir = pepxmlPath.getParent();
        final String nameWithoutExt = FilenameUtils.removeExtension(pepxmlPath.getFileName().toString());
        final String basename = remove_rank_suffix(nameWithoutExt);
        if(!basenames.add(basename))
          continue;
        // Percolator
        List<String> cmdPp = new ArrayList<>();
        final String percolator_bin = OsUtils.isUnix() ? "percolator" :
                OsUtils.isWindows() ? "percolator-v3-05.exe" : null;
        cmdPp.add(FragpipeLocations.checkToolsMissing(Seq.of(percolator_bin)).get(0).toString());

        addFreeCommandLineParams(percolatorParams, cmdPp);
        TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
        cmdPp.add("--num-threads");
        cmdPp.add("" + tabWorkflow.getThreads());
        cmdPp.add("--results-psms");
        cmdPp.add(basename + "_percolator_target_psms.tsv");
        cmdPp.add("--decoy-results-psms");
        cmdPp.add(basename + "_percolator_decoy_psms.tsv");
        cmdPp.add(Paths.get(basename + ".pin").toString());

        ProcessBuilder pbPp = new ProcessBuilder(cmdPp);
        setupEnv(pepxmlDir, pbPp);
        pbisParallel.add(new PbiBuilder()
            .setPb(pbPp)
            .setParallelGroup(basename).create());

        // convert the percolator output tsv to PeptideProphet's pep.xml format
        ProcessBuilder pbRewrite = pbConvertToPepxml(jarFragpipe, "interact-" + basename, ".pep.xml", basename);
        pbRewrite.directory(pepxmlPath.getParent().toFile());
        pbisPostParallel.add(new PbiBuilder().setName("Percolator: Convert to pepxml")
                .setPb(pbRewrite).setParallelGroup(ProcessBuilderInfo.GROUP_SEQUENTIAL).create());
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

  private static ProcessBuilder pbConvertToPepxml(Path jarFragpipe, String outPepxmlBasename, String outExt, String basename) {
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
    cmd.add(basename + ".pin");
    cmd.add(basename);
    cmd.add(".pepXML");
    cmd.add(basename + "_percolator_target_psms.tsv");
    cmd.add(basename + "_percolator_decoy_psms.tsv");
    cmd.add(outPepxmlBasename);
    cmd.add(outExt);
    final TabMsfragger tabMsf = Fragpipe.getStickyStrict(TabMsfragger.class);
    final boolean is_DDA = tabMsf.getParams().getDataType() == 0;
    cmd.add(is_DDA ? "DDA" : "DIA");
    return new ProcessBuilder(cmd);
  }

}
