package com.dmtavt.fragpipe.cmd;

import static com.dmtavt.fragpipe.cmd.CmdPeptideProphet.deleteFiles;

import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.jooq.lambda.Seq;
import org.jooq.lambda.tuple.Tuple2;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdPtmProphet extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPtmProphet.class);
  public static String NAME = "PtmProphet";

  public CmdPtmProphet(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhi, int threads, String cmdLineOpts,
      List<Tuple2<InputLcmsFile, Path>> lcmsToPepxml) {
    initPreConfig();

    Map<Path, List<Tuple2<InputLcmsFile, Path>>> groupByPepxml = Seq.seq(lcmsToPepxml)
        .groupBy(Tuple2::v2);

    // check for existing pepxml files and delete them
    try {
      final List<Path> forDeletion = new ArrayList<>();
      for (Entry<Path, List<Tuple2<InputLcmsFile, Path>>> kv : groupByPepxml.entrySet()) {
        Path workDir = kv.getValue().get(0).v1.outputDir(wd);
        if (Files.exists(workDir)) { // Dry-run does not create the folders.
          forDeletion.addAll(Files.list(workDir).filter(file -> file.toString().endsWith("mod.pep.xml")).collect(Collectors.toList()));
        }
      }
      if (!deleteFiles(comp, forDeletion)) {
        return false;
      }
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }

    for (Entry<Path, List<Tuple2<InputLcmsFile, Path>>> kv : groupByPepxml.entrySet()) {
      Path pepxml = kv.getKey();
      Path workDir = kv.getValue().get(0).v1.outputDir(wd);

      // PTMProphet itself
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhi.useBin(workDir));
      cmd.add("ptmprophet");
      List<String> cmdOpts = Seq.of(cmdLineOpts.split(" "))
          .map(String::trim).filter(StringUtils::isNotBlank).toList();
      if (!cmdOpts.contains("--maxthreads")) {
        cmdOpts.add("--maxthreads");
        cmdOpts.add(Integer.toString(Math.max(1, threads)));
      }
      cmd.addAll(cmdOpts);
      cmd.add(pepxml.getFileName().toString());


      final ProcessBuilder pb = new ProcessBuilder(cmd);
      //pb.directory(lcms.getPath().getParent().toFile()); // PTMProphet is run from the directory where the RAW is
      pb.directory(workDir.toFile());
      pbis.add(new PbiBuilder().setPb(pb).create());
    }

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
