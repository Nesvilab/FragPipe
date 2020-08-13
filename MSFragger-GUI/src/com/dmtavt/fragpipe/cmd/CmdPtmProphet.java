package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.util.DoNothing;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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

  public boolean configure(Path jarFragpipe, UsageTrigger usePhi, int threads, String cmdLineOpts,
      List<Tuple2<InputLcmsFile, Path>> lcmsToPepxml) {
    initPreConfig();

    Map<Path, List<Tuple2<InputLcmsFile, Path>>> groupByPepxml = Seq.seq(lcmsToPepxml)
        .groupBy(Tuple2::v2);

    // slight delay before PTM-P
    pbis.add(new PbiBuilder().setPb(pbDelay(jarFragpipe, 1000)).setName("Delay").create());

    for (Entry<Path, List<Tuple2<InputLcmsFile, Path>>> kv : groupByPepxml.entrySet()) {
      Path pepxml = kv.getKey();
      List<Path> lcmsPaths = Seq.seq(kv.getValue()).map(tuple -> tuple.v1().getPath()).toList();
      Path workDir = kv.getValue().get(0).v1.outputDir(wd);

      // PTM Prophet itself
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhi.useBin(workDir));
      cmd.add("ptmprophet");
      cmd.add("--maxthreads");
      cmd.add(Integer.toString(Math.max(1, threads)));
      Seq.of(cmdLineOpts.split(" "))
          .map(String::trim).filter(StringUtils::isNotBlank).forEach(cmd::add);
      cmd.add(pepxml.getFileName().toString());

      final ProcessBuilder pb = new ProcessBuilder(cmd);
      //pb.directory(lcms.getPath().getParent().toFile()); // PTM Prophet is run from the directory where the RAW is
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

  private static ProcessBuilder pbDelay(Path jarFragpipe, long millis) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }
    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-cp");
    Path root = FragpipeLocations.get().getDirFragpipeRoot();
    String libsDir = root.resolve("lib").toString() + "/*";
    if (Files.isDirectory(jarFragpipe)) {
      libsDir = jarFragpipe.getParent().getParent().getParent().getParent()
          .resolve("build/install/fragpipe/lib").toString() + "/*";
      log.warn("Dev message: Looks like FragPipe was run from IDE, changing libs directory to: {}",
          libsDir);
    }
    cmd.add(libsDir);
    cmd.add(DoNothing.class.getCanonicalName());
    cmd.add(Long.toString(millis));
    return new ProcessBuilder(cmd);
  }
}
