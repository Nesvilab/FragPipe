package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.util.RewritePepxml;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.jooq.lambda.Seq;
import org.jooq.lambda.tuple.Tuple2;

public class CmdPtmProphet extends CmdBase {
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

    for (Entry<Path, List<Tuple2<InputLcmsFile, Path>>> kv : groupByPepxml.entrySet()) {
      Path pepxml = kv.getKey();
      List<Path> lcmsPaths = Seq.seq(kv.getValue()).map(tuple -> tuple.v1().getPath()).toList();
      Path workDir = kv.getValue().get(0).v1.outputDir(wd);

      // we never know if pepxml is rewritten or not, so always rewrite
      ProcessBuilder pbRewrite = pbRewritePepxml(jarFragpipe, pepxml, lcmsPaths);
      pbRewrite.directory(workDir.toFile());
      pbis.add(new PbiBuilder().setName("Rewrite pepxml").setPb(pbRewrite).create());

      // PTM Prophet itself
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhi.useBin(workDir));
      cmd.add("ptmprophet");
      cmd.add("--maxthreads");
      cmd.add(Integer.toString(Math.max(1, threads)));
      Seq.of(cmdLineOpts.split(" "))
          .map(String::trim).filter(StringUtils::isNotBlank).forEach(cmd::add);
      cmd.add(pepxml.toString());

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

  private static ProcessBuilder pbRewritePepxml(Path jarFragpipe, Path pepxml, List<Path> lcmsPaths) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }
    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    cmd.add("-cp");
    Path root = FragpipeLocations.get().getDirFragpipeRoot();
    String libsDir = root.resolve("lib").toString() + "/*";
//    if (!jarFragpipe.getFileName().toString().endsWith(".jar")) {
//      //"\\fragpipe\\MSFragger-GUI\\build\\install\\fragpipe\\lib";
//      libsDir = jarFragpipe.getParent().getParent().getParent().getParent().resolve("build/install/fragpipe/lib").toString() + "/*";
//    }
    cmd.add(libsDir);
    cmd.add(RewritePepxml.class.getCanonicalName());
    cmd.add(pepxml.toAbsolutePath().normalize().toString());
    for (Path lcms : lcmsPaths) {
      cmd.add(lcms.toString());
    }
    return new ProcessBuilder(cmd);
  }
}
