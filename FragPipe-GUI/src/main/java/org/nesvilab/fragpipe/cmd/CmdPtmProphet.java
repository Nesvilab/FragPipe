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
import static org.nesvilab.utils.OsUtils.getOsName;
import static org.nesvilab.utils.OsUtils.isUnix;
import static org.nesvilab.utils.OsUtils.isWindows;

import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.jooq.lambda.Seq;
import org.jooq.lambda.tuple.Tuple2;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdPtmProphet extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdPtmProphet.class);
  private static final Pattern pattern1 = Pattern.compile("\\.pep\\.xml$");
  private static final Pattern pattern2 = Pattern.compile("interact-.+\\.mod\\.pep\\.xml.*");
  private static final Pattern pattern3 = Pattern.compile("--([^\\s]+)\\s+(((?!--).)*)");

  public static String NAME = "PTMProphet";
  public static final String PTMProphet_NAME = "PTMProphet/PTMProphetParser";
  public static final String PTMProphet_VERSION = "6.3.2";

  public CmdPtmProphet(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, String cmdLineOpts, List<Tuple2<InputLcmsFile, Path>> lcmsToPepxml, int threads) {
    initPreConfig();

    final List<Path> ptmprophetPath;
    if (isWindows()) {
      ptmprophetPath = FragpipeLocations.checkToolsMissing(Seq.of(PTMProphet_NAME + "-" + PTMProphet_VERSION + ".exe"));
      if (ptmprophetPath == null || ptmprophetPath.size() != 1) {
        System.err.println("Could not file PTMProphet's executable file " + PTMProphet_NAME + "-" + PTMProphet_VERSION + ".exe");
        return false;
      }
    } else if (isUnix()) {
      ptmprophetPath = FragpipeLocations.checkToolsMissing(Seq.of(PTMProphet_NAME + "-" + PTMProphet_VERSION));
      if (ptmprophetPath == null || ptmprophetPath.size() != 1) {
        System.err.println("Could not file PTMProphet's executable file " + PTMProphet_NAME + "-" + PTMProphet_VERSION);
        return false;
      }
    } else {
      System.err.println("PTMProphet does not support " + getOsName() + ". It only support Windows and Linux.");
      return false;
    }

    Map<Path, List<Tuple2<InputLcmsFile, Path>>> groupByPepxml = Seq.seq(lcmsToPepxml)
        .groupBy(Tuple2::v2);

    // check for existing pepxml files and delete them
    try {
      final Set<Path> forDeletion = new TreeSet<>();
      for (Entry<Path, List<Tuple2<InputLcmsFile, Path>>> kv : groupByPepxml.entrySet()) {
        Path workDir = kv.getValue().get(0).v1.outputDir(wd);
        if (Files.exists(workDir)) { // Dry-run does not create the folders.
          forDeletion.addAll(Files.list(workDir).filter(p -> pattern2.matcher(p.getFileName().toString()).matches()).collect(Collectors.toList()));
        }
      }
      if (!deleteFiles(comp, forDeletion, "mod.pep.xml")) {
        return false;
      }
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }

    int idx = 0;
    int batchNum = Math.min(32, threads);
    for (Entry<Path, List<Tuple2<InputLcmsFile, Path>>> kv : groupByPepxml.entrySet()) {
      Path pepxml = kv.getKey();
      Path workDir = kv.getValue().get(0).v1.outputDir(wd);

      // PTMProphet itself
      List<String> cmd = new ArrayList<>();
      cmd.add(ptmprophetPath.get(0).toAbsolutePath().normalize().toString());
      if (cmdLineOpts.contains("--")) { // This is the Philosopher command style. Translate it to the native style.
        cmdLineOpts = translateCmds(cmdLineOpts);
      }
      List<String> cmdOpts = Seq.of(cmdLineOpts.split("\\s+")).filter(e -> !e.startsWith("MAXTHREADS=")).toList();
      cmdOpts.add("MAXTHREADS=1");
      cmd.addAll(cmdOpts);
      cmd.add(pepxml.getFileName().toString());
      cmd.add(pattern1.matcher(pepxml.getFileName().toString()).replaceFirst(".mod.pep.xml"));

      final ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(workDir.toFile());
      pbis.add(new PbiBuilder().setPb(pb).setParallelGroup(getCmdName() + (idx / batchNum)).create());
      ++idx;
    }

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }

  static String translateCmds(String cmdLineOpts) {
    Matcher matcher = pattern3.matcher(cmdLineOpts.trim());
    StringBuilder sb = new StringBuilder();
    while(matcher.find()) {
      if (matcher.group(2) == null || matcher.group(2).trim().isEmpty()) {
        sb.append(matcher.group(1).toUpperCase()).append(" ");
      } else if (matcher.group(1).equalsIgnoreCase("mods")) {
        sb.append(matcher.group(2).trim()).append(" ");
      } else {
        sb.append(matcher.group(1).toUpperCase()).append("=").append(matcher.group(2).trim()).append(" ");
      }
    }
    return sb.toString().trim();
  }
}
