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

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.fragpipe.Version;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.util.WriteSubMzml;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdWriteSubMzml extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdWriteSubMzml.class);
  public static final String NAME = "WriteSubMzml";
  private static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR};

  public CmdWriteSubMzml(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp,
      Path extLibsThermo,
      Path extLibsBruker,
      Path jarFragpipe,
      int ramGb,
      int nThreads,
      Map<String, LcmsFileGroup> lcmsFileGroups,
      float probabilityThreshold,
      boolean isRunMSFragger,
      boolean hasDia,
      boolean hasGpfDia,
      boolean hasDiaLib,
      boolean hasDdaPlus) {
    initPreConfig();

    if (!isRunMSFragger) {
      SwingUtils.showErrorDialog(comp, "<html><b>Write sub mzML</b> was enabled but <b>MSFragger</b> was not.<br>Please either disable <b>Write sub mzML</b> in the <b>Run</b> tab or enable <b>MSFragger</b> in the <b>MSFragger</b> tab.</html>", "Error");
      return false;
    }

    if (hasDia || hasGpfDia || hasDiaLib || hasDdaPlus) {
      SwingUtils.showErrorDialog(comp, "<html><b>Write sub mzML</b> was enabled but there are non-DDA data types.<br>Please disable <b>Write sub mzML</b> in the <b>Run</b> tab.</html>", "Error");
      return false;
    }

    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(JAR_DEPS));
    if (classpathJars == null) {
      return false;
    }

    Path root = FragpipeLocations.get().getDirFragpipeRoot();
    Path libsDir = root.resolve("lib");
    if (Files.isDirectory(jarFragpipe)) {
      libsDir = jarFragpipe.toAbsolutePath().getParent().getParent().getParent().getParent().resolve("build/install/fragpipe-" + Version.version() + "/lib");
    }

    Set<String> toJoin = classpathJars.stream().map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toSet());
    try {
      toJoin.addAll(Files.walk(libsDir).
          filter(p -> p.getFileName().toString().endsWith(".jar")).
          filter(p -> p.getFileName().toString().startsWith("maven-artifact") ||
              p.getFileName().toString().startsWith("commons-lang3") ||
              p.getFileName().toString().startsWith("fragpipe-")).
          map(p -> p.toAbsolutePath().normalize().toString()).collect(Collectors.toList())
      );
    } catch (IOException ex) {
      ex.printStackTrace();
      return false;
    }

    toJoin.add(jarFragpipe.toAbsolutePath().normalize().toString());
    final String classpath = OsUtils.asSingleArgument(String.join(System.getProperties().getProperty("path.separator"), toJoin));

    int idx = 0;
    int batchNum = Math.min(2, nThreads);
    for (Map.Entry<String, LcmsFileGroup> e : lcmsFileGroups.entrySet()) {
      for (InputLcmsFile inputLcmsFile : e.getValue().lcmsFiles) {
        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        cmd.add("-Xmx" + ramGb + "G");
        if (extLibsBruker == null) {
          if (inputLcmsFile.getPath().toString().toLowerCase().endsWith(".d")) {
            SwingUtils.showErrorDialog(comp, "Could not find the Bruker library for " + inputLcmsFile.getPath().toAbsolutePath() + ". Please make sure that there are ext folder along with the MSFragger jar file.", "Error");
            return false;
          }
        } else {
          cmd.add(createJavaDParamString("libs.bruker.dir", extLibsBruker.toAbsolutePath().normalize().toString()));
        }
        if (extLibsThermo == null) {
          if (inputLcmsFile.getPath().toString().toLowerCase().endsWith(".raw")) {
            SwingUtils.showErrorDialog(comp, "Could not find the Thermo library for " + inputLcmsFile.getPath().toAbsolutePath() + ". Please make sure that there are ext folder along with the MSFragger jar file.", "Error");
            return false;
          }
        } else {
          cmd.add(createJavaDParamString("libs.thermo.dir", extLibsThermo.toAbsolutePath().normalize().toString()));
        }
        cmd.add("-cp");
        cmd.add(classpath);
        cmd.add(WriteSubMzml.class.getCanonicalName());
        cmd.add(inputLcmsFile.getPath().toAbsolutePath().normalize().toString());
        cmd.add(wd.resolve(e.getKey()).resolve("psm.tsv").toAbsolutePath().normalize().toString());
        cmd.add(wd.resolve(StringUtils.upToLastDot(inputLcmsFile.getPath().getFileName().toString()) + "_sub.mzML").toAbsolutePath().normalize().toString());
        cmd.add(probabilityThreshold + "");
        cmd.add("1");
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(wd.resolve(e.getKey()).toFile());
        pbis.add(new PbiBuilder().setPb(pb).setParallelGroup(getCmdName() + (idx / batchNum)).create());
        ++idx;
      }
    }

    isConfigured = true;
    return true;
  }
}
