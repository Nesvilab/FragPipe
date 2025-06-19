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

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.FragpipeLocations;
import org.nesvilab.utils.StringUtils;

public class CmdMetaproteomics extends CmdBase {

  public static String NAME = "Metaproteomics";
  public static String VERSION = "1.0.1";
  public static String JAR_NAME = "FragPipe-Meta-" + VERSION + ".jar";
  private String taxonNameFile;
  private String taxonNodeFile;

  public CmdMetaproteomics(boolean isRun, Path workDir) {
    super(isRun, workDir);
    
    taxonNameFile = FragpipeLocations.get().getDirTools().resolve("metaproteomics").resolve("names.dmp").toAbsolutePath().normalize().toString();
    taxonNodeFile = FragpipeLocations.get().getDirTools().resolve("metaproteomics").resolve("nodes.dmp").toAbsolutePath().normalize().toString();
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component component,
      int ramGb,
      String fastaPath,
      String decoyTag,
      float qvalue,
      float deltaHyperscore,
      int minPeptCntPerProt,
      int minUniqPeptCntPerProt,
      int minUniqPeptCnt,
      String hostName,
      int iterations,
      String cmdLineOpts) {
    initPreConfig();

    
    List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Stream.of(JAR_NAME));
    if (classpathJars == null) {
      return false;
    }

    List<String> cmd = new ArrayList<>();
    cmd.add(Fragpipe.getBinJava());
    if (Fragpipe.headless) {
      cmd.add("-Djava.awt.headless=true");
    }
    cmd.add("-Xmx" + ramGb + "G");
    cmd.add("-jar");
    cmd.add(constructClasspathString(classpathJars));
    cmd.add("DbOptimizer");
    cmd.add("--projectDir");
    cmd.add(wd.toAbsolutePath().normalize().toString());
    cmd.add("--fastaFile");
    cmd.add(fastaPath);
    cmd.add("--outdir");
    cmd.add(wd.toAbsolutePath().normalize().toString());
    cmd.add("--decoyTag");
    cmd.add(decoyTag);
    cmd.add("--qvalue");
    cmd.add(String.valueOf(qvalue));
    cmd.add("--deltaHyperscore");
    cmd.add(String.valueOf(deltaHyperscore));
    cmd.add("--minPeptCntPerProt");
    cmd.add(String.valueOf(minPeptCntPerProt));
    cmd.add("--minUniqPeptCntPerProt");
    cmd.add(String.valueOf(minUniqPeptCntPerProt));
    cmd.add("--minUniqPeptCnt");
    cmd.add(String.valueOf(minUniqPeptCnt));
    cmd.add("--taxonNameFile");
    cmd.add(taxonNameFile.toString());
    cmd.add("--taxonNodeFile");
    cmd.add(taxonNodeFile.toString());
    cmd.add("--hostName");
    cmd.add(StringUtils.appendPrependOnce(hostName, "\""));
    cmd.add("--iterations");
    cmd.add(String.valueOf(iterations));
    if (cmdLineOpts != null && !cmdLineOpts.trim().isEmpty()) {
      cmd.add(cmdLineOpts.trim());
    }

    ProcessBuilder pb = new ProcessBuilder(cmd);
    pb.directory(wd.toFile());
    pbis.add(PbiBuilder.from(pb));
  
    isConfigured = true;
    return true;
  }

}
