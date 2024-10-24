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

package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.exceptions.FileWritingException;
import com.dmtavt.fragpipe.tools.umpire.UmpirePanel;
import com.dmtavt.fragpipe.tools.umpire.UmpireParams;
import com.dmtavt.fragpipe.tools.umpire.UmpireSeGarbageFiles;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JOptionPane;
import org.jooq.lambda.Seq;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdUmpireSe extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdUmpireSe.class);
  public static final String NAME = "UmpireSe";
  public static final String[] JAR_DEPS = {ToolingUtils.BATMASS_IO_JAR};
  public static final String JAR_DIA_UMPIRE_SE_MAIN_CLASS = "dia_umpire_se.DIA_Umpire_SE";
  private static final EXTENSION OUTPUT_EXT = EXTENSION.mzML;
  public enum EXTENSION {mzXML, mzML}

  public CmdUmpireSe(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public List<InputLcmsFile> outputs(List<InputLcmsFile> inputs, boolean generateQ1, boolean generateQ2, boolean generateQ3) {
    if (!isRun)
      return new ArrayList<>(inputs);

    List<InputLcmsFile> out = new ArrayList<>();
    for (InputLcmsFile f: inputs) {
      if (f.getDataType().contentEquals("DDA") || f.getDataType().contentEquals("DIA-Quant") || f.getDataType().contentEquals("DDA+")) {
        out.add(f);
      } else {
        final String inputFn = f.getPath().getFileName().toString();
        final Path outPath = f.outputDir(wd);
        List<String> mgfs = getGeneratedMgfFnsForMzxml(inputFn, generateQ1, generateQ2, generateQ3);
        List<String> lcmsFns = getGeneratedLcmsFns(mgfs);
        for (String lcmsFn : lcmsFns) {
          out.add(new InputLcmsFile(outPath.resolve(lcmsFn), f.getGroup(), f.getReplicate(), "DDA"));
        }
      }
    }
    return out;
  }

  public boolean configure(Component errMsgParent,
      boolean isDryRun,
      Path jarFragpipe,
      int ramGb,
      final Path extLibsThermo,
      UmpirePanel umpirePanel,
      List<InputLcmsFile> lcmsFiles) {

    initPreConfig();

    List<Path> paths = FragpipeLocations
        .checkToolsMissing(Stream.of(UmpireParams.JAR_UMPIRESE_NAME));
    if (paths == null || paths.isEmpty()) {
      return false;
    }

    // write umpire params file
    final UmpireParams collectedUmpireParams = umpirePanel.collect();
    final String umpireParamsFileName =
        UmpireParams.FILE_BASE_NAME + "." + UmpireParams.FILE_BASE_EXT;
    final Path umpireParamsFilePath = wd.resolve(umpireParamsFileName);
    if (!isDryRun) {
      try {
        FileOutputStream fos = new FileOutputStream(umpireParamsFilePath.toFile());
        PropertiesUtils.writePropertiesContent(collectedUmpireParams, fos);
      } catch (FileNotFoundException | FileWritingException e) {
        if (Fragpipe.headless) {
          log.error("Could not write property file, thus can't run DIA-Umpire.");
        } else {
          JOptionPane.showMessageDialog(errMsgParent, "[DIA Umpire SE]\nCould not write property file, thus can't run DIA-Umpire", "Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }
    }

    // run umpire for each file
    final String javaDParmsStringLibsThermoDir = extLibsThermo == null ? null :
            createJavaDParamString("libs.thermo.dir", extLibsThermo.toString());
    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(UmpireParams.JAR_UMPIRESE_NAME).concat(JAR_DEPS));

    for (InputLcmsFile f : lcmsFiles) {
      if (f.getDataType().contentEquals("DDA") || f.getDataType().contentEquals("DIA-Quant") || f.getDataType().contentEquals("DDA+")) {
        continue;
      }

      Path inputDir = f.getPath().toAbsolutePath().getParent();
      Path destDir = f.outputDir(wd);

      // Umpire-SE
      //java -Dbatmass.io.libs.thermo.dir=ext/thermo/ -cp batmass-io-1.23.0.jar:DIA_Umpire_SE.jar dia_umpire_se.DIA_Umpire_SE  (.raw|.mzML|.mzXML) DIA-U_params
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-Xmx" + ramGb + "G");
      if (javaDParmsStringLibsThermoDir != null)
        cmd.add(javaDParmsStringLibsThermoDir);

      cmd.add("-cp");
      cmd.add(constructClasspathString(classpathJars));
      cmd.add(JAR_DIA_UMPIRE_SE_MAIN_CLASS);

      cmd.add(f.getPath().toString());
      cmd.add(umpireParamsFilePath.toString());

      ProcessBuilder pbUmpireSe = new ProcessBuilder(cmd);
      pbis.add(PbiBuilder.from(pbUmpireSe));

      // check if the working dir is the dir where the mzXML file was
      // if it is, then don't do anything, if it is not, then copy
      // UmpireSE outputs to the working directory
      // and also create symlinks to the original files


      if (!inputDir.equals(destDir)) {
        // destination dir is different from mzXML file location
        // need to move output and cleanup
        List<Path> garbage = UmpireSeGarbageFiles.getGarbageFiles(f.getPath(), true, true);
        List<ProcessBuilder> pbsMove = ToolingUtils.pbsMoveFiles(jarFragpipe, destDir, true, garbage);
        pbis.addAll(PbiBuilder.from(pbsMove));
      }

      // delete garbage files
      final List<Path> garbage = UmpireSeGarbageFiles.getGarbageFiles(destDir.resolve(f.getPath().getFileName()), false, false);
      final List<ProcessBuilder> pbsDeleteFiles = ToolingUtils.pbsDeleteFiles(jarFragpipe, garbage);
      pbis.addAll(PbiBuilder.from(pbsDeleteFiles));
    }

    isConfigured = true;
    return true;
  }

  private List<String> getGeneratedMgfFnsForMzxml(String mzxmlFn, boolean generateQ1, boolean generateQ2, boolean generateQ3) {
    String baseName = StringUtils.upToLastDot(mzxmlFn);
    final int n = 3;
    List<String> mgfs = new ArrayList<>(n);
    if (generateQ1) {
      mgfs.add(baseName + "_Q1.mgf");
    }
    if (generateQ2) {
      mgfs.add(baseName + "_Q2.mgf");
    }
    if (generateQ3) {
      mgfs.add(baseName + "_Q3.mgf");
    }
    return mgfs;
  }

  private List<String> getGeneratedLcmsFns(List<String> generatedMgfFns) {
    return generatedMgfFns.stream().map(mgf -> StringUtils.upToLastDot(mgf) + "." + OUTPUT_EXT).collect(Collectors.toList());
  }
}
