package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.FragpipeLocations;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.exceptions.FileWritingException;
import com.dmtavt.fragpipe.tabs.TabWorkflow;
import com.dmtavt.fragpipe.tools.umpire.UmpirePanel;
import com.dmtavt.fragpipe.tools.umpire.UmpireParams;
import com.dmtavt.fragpipe.tools.umpire.UmpireSeGarbageFiles;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
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

  public List<InputLcmsFile> outputs(List<InputLcmsFile> inputs) {
    if (!isRun)
      return new ArrayList<>(inputs);

    List<InputLcmsFile> out = new ArrayList<>();
    for (InputLcmsFile f: inputs) {
      final String inputFn = f.getPath().getFileName().toString();
      final Path outPath = f.outputDir(wd);
      List<String> mgfs = getGeneratedMgfFnsForMzxml(inputFn);
      List<String> lcmsFns = getGeneratedLcmsFns(mgfs);
      for (String lcmsFn : lcmsFns) {
        out.add(new InputLcmsFile(outPath.resolve(lcmsFn), f.getGroup(), f.getReplicate(), f.getDataType()));
      }
    }
    return out;
  }

  public boolean configure(Component errMsgParent, boolean isDryRun,
      Path jarFragpipe, final Path binFragger, UmpirePanel umpirePanel,
      List<InputLcmsFile> lcmsFiles) {

    initPreConfig();

    List<Path> paths = FragpipeLocations
        .checkToolsMissing(Stream.of(UmpireParams.JAR_UMPIRESE_NAME));
    if (paths == null || paths.isEmpty()) {
      return false;
    }
    Path jarUmpireSe = paths.get(0);

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
        JOptionPane.showMessageDialog(errMsgParent,
            "[DIA Umpire SE]\nCould not write property file, thus can't run DIA-Umpire",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }
    }

    // run umpire for each file
    TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);
    int ramGb = tabWorkflow.getRamGb();
    int ram = ramGb > 0 ? ramGb : OsUtils.getDefaultXmx();
    final Path extLibsThermo = CmdMsfragger.searchExtLibsThermo(Collections.singletonList(binFragger.getParent()));
    final String javaDParmsStringLibsThermoDir = extLibsThermo == null ? null :
            createJavaDParamString("libs.thermo.dir", extLibsThermo.toString());
    final List<Path> classpathJars = FragpipeLocations.checkToolsMissing(Seq.of(UmpireParams.JAR_UMPIRESE_NAME).concat(JAR_DEPS));

    for (InputLcmsFile f: lcmsFiles) {
      Path inputFn = f.getPath().getFileName();
      Path inputDir = f.getPath().getParent();
      Path destDir = f.outputDir(wd);

      // Umpire-SE
      // java -jar -Xmx8G DIA_Umpire_SE.jar mzMXL_file diaumpire_se.params
      if(false) // for standalone jar
      {
        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        //commands.add("-d64");
        cmd.add("-jar");
        if (ram > 0 && ram < 256)
          cmd.add("-Xmx" + ram + "G");
        if (javaDParmsStringLibsThermoDir != null)
          cmd.add(javaDParmsStringLibsThermoDir);
        cmd.add(jarUmpireSe.toString()); // unpacked UmpireSE jar
        cmd.add(f.getPath().toString());
        cmd.add(umpireParamsFilePath.toString());

        ProcessBuilder pbUmpireSe = new ProcessBuilder(cmd);
        pbis.add(PbiBuilder.from(pbUmpireSe));
      }

      // Umpire-SE
      //java -Dbatmass.io.libs.thermo.dir=ext/thermo/ -cp batmass-io-1.23.0.jar:DIA_Umpire_SE.jar dia_umpire_se.DIA_Umpire_SE  (.raw|.mzML|.mzXML) DIA-U_params
      {
        List<String> cmd = new ArrayList<>();
        cmd.add(Fragpipe.getBinJava());
        if (ram > 0 && ram < 256)
          cmd.add("-Xmx" + ram + "G");
        if (javaDParmsStringLibsThermoDir != null)
          cmd.add(javaDParmsStringLibsThermoDir);

        cmd.add("-cp");
        cmd.add(constructClasspathString(classpathJars));
        cmd.add(JAR_DIA_UMPIRE_SE_MAIN_CLASS);

        cmd.add(f.getPath().toString());
        cmd.add(umpireParamsFilePath.toString());

        ProcessBuilder pbUmpireSe = new ProcessBuilder(cmd);
        pbis.add(PbiBuilder.from(pbUmpireSe));
      }

      // check if the working dir is the dir where the mzXML file was
      // if it is, then don't do anything, if it is not, then copy
      // UmpireSE outputs to the working directory
      // and also create symlinks to the original files


      if (!inputDir.equals(destDir)) {
        // destination dir is different from mzXML file location
        // need to move output and cleanup
        List<Path> garbage = UmpireSeGarbageFiles.getGarbageFiles(f.getPath(), true, true);
        List<ProcessBuilder> pbsMove = ToolingUtils.pbsMoveFiles(jarFragpipe, destDir, garbage);
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

  private List<String> getGeneratedMgfFnsForMzxml(String mzxmlFn) {
    String baseName = StringUtils.upToLastDot(mzxmlFn);
    final int n = 3;
    List<String> mgfs = new ArrayList<>(n);
    for (int i = 1; i <= n; i++) {
      mgfs.add(baseName + "_Q" + i + ".mgf");
    }
    return mgfs;
  }

  private List<String> getGeneratedLcmsFns(List<String> generatedMgfFns) {
    return generatedMgfFns.stream()
        .map(mgf -> StringUtils.upToLastDot(mgf) + "." + OUTPUT_EXT.toString())
        .collect(Collectors.toList());
  }
}
