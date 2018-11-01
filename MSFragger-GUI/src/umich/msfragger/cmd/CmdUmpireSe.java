package umich.msfragger.cmd;

import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Date;
import java.util.Deque;
import java.util.List;
import javax.swing.JOptionPane;
import org.apache.commons.lang3.NotImplementedException;
import umich.msfragger.exceptions.FileWritingException;
import umich.msfragger.gui.InputLcmsFile;
import umich.msfragger.gui.ToolingUtils;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.umpire.UmpirePanel;
import umich.msfragger.params.umpire.UmpireParams;
import umich.msfragger.params.umpire.UmpireSeGarbageFiles;
import umich.msfragger.util.FileMove;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.UsageTrigger;

public class CmdUmpireSe {
  final boolean isRun;
  final Path wd;
  final Deque<ProcessBuilder> pbs;
  boolean isConfigured;


  public CmdUmpireSe(boolean isRun, Path workDir) {
    this.isRun = isRun;
    this.wd = workDir;
    this.pbs = new ArrayDeque<>();
  }

  public boolean isRun() {
    return isRun;
  }

  public Path getWd() {
    return wd;
  }

  public List<InputLcmsFile> outputs(List<InputLcmsFile> inputs) {
    if (!isRun)
      return new ArrayList<>(inputs);
    throw new NotImplementedException("TODO"); // TODO: Not implemented
  }

  public List<ProcessBuilder> processBuilders() {
    if (!isConfigured)
      throw new IllegalStateException("Call to #processBuilders() before calling #configure()");
    return new ArrayList<>(pbs);
  }

  public boolean configure(Component errMsgParent, boolean isDryRun,
      Path jarFragpipe, UsageTrigger philo, UmpirePanel umpirePanel,
      List<InputLcmsFile> lcmsFiles) {
    if (!isRun)
      return false;

    // check if there are only mzXML input files
    boolean hasNonMzxml = lcmsFiles.stream().map(f -> f.path.getFileName().toString().toLowerCase())
        .anyMatch(p -> !p.endsWith("mzxml"));
    if (hasNonMzxml) {
      JOptionPane.showMessageDialog(errMsgParent,
          "[DIA Umpire SE]\nNot all input files are mzXML.\n"
              + "DIA-Umpire only supports mzXML.",
          "Error", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    // unpack Umpire jar
    Path jarUmpireSe;
    try {
      jarUmpireSe = JarUtils.unpackFromJar(ToolingUtils.class,"/" + UmpireParams.JAR_UMPIRESE_NAME,
          ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);

    } catch (IOException | NullPointerException ex) {
      JOptionPane.showMessageDialog(errMsgParent,
          "Could not unpack UmpireSE jar to a temporary directory.\n",
          "Can't unpack", JOptionPane.ERROR_MESSAGE);
      return false;
    }

    // write umpire params file
    final DateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    final String dateStr = df.format(new Date());
    final UmpireParams collectedUmpireParams = umpirePanel.collect();
    final String umpireParamsFileName =
        UmpireParams.FILE_BASE_NAME + "_" + dateStr + "." + UmpireParams.FILE_BASE_EXT;
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
    int ramGb = (Integer)umpirePanel.spinnerRam.getValue();
    int ram = ramGb > 0 ? ramGb : 0;

    for (InputLcmsFile f: lcmsFiles) {
      Path inputFn = f.path.getFileName();
      Path inputDir = f.path.getParent();
      Path dest = wd.resolve(f.experiment);

      // Umpire-SE
      // java -jar -Xmx8G DIA_Umpire_SE.jar mzMXL_file diaumpire_se.params
      List<String> cmd = new ArrayList<>();
      cmd.add("java");
      //commands.add("-d64");
      cmd.add("-jar");
      if (ram > 0 && ram < 256)
        cmd.add("-Xmx" + ram + "G");
      cmd.add(jarUmpireSe.toString()); // unpacked UmpireSE jar
      cmd.add(f.path.toString());
      cmd.add(umpireParamsFilePath.toString());

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pbs.add(pb);

      // check if the working dir is the dir where the mzXML file was
      // if it is, then don't do anything, if it is not, then copy
      // UmpireSE outputs to the working directory
      // and also create symlinks to the original files


      if (!inputDir.equals(dest)) {
        // destination dir is different from mzXML file location
        // need to move output and cleanup
        UmpireSeGarbageFiles garbage = UmpireSeGarbageFiles.create(f.path);
        for (Path pGarbage : garbage.toMove) {
          List<String> cmdMove = new ArrayList<>();
          cmdMove.add("java");
          cmdMove.add("-cp");
          cmdMove.add(jarFragpipe.toString());
          // TODO: verify this actually moves all the garbage files. Use files at: C:\data\dia\40-50-minutes
          cmdMove.add(FileMove.class.getCanonicalName());
          String origin = pGarbage.toString();
          String destination = f.outputDir(wd).resolve(pGarbage.getFileName()).toString();
          cmdMove.add(origin);
          cmdMove.add(destination);
          pbs.add(new ProcessBuilder(cmdMove));
        }
      }

      // msconvert
      // now all the generated garbage is in the working directory
      final boolean isWin = OsUtils.isWindows();
      final String binMsconvert = umpirePanel.getBinMsconvert();
      if (isWin && StringUtils.isNullOrWhitespace(binMsconvert)) {
        JOptionPane.showMessageDialog(errMsgParent,
            "[DIA Umpire SE]\nOn Windows specifying path to msconvert binary is required.",
            "Error", JOptionPane.ERROR_MESSAGE);
        return false;
      }

      List<String> mgfs = getGeneratedMgfsForMzxml(inputFn.toString());
      for (String mgf : mgfs) {
        List<String> cmdMsConvert = new ArrayList<>();

        if (isWin) {
          cmdMsConvert.add(binMsconvert);
          cmdMsConvert.add("--verbose");
          cmdMsConvert.add("--32");
          cmdMsConvert.add("--zlib");
          cmdMsConvert.add("--mzXML");
          cmdMsConvert.add("--outdir");
          cmdMsConvert.add(f.outputDir(wd).toString());
        } else {
          // on Linux philosopher includes msconvert
          cmdMsConvert.add(philo.useBin());
          cmdMsConvert.add("--format");
          cmdMsConvert.add("mzXML");
          cmdMsConvert.add("--intencoding");
          cmdMsConvert.add("32");
          cmdMsConvert.add("--mzencoding");
          cmdMsConvert.add("32");
          cmdMsConvert.add("--zlib");
        }
        cmdMsConvert.add(f.outputDir(wd).resolve(mgf).toString());
        pbs.add(new ProcessBuilder(cmdMsConvert));
      }
    }

    isConfigured = true;
    return true;
  }

  private List<String> getGeneratedMgfsForMzxml(String mzxmlFn) {
    String baseName = StringUtils.upToLastDot(mzxmlFn);
    final int n = 3;
    List<String> mgfs = new ArrayList<>(n);
    for (int i = 1; i <= n; i++) {
      mgfs.add(baseName + "_Q" + i + ".mgf");
    }
    return mgfs;
  }
}
