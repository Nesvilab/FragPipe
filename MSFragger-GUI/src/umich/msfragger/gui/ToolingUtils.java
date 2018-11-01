package umich.msfragger.gui;

import static umich.msfragger.util.PathUtils.testBinaryPath;
import static umich.msfragger.util.PathUtils.testFilePath;

import java.awt.Component;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import umich.msfragger.Version;
import umich.msfragger.exceptions.FileWritingException;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.crystalc.CrystalcProps;
import umich.msfragger.params.dbslice.DbSlice;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.params.pepproph.PeptideProphetParams;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.protproph.ProteinProphetParams;
import umich.msfragger.params.speclib.SpecLibGen;
import umich.msfragger.params.umpire.UmpirePanel;
import umich.msfragger.params.umpire.UmpireParams;
import umich.msfragger.params.umpire.UmpireSeGarbageFiles;
import umich.msfragger.util.FileMove;
import umich.msfragger.util.Holder;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.PropertiesUtils;
import umich.msfragger.util.PythonInfo;
import umich.msfragger.util.UsageTrigger;
import umich.msfragger.util.StringUtils;

public class ToolingUtils {
  private String binJava = null;
  private ToolingUtils() {}


  /**
   * @return Full absolute normalized path to the output combined protein file.
   */
  public static Path getCombinedProtFilePath(String combinedProtFn, Path workingDir) {
    combinedProtFn = combinedProtFn.trim();
    final String ext = ".prot.xml";
    if (!combinedProtFn.toLowerCase().endsWith(ext)) {
      combinedProtFn = combinedProtFn + ext;
    }
    return workingDir.resolve(combinedProtFn).normalize().toAbsolutePath();
  }

  /**
   * @param jarUri Use {@link PathUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsCopyFiles(URI jarUri, Path destination,
      List<Path> files) {
    if (jarUri == null) {
      throw new IllegalArgumentException("JAR URI must ne non null");
    }
    List<ProcessBuilder> pbs = new LinkedList<>();
    String jarPath = Paths.get(jarUri).toAbsolutePath().toString();

    for (Path file : files) {
      if (destination.equals(file.getParent())) {
        continue;
      }
      List<String> cmd = new ArrayList<>();
      cmd.add("java");
      cmd.add("-cp");
      cmd.add(jarPath);
      cmd.add(FileMove.class.getCanonicalName());
      cmd.add(file.toAbsolutePath().normalize().toString());
      cmd.add(destination.resolve(file.getFileName()).toString());
      pbs.add(new ProcessBuilder(cmd));
    }
    return pbs;
  }

  public static Map<InputLcmsFile, Path> getPepxmlFilePathsAfterSearch(List<InputLcmsFile> lcmsFiles, String ext) {
    HashMap<InputLcmsFile, Path> pepxmls = new HashMap<>();
    for (InputLcmsFile f : lcmsFiles)
      pepxmls.put(f, Paths.get(StringUtils.upToLastDot(f.path.toString()) + "." + ext));
    return pepxmls;
  }

  public static Map<InputLcmsFile, Path> getPepxmlFilePathsAfterMove(
      Map<InputLcmsFile, Path> pepxmlFilesAfterSearch, Path workingDir, boolean crystalC,
      boolean isProcessGroupsSeparately) {
    HashMap<InputLcmsFile, Path> pepxmls = new HashMap<>();
    for (Map.Entry<InputLcmsFile, Path> entry : pepxmlFilesAfterSearch.entrySet()) {
      InputLcmsFile f = entry.getKey();
      Path dirty = entry.getValue();
      String fn = dirty.getFileName().toString();
      String fnMod = !crystalC
          ? fn
          : StringUtils.upToLastDot(fn) + "_c." + StringUtils.afterLastDot(fn);
      Path pepxmlClean = isProcessGroupsSeparately
        ? workingDir.resolve(fnMod).toAbsolutePath()
        : workingDir.resolve(f.experiment).resolve(fnMod).toAbsolutePath();
      pepxmls.put(f, pepxmlClean);
    }
    return pepxmls;
  }

  public static Map<InputLcmsFile, Path> getPepxmlInteractFilePaths(
      Map<InputLcmsFile, Path> cleanPepXmls, String pepxmlExt) {

    HashMap<InputLcmsFile, Path> interacts = new HashMap<>();
    for (Map.Entry<InputLcmsFile, Path> entry : cleanPepXmls.entrySet()) {
      InputLcmsFile lcms = entry.getKey();
      final Path pepxmlClean = entry.getValue();
      final String cleanFn = pepxmlClean.getFileName().toString();
      final Path cleanDir = pepxmlClean.getParent();

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
        throw new IllegalStateException(String.format("Could not identify the extension for file: %s", pepxmlClean));
      }

      Path interactXml = cleanDir.resolve("interact-" + nameWithoutExt + "pep.xml").toAbsolutePath();
      interacts.put(lcms, interactXml);
    }
    return interacts;
  }

  public static List<ProcessBuilder> pbsFragger(Component comp, String programsDir, String workingDir, List<InputLcmsFile> lcmsFilePaths,
      boolean isDryRun, FraggerPanel fp, URI jar, String msfraggerPath, DbSlice dbslice) {
    if (jar == null)
      throw new IllegalArgumentException("Argument JAR can't be null");
    List<ProcessBuilder> builders = new LinkedList<>();
    if (fp.isRunMsfragger()) {

      final int numSlices = fp.getNumSlices();
      final boolean isSlicing = numSlices > 1;
      if (isSlicing) {
        // slicing requested
        if (!dbslice.isInitialized()) {
          JOptionPane.showMessageDialog(comp,
              "MSFragger number of DB slices requested was more than 1.\n"
                  + "However not all preconditions for enabling slicing were met.\n"
                  + "Check the bottom of \"Config\" tab for details.",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }
      }

      String bin = msfraggerPath;
      if (StringUtils.isNullOrWhitespace(bin)) {
        JOptionPane.showMessageDialog(comp, "Binary for running Fragger can not be an empty string.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      bin = testFilePath(bin, programsDir);
      if (bin == null) {
        JOptionPane.showMessageDialog(comp, "Binary for running Fragger not found or could not be run.\n"
                + "Neither on PATH, nor in the working directory",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }

      String fastaPath = fp.getFastaPath();
      if (StringUtils.isNullOrWhitespace(fastaPath)) {
        JOptionPane.showMessageDialog(comp, "Fasta file path (Fragger) can't be empty",
            "Warning", JOptionPane.WARNING_MESSAGE);
        return null;
      }

      // create a params file in the output directory
      MsfraggerParams params = null;
      try {
        params = fp.collectParams();
      } catch (IOException ex) {
        JOptionPane.showMessageDialog(comp, "Could not collect MSFragger params from GUI.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      Path savedParamsPath = Paths.get(workingDir, MsfraggerParams.DEFAULT_FILE);
      if (!isDryRun) {
        try {
          params.save(new FileOutputStream(savedParamsPath.toFile()));
          // cache the params
          params.save();
        } catch (IOException ex) {
          JOptionPane.showMessageDialog(comp,
              "Could not save fragger.params file to working dir.\n",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }
      }

      int ramGb = fp.getRamGb();

      StringBuilder sb = new StringBuilder();
      // 32k symbols splitting for regular command.
      // But for slicing it's all up to the python script.
      //final int commandLenLimit = isSlicing ? Integer.MAX_VALUE : 1 << 15;
      final int commandLenLimit = 1 << 15;


      final String currentJarPath = Paths.get(jar).toAbsolutePath().toString();
      final Path wdPath = Paths.get(workingDir);


      if (isSlicing) {
        // schedule to always try to delete the temp dir when FragPipe finishes execution
        final String tempDirName = "split_peptide_index_tempdir";
        Path toDelete = wdPath.resolve(tempDirName).toAbsolutePath().normalize();
        toDelete.toFile().deleteOnExit();
      }

      int fileIndex = 0;

      while (fileIndex < lcmsFilePaths.size()) {
        int fileIndexLo = fileIndex;
        ArrayList<String> cmd = new ArrayList<>();
        if (isSlicing) {
          cmd.add(PythonInfo.get().getCommand());
          cmd.add(dbslice.getScriptDbslicingPath().toAbsolutePath().normalize().toString());
          cmd.add(Integer.toString(numSlices));
          cmd.add("\"");
        }
        cmd.add("java");
        cmd.add("-jar");
        if (ramGb > 0) {
          cmd.add("-Xmx" + ramGb + "G");
        }
        if (isSlicing) {
          cmd.add("\"");
        }
        cmd.add(bin);
        cmd.add(savedParamsPath.toString());

        for (String s : cmd) {
          sb.append(s).append(" ");
        }
        if (sb.length() > commandLenLimit) {
          JOptionPane.showMessageDialog(comp, "MSFragger command line length too large even for a single file.",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }

        while (fileIndex < lcmsFilePaths.size()) {
          InputLcmsFile f = lcmsFilePaths.get(fileIndex);
          if (sb.length() + f.path.toString().length() + 1 > commandLenLimit) {
            break;
          }
          sb.append(f.path.toString()).append(" ");
          cmd.add(f.path.toString());
          fileIndex++;
        }

        builders.add(new ProcessBuilder(cmd));
        sb.setLength(0);

        // move the files if the output directory is not the same as where
        // the lcms files were
        Map<InputLcmsFile, Path> mapRawToPep = getPepxmlFilePathsAfterSearch(lcmsFilePaths, params.getOutputFileExtension());
        for (int i = fileIndexLo; i < fileIndex; i++) {
          Path pepPath = mapRawToPep.get(lcmsFilePaths.get(i));

          if (!wdPath.equals(pepPath.getParent())) {
            ArrayList<String> cmdMove = new ArrayList<>();
            cmdMove.add("java");
            cmdMove.add("-cp");
            cmdMove.add(currentJarPath);
            cmdMove.add(FileMove.class.getCanonicalName());
            String origin = pepPath.toAbsolutePath().toString();
            String destination = Paths.get(wdPath.toString(), pepPath.getFileName().toString()).toString();
            cmdMove.add(origin);
            cmdMove.add(destination);
            ProcessBuilder pbFileMove = new ProcessBuilder(cmdMove);
            builders.add(pbFileMove);
          }
        }
      }
    }


    return builders;
  }

  public static String getBinJava(Component errroDialogParent, String programsDir) {
    String binJava = "java";
    synchronized (ToolingUtils.class) {
      binJava = testBinaryPath(binJava, programsDir);
      if (binJava != null) {
        return binJava;
      }
    }
    JOptionPane.showMessageDialog(errroDialogParent, "Java could not be found.\n"
        + "please make sure you have it installed \n"
        + "and that java.exe can be found on PATH", "Error", JOptionPane.ERROR_MESSAGE);
    return null;
  }

  public static List<ProcessBuilder> pbsMsadjuster(URI jarUri, Component comp, String workingDir,
      List<InputLcmsFile> lcmsFiles, FraggerPanel fp, boolean cleanUp) {
    List<ProcessBuilder> pbs = new LinkedList<>();

    if (fp.isRunMsfragger() && fp.isMsadjuster()) {
      String currentJarPath = Paths.get(jarUri).toAbsolutePath().toString();
      Path wd = Paths.get(workingDir);

      Path jarMsadjusterPath;
      Path jarDepsPath;
      try {
        // common deps
        jarDepsPath = JarUtils
            .unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_COMMON_DEPS,
            ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);
        // msadjuster jar
        jarMsadjusterPath = JarUtils
            .unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_MSADJUSTER_NAME,
            ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);

      } catch (IOException | NullPointerException ex) {
        JOptionPane.showMessageDialog(comp,
            "Could not unpack tools to a temporary directory.\n"
                + "Disable precursor mass adjustment in MSFragger tab.",
            "Can't unpack", JOptionPane.ERROR_MESSAGE);
        return null;
      }

      int ramGb = fp.getRamGb();

      for (InputLcmsFile f : lcmsFiles) {

        if (!cleanUp) {
          ArrayList<String> cmd = new ArrayList<>();
          cmd.add("java");
          if (ramGb > 0) {
            cmd.add("-Xmx" + ramGb + "G");
          }
          if (jarDepsPath != null) {
            cmd.add("-cp");
            List<String> toJoin = new ArrayList<>();
            toJoin.add(jarDepsPath.toString());
            toJoin.add(jarMsadjusterPath.toString());
            final String sep = System.getProperties().getProperty("path.separator");
            cmd.add("\"" + org.apache.commons.lang3.StringUtils.join(toJoin, sep) + "\"");
            cmd.add(CrystalcProps.JAR_MSADJUSTER_MAIN_CLASS);
          } else {
            cmd.add("-jar");
            cmd.add(jarMsadjusterPath.toAbsolutePath().normalize().toString());
          }
          cmd.add("20");
          cmd.add(f.path.toString());
          pbs.add(new ProcessBuilder(cmd));

        } else {

          // cleanup
          ArrayList<String> cmd = new ArrayList<>();
          cmd.add("java");
          cmd.add("-cp");
          cmd.add(currentJarPath);
          cmd.add(FileMove.class.getCanonicalName());
          String origin =  StringUtils.upToLastDot(f.path.toString()) + ".ma"; // MSAdjuster creates these files
          String destination = wd.resolve(Paths.get(origin).getFileName().toString()).toString();
          cmd.add(origin);
          cmd.add(destination);
          pbs.add(new ProcessBuilder(cmd));
        }
      }
    }
    return pbs;
  }

  /**
   * @param ccParams Get these by calling {@link MsfraggerGuiFrame#crystalcFormToParams()}.
   */
  public static List<ProcessBuilder> pbsCrystalc(Component comp, FraggerPanel fp,
      CrystalcParams ccParams, boolean isDryRun, boolean isCrystalc, String workingDir,
      String fastaPath, List<InputLcmsFile> lcmsFiles, boolean isProcessGroupsSeparately) {
    List<ProcessBuilder> pbs = new LinkedList<>();
    if (isCrystalc) {
      Path wd = Paths.get(workingDir);

      Path jarPath;
      Path depsPath;
      try {
        // common deps
        depsPath = JarUtils
            .unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_COMMON_DEPS,
                ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);
        // msadjuster jar
        jarPath = JarUtils.unpackFromJar(ToolingUtils.class, "/" + CrystalcProps.JAR_CRYSTALC_NAME,
            ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);

      } catch (IOException | NullPointerException ex) {
        JOptionPane.showMessageDialog(comp,
            "Could not unpack tools to a temporary directory.\n"
                + "Disable Crystal-C.",
            "Can't unpack", JOptionPane.ERROR_MESSAGE);
        return null;
      }

      // check if all input files are in the same folder
      Set<Path> dirs = lcmsFiles.stream()
          .map(f -> f.path.getParent().toAbsolutePath().normalize())
          .collect(Collectors.toSet());
      Set<String> exts = lcmsFiles.stream()
          .map(f -> StringUtils.afterLastDot(f.path.getFileName().toString()))
          .collect(Collectors.toSet());
      boolean anyMatch = exts.stream().map(String::toLowerCase)
          .anyMatch(ext -> !("mzml".equals(ext) || "mzxml".equals(ext)));
      if (exts.isEmpty() || anyMatch) {
        String foundExts = String.join(", ", exts);
        JOptionPane.showMessageDialog(comp,
            "Crystal-C only supports mzML and mzXML input files.\n" +
                "The following LCMS file extensions found: " + foundExts + ".\n"
                + "Disable Crystal-C.",
            "Unsupported by Crystal-C", JOptionPane.ERROR_MESSAGE);
        return null;
      }

      int ramGb = fp.getRamGb();
      final String pepxmlExtFragger = fp.getOutputFileExt();
      if (!"pepxml".equals(pepxmlExtFragger.toLowerCase())) {
        JOptionPane.showMessageDialog(comp,
            "Crystal-C only pepXML file extension.\n"
                + "Switch to pepXML in MSFragger options or Disable Crystal-C.",
            "Unsupported by Crystal-C", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      Map<InputLcmsFile, Path> pepxmlDirty = getPepxmlFilePathsAfterSearch(lcmsFiles, pepxmlExtFragger);
      Map<InputLcmsFile, Path> pepxmlClean = getPepxmlFilePathsAfterMove(pepxmlDirty, wd, false, isProcessGroupsSeparately);
      final String ccParamsFilePrefix = "crystalc";
      final String ccParamsFileSuffix = ".params";

      if (dirs.size() == 1 && exts.size() == 1) {
        // everythin with the same extension in the same folder
        CrystalcParams p;
        Path ccParamsPath = wd.resolve(ccParamsFilePrefix + ccParamsFileSuffix);
        try {
          p = ccParams;
          String ext = exts.iterator().next();
          Path dir = dirs.iterator().next();
          p.setRawFileExt(ext);
          p.setRawDirectory(dir.toString());
          p.setOutputFolder(workingDir);
          p.setFasta(fastaPath);
          if (!isDryRun) {
            Files.deleteIfExists(ccParamsPath);
            p.save(Files.newOutputStream(ccParamsPath, StandardOpenOption.CREATE));
          }
        } catch (IOException ex) {
          JOptionPane.showMessageDialog(comp,
              "Could not create Crystal-C parameter file.",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }

        List<String> cmd = new ArrayList<>();
        cmd.add("java");
        if (ramGb > 0) {
          cmd.add("-Xmx" + ramGb + "G");
        }
        cmd.add("-cp");
        List<String> toJoin = new ArrayList<>();
        toJoin.add(depsPath.toAbsolutePath().normalize().toString());
        toJoin.add(jarPath.toAbsolutePath().normalize().toString());
        final String sep = System.getProperties().getProperty("path.separator");
        cmd.add("\"" + org.apache.commons.lang3.StringUtils.join(toJoin, sep) + "\"");
        cmd.add(CrystalcProps.JAR_CRYSTALC_MAIN_CLASS);
        cmd.add(ccParamsPath.toString());
        cmd.addAll(pepxmlClean.values().stream().map(Path::toString).collect(Collectors.toList()));

        pbs.add(new ProcessBuilder(cmd));

      } else {

        // multiple raw file extensions or multiple lcms file locaitons
        // issue a separate command for each pepxml file
        int index = -1;
        for (Map.Entry<InputLcmsFile, Path> kv : pepxmlClean.entrySet()) {
          Path lcms = kv.getKey().path.toAbsolutePath().normalize();
          String lcmsFn = lcms.getFileName().toString();
          Path pepxml = kv.getValue().toAbsolutePath().normalize();
          String pepxmlFn = pepxml.getFileName().toString();

          CrystalcParams p;
          Path ccParamsPath = wd.resolve(ccParamsFilePrefix + "-" + (++index) + "-" + pepxmlFn + ccParamsFileSuffix);
          try {
            p = ccParams;
            String ext = StringUtils.afterLastDot(lcmsFn);
            Path dir = lcms.getParent();
            p.setRawFileExt(ext);
            p.setRawDirectory(dir.toString());
            p.setOutputFolder(workingDir);
            p.setFasta(fastaPath);
            if (!isDryRun) {
              Files.deleteIfExists(ccParamsPath);
              p.save(Files.newOutputStream(ccParamsPath, StandardOpenOption.CREATE));
            }
          } catch (IOException ex) {
            JOptionPane.showMessageDialog(comp,
                "Could not create Crystal-C parameter file.",
                "Error", JOptionPane.ERROR_MESSAGE);
            return null;
          }

          List<String> cmd = new ArrayList<>();
          cmd.add("java");
          if (ramGb > 0) {
            cmd.add("-Xmx" + ramGb + "G");
          }
          cmd.add("-cp");
          List<String> toJoin = new ArrayList<>();
          toJoin.add(depsPath.toAbsolutePath().normalize().toString());
          toJoin.add(jarPath.toAbsolutePath().normalize().toString());
          final String sep = System.getProperties().getProperty("path.separator");
          cmd.add("\"" + org.apache.commons.lang3.StringUtils.join(toJoin, sep) + "\"");
          cmd.add(CrystalcProps.JAR_CRYSTALC_MAIN_CLASS);
          cmd.add(ccParamsPath.toString());
          cmd.add(pepxml.toString());

          pbs.add(new ProcessBuilder(cmd));
        }
      }

    }
    return pbs;
  }

  public static ProcessBuilder pbsPhilosopherWorkspaceInit(String binPhilosopher, Path workDir) {
    ProcessBuilder pb = new ProcessBuilder(Arrays.asList(binPhilosopher, "workspace", "--init"));
    pb.directory(workDir.toFile());
    return pb;
  }

  public static ProcessBuilder pbsPhilosopherWorkspaceClean(String binPhilosopher, Path workDir) {
    ProcessBuilder pb = new ProcessBuilder(Arrays.asList(binPhilosopher, "workspace", "--clean"));
    pb.directory(workDir.toFile());
    return pb;
  }

  /**
   * Creates the ProcessBuilders for running PeptideProphet.
   *
   * @return null in case of errors, or a list of process builders.
   */
  public static List<ProcessBuilder> pbsPeptideProphet(boolean isRun, Component comp, FraggerPanel fp,
      boolean isCrystalc, String binPhilosopher, UsageTrigger isPhiloUsed,
      String seqDbPath, String textPepProphCmd, String programsDir, Path workingDir,
      List<InputLcmsFile> lcmsFiles, boolean isProcessGroupsSeparately) {
    List<ProcessBuilder> builders = new LinkedList<>();
    if (isRun) {
      if (StringUtils.isNullOrWhitespace(binPhilosopher)) {
        JOptionPane.showMessageDialog(comp, "Philosopher (PeptideProphet) binary can not be an empty string.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      binPhilosopher = testBinaryPath(binPhilosopher, programsDir);
      if (binPhilosopher == null) {
        JOptionPane.showMessageDialog(comp, "Philosopher (PeptideProphet) binary not found.\n"
                + "Neither on PATH, nor in the working directory",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      isPhiloUsed.setUsed(true);

      String fastaPath = seqDbPath;

      if (StringUtils.isNullOrWhitespace(fastaPath)) {
        JOptionPane.showMessageDialog(comp, "Fasta file (PeptideProphet) path can't be empty",
            "Warning", JOptionPane.WARNING_MESSAGE);
        return null;
      }
      String fastaPathOrig = fastaPath;
      fastaPath = testFilePath(fastaPath, workingDir.toString());
      if (fastaPath == null) {
        JOptionPane.showMessageDialog(comp, String.format("Could not find fasta file (PeptideProphet) at:\n%s", fastaPathOrig),
            "Errors", JOptionPane.ERROR_MESSAGE);
        return null;
      }

      PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
      peptideProphetParams.setCmdLineParams(textPepProphCmd);

      boolean isPhilosopherAndNotTpp = isPhilosopherAndNotTpp(binPhilosopher);

      Map<InputLcmsFile, Path> pepxmlDirty = getPepxmlFilePathsAfterSearch(lcmsFiles, fp.getOutputFileExt());
      Map<InputLcmsFile, Path> pepxmlClean = getPepxmlFilePathsAfterMove(pepxmlDirty, workingDir, isCrystalc, isProcessGroupsSeparately);
      for (InputLcmsFile f : lcmsFiles) {
        List<String> cmd = new ArrayList<>();
        cmd.add(binPhilosopher);
        if (isPhilosopherAndNotTpp) // for philosopher we always add the correct command
        {
          cmd.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);
        }

        if (!peptideProphetParams.getCmdLineParams().isEmpty()) {
          String cmdOpts = peptideProphetParams.getCmdLineParams();
          List<String> opts = StringUtils.splitCommandLine(cmdOpts);
          for (String opt : opts) {
            if (!opt.isEmpty()) {
              if (opt.equals(PhilosopherProps.CMD_PEPTIDE_PROPHET)) {
                continue;
              }
              cmd.add(opt);
            }
          }
        }
        cmd.add("--database");
        cmd.add(fastaPath);

        Path pepxmlInWd = pepxmlClean.get(f);
        if (pepxmlInWd == null) {
          JOptionPane.showMessageDialog(comp, "PeptideProphet process could not figure where a pepxml was.\n"
                  + "RAW: " + f.path.toString() + "\n",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }

        cmd.add(pepxmlInWd.toString());
        ProcessBuilder pb = new ProcessBuilder(cmd);
        pb.directory(workingDir.toFile());
        Map<String, String> env = pb.environment();
        // set environment
        String ENV_WEBSERVER_ROOT = "WEBSERVER_ROOT";
        String webroot = env.get(ENV_WEBSERVER_ROOT);
        if (webroot == null) {
          env.put(ENV_WEBSERVER_ROOT, "fake-WEBSERVER_ROOT-value");
        }
        builders.add(pb);
      }

    }
    return builders;
  }

  /**
   * Creates the pbs for running ProteinProphet.
   *
   * @return null in case of error, empty list if nothing needs to be added.
   */
  public static List<ProcessBuilder> pbsProteinProphet(Component comp, boolean isProteinProphet,
      String binPhilosopher, String txtProteinProphetCmdLineOpts, FraggerPanel fp,
      boolean isProteinProphetInteractStar, boolean isCrystalc, UsageTrigger isPhiloUsed,
      String programsDir, List<InputLcmsFile> lcmsFiles, Path wdPath, boolean isProcessGroupsSeparately,
      Path combinedProtFilePath) {
    if (isProteinProphet) {
      String bin = binPhilosopher;
      if (StringUtils.isNullOrWhitespace(bin)) {
        JOptionPane.showMessageDialog(comp, "ProteinProphet binary can not be an empty string.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      bin = testBinaryPath(bin, programsDir);
      if (bin == null) {
        JOptionPane.showMessageDialog(comp, "ProteinProphet binary not found or could not be launched.\n"
                + "Neither on PATH, nor in the working directory",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      isPhiloUsed.setUsed(true);

      if (combinedProtFilePath == null) {
        JOptionPane.showMessageDialog(comp, "ProteinProphet output file name can not be an empty string.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }

      ProteinProphetParams proteinProphetParams = new ProteinProphetParams();
      proteinProphetParams.setCmdLineParams(txtProteinProphetCmdLineOpts);
      List<ProcessBuilder> builders = new ArrayList<>();

      List<Path> createdInteractPepxmlFiles = new ArrayList<>();
      List<String> cmd = new ArrayList<>();
      cmd.add(bin);
      boolean isPhilosopherAndNotTpp = isPhilosopherAndNotTpp(bin);


      Map<InputLcmsFile, Path> pepxmlDirty = getPepxmlFilePathsAfterSearch(lcmsFiles, fp.getOutputFileExt());
      Map<InputLcmsFile, Path> pepxmlClean = getPepxmlFilePathsAfterMove(pepxmlDirty, wdPath, isCrystalc, isProcessGroupsSeparately);
      Map<InputLcmsFile, Path> interacts = getPepxmlInteractFilePaths(pepxmlClean, fp.getOutputFileExt());

      if (isPhilosopherAndNotTpp) {
        cmd.add(PhilosopherProps.CMD_PROTEIN_PROPHET);

        // --output flag should be available in the latest philosopher
//        cmd.add("--output");
//        cmd.add("combined");
//        cmd.add(combinedProtFilePath.toString());

        // for Philosopher command line flags go before files
        String cmdLineOpts = proteinProphetParams.getCmdLineParams().trim();
        if (!StringUtils.isNullOrWhitespace(cmdLineOpts)) {
          List<String> opts = StringUtils.splitCommandLine(cmdLineOpts);
          cmd.addAll(opts);
        }

        if (isProteinProphetInteractStar) {
          final String sep = FileSystems.getDefault().getSeparator();
          final String interactsGlob = wdPath.toString() + sep + "interact-*.pep.xml";
          cmd.add(interactsGlob);
          //commands.add(getCombinedProtFilePath(workingDir).getFileName().toString());
          ProcessBuilder pb = new ProcessBuilder(cmd);
          builders.add(pb);
        } else {
          for (InputLcmsFile f : lcmsFiles) {
            Path interact = interacts.get(f);
            if (interact != null)
              createdInteractPepxmlFiles.add(interact);
          }
          for (Path interactFullPath : createdInteractPepxmlFiles) {
            cmd.add(interactFullPath.toString());
          }
          ProcessBuilder pb = new ProcessBuilder(cmd);
          builders.add(pb);
        }
      } else {
        throw new UnsupportedOperationException("Native ProteinProphet is not supported anymore, use Philosopher instead.");
//        for (InputLcmsFile f : lcmsFiles) {
//          Path interact = interacts.get(f);
//          if (interact != null)
//            createdInteractPepxmlFiles.add(interact);
//        }
//
//        // output file
//        cmd.add(combinedProtFilePath.toString());
//
//        // for native ProteinProphet command line flags go in the end
//        String cmdLineOpts = proteinProphetParams.getCmdLineParams().trim();
//        if (!cmdLineOpts.isEmpty()) {
//          List<String> opts = StringUtils.splitCommandLine(cmdLineOpts);
//          cmd.addAll(opts);
//        }
//        ProcessBuilder pb = new ProcessBuilder(cmd);
//        builders.add(pb);
      }

      for (ProcessBuilder pb : builders) {
        pb.directory(wdPath.toFile());
        Map<String, String> env = pb.environment();

        // add this variable so that TPP didn't try to use webserver stuff
        String ENV_XML_ONLY = "XML_ONLY";
        env.put(ENV_XML_ONLY, "1");

        // collect variables from system
        StringBuilder pathEnv = new StringBuilder();
        Set<String> mergedKeys = new HashSet<>();
        Set<String> envKeys = env.keySet();
        for (String key : envKeys) {
          if (key.toLowerCase().equals("path")) {
            String pathVal = env.get(key);
            pathVal = pathVal.trim();
            pathEnv.append(pathVal);
            if (!pathVal.endsWith(";")) {
              pathEnv.append(";");
            }
            mergedKeys.add(key);
          }
        }
        for (String key : mergedKeys) {
          env.remove(key);
        }

        String ENV_PATH = "PATH";
        Path binPath = Paths.get(bin);
        String binFolder = null;
        if (binPath.isAbsolute()) {
          // the path to the executable was specified as absolute, other needed files must be there as well
          binFolder = binPath.toAbsolutePath().getParent().toString();
        } else if (Files.exists(binPath)) {
          binFolder = binPath.toAbsolutePath().getParent().toString();
        } else {
          binPath = wdPath.resolve(bin);
          if (Files.exists(binPath)) {
            binFolder = binPath.toAbsolutePath().getParent().toString();
          }
        }
        if (binFolder != null) {
          pathEnv.append(";").append(binFolder);
        }
        String pathEnvValue = pathEnv.toString();
        env.put(ENV_PATH, pathEnvValue);
      }

      return builders;
    }
    return Collections.emptyList();
  }

  public static List<Image> loadIcon() {
    // Icon attribution string:
    // <div>Icons made by <a href="http://www.freepik.com" title="Freepik">Freepik</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
    List<Image> images = new ArrayList<>();
    int[] sizes = {16, 24, 32, 64, 128, 256};
    final String path = "icons/";
    final String baseName = "fragpipe-";
    final String ext = ".png";
    for (int size : sizes) {
      String location = path + baseName + size + ext;
      Image icon = Toolkit.getDefaultToolkit().getImage(MsfraggerGuiFrame.class.getResource(location));
      images.add(icon);
    }
    return images;
  }

  /**
   * Creates the pbs for running ProteinProphet.
   *
   * @return null in case of error, empty list if nothing needs to be added.
   */
  public static List<ProcessBuilder> pbsReport(boolean isReport, String binPhilosopher, Component comp,
      FraggerPanel fraggerPanel,
      boolean isCrystalc, UsageTrigger isPhiloUsed,
      boolean isReportDbAnnotate, String textReportAnnotate, String dbPath,
      boolean isReportFilter, String textReportFilter,
      boolean isReportProteinLevelFdr,
      boolean isLabelfree, String textReportLabelfree,
      String programsDir, Path workingDir, Path combinedProtFilePath, List<InputLcmsFile> lcmsFiles) {
    if (isReport) {

      if (StringUtils.isNullOrWhitespace(binPhilosopher)) {
        JOptionPane.showMessageDialog(comp, "Philosopher binary can not be an empty string.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      binPhilosopher = testBinaryPath(binPhilosopher, programsDir);
      if (binPhilosopher == null) {
        JOptionPane.showMessageDialog(comp, "Philosopher binary not found or could not be launched.\n"
                + "Neither on PATH, nor in the working directory",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }
      isPhiloUsed.setUsed(true);

      if (combinedProtFilePath == null) {
        JOptionPane.showMessageDialog(comp, "ProteinProphet output file name can not be an empty string.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      } else if (!combinedProtFilePath.toString().toLowerCase().endsWith(".prot.xml")) {
        JOptionPane.showMessageDialog(comp, "ProteinProphet output file name must end with '.prot.xml'.\n",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      } else {
        int index = combinedProtFilePath.toString().toLowerCase().indexOf(".prot.xml");
        if (index <= 0) {
          JOptionPane.showMessageDialog(comp, "ProteinProphet output file name must have text before '.prot.xml'.\n",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }
      }

      List<ProcessBuilder> builders = new ArrayList<>();
      //Map<InputLcmsFile, Path> pepxmlDirty = getPepxmlFilePathsAfterSearch(lcmsFiles, fraggerPanel.getOutputFileExt());
      //Map<InputLcmsFile, Path> pepxmlClean = getPepxmlFilePathsAfterMove(pepxmlDirty, workingDir, isCrystalc);
      //Map<InputLcmsFile, Path> interacts = getPepxmlInteractFilePaths(pepxmlClean, workingDir, fraggerPanel.getOutputFileExt());

      if (isReportDbAnnotate) {
        isPhiloUsed.setUsed(true);
        List<String> cmd = new ArrayList<>();
        cmd.add(binPhilosopher);
        cmd.add(PhilosopherProps.CMD_DATABASE);
        cmd.add("--annotate");
        if (dbPath == null) {
          JOptionPane.showMessageDialog(comp, "Fasta file path can't be empty (Report)",
              "Warning", JOptionPane.WARNING_MESSAGE);
          return null;
        }
        cmd.add(dbPath);
        if (!StringUtils.isNullOrWhitespace(textReportAnnotate)) {
          String[] params = textReportAnnotate.split("[\\s]+");
          cmd.addAll(Arrays.asList(params));
        }
        builders.add(new ProcessBuilder(cmd));
      }

      // philosopher filter
      if (isReportFilter) {
        isPhiloUsed.setUsed(true);
        List<String> cmd = new ArrayList<>();
        cmd.add(binPhilosopher);
        cmd.add(PhilosopherProps.CMD_FILTER);
        if (!StringUtils.isNullOrWhitespace(textReportFilter)) {
          String[] params = textReportFilter.split("[\\s]+");
          cmd.addAll(Arrays.asList(params));
        }
        cmd.add("--pepxml");
        cmd.add(workingDir.toString());
        if (isReportProteinLevelFdr) {
          cmd.add("--protxml");
          cmd.add(combinedProtFilePath.toString());
        }
        builders.add(new ProcessBuilder(cmd));
      }

      // philosopher freequant (labelfree)
      if (isLabelfree) {
        isPhiloUsed.setUsed(true);
        // check again if all input files are in the same folder for free quant
        if (lcmsFiles.stream().map(f -> f.path.getParent()).collect(Collectors.toSet()).size() > 1) {
          JOptionPane.showMessageDialog(comp, "For free-quant all input files must be in the same folder.\n",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }
        List<String> cmd = new ArrayList<>();
        cmd.add(binPhilosopher);
        cmd.add(PhilosopherProps.CMD_LABELFREE);

        List<String> allowed = new ArrayList<>();
        allowed.add("ptw");
        allowed.add("tol");
        for (String paramName : allowed) {
          Pattern reFullParam = Pattern.compile(String.format("--%s\\s+(\\d+(?:\\.\\d+)?)", paramName));
          Matcher m = reFullParam.matcher(textReportLabelfree);
          if (m.find()) {
            cmd.add("--" + paramName);
            cmd.add(m.group(1));
          }
        }


        // we have checked that all lcms files are in the same folder, so
        Path lcmsDir = lcmsFiles.get(0).path.getParent();
        cmd.add("--dir");
        cmd.add(lcmsDir.toAbsolutePath().toString());

        builders.add(new ProcessBuilder(cmd));
      }

      // philosopher report
      List<String> cmd = new ArrayList<>();
      cmd.add(binPhilosopher);
      cmd.add(PhilosopherProps.CMD_REPORT);
      builders.add(new ProcessBuilder(cmd));

      // set working dir for all processes
      for (ProcessBuilder pb : builders) {
        pb.directory(workingDir.toFile());
      }

      return builders;
    }
    return Collections.emptyList();
  }

  public static List<ProcessBuilder> pbsUmpire(boolean isRunUmpire, boolean isDryRun,
      Component errMsgParent, URI jarUri, UmpirePanel umpirePanel, String binPhilosopher,
      Path workingDir, List<InputLcmsFile> lcmsFiles) {


    List<ProcessBuilder> pbs = new LinkedList<>();
    if (isRunUmpire) {

      // check if input files contain only mzxml files
      boolean hasNonMzxml = lcmsFiles.stream().map(f -> f.path.getFileName().toString().toLowerCase())
          .anyMatch(p -> !p.endsWith("mzxml"));
      if (hasNonMzxml) {
        JOptionPane.showMessageDialog(errMsgParent,
            "[DIA Umpire SE]\nNot all input files are mzXML.\n"
                + "DIA-Umpire only supports mzXML.",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }


      // unpack Umpire jar
      String currentJarPath = Paths.get(jarUri).toAbsolutePath().toString();
      Path jarUmpireSe;
      try {
        // msadjuster jar
        jarUmpireSe = JarUtils.unpackFromJar(ToolingUtils.class,"/" + UmpireParams.JAR_UMPIRESE_NAME,
            ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);

      } catch (IOException | NullPointerException ex) {
        JOptionPane.showMessageDialog(errMsgParent,
            "Could not unpack UmpireSE jar to a temporary directory.\n",
            "Can't unpack", JOptionPane.ERROR_MESSAGE);
        return null;
      }


      // write umpire params file
      final DateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
      final String dateStr = df.format(new Date());
      final UmpireParams collectedUmpireParams = umpirePanel.collect();
      final String umpireParamsFileName =
          UmpireParams.FILE_BASE_NAME + "_" + dateStr + "." + UmpireParams.FILE_BASE_EXT;
      final Path umpireParamsFilePath = workingDir.resolve(umpireParamsFileName);
      if (!isDryRun) {
        try {
          FileOutputStream fos = new FileOutputStream(umpireParamsFilePath.toFile());
          PropertiesUtils.writePropertiesContent(collectedUmpireParams, fos);
        } catch (FileNotFoundException | FileWritingException e) {
          JOptionPane.showMessageDialog(errMsgParent,
              "[DIA Umpire SE]\nCould not write property file, thus can't run DIA-Umpire",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }
      }


      // copy/symlink input lcms files
//      if (OsUtils.isWindows()) {
//        // On windows copy the files over to the working directory
//        List<ProcessBuilder> processBuildersCopyFiles = pbsCopyFiles(jarUri, workingDir, lcmsFiles);
//        pbs.addAll(processBuildersCopyFiles);
//      } else {
//        // On Other systems try to create symlinks to mzXML files
//        try {
//          if (!isDryRun)
//            UmpireParams.createFileSymlinks(workingDir, lcmsFiles);
//        } catch (IOException ex) {
//          String msg = String.format("Something went wrong when creating symlinks to LCMS files.\n%s", ex.getMessage());
//          JOptionPane.showMessageDialog(errMsgParent, msg, "Error", JOptionPane.ERROR_MESSAGE);
//          return null;
//        }
//      }


      // run umpire for each file
      int ramGb = (Integer)umpirePanel.spinnerRam.getValue();
      int ram = ramGb > 0 ? ramGb : 0;

      List<String> createdMgfFiles = new ArrayList<>();
      List<String> createdMzXmlFiles = new ArrayList<>();
//      List<Path> lcmsFileSymlinks = UmpireParams.getLcmsFilePathsInWorkdir(workingDir, lcmsFiles);
      for (InputLcmsFile f: lcmsFiles) {
        Path inputFn = f.path.getFileName();
        Path inputDir = f.path.getParent();

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


        if (!workingDir.equals(inputDir)) {
          // working dir is different from mzXML file location
          // //need to move output and cleanup
          UmpireSeGarbageFiles garbage = UmpireSeGarbageFiles.create(f.path);
          for (String path : garbage.toMove) {
            List<String> cmdMove = new ArrayList<>();
            cmdMove.add("java");
            cmdMove.add("-cp");
            cmdMove.add(currentJarPath); // FragPipe jar
            // TODO: verify this actually moves all the garbage files. Use files at: C:\data\dia\40-50-minutes
            cmdMove.add(FileMove.class.getCanonicalName());
            String origin = inputDir.resolve(Paths.get(path).getFileName())
                .toString();
            String destination = workingDir.resolve(Paths.get(path).getFileName())
                .toString();
            cmdMove.add(origin);
            cmdMove.add(destination);
            pbs.add(new ProcessBuilder(cmdMove));
          }
        }

        // msconvert
        // now all the generated garbage is in the working directory
        final boolean isWin = OsUtils.isWindows();
        String binMsconvert = umpirePanel.getBinMsconvert();
        if (isWin && StringUtils.isNullOrWhitespace(binMsconvert)) {
          JOptionPane.showMessageDialog(errMsgParent,
              "[DIA Umpire SE]\nOn Windows specifying path to msconvert binary is required.",
              "Error", JOptionPane.ERROR_MESSAGE);
          return null;
        }

        List<String> mgfs = getUmpireSeMgfsForMzxml(inputFn.toString());
        for (String mgf : mgfs) {
          List<String> cmdMsConvert = new ArrayList<>();

          if (isWin) {
            cmdMsConvert.add(binMsconvert);
            cmdMsConvert.add("--verbose");
            cmdMsConvert.add("--32");
            cmdMsConvert.add("--zlib");
            cmdMsConvert.add("--mzXML");
            cmdMsConvert.add("--outdir");
            cmdMsConvert.add(workingDir.toString());
          } else {
            // on Linux philosopher includes msconvert
            cmdMsConvert.add(binPhilosopher);
            cmdMsConvert.add("--format");
            cmdMsConvert.add("mzXML");
            cmdMsConvert.add("--intencoding");
            cmdMsConvert.add("32");
            cmdMsConvert.add("--mzencoding");
            cmdMsConvert.add("32");
            cmdMsConvert.add("--zlib");
          }
          cmdMsConvert.add(workingDir.resolve(mgf).toString());

        }
      }
    }
    return pbs;
  }

  public static List<ProcessBuilder> pbsSpecLibGen(Component errMsgParent, boolean isRunSpeclibgen,
      Path workingDir, Path combinedProteinFile, String fastaPath, String binPhilosopher) {
    final List<ProcessBuilder> pbs = new ArrayList<>();

    final SpecLibGen slg = SpecLibGen.get();
    if (isRunSpeclibgen) {
      if (!slg.isInitialized()) {
        JOptionPane.showMessageDialog(errMsgParent,
            "Spectral Library Generation scripts did not initialize correctly.",
            "Error", JOptionPane.ERROR_MESSAGE);
        return null;
      }



      List<String> cmd = new ArrayList<>();
      cmd.add(slg.getPi().getCommand());
      cmd.add(slg.getScriptSpecLibGenPath().toString());
      cmd.add(fastaPath);
      cmd.add(workingDir.toString()); // this is "Pep xml directory"
      cmd.add(combinedProteinFile.toString());
      cmd.add(workingDir.toString());
      cmd.add("True");
      cmd.add(binPhilosopher);

      pbs.add(new ProcessBuilder(cmd));
    }

    return pbs;
  }

  public static List<String> getUmpireSeMgfsForMzxml(String inputMzxmlFileName) {
    String baseName = StringUtils.upToLastDot(inputMzxmlFileName);
    final int n = 3;
    List<String> mgfs = new ArrayList<>(n);
    for (int i = 1; i <= n; i++) {
      mgfs.add(baseName + "_Q" + i + ".mgf");
    }
    return mgfs;
  }

  public static List<Path> getUmpireCreatedMzxmlFiles(List<InputLcmsFile> lcmsFiles, Path workingDir) {
    return lcmsFiles.stream()
        .map(f -> workingDir.resolve(f.path.getFileName()))
        .collect(Collectors.toList());
  }

  public static String getDefaultBinMsfragger() {
    String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSFRAGGER);
    return path == null ? "MSFragger.jar" : path;
  }

  public static String getDefaultBinPhilosopher() {
    String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER);
    if (path != null) {
      return path;
    }
    ResourceBundle bundle = ResourceBundle.getBundle(Version.PATH_BUNDLE);
    String winName = bundle.getString("default.philosopher.win"); // NOI18N
    String nixName = bundle.getString("default.philosopher.nix"); // NOI18N
    return OsUtils.isWindows() ? winName : nixName;
  }

  static boolean isPhilosopherAndNotTpp(String binPathToCheck) {
    Pattern isPhilosopherRegex = Pattern.compile("philosopher", Pattern.CASE_INSENSITIVE);
    Matcher matcher = isPhilosopherRegex.matcher(binPathToCheck);
    return matcher.find();
  }

  public static String getBinMsconvert() {
    String value = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSCONVERT);
    if (value != null) {
      return value;
    }

    String binaryName;
    ResourceBundle bundle = ResourceBundle.getBundle(Version.PATH_BUNDLE); // NOI18N
    binaryName = OsUtils.isWindows() ? bundle.getString("default.msconvert.win")
        : bundle.getString("default.msconvert.nix");
    String testedBinaryPath = testBinaryPath(binaryName);
    if (!StringUtils.isNullOrWhitespace(testedBinaryPath)) {
      return testedBinaryPath;
    }

    if (OsUtils.isWindows()) {
      try {
        // on Windows try to find MSConvert in a few predefined locations
        final List<String> searchPaths = Arrays.asList("program files (x64)", "program files", "programs");
        final List<String> folderNames = Arrays.asList("proteowizard", "pwiz");
        final String toSearch = "msconvert.exe";

        final Holder<Path> foundPathHolder = new Holder<>();

        FileVisitor<Path> fileVisitor = new FileVisitor<Path>() {
          @Override
          public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            if (file.getFileName().toString().toLowerCase().equals(toSearch)) {
              foundPathHolder.obj = file;
              return FileVisitResult.TERMINATE;
            }
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult visitFileFailed(Path file, IOException exc) {
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult postVisitDirectory(Path dir, IOException exc) {
            return FileVisitResult.CONTINUE;
          }
        };

        Iterable<Path> rootDirs = FileSystems.getDefault().getRootDirectories();
        for (Path rootDir : rootDirs) {
          try {
            DirectoryStream<Path> dirStream = Files.newDirectoryStream(rootDir);
            for (Path file : dirStream) {
              for (String path : searchPaths) {
                if (file.getFileName().toString().toLowerCase().startsWith(path)) {
                  // search for proteowizard
                  DirectoryStream<Path> dirStream2 = Files.newDirectoryStream(file);
                  for (Path file2 : dirStream2) {
                    String toLowerCase = file2.getFileName().toString().toLowerCase();
                    for (String folder : folderNames) {
                      if (toLowerCase.startsWith(folder)) {
                        // this might be a proteo wizard folder, recursively search it
                        Files.walkFileTree(file2, fileVisitor);
                        if (foundPathHolder.obj != null) {
                          return foundPathHolder.obj.toAbsolutePath().toString();
                        }
                      }
                    }
                  }
                }
              }
            }
          } catch (IOException ignore) {}
        }
      } catch (Exception ignore) {}
    }
    return "";
  }

}
