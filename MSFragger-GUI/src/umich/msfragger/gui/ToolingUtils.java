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
