package umich.msfragger.gui;

import static umich.msfragger.params.ThisAppProps.JAR_FILE_AS_RESOURCE_EXT;

import java.awt.Component;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.params.crystalc.CrystalcParams;
import umich.msfragger.params.crystalc.CrystalcProps;
import umich.msfragger.params.fragger.MsfraggerParams;
import umich.msfragger.params.pepproph.PeptideProphetParams;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.protproph.ProteinProphetParams;
import umich.msfragger.util.FileDelete;
import umich.msfragger.util.FileMove;
import umich.msfragger.util.Holder;
import umich.msfragger.util.OsUtils;
import umich.msfragger.util.PathUtils;
import umich.msfragger.util.StringUtils;

public class ToolingUtils {
  private ToolingUtils() {}


  /**
   * @return Combined protein file name without extension.
   * @param txtCombinedProtFile
   */
  static String getCombinedProtOpt(String txtCombinedProtFile) {
      String combined = txtCombinedProtFile.trim();
      String combinedOpt;
      if (StringUtils.isNullOrWhitespace(combined)) {
          combinedOpt = "interact";
      } else {
          final String protExt = ".prot.xml";
          if (combined.toLowerCase().endsWith(protExt)) {
              combinedOpt = combined.substring(0, combined.toLowerCase().indexOf(protExt));
          } else {
              combinedOpt = combined;
          }
      }

      return combinedOpt;
  }

  /**
   * @return Combined protein file name with extension.
   * @param txtCombinedProtFile
   */
  static String getCombinedProtFileName(String txtCombinedProtFile) {
      return getCombinedProtOpt(txtCombinedProtFile) + ".prot.xml";
  }

  /**
   *
   * @param comp Component to display error messages relative to.
   * @param txtCombinedProtFile
   * @param workingDir
   * @return Full absolute normalized path to the output combined protein file.
   */
  public static Path getCombinedProtFilePath(Component comp, String txtCombinedProtFile, String workingDir) {
      String combinedProtFile = getCombinedProtFileName(txtCombinedProtFile);
      if (StringUtils.isNullOrWhitespace(combinedProtFile)) {
          JOptionPane.showMessageDialog(comp,
              "Please specify ProteinProphet output path on ProteinProphet tab.\n"
                      + "This is needed even if you're not running ProteinProphet right now.\n"
                      + "In which case check the box to run it, add the filename and uncheck the filebox.\n"
                      + "Sorry for the inconvenience.",
                  "Errors", JOptionPane.ERROR_MESSAGE);
          return null;
      } else {
          return Paths.get(workingDir, combinedProtFile).toAbsolutePath().normalize();
      }
  }

  static boolean isPhilosopherBin(String binPathToCheck) {
      Pattern isPhilosopherRegex = Pattern.compile("philosopher", Pattern.CASE_INSENSITIVE);
      Matcher matcher = isPhilosopherRegex.matcher(binPathToCheck);
      return matcher.find();
  }

  public static String getDefaultTextMsconvert() {
      String value = ThisAppProps.load(ThisAppProps.PROP_TEXTFIELD_PATH_MSCONVERT);
      if (value != null) {
          return value;
      }

      String binaryName;
      ResourceBundle bundle = ResourceBundle.getBundle("dia/umpire/gui/Bundle"); // NOI18N
      if (OsUtils.isWindows()) {
          binaryName = bundle.getString("default.msconvert.win");
      } else {
          binaryName = bundle.getString("default.msconvert.nix");
      }
      String testedBinaryPath = PathUtils.testBinaryPath(binaryName);
      if (testedBinaryPath != null && !testedBinaryPath.isEmpty()) {
          return testedBinaryPath;
      }

      if (OsUtils.isWindows()) {
          try {
              // on Windows try to find MSConvert in a few predefined locations
              List<String> paths = Arrays.asList(
                      "program files (x64)",
                      "program files"
              );
              String folder = "proteowizard";
              String folder2 = "pwiz";
              final String toSearch = "msconvert.exe";

              final Holder<Path> foundPathHolder = new Holder<>();

              FileVisitor<Path> fileVisitor = new FileVisitor<Path>() {
                  @Override
                  public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                      return FileVisitResult.CONTINUE;
                  }

                  @Override
                  public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                      if (file.getFileName().toString().toLowerCase().equals(toSearch)) {
                          foundPathHolder.obj = file;
                          return FileVisitResult.TERMINATE;
                      }
                      return FileVisitResult.CONTINUE;
                  }

                  @Override
                  public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                      return FileVisitResult.CONTINUE;
                  }

                  @Override
                  public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                      return FileVisitResult.CONTINUE;
                  }
              };

              Iterable<Path> rootDirs = FileSystems.getDefault().getRootDirectories();
              for (Path rootDir : rootDirs) {
                  try {
                      DirectoryStream<Path> dirStream = Files.newDirectoryStream(rootDir);
                      for (Path file : dirStream) {
                          for (String path : paths) {
                              if (file.getFileName().toString().toLowerCase().startsWith(path)) {
                                  // search for proteowizard
                                  DirectoryStream<Path> dirStream2 = Files.newDirectoryStream(file);
                                  for (Path file2 : dirStream2) {
                                      String toLowerCase = file2.getFileName().toString().toLowerCase();
                                      if (toLowerCase.startsWith(folder) || toLowerCase.startsWith(folder2)) {
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
                  } catch (IOException ex) {
                      // doesn't matter
                  }
              }
          } catch (Exception e) {
              // we don't care if anything within this block breaks
          }
      }
      return "";
  }


    /**
     *
     * @param jar Use {@link PathUtils#getCurrentJarUri()} to get that from the current Jar.
     * @param destination
     * @param files
     * @return
     */
    public static List<ProcessBuilder> pbsCopyFiles(URI jar, String destination, List<String> files) {
        if (jar == null)
            throw new IllegalArgumentException("JAR URI must ne non null");
        List<ProcessBuilder> pbs = new LinkedList<>();
        String currentJarPath = Paths.get(jar).toAbsolutePath().toString();
        Path wd = Paths.get(destination);

        for (String lcmsFilePath : files) {
            if (wd.equals(Paths.get(lcmsFilePath).getParent()))
                continue;
            List<String> commands = new ArrayList<>();
            commands.add("java");
            commands.add("-cp");
            commands.add(currentJarPath);
            commands.add(FileDelete.class.getCanonicalName());
            Path copyTo = Paths.get(destination, Paths.get(lcmsFilePath).getFileName().toString());
            commands.add(copyTo.toString());
            ProcessBuilder pb = new ProcessBuilder(commands);
            pbs.add(pb);
        }
        return pbs;
    }

    public static Map<String, String> createPepxmlFilePathsDirty(List<String> lcmsFilePaths, String ext) {
        HashMap<String, String> pepxmls = new HashMap<>();
        for (String s : lcmsFilePaths) {
            String baseName = s.substring(0, s.lastIndexOf(".") + 1);
            pepxmls.put(s, baseName + ext);
        }
        return pepxmls;
    }

    public static Map<String, String> createPepxmlFilePathsAfterMove(Map<String, String> dirtyPepXmls, String workingDir,
            boolean crystalC) {
        HashMap<String, String> pepxmls = new HashMap<>();
        Path wd = Paths.get(workingDir);
        for (Map.Entry<String, String> entry : dirtyPepXmls.entrySet()) {
            String raw = entry.getKey();
            String dirty = entry.getValue();
            String fn = Paths.get(dirty).getFileName().toString();
            String fnMod = !crystalC ? fn :
                    StringUtils.upToLastDot(fn) + "_c." + StringUtils.afterLastDot(fn);
            Path pepxmlClean = wd.resolve(fnMod).toAbsolutePath();
            pepxmls.put(raw, pepxmlClean.toString());
        }
        return pepxmls;
    }

  public static Map<String, String> createInteractFilePaths(Map<String, String> cleanPepXmls, String workingDir, String pepxmlExt) {
      HashMap<String, String> interacts = new HashMap<>();
      Path wd = Paths.get(workingDir);
      for (Map.Entry<String, String> entry : cleanPepXmls.entrySet()) {
          String raw = entry.getKey();
          String clean = entry.getValue();
          String cleanFn = Paths.get(clean).getFileName().toString();

          // hardcode typical params
          String[] typicalExts = {pepxmlExt, "pep.xml", "pepxml"};
          String lowerCase = cleanFn.toLowerCase();
          String nameWithoutExt = null;
          for (String ext : typicalExts) {
              if (cleanFn.toLowerCase().endsWith(ext)) {
                  int lastIndex = lowerCase.lastIndexOf(ext);
                  nameWithoutExt = cleanFn.substring(0, lastIndex);
                  break;
              }
          }
          if (nameWithoutExt == null) {
              throw new IllegalStateException(String.format("Could not identify the extension for file: %s", clean));
          }

          Path interactXml = wd.resolve("interact-" + nameWithoutExt + "pep.xml").toAbsolutePath();
          interacts.put(raw, interactXml.toString());
      }
      return interacts;
  }

  public static List<ProcessBuilder> pbsFragger(Component comp, String programsDir, String workingDir, List<String> lcmsFilePaths,
      boolean isDryRun, FraggerPanel fp, String pythonCommand, URI jar, String msfraggerPath, boolean isSlicingAvailable, Path slicingScriptPath) {
      if (jar == null)
          throw new IllegalArgumentException("Argument JAR can't be null");
      List<ProcessBuilder> builders = new LinkedList<>();
      if (fp.isRunMsfragger()) {

          final int numSlices = fp.getNumSlices();
          final boolean isSlicing = numSlices > 1;
          if (isSlicing) {
              // slicing requested
              if (!isSlicingAvailable || slicingScriptPath == null) {
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
          bin = PathUtils.testFilePath(bin, programsDir);
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

          Map<String, String> mapRawToPep = createPepxmlFilePathsDirty(lcmsFilePaths, params.getOutputFileExtension());

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
                  cmd.add(pythonCommand);
                  cmd.add(slicingScriptPath.toAbsolutePath().normalize().toString());
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
                  String nextFilePath = lcmsFilePaths.get(fileIndex);
                  if (sb.length() + nextFilePath.length() + 1 > commandLenLimit) {
                      break;
                  }
                  sb.append(nextFilePath).append(" ");
                  cmd.add(nextFilePath);
                  fileIndex++;
              }

              ProcessBuilder pbFragger = new ProcessBuilder(cmd);
              builders.add(pbFragger);
              sb.setLength(0);

              // move the files if the output directory is not the same as where
              // the lcms files were

              for (int i = fileIndexLo; i < fileIndex; i++) {
                  String pepFile = mapRawToPep.get(lcmsFilePaths.get(i));
                  Path pepPath = Paths.get(pepFile);

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

  public static String getBinJava(String programsDir, Component comp) {
      String binJava = "java";
      binJava = PathUtils.testBinaryPath(binJava, programsDir);
      if (binJava != null) {
          return binJava;
      }
      JOptionPane.showMessageDialog(comp, "Java could not be found.\n"
              + "please make sure you have it installed \n"
              + "and that java.exe can be found on PATH", "Error", JOptionPane.ERROR_MESSAGE);
      return null;
  }

    public static List<ProcessBuilder> pbsMsadjuster(URI jarUri, Component comp, String workingDir, List<String> lcmsFilePaths, FraggerPanel fp, boolean cleanUp) {
        List<ProcessBuilder> pbs = new LinkedList<>();

        if (fp.isRunMsfragger() && fp.isMsadjuster()) {
            String currentJarPath = Paths.get(jarUri).toAbsolutePath().toString();
            Path wd = Paths.get(workingDir);

            Path jarPath;
            Path depsPath;
            try {
                // common deps
                depsPath = unpackFromJar("", CrystalcProps.JAR_COMMON_DEPS, false, true);
                // msadjuster jar
                jarPath = unpackFromJar("", CrystalcProps.JAR_MSADJUSTER_NAME, false, true);

            } catch (IOException | NullPointerException ex) {
                JOptionPane.showMessageDialog(comp,
                        "Could not unpack tools to a temporary directory.\n"
                                + "Disable precursor mass adjustment in MSFragger tab.",
                            "Can't unpack", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            int ramGb = fp.getRamGb();

            for (String lcmsFilePath : lcmsFilePaths) {
                if (!cleanUp) {
                    ArrayList<String> cmd = new ArrayList<>();
                    Path fileIn = Paths.get(lcmsFilePath);
                    cmd.add("java");
                    if (ramGb > 0) {
                        cmd.add("-Xmx" + ramGb + "G");
                    }
                    if (depsPath != null) {
                        cmd.add("-cp");
                        List<String> toJoin = new ArrayList<>();
                        toJoin.add(depsPath.toAbsolutePath().normalize().toString());
                        toJoin.add(jarPath.toAbsolutePath().normalize().toString());
                        final String sep = System.getProperties().getProperty("path.separator");
                        cmd.add("\"" + org.apache.commons.lang3.StringUtils.join(toJoin, sep) + "\"");
                        cmd.add(CrystalcProps.JAR_MSADJUSTER_MAIN_CLASS);
                    } else {
                        cmd.add("-jar");
                        cmd.add(jarPath.toAbsolutePath().normalize().toString());
                    }
                    cmd.add("20");
                    cmd.add(lcmsFilePath);
                    pbs.add(new ProcessBuilder(cmd));
                } else {
                    ArrayList<String> cmd = new ArrayList<>();
                    Path fileIn = Paths.get(lcmsFilePath);

                    cmd.add("java");
                    cmd.add("-cp");
                    cmd.add(currentJarPath);
                    cmd.add(FileMove.class.getCanonicalName());
                    String origin = lcmsFilePath + ".ma"; // MSAdjuster creates these files
                    String destination = wd.resolve(Paths.get(origin).getFileName().toString()).toString();
                    cmd.add(origin);
                    cmd.add(destination);
                    pbs.add(new ProcessBuilder(cmd));
                }
            }
        }
        return pbs;
    }

    public static Path unpackFromJar(String resourcePath, String resourceName,
            boolean randomizeName, boolean scheduleForDeletion) throws IOException {

        try (InputStream in = MsfraggerGuiFrame.class.getResourceAsStream(resourcePath + "/" + resourceName)) {
            Path tempFile;
            String resourceNameMod = resourceName.toLowerCase().endsWith(JAR_FILE_AS_RESOURCE_EXT)
                    ? StringUtils.upToLastDot(resourceName) + ".jar"
                    : resourceName;

            if (randomizeName) {
                tempFile = Files.createTempFile("fragpipe-", "-" + resourceNameMod);
            } else {
                tempFile = Paths.get(ThisAppProps.TEMP_DIR, resourceNameMod);
            }
            Files.copy(in, tempFile, StandardCopyOption.REPLACE_EXISTING);
            if (scheduleForDeletion)
                tempFile.toFile().deleteOnExit();
            return tempFile;
        }
    }

  /**
   *
   * @param comp
   * @param fp
   * @param ccParams Get these by calling {@link MsfraggerGuiFrame#crystalcFormToParams()}.
   * @param isDryRun
   * @param isCrystalc
   * @param workingDir
   * @param fastaPath
   * @param lcmsFilePaths
   * @return
   */
  public static List<ProcessBuilder> pbsCrystalc(Component comp, FraggerPanel fp, CrystalcParams ccParams,
      boolean isDryRun, boolean isCrystalc, String workingDir, String fastaPath, List<String> lcmsFilePaths) {
      List<ProcessBuilder> pbs = new LinkedList<>();
      if (isCrystalc) {
          Path wd = Paths.get(workingDir);

          Path jarPath;
          Path depsPath;
          try {
              // common deps
              depsPath = unpackFromJar("", CrystalcProps.JAR_COMMON_DEPS, false, true);
              // msadjuster jar
              jarPath = unpackFromJar("", CrystalcProps.JAR_CRYSTALC_NAME, false, true);

          } catch (IOException | NullPointerException ex) {
              JOptionPane.showMessageDialog(comp,
                      "Could not unpack tools to a temporary directory.\n"
                              + "Disable Crystal-C.",
                          "Can't unpack", JOptionPane.ERROR_MESSAGE);
              return null;
          }

          // check if all input files are in the same folder
          Set<Path> dirs = lcmsFilePaths.stream()
                  .map(Paths::get).map(p -> p.getParent().toAbsolutePath().normalize()).collect(
                  Collectors.toSet());
          Set<String> exts = lcmsFilePaths.stream()
                  .map(Paths::get).map(p -> p.getFileName().toString())
                  .map(StringUtils::afterLastDot).collect(Collectors.toSet());
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
          Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, pepxmlExtFragger);
          Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir, false);
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
                  cmd.add(new StringBuilder().append("-Xmx").append(ramGb).append("G").toString());
              }
              cmd.add("-cp");
              List<String> toJoin = new ArrayList<>();
              toJoin.add(depsPath.toAbsolutePath().normalize().toString());
              toJoin.add(jarPath.toAbsolutePath().normalize().toString());
              final String sep = System.getProperties().getProperty("path.separator");
              cmd.add("\"" + org.apache.commons.lang3.StringUtils.join(toJoin, sep) + "\"");
              cmd.add(CrystalcProps.JAR_CRYSTALC_MAIN_CLASS);
              cmd.add(ccParamsPath.toString());
              cmd.addAll(pepxmlClean.values());

              pbs.add(new ProcessBuilder(cmd));

          } else {

              // multiple raw file extensions or multiple lcms file locaitons
              // issue a separate command for each pepxml file
              int index = -1;
              for (Map.Entry<String, String> kv : pepxmlClean.entrySet()) {
                  Path lcms = Paths.get(kv.getKey()).toAbsolutePath().normalize();
                  String lcmsFn = lcms.getFileName().toString();
                  Path pepxml = Paths.get(kv.getValue()).toAbsolutePath().normalize();
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

    /**
     * Creates the ProcessBuilders for running PeptideProphet.
     *
     * @param workingDir
     * @param lcmsFilePaths
     * @return null in case of errors, or a list of process builders.
     */
    public static List<ProcessBuilder> pbsPeptideProphet(boolean isRun, Component comp, FraggerPanel fp,
        boolean isCrystalc, String textBinPhilosopher, String seqDbPath, String textPepProphCmd,
        String programsDir, String workingDir, List<String> lcmsFilePaths) {
        List<ProcessBuilder> builders = new LinkedList<>();
        if (isRun) {
            String bin = textBinPhilosopher;
            if (StringUtils.isNullOrWhitespace(bin)) {
                JOptionPane.showMessageDialog(comp, "Philosopher (PeptideProphet) binary can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            bin = PathUtils.testBinaryPath(bin, programsDir);
            if (bin == null) {
                JOptionPane.showMessageDialog(comp, "Philosopher (PeptideProphet) binary not found.\n"
                        + "Neither on PATH, nor in the working directory",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            String fastaPath = seqDbPath;

            if (StringUtils.isNullOrWhitespace(fastaPath)) {
                JOptionPane.showMessageDialog(comp, "Fasta file (PeptideProphet) path can't be empty",
                            "Warning", JOptionPane.WARNING_MESSAGE);
                    return null;
            }
            String fastaPathOrig = fastaPath;
            fastaPath = PathUtils.testFilePath(fastaPath, workingDir);
            if (fastaPath == null) {
                JOptionPane.showMessageDialog(comp, String.format("Could not find fasta file (PeptideProphet) at:\n%s", fastaPathOrig),
                        "Errors", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            PeptideProphetParams peptideProphetParams = new PeptideProphetParams();
            peptideProphetParams.setCmdLineParams(textPepProphCmd);

            boolean isPhilosopher = isPhilosopherBin(bin);

            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fp.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir, isCrystalc);
            for (String rawFilePath : lcmsFilePaths) {
                // Comet
                List<String> commands = new ArrayList<>();
                commands.add(bin);
                if (isPhilosopher) // for philosopher we always add the correct command
                {
                    commands.add(PhilosopherProps.CMD_PEPTIDE_PROPHET);
                }

                if (!peptideProphetParams.getCmdLineParams().isEmpty()) {
                    String cmdOpts = peptideProphetParams.getCmdLineParams();
                    List<String> opts = StringUtils.splitCommandLine(cmdOpts);
                    for (String opt : opts) {
                        if (!opt.isEmpty()) {
                            if (opt.equals(PhilosopherProps.CMD_PEPTIDE_PROPHET)) {
                                continue;
                            }
                            commands.add(opt);
                        }
                    }
                }
                commands.add("--database");
                commands.add(fastaPath);

                String pepxmlInWd = pepxmlClean.get(rawFilePath);
                if (pepxmlInWd == null) {
                    JOptionPane.showMessageDialog(comp, "PeptideProphet process could not figure where a pepxml was.\n"
                            + "RAW: " + rawFilePath + "\n",
                            "Error", JOptionPane.ERROR_MESSAGE);
                    return null;
                }

                commands.add(pepxmlInWd);
                ProcessBuilder pb = new ProcessBuilder(commands);
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
    public static List<ProcessBuilder> pbsProteinProphet(Component comp, boolean isProteinProphet, String binPhilosopher, String txtProteinProphetCmdLineOpts,
        FraggerPanel fraggerPanel, boolean isProteinProphetInteractStar, boolean isCrystalc,
        String programsDir, String workingDir, String txtCombinedProtFile, List<String> lcmsFilePaths) {
        if (isProteinProphet) {
            String bin = binPhilosopher;
            if (StringUtils.isNullOrWhitespace(bin)) {
                JOptionPane.showMessageDialog(comp, "ProteinProphet binary can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            bin = PathUtils.testBinaryPath(bin, programsDir);
            if (bin == null) {
                JOptionPane.showMessageDialog(comp, "ProteinProphet binary not found or could not be launched.\n"
                        + "Neither on PATH, nor in the working directory",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            Path combinedProtFilePath = getCombinedProtFilePath(comp, txtCombinedProtFile, workingDir);
            if (combinedProtFilePath == null) {
                JOptionPane.showMessageDialog(comp, "ProteinProphet output file name can not be an empty string.\n",
                        "Error", JOptionPane.ERROR_MESSAGE);
                return null;
            }

            ProteinProphetParams proteinProphetParams = new ProteinProphetParams();
            proteinProphetParams.setCmdLineParams(txtProteinProphetCmdLineOpts);
            List<ProcessBuilder> builders = new ArrayList<>();

            List<String> createdInteractFiles = new ArrayList<>();
            List<String> commands = new ArrayList<>();
            commands.add(bin);
            boolean isPhilosopher = isPhilosopherBin(bin);

            Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
            Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir, isCrystalc);
            Map<String, String> interacts = createInteractFilePaths(pepxmlClean, workingDir, fraggerPanel.getOutputFileExt());

            if (isPhilosopher) {
                commands.add(PhilosopherProps.CMD_PROTEIN_PROPHET);

                // --output flag should be available in the latest philosopher
                String combined = txtCombinedProtFile.trim();
                String combinedOpt = null;
                if (StringUtils.isNullOrWhitespace(combined)) {
                    combinedOpt = "interact";
                } else {
                    final String pepxmlExt = ".prot.xml";
                    if (combined.toLowerCase().endsWith(pepxmlExt)) {
                        combinedOpt = combined.substring(0, combined.toLowerCase().indexOf(pepxmlExt));
                    } else {
                        combinedOpt = combined;
                    }
                }
                if (combinedOpt != null) {
                    commands.add("--output");
                    commands.add(combinedOpt);
                }

                // for Philosopher command line flags go before files
                String cmdLineOpts = proteinProphetParams.getCmdLineParams().trim();
                if (!StringUtils.isNullOrWhitespace(cmdLineOpts)) {
                    List<String> opts = StringUtils.splitCommandLine(cmdLineOpts);
                    commands.addAll(opts);
                }

                if (isProteinProphetInteractStar) {
                    String sep = FileSystems.getDefault().getSeparator();
                    String interactsGlob = workingDir + sep + "interact-*.pep.xml";
                    commands.add(interactsGlob);
                    //commands.add(getCombinedProtFilePath(workingDir).getFileName().toString());
                    ProcessBuilder pb = new ProcessBuilder(commands);
                    builders.add(pb);
                } else {
                    for (String filePath : lcmsFilePaths) {
                        String interact = interacts.get(filePath);
                        if (!StringUtils.isNullOrWhitespace(interact)) {
                            createdInteractFiles.add(interact);
                        }
                    }
                    for (String f : createdInteractFiles) {
                        Path interactFullPath = Paths.get(f);
                        String interactFileName = interactFullPath.getFileName().toString();
                        commands.add(interactFileName);
                    }
                    ProcessBuilder pb = new ProcessBuilder(commands);
                    builders.add(pb);
                }
            } else {
                for (String filePath : lcmsFilePaths) {
                    String interact = interacts.get(filePath);
                    if (!StringUtils.isNullOrWhitespace(interact)) {
                        createdInteractFiles.add(interact);
                    }
                }

                // output file
                commands.add(combinedProtFilePath.toString());

                // for native ProteinProphet command line flags go in the end
                String cmdLineOpts = proteinProphetParams.getCmdLineParams().trim();
                if (!cmdLineOpts.isEmpty()) {
                    List<String> opts = StringUtils.splitCommandLine(cmdLineOpts);
                    commands.addAll(opts);
                }
                ProcessBuilder pb = new ProcessBuilder(commands);
                builders.add(pb);
            }

            for (ProcessBuilder pb : builders) {
                pb.directory(Paths.get(workingDir).toFile());
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
                    binPath = Paths.get(workingDir, bin);
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

            // for native TPP we will add some magic variables
//            if (!isPhilosopher) {
//                String ENV_XML_ONLY = "XML_ONLY";
//                env.put(ENV_XML_ONLY, "1");
//
//                String ENV_PATH = "PATH";
//                String envPath = env.get(ENV_PATH);
//                if (envPath == null) {
//                    envPath = "";
//                } else {
//                    envPath = envPath.trim();
//                }
//                StringBuilder sbEnvPath = new StringBuilder(envPath);
//                if (sbEnvPath.length() != 0)
//                    sbEnvPath.append(";");
//                // the ProteinProphet can be either in working directory, or in some directory
//                // that we can get from the executable absolute path
//                String binFolder = null;
//                try {
//                    Path binPath = Paths.get(bin);
//                    if (binPath.isAbsolute()) {
//                        // the path to the executable was specified as absolute, other needed files must be there as well
//                        binFolder = binPath.toAbsolutePath().getParent().toString();
//                    } else if (Files.exists(binPath)) {
//                        binFolder = binPath.toAbsolutePath().getParent().toString();
//                    } else {
//                        binPath = Paths.get(workingDir, bin);
//                        if (Files.exists(binPath)) {
//                            binFolder = binPath.toAbsolutePath().getParent().toString();
//                        }
//                    }
//                } catch (Exception ignore) {
//                    // let's hope that everything ProteinProphet needs can be found on system PATH
//                }
//                if (binFolder != null) {
//                    sbEnvPath.append(binFolder);
//                    env.put(ENV_PATH, sbEnvPath.toString());
//                }
//            }
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
      boolean isCrystalc,
      boolean isReportDbAnnotate, String textReportAnnotate, String dbPath,
      boolean isReportFilter, String textReportFilter,
      boolean isReportProteinLevelFdr,
      boolean isLabelfree, String textReportLabelfree,
      String programsDir, String workingDir, String txtCombinedProtFile, List<String> lcmsFilePaths) {
      if (isReport) {
          String bin = binPhilosopher;
          if (StringUtils.isNullOrWhitespace(bin)) {
              JOptionPane.showMessageDialog(comp, "Philosopher binary can not be an empty string.\n",
                      "Error", JOptionPane.ERROR_MESSAGE);
              return null;
          }
          bin = PathUtils.testBinaryPath(bin, programsDir);
          if (bin == null) {
              JOptionPane.showMessageDialog(comp, "Philosopher binary not found or could not be launched.\n"
                      + "Neither on PATH, nor in the working directory",
                      "Error", JOptionPane.ERROR_MESSAGE);
              return null;
          }

          Path combinedProtFilePath = getCombinedProtFilePath(comp, txtCombinedProtFile, workingDir);
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
          boolean isPhilosopher = isPhilosopherBin(bin);

          Map<String, String> pepxmlDirty = createPepxmlFilePathsDirty(lcmsFilePaths, fraggerPanel.getOutputFileExt());
          Map<String, String> pepxmlClean = createPepxmlFilePathsAfterMove(pepxmlDirty, workingDir, isCrystalc);
          Map<String, String> interacts = createInteractFilePaths(pepxmlClean, workingDir, fraggerPanel.getOutputFileExt());

          if (isReportDbAnnotate) {
              List<String> cmd = new ArrayList<>();
              cmd.add(bin);
              cmd.add(PhilosopherProps.CMD_DATABASE);
              cmd.add("--annotate");
              String fastaPath = dbPath;
              if (fastaPath == null) {
                  JOptionPane.showMessageDialog(comp, "Fasta file path can't be empty (Report)",
                          "Warning", JOptionPane.WARNING_MESSAGE);
                  return null;
              }
              cmd.add(fastaPath);
              String annotateParams = textReportAnnotate;
              if (!StringUtils.isNullOrWhitespace(annotateParams)) {
                  String[] params = annotateParams.split("[\\s]+");
                  cmd.addAll(Arrays.asList(params));
              }
              builders.add(new ProcessBuilder(cmd));
          }

          // philosopher filter
          if (isReportFilter) {
              List<String> cmd = new ArrayList<>();
              cmd.add(bin);
              cmd.add(PhilosopherProps.CMD_FILTER);
              String filterParams = textReportFilter;
              if (!StringUtils.isNullOrWhitespace(filterParams)) {
                  String[] params = filterParams.split("[\\s]+");
                  cmd.addAll(Arrays.asList(params));
              }
              cmd.add("--pepxml");
              cmd.add(workingDir);
              if (isReportProteinLevelFdr) {
                  cmd.add("--protxml");
                  cmd.add(combinedProtFilePath.toString());
              }
              builders.add(new ProcessBuilder(cmd));
          }

          // philosopher freequant (labelfree)
          if (isLabelfree) {
              List<String> cmd = new ArrayList<>();
              cmd.add(bin);
              cmd.add(PhilosopherProps.CMD_LABELFREE);

              List<String> allowed = new ArrayList<>();
              allowed.add("ptw");
              allowed.add("tol");
              String labelfreeParams = textReportLabelfree;
              for (String paramName : allowed) {
                  Pattern reFullParam = Pattern.compile(String.format("--%s\\s+(\\d+(?:\\.\\d+)?)", paramName));
                  Matcher m = reFullParam.matcher(labelfreeParams);
                  if (m.find()) {
                      cmd.add("--" + paramName);
                      cmd.add(m.group(1));
                  }
              }
              // we have checked that all lcms files are in the same folder, so
              Path lcmsDir = Paths.get(lcmsFilePaths.get(0)).getParent();
              cmd.add("--dir");
              cmd.add(lcmsDir.toAbsolutePath().toString());

              builders.add(new ProcessBuilder(cmd));
          }

          // philosopher report
          List<String> cmd = new ArrayList<>();
          cmd.add(bin);
          cmd.add(PhilosopherProps.CMD_REPORT);
          builders.add(new ProcessBuilder(cmd));

          // set working dir for all processes
          final File wd = new File(workingDir);
          for (ProcessBuilder pb : builders) {
              pb.directory(wd);
          }

          return builders;
      }
      return Collections.emptyList();
  }
}
