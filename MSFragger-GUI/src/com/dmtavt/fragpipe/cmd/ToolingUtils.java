package com.dmtavt.fragpipe.cmd;

import static com.github.chhh.utils.PathUtils.testBinaryPath;

import com.dmtavt.fragpipe.Fragpipe;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.FileCopy;
import com.github.chhh.utils.FileDelete;
import com.github.chhh.utils.FileMove;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.OsUtils;
import com.github.chhh.utils.StringUtils;
import java.awt.Component;
import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.ResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ToolingUtils {
  private static final Logger log = LoggerFactory.getLogger(ToolingUtils.class);
  private ToolingUtils() {}

  public static final String BATMASS_IO_JAR = "batmass-io-1.23.0.jar";

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

  private enum Op {COPY, MOVE, DELETE}

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsCopyFiles(Path jarFragpipe, Path dest,
      boolean ignoreMissingFiles, List<Path> files) {
    return pbsCopyMoveDeleteFiles(jarFragpipe, Op.COPY, dest, ignoreMissingFiles, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsCopyFiles(Path jarFragpipe, Path dest, List<Path> files) {
    return pbsCopyMoveDeleteFiles(jarFragpipe, Op.COPY, dest, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsMoveFiles(Path jarFragpipe, Path dest,
      boolean ignoreMissingFiles, List<Path> files) {
    return pbsCopyMoveDeleteFiles(jarFragpipe, Op.MOVE, dest, ignoreMissingFiles, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsMoveFiles(Path jarFragpipe, Path dest, List<Path> files) {
    return pbsCopyMoveDeleteFiles(jarFragpipe, Op.MOVE, dest, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  public static List<ProcessBuilder> pbsDeleteFiles(Path jarFragpipe, List<Path> files) {
    return pbsCopyMoveDeleteFiles(jarFragpipe, Op.DELETE, null, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  private static List<ProcessBuilder> pbsCopyMoveDeleteFiles(Path jarFragpipe, Op operation, Path dest,
      List<Path> files) {
    return pbsCopyMoveDeleteFiles(jarFragpipe, operation, dest, false, files);
  }

  /**
   * @param jarFragpipe Use {@link JarUtils#getCurrentJarUri()} to get that from the current Jar.
   */
  private static List<ProcessBuilder> pbsCopyMoveDeleteFiles(Path jarFragpipe, Op operation, Path dest,
      boolean ignoreMissingFiles, List<Path> files) {
    if (jarFragpipe == null) {
      throw new IllegalArgumentException("jar can't be null");
    }

    List<ProcessBuilder> pbs = new LinkedList<>();
    for (Path file : files) {
      if (Objects.equals(file.getParent(), (dest))) {
        continue;
      }
      List<String> cmd = new ArrayList<>();
      cmd.add(Fragpipe.getBinJava());
      cmd.add("-cp");
      final String commons_io_jar_path = org.apache.commons.io.FileUtils.class.getProtectionDomain().getCodeSource().getLocation().getPath();
      cmd.add(jarFragpipe.toAbsolutePath() +
              (operation == Op.MOVE ?
                      File.pathSeparator + commons_io_jar_path :
                      ""));
      switch (operation) {
        case COPY:
          cmd.add(FileCopy.class.getCanonicalName());
          break;
        case MOVE:
          cmd.add(FileMove.class.getCanonicalName());
          break;
        case DELETE:
          cmd.add(FileDelete.class.getCanonicalName());
          break;
        default:
          throw new IllegalStateException("Unknown enum value: " + operation.toString());
      }
      if (ignoreMissingFiles) {
        cmd.add(FileMove.NO_ERR);
      }
      cmd.add(file.toAbsolutePath().normalize().toString());
      if (dest != null)
        cmd.add(dest.resolve(file.getFileName()).toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      pbs.add(pb);
    }
    return pbs;
  }

  public static Map<InputLcmsFile, Path> getPepxmlFilePathsAfterSearch(List<InputLcmsFile> lcmsFiles, String ext) {
    HashMap<InputLcmsFile, Path> pepxmls = new HashMap<>();
    for (InputLcmsFile f : lcmsFiles)
      pepxmls.put(f, Paths.get(StringUtils.upToLastDot(f.getPath().toString()) + "." + ext));
    return pepxmls;
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
      Image icon = Toolkit.getDefaultToolkit().getImage(Fragpipe.class.getResource(location));
      images.add(icon);
    }
    return images;
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
        .map(f -> workingDir.resolve(f.getPath().getFileName()))
        .collect(Collectors.toList());
  }

  public static String getDefaultBinMsfragger() {
    log.debug("Loading MSFragger bin path: ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSFRAGGER)");
    String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_MSFRAGGER);
    return path == null ? "MSFragger.jar" : path;
  }

  public static String getDefaultBinPhilosopher() {
    String path = ThisAppProps.load(ThisAppProps.PROP_BIN_PATH_PHILOSOPHER);
    if (path != null) {
      return path;
    }
    ResourceBundle bundle = ThisAppProps.getLocalBundle();
    String winName = bundle.getString("default.philosopher.win"); // NOI18N
    String nixName = bundle.getString("default.philosopher.nix"); // NOI18N
    return OsUtils.isWindows() ? winName : nixName;
  }

  static boolean isPhilosopherAndNotTpp(String binPathToCheck) {
    Pattern isPhilosopherRegex = Pattern.compile("philosopher", Pattern.CASE_INSENSITIVE);
    Matcher matcher = isPhilosopherRegex.matcher(binPathToCheck);
    return matcher.find();
  }

}
