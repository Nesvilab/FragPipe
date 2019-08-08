package umich.msfragger.cmd;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.params.ThisAppProps;
import umich.msfragger.util.JarUtils;
import umich.msfragger.util.OsUtils;

public abstract class CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdBase.class);

  final boolean isRun;
  final Path wd;
  final LinkedList<ProcessBuilder> pbs;
  final String fileCaptureStdout;
  final String fileCaptureStderr;
  boolean isConfigured;

  public CmdBase(
      boolean isRun, Path workDir, String fileCaptureStdout, String fileCaptureStderr) {
    this.isRun = isRun;
    this.wd = workDir;
    this.fileCaptureStdout = fileCaptureStdout;
    this.fileCaptureStderr = fileCaptureStderr;
    this.pbs = new LinkedList<>();
  }

  public CmdBase(
      boolean isRun, Path workDir) {
    this.isRun = isRun;
    this.wd = workDir;
    this.fileCaptureStdout = "";
    this.fileCaptureStderr = "";
    this.pbs = new LinkedList<>();
  }

  public static String constructClasspathString(List<Path> jarDepsPaths, Path ... additionalJars) {
    List<String> toJoin = new ArrayList<>();
    final Function<Path, String> pathMapping = (Path p) -> p.toAbsolutePath().normalize().toString();
    toJoin.addAll(jarDepsPaths.stream().map(pathMapping).collect(Collectors.toList()));
    toJoin.addAll(Arrays.stream(additionalJars).map(pathMapping).collect(Collectors.toList()));
    final String sep = System.getProperties().getProperty("path.separator");
    final String classpath = org.apache.commons.lang3.StringUtils.join(toJoin, sep);
    return OsUtils.isWindows() ? "\"" + classpath + "\"" : classpath;
  }

  /**
   * Unpacks jar files from the final FragPipe jar (next to ToolingUtils.class).
   * @param jars The names of files to unpack. Here we name them `.jazz` files.
   */
  protected static boolean unpackJars(List<String> jars, final List<Path> unpackedPaths, String nameForErrorMsgs) {
    try {
      for (String jarDep : jars) {
        Path unpacked = JarUtils
            .unpackFromJar(ToolingUtils.class, "/" + jarDep,
                ThisAppProps.UNPACK_TEMP_SUBDIR, true, true);
        unpackedPaths.add(unpacked);
      }
    } catch (IOException e) {
      String msg = String.format("Could not unpack %s to temp dir", nameForErrorMsgs);
      log.error(msg, e);
      JOptionPane.showMessageDialog(null,
          "Could not unpack tools to a temporary directory.\n"
              + "Try disabling " + nameForErrorMsgs + ".", "Can't unpack", JOptionPane.ERROR_MESSAGE);
      return false;
    }
    return true;
  }

  public boolean isRun() {
    return isRun;
  }

  public Path getWd() {
    return wd;
  }

  /**
   * Extending classes can override this to modify the priority level.
   */
  public int getPriority() {
    return 100;
  }

  public abstract String getCmdName();

  public ProcessBuildersDescriptor builders() {
    if (!isConfigured)
      throw new IllegalStateException("Call to #processBuilders() before calling #configure()");
    return new ProcessBuildersDescriptor(getCmdName(), getPriority(), fileCaptureStdout,
        fileCaptureStderr).addAll(pbs);
  }
}
