package com.dmtavt.fragpipe.cmd;

import com.github.chhh.utils.StringUtils;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.JarUtils;
import com.github.chhh.utils.OsUtils;

public abstract class CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdBase.class);

  protected boolean isRun;
  final Path wd;
  final LinkedList<ProcessBuilderInfo> pbis;
  final String fileCaptureStdout;
  final String fileCaptureStderr;
  boolean isConfigured;

  public CmdBase(boolean isRun, Path workDir, String fileCaptureStdout, String fileCaptureStderr) {
    this.isRun = isRun;
    this.wd = workDir;
    this.pbis = new LinkedList<>();
    this.fileCaptureStdout = fileCaptureStdout;
    this.fileCaptureStderr = fileCaptureStderr;
  }

  public CmdBase(boolean isRun, Path workDir) {
    this(isRun, workDir, "", "");
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

  public static List<String> getNotSupportedExts(Map<LcmsFileGroup, Path> mapGroupsToProtxml, List<String> supportedExts) {
    List<String> supportedLoCase = supportedExts.stream().map(String::toLowerCase)
        .collect(Collectors.toList());
    List<String> exts = mapGroupsToProtxml.keySet().stream().flatMap(g -> g.lcmsFiles.stream())
        .map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase()))
        .distinct()
        .filter(ext -> !supportedLoCase.contains(ext)).collect(Collectors.toList());
    return exts;
  }

  public static List<String> getNotSupportedExts1(Map<InputLcmsFile, List<Path>> pepxmlFiles, List<String> supportedExts) {
    List<String> supportedLoCase = supportedExts.stream().map(String::toLowerCase)
        .collect(Collectors.toList());
    List<String> exts = pepxmlFiles.keySet().stream()
        .map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase()))
        .distinct()
        .filter(ext -> !supportedLoCase.contains(ext)).collect(Collectors.toList());
    return exts;
  }

  public static List<String> getNotSupportedExts(List<InputLcmsFile> lcmsFiles, List<String> supportedExts) {
    List<String> supportedLoCase = supportedExts.stream().map(String::toLowerCase)
        .collect(Collectors.toList());
    List<String> exts = lcmsFiles.stream()
        .map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase()))
        .distinct()
        .filter(ext -> !supportedLoCase.contains(ext)).collect(Collectors.toList());
    return exts;
  }

  public static List<String> asParts(String cmdString) {
    return StringUtils.splitCommandLine(cmdString);
  }

  public boolean isRun() {
    return isRun;
  }

  public void isRun(boolean doRun) {
    isRun = doRun;
  }

  public Path getWd() {
    return wd;
  }

  /**
   * Extending classes can override this to modify the priority level.
   * @deprecated Not used anymore.
   */
  @Deprecated
  public int getPriority() {
    return 100;
  }

  /**
   * @param graph Graph to which a node should be added and wired to other nodes.
   * @param startNode The start-of-processing graph node name.
   */
  public void configureDependencies(Graph<String, DefaultEdge> graph, String startNode) {
    configureDependencies0(graph, startNode);
  }

  /**
   * Convenience method, almost all commands should call that first in their impl of {@link #configureDependencies}.
   */
  protected void configureDependencies0(Graph<String, DefaultEdge> graph, String startNode) {
    graph.addVertex(getCmdName());
    graph.addEdge(startNode, getCmdName());
  }

  public abstract String getCmdName();

  public ProcessBuildersDescriptor getBuilderDescriptor() {
    if (!isConfigured)
      throw new IllegalStateException("Call to #getBuilderDescriptor() before calling #configure()");
    return new ProcessBuildersDescriptor(getCmdName(), getPriority(), fileCaptureStdout,
        fileCaptureStderr).addAll(pbis);
  }

  public static String createJavaDParamString(String name, String value) {
    return OsUtils.isWindows()
            ? "-D" + name + "=\"" + value + "\""
            : "-D" + name + "=" + value;
  }
}
