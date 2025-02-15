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

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;
import org.nesvilab.fragpipe.api.IConfig;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdBase.class);

  protected boolean isRun;
  protected String title = null;
  final Path wd;
  final LinkedList<ProcessBuilderInfo> pbis;
  final String fileCaptureStdout;
  final String fileCaptureStderr;
  IConfig config = () -> {
    log.warn("No config set for command [{}]", getCmdName());
    return true;
  };
  boolean isConfigured;

  public CmdBase(boolean isRun, String title, Path workDir, String fileCaptureStdout, String fileCaptureStderr) {
    this.isRun = isRun;
    this.title = title;
    this.wd = workDir;
    this.pbis = new LinkedList<>();
    this.fileCaptureStdout = fileCaptureStdout;
    this.fileCaptureStderr = fileCaptureStderr;
  }

  public CmdBase(boolean isRun, Path workDir) {
    this(isRun, null, workDir, "", "");
  }

  public CmdBase(boolean isRun, String title, Path workDir) {
    this(isRun, title, workDir, "", "");
  }

  protected void initPreConfig() {
    pbis.clear();
    isConfigured = false;
  }

  public String getTitle() {
    return StringUtils.isBlank(title) ? getCmdName() : title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  @Override
  public String toString() {
    return getCmdName();
//    return new StringJoiner(", ", CmdBase.class.getSimpleName() + "[", "]")
//        .add("name=" + getCmdName())
//        .add("isRun=" + isRun)
//        .add("wd=" + wd)
//        .toString();
  }

  public IConfig getConfig() {
    return config;
  }

  public void setConfig(IConfig config) {
    this.config = config;
  }

  public boolean usesPhi() {
    return false;
  }

  public static String constructClasspathString(List<Path> jarDepsPaths, Path ... additionalJars) {
    List<String> toJoin = new ArrayList<>();
    final Function<Path, String> pathMapping = (Path p) -> p.toAbsolutePath().normalize().toString();
    toJoin.addAll(jarDepsPaths.stream().map(pathMapping).collect(Collectors.toList()));
    toJoin.addAll(Arrays.stream(additionalJars).map(pathMapping).collect(Collectors.toList()));
    final String sep = System.getProperties().getProperty("path.separator");
    final String classpath = String.join(sep, toJoin);
    return OsUtils.asSingleArgument(classpath);
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

  public void setRun(boolean doRun) {
    isRun = doRun;
  }

  public Path getWd() {
    return wd;
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
      throw new IllegalStateException("Call to #getBuilderDescriptor() before calling #configure(). From command [" + getCmdName() + "]");
    return new ProcessBuildersDescriptor(getCmdName(), fileCaptureStdout,
        fileCaptureStderr).addAll(pbis);
  }

  public static String createJavaDParamString(String name, String value) {
    return "-D" + name + "=" + OsUtils.asSingleArgument(value);
  }
}
