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

import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tools.philosopher.PhilosopherProps;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.UsageTrigger;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class CmdPhilosopherFilter extends CmdBase {

  private static final Pattern pattern = Pattern.compile("interact-.+\\.pep\\.xml\\.tmp\\..+");

  public static final String NAME = "PhilosopherFilter";
  public static final String FN_CAPTURE_STDOUT = "filter.log";
  public static final String FN_CAPTURE_STDERR = "filter.log";

  public CmdPhilosopherFilter(boolean isRun, Path workDir) {
    super(isRun, NAME, workDir, FN_CAPTURE_STDOUT, FN_CAPTURE_STDERR);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, int ramGb, int threads, UsageTrigger usePhilosopher,
                           String decoyTag, String textReportFilter, boolean dontUseFilterProtxml,
                           Map<LcmsFileGroup, Path> mapGroupsToProtxml, InputLcmsFile firstInputLcmsFile) {

    initPreConfig();

    for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path protxml = e.getValue();
      if (group.lcmsFiles.isEmpty())
        throw new IllegalStateException("CmdReportFilter - LCMS file group is empty. "
            + "This is a bug, report to developers.");
      Path groupWd = group.outputDir(wd);

      if (Files.exists(groupWd) && Files.isDirectory(groupWd)) {
        try {
          for (Path p : Files.list(groupWd).filter(Files::isRegularFile).filter(p -> pattern.matcher(p.getFileName().toString()).matches()).collect(Collectors.toList())) {
            Files.deleteIfExists(p);
          }
        } catch (IOException ex) {
          ex.printStackTrace();
        }
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(wd));
      cmd.add(PhilosopherProps.CMD_FILTER);

      // check for extra arguments
      if (!StringUtils.isNullOrWhitespace(textReportFilter)) {
        // Always use --razor, so it is always appended later. Remove it here to avoid duplicated flags.
        textReportFilter = textReportFilter.replaceAll("--razor", "");
        if (dontUseFilterProtxml) {
          // add everything except --sequential --razor --prot 0.01`
          textReportFilter = textReportFilter.replaceAll("--sequential", "");
          textReportFilter = textReportFilter.replaceAll("--prot\\s+\\d+(?:\\.\\d+)?", "");
        }
        cmd.addAll(StringUtils.splitCommandLine(textReportFilter));
      }
      cmd.add("--tag");
      cmd.add(decoyTag);
      cmd.add("--pepxml");
      cmd.add(groupWd.toString());
      if (!group.name.contentEquals(firstInputLcmsFile.getGroup())) {
        cmd.add("--dbbin");
        cmd.add(wd.resolve(firstInputLcmsFile.getGroup()).toAbsolutePath().normalize().toString());
      }
      if (!dontUseFilterProtxml) {
        cmd.add("--protxml");
        cmd.add(protxml.toAbsolutePath().normalize().toString());
        if (!group.name.contentEquals(firstInputLcmsFile.getGroup())) {
          cmd.add("--probin");
          cmd.add(wd.resolve(firstInputLcmsFile.getGroup()).toAbsolutePath().normalize().toString());
        }
        cmd.add("--razor");
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.environment().put("GOMEMLIMIT", ramGb + "GiB");
      pb.environment().put("GOGC", "200");
      pb.environment().put("GOMAXPROCS", String.valueOf(threads));
      pb.directory(groupWd.toFile());
      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
