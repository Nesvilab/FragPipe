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

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tools.philosopher.PhilosopherProps;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.UsageTrigger;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdFreequant extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdFreequant.class);
  public static final String NAME = "FreeQuant";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML");

  public CmdFreequant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml, boolean isTMT, boolean isOpenSearch) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, SUPPORTED_FORMATS);
    if (!notSupportedExts.isEmpty()) {
      if (Fragpipe.headless) {
        if (isTMT) {
          log.error(String.format("Philosopher as the TMT intensity extraction tool doesn't support '.%s' files. Please replace with mzML format or set IonQuant as the TMT intensity extraction tool.", String.join(", ", notSupportedExts)));
        } else if (isOpenSearch) {
          log.error(String.format("FreeQuant doesn't support '.%s' files. Please replace with mzML format or disable the Quant (MS1) tab.", String.join(", ", notSupportedExts)));
        } else {
          log.error(String.format("FreeQuant doesn't support '.%s' files. Please switch to IonQuant in the Quant (MS1) tab.", String.join(", ", notSupportedExts)));
        }
      } else {
        if (isTMT) {
          JOptionPane.showMessageDialog(comp, String.format("<html>Philosopher as the TMT intensity extraction tool doesn't support '.%s' files.<br>Please replace with mzML format or set IonQuant as the TMT intensity extraction tool.<br>", String.join(", ", notSupportedExts)), NAME + " error", JOptionPane.WARNING_MESSAGE);
        } else if (isOpenSearch) {
          JOptionPane.showMessageDialog(comp, String.format("<html>FreeQuant doesn't support '.%s' files.<br>Please replace with mzML format or disable the Quant (MS1) tab.<br>", String.join(", ", notSupportedExts)), NAME + " error", JOptionPane.WARNING_MESSAGE);
        } else {
          JOptionPane.showMessageDialog(comp, String.format("<html>FreeQuant doesn't support '.%s' files.<br>Please switch to IonQuant in the Quant (MS1) tab.<br>", String.join(", ", notSupportedExts)), NAME + " error", JOptionPane.WARNING_MESSAGE);
        }
      }
      return false;
    } else if (mapGroupsToProtxml.keySet().stream().flatMap(g -> g.lcmsFiles.stream()).map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase())).distinct().anyMatch(ext -> ext.endsWith("raw"))) {
      int answer = SwingUtils.showConfirmDialog(comp, new JLabel("Philosopher freequant does not work well with raw files. Continue?"));
      return JOptionPane.OK_OPTION == answer;
    }
    return true;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher, String textReportLabelfree, Map<LcmsFileGroup, Path> mapGroupsToProtxml, boolean isTMT, boolean isOpenSearch) {

    initPreConfig();

    if (!checkCompatibleFormats(comp, mapGroupsToProtxml, isTMT, isOpenSearch)) {
      return false;
    }

    for (Map.Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();

      final Set<Path> lcmsDirsForProtxml = group.lcmsFiles.stream()
          .map(f -> f.getPath().toAbsolutePath().getParent())
          .collect(Collectors.toSet());
      if (lcmsDirsForProtxml.size() > 1) {
        if (Fragpipe.headless) {
          log.error("All LCMS input files for an experiment/group must be located in the same directory for Freequant to work.");
        } else {
          String msg = "All LCMS input files for an experiment/group must be\n"
              + "located in the same directory for Freequant to work.";
          JOptionPane.showMessageDialog(comp, msg, "Freequant Error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }

      final Path lcmsDir = lcmsDirsForProtxml.iterator().next().toAbsolutePath();
      final Path groupWd = group.outputDir(wd);

      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_LABELFREE);
      cmd.addAll(StringUtils.splitCommandLine(textReportLabelfree));

      for (InputLcmsFile p : group.lcmsFiles) {
        if (p.getPath().toString().toLowerCase().endsWith(".raw")) {
          cmd.add("--raw");
          break;
        }
      }

      // we have checked that all lcms files are in the same folder, so
      cmd.add("--dir");
      cmd.add(lcmsDir.toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
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
