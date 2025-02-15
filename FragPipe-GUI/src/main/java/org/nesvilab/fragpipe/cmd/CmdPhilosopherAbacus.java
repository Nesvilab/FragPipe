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
import org.nesvilab.utils.StringUtils;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.UsageTrigger;

/**
 * The `Multi-Experiment Report`.
 */
public class CmdPhilosopherAbacus extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdPhilosopherAbacus.class);

  private static final String NAME = "PhilosopherAbacus";

  public CmdPhilosopherAbacus(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher, String textReportFilterCmdOpts, boolean isProtLevelSummary, boolean isPepLevelSummary, boolean isCheckFilterNoProtxml, String decoyTag, int plex, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    final List<String> flagsAbacus = Arrays.asList("--picked", "--uniqueonly");

    initPreConfig();

    final long numGroups = mapGroupsToProtxml.keySet().stream().map(group -> group.name).distinct().count();
    if (numGroups < 2) {
      if (Fragpipe.headless) {
        log.error("Multi-experiment report requires more than one experiment/group.");
      } else {
        String msg = "<code>Multi-experiment report</code> requires more than one experiment/group.<br/>\n"
            + "You can assign experiment/group names to LCMS files on the LCMS file selection tab.<br/>\n"
            + "Alternatively, you can turn off <code>Multi-experiment report<code> checkbox on Report tab.<br/>\n";
        JEditorPane ep = SwingUtils.createClickableHtml(msg);
        SwingUtils.showDialog(comp, ep, "Multi-experiment report configuration error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }

    final Map<Path, List<LcmsFileGroup>> mapProtxmlToGroups = new HashMap<>();
    mapGroupsToProtxml.forEach((group, protxml) -> {
      final List<LcmsFileGroup> groups = mapProtxmlToGroups.get(protxml);
      if (groups == null) {
        mapProtxmlToGroups.put(protxml, new ArrayList<>(Collections.singleton(group)));
      } else {
        groups.add(group);
      }
    });

    for (Entry<Path, List<LcmsFileGroup>> entry : mapProtxmlToGroups.entrySet()) {
      Path protxml = entry.getKey();
      List<LcmsFileGroup> groups = entry.getValue();

      List<Path> outputDirsForPepXML = groups.stream().map(group -> group.outputDir(wd)).distinct().collect(Collectors.toList());
      log.debug("Protxml: {}, outputDirsForPepXML: {}", protxml.toString(), outputDirsForPepXML.stream().map(Path::toString).collect(Collectors.joining(", ")));

      if (outputDirsForPepXML.size() < 2) {
        if (Fragpipe.headless) {
          log.error("Multi-experiment report requires experiments processed together by ProteinProphet.");
        } else {
          String msg = "Multi-experiment report requires experiments processed together by "
              + "ProteinProphet.<br/><br/>"
              + "Encountered a prot-xml file mapped to only one experiment/group:<br/>"
              + "&nbsp;&nbsp;" + protxml + "<br/><br/>"
              + "On <b>Downstream tab</b> in <b>ProteinProphet group</b> please uncheck "
              + "the checkbox <i>Separate prot-xml per experiment/group</i>.";
          JEditorPane ep = SwingUtils.createClickableHtml(msg);
          SwingUtils.showDialog(comp, ep, "Multi-experiment report configuration error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }

      // we'll only take the flags from the command that Abacus recognizes, and other than --razor and --reprint
      final List<String> filterCmdLineParts = StringUtils.splitCommandLine(textReportFilterCmdOpts);
      final LinkedHashSet<String> cmdAddonParts = filterCmdLineParts.stream()
          .filter(flagsAbacus::contains).collect(Collectors.toCollection(LinkedHashSet::new));

      // Alexey wants to always use only `--reprint --razor`
      cmdAddonParts.add("--razor");
      cmdAddonParts.add("--reprint");

      List<String> cmd = new ArrayList<>();
      final Path executeInDir = protxml.toAbsolutePath().getParent();
      cmd.add(usePhilosopher.useBin(executeInDir));
      cmd.add("abacus");
      cmd.addAll(cmdAddonParts);
      cmd.add("--tag");
      cmd.add(decoyTag);
      if (!isCheckFilterNoProtxml && isProtLevelSummary) {
        cmd.add("--protein");
      }
      if (isPepLevelSummary) {
        cmd.add("--peptide");
      }
      if (plex > 0) {
        cmd.add("--labels");
        cmd.add("--plex");
        cmd.add(String.valueOf(plex));
      }
      // list locations with pepxml files
      for (Path pepxmlDir : outputDirsForPepXML) {
        cmd.add(pepxmlDir.getFileName().toString());
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(executeInDir.toFile());
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
