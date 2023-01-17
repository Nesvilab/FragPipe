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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe.  If not, see <https://www.gnu.org/licenses/>.
 */

package com.dmtavt.fragpipe.cmd;

import static com.github.chhh.utils.SwingUtils.showErrorDialogWithStacktrace;

import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.github.chhh.utils.UsageTrigger;
import java.awt.Component;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.io.FilenameUtils;

public class CmdPhilosopherReport extends CmdBase {

  public static final String NAME = "PhilosopherReport";

  public CmdPhilosopherReport(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher, boolean isDryRun, boolean doPrintDecoys, boolean doMsstats, boolean doRemoveContaminants, Map<LcmsFileGroup, Path> mapGroupsToProtxml, Map<LcmsFileGroup, Path> groupAnnotationMap) {

    initPreConfig();

    if (!isDryRun && doMsstats) {
      try {
        String line;
        BufferedWriter bw = Files.newBufferedWriter(wd.resolve("MSstatsTMT_annotation.csv"));
        bw.write("Run,Fraction,TechRepMixture,Mixture,Channel,BioReplicate,Condition\n");
        for (Map.Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
          Path annotationPath = groupAnnotationMap.get(e.getKey());
          BufferedReader br = new BufferedReader(new FileReader(annotationPath.toFile()));
          while ((line = br.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) {
              continue;
            }
            String[] parts = line.split("\\s");
            if (parts[1].trim().equalsIgnoreCase("na")) {
              continue;
            }
            String[] parts2 = parts[1].trim().split("_");

            int fraction = 1;
            int replicate = 1;
            for (InputLcmsFile inputLcmsFile : e.getKey().lcmsFiles) {
              if (parts2.length == 2) {
                try {
                  replicate = Integer.parseInt(parts2[1]);
                  bw.write(FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString()) + "," + fraction + "," + replicate + "," + parts[1].trim() + "," + parts[0].trim() + "," + replicate + "," + parts2[0].trim() + "\n");
                } catch (NumberFormatException ex) {
                  bw.write(FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString()) + "," + fraction + "," + replicate + "," + parts[1].trim() + "," + parts[0].trim() + "," + replicate + "," + parts[1].trim() + "\n");
                }
              } else {
                bw.write(FilenameUtils.getBaseName(inputLcmsFile.getPath().getFileName().toString()) + "," + fraction + "," + replicate + "," + parts[1].trim() + "," + parts[0].trim() + "," + replicate + "," + parts[1].trim() + "\n");
              }
              ++fraction;
            }
          }
          br.close();
        }
        bw.close();
      } catch (Exception ex) {
        showErrorDialogWithStacktrace(ex, comp);
        return false;
      }
    }

    Set<Path> groupWds = mapGroupsToProtxml.keySet().stream().map(g -> g.outputDir(wd))
        .collect(Collectors.toSet());
    for (Path groupWd : groupWds) {
      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_REPORT);
      if (doPrintDecoys) {
        cmd.add("--decoys");
      }
      if (doMsstats) {
        cmd.add("--msstats");
      }
      if (doRemoveContaminants) {
        cmd.add("--removecontam");
      }
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
