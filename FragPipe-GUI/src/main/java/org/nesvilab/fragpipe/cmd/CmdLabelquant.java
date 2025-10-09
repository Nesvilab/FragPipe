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

import static org.nesvilab.fragpipe.cmd.ToolingUtils.writeIsobaricQuantExperimentAnnotation;
import static org.nesvilab.utils.SwingUtils.showErrorDialogWithStacktrace;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.InputLcmsFile;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.tools.philosopher.PhilosopherProps;
import org.nesvilab.fragpipe.tools.tmtintegrator.QuantLabel;
import org.nesvilab.utils.StringUtils;
import org.nesvilab.utils.SwingUtils;
import org.nesvilab.utils.UsageTrigger;
import java.awt.Component;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
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

public class CmdLabelquant extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdLabelquant.class);
  public static final String NAME = "Quant (Isobaric)";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML");

  public CmdLabelquant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, boolean isDryRun, UsageTrigger phi, String quantLevel, int tolerance, double minprob, double purity, double minIntensityPercent, QuantLabel label, Map<LcmsFileGroup, Path> annotations, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    initPreConfig();

    if (!checkCompatibleFormats(comp, mapGroupsToProtxml)) {
      return false;
    }

    for (Map.Entry<LcmsFileGroup, Path> e : annotations.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path annotationFile = e.getValue();
      final Set<Path> lcmsGroupParentDir = group.lcmsFiles.stream().map(f -> f.getPath().toAbsolutePath().getParent()).collect(Collectors.toSet());
      if (lcmsGroupParentDir.size() > 1) {
        if (Fragpipe.headless) {
          log.error("All LCMS input files for an experiment/group must be located in the same directory for " + NAME + " to work.");
        } else {
          String msg = "<html>All LCMS input files for an experiment/group must be<br/>\n"
              + "located in the same directory for " + NAME + " to work.";
          JOptionPane.showMessageDialog(comp, msg, NAME + " Error", JOptionPane.WARNING_MESSAGE);
        }
        return false;
      }

      final Path lcmsDir = lcmsGroupParentDir.iterator().next().toAbsolutePath();
      final Path groupWd = group.outputDir(wd);

      if (!lcmsDir.equals(annotationFile.toAbsolutePath().getParent())) {
        String msg = "LCMS files for an experiment/group must be in the same direcotry\n"
            + "as the annotation file for " + NAME + " to work.";
        SwingUtils.showWarningDialog(comp, msg, NAME + " Config");
        return false;
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(phi.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_LABELQUANT);

      cmd.add("--tol");
      cmd.add(tolerance + "");
      cmd.add("--level");
      if (quantLevel.equals("MS2")) {
        cmd.add("2");
      } else if (quantLevel.equals("MS3")) {
        cmd.add("3");
      } else if (quantLevel.equals("ZOOM-HR")) {
        SwingUtils.showErrorDialog(comp, "ZOOM-HR is not supported for Philosopher" + NAME + ". Please switch the `Intensity extraction tool` to `IonQuant`.", NAME + " Error");
        return false;
      }
      cmd.add("--minprob");
      cmd.add(String.format("%.3f", minprob)); // Need to match the Spinner's format: DecimalFormat df3 = new DecimalFormat("#.###");
      cmd.add("--purity");
      cmd.add(String.format("%.3f", purity)); // Need to match the Spinner's format: DecimalFormat df3 = new DecimalFormat("#.###");
      cmd.add("--removelow");
      cmd.add(String.format("%.3f", minIntensityPercent)); // Need to match the Spinner's format: DecimalFormat df3 = new DecimalFormat("#.###");
      cmd.add("--plex");
      cmd.add(Integer.toString(label.getReagentNames().size()));
      cmd.add("--annot");
      if (StringUtils.isNullOrWhitespace(annotationFile.toString())) {
        String msg = "Need to specify TMT file annotations in TMT-Integrator configuration.\n";
        SwingUtils.showWarningDialog(comp, msg, NAME + " Error");
        return false;
      }

      try {
        cmd.add(annotationFile.toFile().getCanonicalPath());
      } catch (Exception ex) {
        ex.printStackTrace();
        return false;
      }

      if (!isDryRun) {
        try {
          // copy annotation files to output directory for Report command to pick up
          Path annotationFileInGroupDir = groupWd.resolve(annotationFile.getFileName());
          if (!annotationFileInGroupDir.equals(annotationFile.toRealPath())) {
            Files.copy(annotationFile.toRealPath(), annotationFileInGroupDir, StandardCopyOption.REPLACE_EXISTING);
          }
        } catch (IOException ex) {
          ex.printStackTrace();
          return false;
        }
      }

      cmd.add("--brand");
      cmd.add(label.getType().toLowerCase());

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
      // labelQuant needs to be executed in the dir where mzml files are (and the annotation file)
      pb.directory(groupWd.toFile());

      pbis.add(PbiBuilder.from(pb));
    }

    if (!isDryRun) {
      try {
        writeIsobaricQuantExperimentAnnotation(wd, annotations);
      } catch (Exception ex) {
        showErrorDialogWithStacktrace(ex, comp);
        return false;
      }
    }

    isConfigured = true;
    return true;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, SUPPORTED_FORMATS);
    if (!notSupportedExts.isEmpty()) {
      if (Fragpipe.headless) {
        log.error(String.format("Philosopher as the TMT intensity extraction tool doesn't support '.%s' files. Please replace with mzML format or set IonQuant as the TMT intensity extraction tool.", String.join(", ", notSupportedExts)));
      } else {
        JOptionPane.showMessageDialog(comp, String.format("<html>Philosopher as the TMT intensity extraction tool doesn't support '.%s' files.<br>Please replace with mzML format or set IonQuant as the TMT intensity extraction tool.<br/>", String.join(", ", notSupportedExts)), NAME + " error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    } else if (mapGroupsToProtxml.keySet().stream().flatMap(g -> g.lcmsFiles.stream()).map(f -> StringUtils.afterLastDot(f.getPath().getFileName().toString().toLowerCase())).distinct().anyMatch(ext -> ext.endsWith("raw"))) {
      int answer = SwingUtils.showConfirmDialog(comp, new JLabel("Philosopher labelquant does not work well with raw files. Continue?"));
      return JOptionPane.OK_OPTION == answer;
    }
    return true;
  }

  @Override
  public boolean usesPhi() {
    return true;
  }
}
