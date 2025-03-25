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

import static org.nesvilab.fragpipe.cmd.ToolingUtils.getUnimodOboPath;
import static org.nesvilab.utils.OsUtils.isWindows;

import org.nesvilab.fragpipe.Fragpipe;
import org.nesvilab.fragpipe.api.LcmsFileGroup;
import org.nesvilab.fragpipe.api.PyInfo;
import org.nesvilab.fragpipe.tabs.TabWorkflow;
import org.nesvilab.fragpipe.tabs.TabWorkflow.InputDataType;
import org.nesvilab.fragpipe.tools.speclibgen.SpecLibGen2;
import org.nesvilab.fragpipe.tools.speclibgen.SpeclibPanel;
import org.nesvilab.utils.OsUtils;
import org.nesvilab.utils.SwingUtils;
import java.awt.Component;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CmdSpecLibGen extends CmdBase {

  private static final Logger log = LoggerFactory.getLogger(CmdSpecLibGen.class);
  public static final String NAME = "SpecLibGen";

  public CmdSpecLibGen(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, SpecLibGen2 slg, Map<LcmsFileGroup, Path> mapGroupsToProtxml, String fastaPath, boolean isRunProteinProphet, InputDataType dataType, int threads, String decoyTag) {

    initPreConfig();

    final String[] compatibleExts = new String[]{".d", ".mzml", ".mzxml", ".raw", ".mgf"};
    final Predicate<String> isFileCompatible = fn -> Arrays.stream(compatibleExts).anyMatch(ext -> fn.toLowerCase().endsWith(ext));

    boolean isIncompatibleInputs = mapGroupsToProtxml.keySet().stream()
        .flatMap(g -> g.lcmsFiles.stream())
        .anyMatch(lcms -> isFileCompatible.negate().test(lcms.getPath().getFileName().toString()));
    if (isIncompatibleInputs) {
      if (Fragpipe.headless) {
        log.error(String.format("Spectral library generation with %s is currently only compatible with %s input files. You can convert your data using msconvert program from ProteoWizard.", "EasyPQP", String.join(", ", compatibleExts)));
      } else {
        JOptionPane.showMessageDialog(comp, String.format("Spectral library generation with %s is currently only\n"
            + "compatible with %s input files.\n"
            + "You can convert your data using msconvert program from ProteoWizard.", "EasyPQP", String.join(", ", compatibleExts)), "Incompatible input data", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }


    if (mapGroupsToProtxml.size() > 1) {
      if (!Fragpipe.headless) {
        int res = JOptionPane.showConfirmDialog(comp,
            "<html>You have more than 1 experiment/group and spectral<br/>"
                + "library generation is turned on. In that case a separate<br/>"
                + "spectral library is created for each group.<br/><br/>"
                + "<b>Select Yes</b> to continue as-is.<br/><br/>"
                + "<b>Otherwise</b>, if you want a single spectral library generated<br/>"
                + "from ALL input files:<br/>"
                + "On Workflow tab, LCMS files section change Experiment/Group configuration.<br/>"
                + "E.g. press the <i>Clear Experiments</i> button there.",
            "SpecLibGen config warning", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
        if (JOptionPane.YES_OPTION != res) {
          return false;
        }
      }
    }

    final SpeclibPanel speclibPanel = Fragpipe.getStickyStrict(SpeclibPanel.class);
    if (!speclibPanel.checkGlycoMode()) {
      if (Fragpipe.headless) {
        log.error("EasyPQP glyco modes are only supported for psm.tsv conversion. Please change conversion filetype to psm.tsv and try again.");
      } else {
        JOptionPane.showMessageDialog(comp, "EasyPQP glyco modes are only supported for psm.tsv conversion. Please change conversion filetype to psm.tsv and try again.", "Conversion type error", JOptionPane.WARNING_MESSAGE);
      }
      return false;
    }

    for (Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path protxml = e.getValue();
      final Path groupWd = group.outputDir(wd);

      if (!isRunProteinProphet && !Files.exists(protxml)) {
        if (Fragpipe.headless) {
          log.error("ProteinProphet not selected and the output directory: " + groupWd.toString() + " does not contain a '" + protxml.getFileName().toString() + "' file. Either uncheck Spectral Library Generation checkbox or enable ProteinProphet.");
        } else {
          JOptionPane.showMessageDialog(comp,
              "ProteinProphet not selected and the output directory:\n"
                  + "    " + groupWd.toString() + "\n"
                  + "does not contain a '" + protxml.getFileName().toString() + "' file.\n\n"
                  + "Either uncheck Spectral Library Generation checkbox or enable ProteinProphet.", "Spec Lib Gen configuration Error", JOptionPane.ERROR_MESSAGE);
        }
        return false;
      }

      // for current implementation of speclibgen scripts mzml files need to be
      // located next to pepxml files
      final List<ProcessBuilder> pbsDeleteLcmsFiles = new ArrayList<>();

      List<String> cmd = new ArrayList<>();
      cmd.add(slg.getPython().getCommand());
      cmd.add("-u"); // PYTHONUNBUFFERED: when mixing subprocess output with Python output, use this to keep the outputs in order
      if (isWindows()) {
        cmd.add("-I"); // isolated mode
      }
      cmd.add(slg.getScriptSpecLibGenPath().toString());

      /**
       * See https://github.com/grosenberger/easypqp on how to install EasyPQP
       * EasyPQP needs the following placed in the pep xml directory
       * - MGFs or mzMLs
       * - interact.pep.xml
       * - peptide.tsv, psm.tsv from Philosopher
       * */
      cmd.add(fastaPath);
      cmd.add(groupWd.toString()); // this is "Pep xml directory"
      cmd.add("unused");  // lcms files, `File.pathSeparator` separated
      cmd.add(groupWd.toString()); // output directory
      cmd.add("True"); // overwrite (true/false), optional arg
      cmd.add("unused"); // philosopher binary path (not needed for easyPQP)
      cmd.add("use_easypqp"); // philosopher binary path (not needed for easyPQP)

      TabWorkflow tabWorkflow = Fragpipe.getStickyStrict(TabWorkflow.class);

      final String cal = speclibPanel.getEasypqpCalOption();
      final String im_cal = speclibPanel.getEasypqpIMCalOption();
      final Path calTsvPath = speclibPanel.getEasypqpCalFilePath();
      final Path imCalTsvPath = speclibPanel.getEasypqpIMCalFilePath();
      cmd.add((cal.equals("a tsv file") ? calTsvPath.toString() : cal) + File.pathSeparator +
              (im_cal.equals("a tsv file") ? imCalTsvPath.toString() : im_cal)); // alignment options
      cmd.add(String.valueOf(tabWorkflow.getThreads()));

      Path unimodPath;
      try {
        unimodPath = getUnimodOboPath("unimod_old.xml"); // Use the same old unimod file as in OpenMS to avoid an error due to mismatches of some modifications.
      } catch (Exception ex) {
        ex.printStackTrace();
        return false;
      }

      if (unimodPath.toAbsolutePath().normalize().toString().contains(" ")) {
        SwingUtils.showErrorDialog(comp, "There are spaces in " + unimodPath.toAbsolutePath() + ". Please put FragPipe to a place where there are no spaces in the path.", "Spaces are not allowed");
        return false;
      }

      final double max_delta_unimod = speclibPanel.getEasypqp_max_delta_unimod(); // EasyPQP convert
      final double max_delta_ppm = speclibPanel.getEasypqp_max_delta_ppm(); // EasyPQP convert
      final String fragment_types = speclibPanel.getEasypqp_fragment_types(); // EasyPQP convert
      final double rt_lowess_fraction = speclibPanel.getEasypqpRTLowessFraction(); // EasyPQP library

      cmd.add(OsUtils.asSingleArgument(String.format("--unimod %s --max_delta_unimod %s --max_delta_ppm %s --fragment_types %s %s%s%s%s",
              unimodPath.toAbsolutePath().normalize().toString().replace("\\", "/"),
              max_delta_unimod,
              max_delta_ppm,
              fragment_types.replace("'", "\\'"),
              speclibPanel.hasNeutralLoss() ? "--enable_unspecific_losses " : "",
              speclibPanel.isConvertPSM() ? "--decoy_prefix " + decoyTag + " ": "",
              speclibPanel.isConvertPSM() && !speclibPanel.getEasypqpGlycoOption().isEmpty() ? "--labile_mods " + speclibPanel.getEasypqpGlycoOption() + " " : "",
              speclibPanel.isConvertPSM() ? String.format("--max_glycan_q %s ", speclibPanel.getEasypqpMaxGlycanQ()) : ""
      )));   // EasyPQP convert extra args

      cmd.add(OsUtils.asSingleArgument(String.format("--rt_lowess_fraction %s", rt_lowess_fraction))); // EasyPQP library args

      final List<String> lcmsfiles = group.lcmsFiles.stream()
          .map(lcms -> {
            final String fn = lcms.getPath().getFileName().toString();
            final String fn_sans_extension = FilenameUtils.removeExtension(fn);
            final boolean isTimsTOF = dataType == InputDataType.ImMsTimsTof;
            final boolean isRaw = fn.toLowerCase().endsWith(".raw");
            final String sans_suffix = lcms.getPath().toAbsolutePath().getParent().resolve(fn_sans_extension).toString();
            if ((isTimsTOF && fn.toLowerCase().endsWith(".d")) || isRaw) {
              return sans_suffix + "_uncalibrated.mzML";
            } else {
              return lcms.getPath().toString();
            }
          }).collect(Collectors.toList());

      cmd.add(speclibPanel.checkKeepIntermediateFiles.isSelected() ?
              "keep_intermediate_files" : "delete_intermediate_files");

      final Path filelist = groupWd.resolve("filelist_speclibgen.txt");

      if (Files.exists(filelist.toAbsolutePath().getParent())) { // Dry run does not make directories, so does not write the file.
        try (BufferedWriter bw = Files.newBufferedWriter(filelist)) {
          for (String f : lcmsfiles) {
            bw.write(f);
            bw.newLine();
          }
        } catch (IOException ex) {
          throw new UncheckedIOException(ex);
        }
      }

      cmd.add(filelist.toString());

      ProcessBuilder pb = new ProcessBuilder(cmd);
      PyInfo.modifyEnvironmentVariablesForPythonSubprocesses(pb);
      pb.directory(groupWd.toFile());
      pb.environment().put("PYTHONIOENCODING", "utf-8");
      pb.environment().put("OMP_NUM_THREADS", String.valueOf(threads));

      pbis.add(PbiBuilder.from(pb));
      pbis.addAll(PbiBuilder.from(pbsDeleteLcmsFiles));
    }

    isConfigured = true;
    return true;
  }
}
