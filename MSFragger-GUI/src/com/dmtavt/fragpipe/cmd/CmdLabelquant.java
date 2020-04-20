package com.dmtavt.fragpipe.cmd;

import com.github.chhh.utils.StringUtils;
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
import javax.swing.JOptionPane;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.params.philosopher.PhilosopherProps;
import com.dmtavt.fragpipe.params.tmtintegrator.QuantLabel;
import com.github.chhh.utils.SwingUtils;
import com.github.chhh.utils.UsageTrigger;

public class CmdLabelquant extends CmdBase {
  public static final String NAME = "LabelQuant";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML");

  public CmdLabelquant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  @Override
  public int getPriority() {
    return 99;
  }

  public boolean configure(Component comp, boolean isDryRun, UsageTrigger phi,
      String textLabelquantOpts, QuantLabel label, final List<String> forbiddenOpts,
      Map<LcmsFileGroup, Path> annotations, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    pbis.clear();
    if (!checkCompatibleFormats(comp, mapGroupsToProtxml)) {
      return false;
    }

    for (Map.Entry<LcmsFileGroup, Path> e : annotations.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path annotationFile = e.getValue();
      final Set<Path> lcmsGroupParentDir = group.lcmsFiles.stream()
          .map(f -> f.getPath().getParent())
          .collect(Collectors.toSet());
      if (lcmsGroupParentDir.size() > 1) {
        String msg = "<html>All LCMS input files for an experiment/group must be<br/>\n"
            + "located in the same directory for " + NAME + " to work.";
        JOptionPane.showMessageDialog(comp, msg, NAME + " Error", JOptionPane.WARNING_MESSAGE);
        return false;
      }

      final Path lcmsDir = lcmsGroupParentDir.iterator().next().toAbsolutePath();
      final Path groupWd = group.outputDir(wd);

      if (!lcmsDir.equals(annotationFile.getParent())) {
        String msg = "LCMS files for an experiment/group must be in the same direcotry\n"
            + "as the annotation file for " + NAME + " to work.";
        SwingUtils.showWarningDialog(comp, msg, NAME + " Config");
        return false;
      }

      List<String> cmd = new ArrayList<>();
      cmd.add(phi.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_LABELQUANT);
      List<String> opts = StringUtils.splitCommandLine(textLabelquantOpts);
      List<String> badGiven = opts.stream().map(String::toLowerCase).filter(forbiddenOpts::contains)
          .collect(Collectors.toList());
      if (!badGiven.isEmpty()) {
        String msg = String.format("<html>Please don't include [%s] in Labelquant options string",
            String.join(", ", badGiven));
        JOptionPane.showMessageDialog(comp, msg, NAME + " Error", JOptionPane.WARNING_MESSAGE);
        return false;
      }
      cmd.addAll(opts);

      cmd.add("--plex");
      cmd.add(Integer.toString(label.getReagentNames().size()));
      cmd.add("--annot");
      if (annotationFile == null || StringUtils.isNullOrWhitespace(annotationFile.toString())) {
        String msg = String.format("Need to specify TMT file annotations in TMT-Integrator\n"
            + "configuration.\n");
        SwingUtils.showWarningDialog(comp, msg, NAME + " Error");
        return false;
      }
      cmd.add(annotationFile.toString());
      if (!isDryRun) {
        // copy annotation files to output directory for Report command to pick up
        Path annotationFileInGroupDir = groupWd.resolve(annotationFile.getFileName());
        if (!annotationFileInGroupDir.equals(annotationFile)) {
          try {
            //Files.deleteIfExists(annotationFileInGroupDir);
            Files.copy(annotationFile, annotationFileInGroupDir, StandardCopyOption.REPLACE_EXISTING);
          } catch (IOException ex) {
            throw new IllegalStateException(ex);
          }
        }
      }

      cmd.add("--brand");
      if (!"tmt".equalsIgnoreCase(label.getType())) {
        throw new IllegalStateException("Only tmt is supported");
      }
      cmd.add(label.getType().toLowerCase());

      // we have checked that all lcms files are in the same folder, so
      cmd.add("--dir");
      cmd.add(lcmsDir.toString());
      ProcessBuilder pb = new ProcessBuilder(cmd);
      // labelQuant needs to be executed in the dir where mzml files are (and the annotation file)
      pb.directory(groupWd.toFile());

      pbis.add(PbiBuilder.from(pb));
    }

    isConfigured = true;
    return true;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, SUPPORTED_FORMATS);
    if (!notSupportedExts.isEmpty()) {
      JOptionPane.showMessageDialog(comp, String.format(
          "<html>%s doesn't support '.%s' files.<br/>"
              + "Either remove them from input or disable %s<br/>"
              + "You can convert files using <i>msconvert</i> from ProteoWizard.", NAME, String.join(", ", notSupportedExts), NAME),
          NAME + "error", JOptionPane.WARNING_MESSAGE);
      return false;
    }
    return true;
  }

}
