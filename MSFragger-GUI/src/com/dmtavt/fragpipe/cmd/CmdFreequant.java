package com.dmtavt.fragpipe.cmd;

import com.dmtavt.fragpipe.api.InputLcmsFile;
import com.dmtavt.fragpipe.api.LcmsFileGroup;
import com.dmtavt.fragpipe.tools.philosopher.PhilosopherProps;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.UsageTrigger;
import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;

public class CmdFreequant extends CmdBase {

  public static final String NAME = "FreeQuant";
  public static final List<String> SUPPORTED_FORMATS = Arrays.asList("mzML", "raw");

  public CmdFreequant(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  private boolean checkCompatibleFormats(Component comp, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    List<String> notSupportedExts = getNotSupportedExts(mapGroupsToProtxml, SUPPORTED_FORMATS);
    if (!notSupportedExts.isEmpty()) {
      JOptionPane.showMessageDialog(comp, String.format(
          "<html>%s doesn't support '.%s' files.<br/>"
              + "Either remove them from input or disable %s<br/>"
              + "You can convert files using <i>msconvert</i> from ProteoWizard.", NAME, String.join(", ", notSupportedExts), NAME),
          NAME + " error", JOptionPane.WARNING_MESSAGE);
      return false;
    }
    return true;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String textReportLabelfree, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

    initPreConfig();

    if (!checkCompatibleFormats(comp, mapGroupsToProtxml)) {
      return false;
    }

    for (Map.Entry<LcmsFileGroup, Path> e : mapGroupsToProtxml.entrySet()) {
      final LcmsFileGroup group = e.getKey();

      final Set<Path> lcmsDirsForProtxml = group.lcmsFiles.stream()
          .map(f -> f.getPath().getParent())
          .collect(Collectors.toSet());
      if (lcmsDirsForProtxml.size() > 1) {
        String msg = "All LCMS input files for an experiment/group must be\n"
            + "located in the same directory for Freequant to work.";
        JOptionPane.showMessageDialog(comp, msg, "Freequant Error", JOptionPane.WARNING_MESSAGE);
        return false;
      }

      final Path lcmsDir = lcmsDirsForProtxml.iterator().next().toAbsolutePath();
      final Path groupWd = group.outputDir(wd);

      List<String> cmd = new ArrayList<>();
      cmd.add(usePhilosopher.useBin(groupWd));
      cmd.add(PhilosopherProps.CMD_LABELFREE);
      cmd.addAll(StringUtils.splitCommandLine(textReportLabelfree));

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
