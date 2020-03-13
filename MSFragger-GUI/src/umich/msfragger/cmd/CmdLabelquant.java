package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.params.philosopher.PhilosopherProps;
import umich.msfragger.params.tmtintegrator.QuantLabel;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.UsageTrigger;

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

  public boolean configure(Component comp, UsageTrigger phi,
      String textLabelquantOpts, QuantLabel label, final List<String> forbiddenOpts,
      Map<LcmsFileGroup, Path> annotations, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {
    pbis.clear();
    if (!checkCompatibleFormats(comp, mapGroupsToProtxml)) {
      return false;
    }

    for (Map.Entry<LcmsFileGroup, Path> e : annotations.entrySet()) {
      final LcmsFileGroup group = e.getKey();
      final Path annotation = e.getValue();

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
      if (!annotation.getParent().equals(lcmsDir)) {
        String msg = String.format("Current implementation requires the annotation file:\n\n"
            + "%s\n\n"
            + "to be in the same directory as corresponding LCMS files:\n\n"
            + "%s", annotation.toString(), lcmsDir.toString());
        SwingUtils.showWarningDialog(comp, msg, NAME + " Error");
        return false;
      }
      cmd.add(annotation.getFileName().toString());

      cmd.add("--brand");
      if (!"tmt".equalsIgnoreCase(label.getType())) {
        throw new IllegalStateException("Only tmt is supported");
      }
      cmd.add(label.getType().toLowerCase());

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
