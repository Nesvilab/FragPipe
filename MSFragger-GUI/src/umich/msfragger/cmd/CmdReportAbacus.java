package umich.msfragger.cmd;

import java.awt.Component;
import java.lang.reflect.Array;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.swing.JEditorPane;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.util.StringUtils;
import umich.msfragger.util.SwingUtils;
import umich.msfragger.util.UsageTrigger;

/**
 * The `Multi-Experiment Report`.
 */
public class CmdReportAbacus extends CmdBase {
  private static final Logger log = LoggerFactory.getLogger(CmdReportAbacus.class);

  private static final String NAME = "ReportAbacus";

  public CmdReportAbacus(boolean isRun, Path workDir) {
    super(isRun, workDir);
  }

  @Override
  public String getCmdName() {
    return NAME;
  }

  public boolean configure(Component comp, UsageTrigger usePhilosopher,
      String textReportFilterCmdOpts, String decoyTag, Map<LcmsFileGroup, Path> mapGroupsToProtxml) {

//    Usage:
//    philosopher abacus [flags]
//
//    Flags:
//    -h, --help             help for abacus
//    --labels           indicates whether the data sets includes TMT labels or not
//    --pepProb float    minimum peptide probability (default 0.5)
//    --peptide string   combined peptide file
//    --picked           apply the picked FDR algorithm before the protein scoring
//    --protein string   combined protein file
//    --prtProb float    minimum protein probability (default 0.9)
//    --razor            use razor peptides for protein FDR scoring
//    --reprint          create abacus reports using the Reprint format
//    --tag string       decoy tag (default "rev_")
//    --uniqueonly       report TMT quantification based on only unique peptides

//    Usage:
//    philosopher filter [flags]
//
//    Flags:
//    -h, --help             help for filter
//    --ion float        peptide ion FDR level (default 0.01)
//    --mapmods          map modifications aquired by an open search
//    --models           print model distribution
//    --pep float        peptide FDR level (default 0.01)
//    --pepProb float    top peptide probability treshold for the FDR filtering (default 0.7)
//    --pepxml string    pepXML file or directory containing a set of pepXML files
//    --picked           apply the picked FDR algorithm before the protein scoring
//    --prot float       protein FDR level (default 0.01)
//    --protProb float   protein probability treshold for the FDR filtering (not used with the razor algorithm) (default 0.5)
//    --protxml string   protXML file path
//    --psm float        psm FDR level (default 0.01)
//    --razor            use razor peptides for protein FDR scoring
//    --sequential       alternative algorithm that estimates FDR using both filtered PSM and Protein lists
//    --tag string       decoy tag (default "rev_")
//    --weight float     threshold for defining peptide uniqueness (default 1)


    final List<String> flagsAbacus = Arrays.asList("--picked", "--razor", "--reprint", "--uniqueonly");
    final List<String> flagsFilter = Arrays.asList("--picked", "--razor", "--mapmods", "--sequential", "--models");

    pbs.clear();

    final long numGroups = mapGroupsToProtxml.keySet().stream()
        .map(group -> group.name).distinct().count();
    if (numGroups < 2) {
      String msg = "Multi-experiment report requires more than one experiment/group.<br/>"
          + "You can assign experiment/group names to LCMS files on the LCMS file selection tab.";
      JEditorPane ep = SwingUtils.createClickableHtml(msg);
      SwingUtils.showDialog(comp, ep, "Multi-experiment report configuration error", JOptionPane.WARNING_MESSAGE);
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

      List<Path> outputDirsForProtxml = groups.stream().map(group -> group.outputDir(wd))
          .distinct().collect(Collectors.toList());
      log.error("Protxml: {}, outputDirsForProtxml: {}", protxml.toString(),
          outputDirsForProtxml.stream().map(Path::toString).collect(Collectors.joining(", ")));

      if (outputDirsForProtxml.size() < 2) {
        String msg = "Multi-experiment report requires experiments processed together by "
            + "Protein Prophet.<br/><br/>"
            + "Encountered a prot-xml file mapped to only one experiment/group:<br/>"
            + "&nbsp;&nbsp;" + protxml.toString() + "<br/><br/>"
            + "On <b>Downstream tab</b> in <b>Protein Prophet group</b> please uncheck "
            + "the checkbox <i>Separate prot-xml per experiment/group</i>.";
        JEditorPane ep = SwingUtils.createClickableHtml(msg);
        SwingUtils.showDialog(comp, ep, "Multi-experiment report configuration error", JOptionPane.WARNING_MESSAGE);
        return false;
      }

      // we'll only take the flags from the command that Abacus recognizes
      final List<String> filterCmdLineParts = StringUtils.splitCommandLine(textReportFilterCmdOpts);
      final LinkedHashSet<String> cmdAddonParts = filterCmdLineParts.stream()
          .filter(flagsAbacus::contains).collect(Collectors.toCollection(LinkedHashSet::new));
      cmdAddonParts.add("--reprint"); // Alexey wants to always use only `--reprint  --razor`

      String pepxmlCombined = wd.resolve(getCombinedPepFileName()).toString();
      List<String> cmd = new ArrayList<>();
      final Path executeInDir = protxml.getParent();
      cmd.add(usePhilosopher.useBin(executeInDir));
      cmd.add("abacus");
      cmd.addAll(cmdAddonParts);
      cmd.add("--tag");
      cmd.add(decoyTag);
      cmd.add("--protein");
      //cmd.add(protxml.toString()); // Commented out as newer Philosopher won't work
      cmd.add("--peptide");
      //cmd.add(pepxmlCombined); // Commented out as newer Philosopher won't work

      // list locations with pepxml files
      for (Path pepxmlDir : outputDirsForProtxml) {
        cmd.add(pepxmlDir.getFileName().toString());
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(executeInDir.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }

  private String getCombinedPepFileName() {
    return "combined.pep.xml";
  }
}
