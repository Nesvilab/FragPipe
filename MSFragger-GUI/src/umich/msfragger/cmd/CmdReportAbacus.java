package umich.msfragger.cmd;

import java.awt.Component;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import umich.msfragger.gui.LcmsFileGroup;
import umich.msfragger.util.UsageTrigger;

public class CmdReportAbacus extends CmdBase {

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

    final List<String> flagsAbacus = Arrays.asList("--picked", "--razor", "--reprint", "--uniqueonly");
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

    final List<String> flagsFilter = Arrays.asList("--mapmods", "--models", "--picked", "--razor", "--sequential");

    pbs.clear();
    Map<Path, List<Entry<LcmsFileGroup, Path>>> protxmlToGroups = mapGroupsToProtxml.entrySet().stream()
        .collect(Collectors.groupingBy(Entry::getValue));

    for (Entry<Path, List<Entry<LcmsFileGroup, Path>>> e : protxmlToGroups.entrySet()) {
      Path protxml = e.getKey();
      Set<Path> foldersWithPepxmls = e.getValue().stream().map(g -> g.getKey().outputDir(wd))
          .collect(Collectors.toSet());

      if (foldersWithPepxmls.size() < 2) {
        JOptionPane.showMessageDialog(comp,
            "Multi-experiment report requires more than one\n"
            + "experiment/group being processed together.\n\n"
                + "Turn off separate processing of groups on LCMS\n"
                + "tab or add another experiment/group.",
            "Multi-experiment report error", JOptionPane.WARNING_MESSAGE);
        return false;
      }

      List<String> filterCmdParts = Arrays.asList(textReportFilterCmdOpts.trim().split("[\\s]+"));
      List<String> matchingFlags = filterCmdParts.stream()
          .filter(flagsFilter::contains) // these are 'filter' command's flags
          .filter(flagsAbacus::contains) // and as well are 'abacus' command's flags
          .collect(Collectors.toList());

      List<String> cmd = new ArrayList<>();
      final Path executeInDir = protxml.getParent();
      cmd.add(usePhilosopher.useBin(executeInDir));
      cmd.add("abacus");
      cmd.addAll(matchingFlags);
      cmd.add("--tag");
      cmd.add(decoyTag);
      cmd.add("--protein");
      cmd.add(protxml.toString());

      // list locations with pepxml files
      for (Path pepxmlDir : foldersWithPepxmls) {
        cmd.add(pepxmlDir.getFileName().toString());
      }

      ProcessBuilder pb = new ProcessBuilder(cmd);
      pb.directory(executeInDir.toFile());
      pbs.add(pb);
    }

    isConfigured = true;
    return true;
  }
}
