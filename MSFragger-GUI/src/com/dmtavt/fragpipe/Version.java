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
package com.dmtavt.fragpipe;

import static com.dmtavt.fragpipe.params.ThisAppProps.PATH_BUNDLE;

import com.dmtavt.fragpipe.api.Bus;
import com.dmtavt.fragpipe.messages.MessageFragpipeUpdate;
import com.dmtavt.fragpipe.params.ThisAppProps;
import com.github.chhh.utils.PropertiesUtils;
import com.github.chhh.utils.StringUtils;
import com.github.chhh.utils.VersionComparator;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.TreeMap;
import java.util.TreeSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Dmitry Avtonomov
 */
public class Version {

  private static final Logger log = LoggerFactory.getLogger(Version.class);
  public static final String PROGRAM_TITLE = "FragPipe";
  public static final String PROP_ANNOUNCE = "fragpipe.announcements";
  public static final String PROP_VER = "msfragger.gui.version";
  public static final String PROP_LAST_RELEASE_VER = "fragpipe.last.release.version";
  public static final String PROP_DOWNLOAD_URL = "msfragger.gui.download-url";
  public static final String PROP_ISSUE_TRACKER_URL = "msfragger.gui.issue-tracker";
  public static final String PROP_ISSUE_TRACKER_URL_DEV = "msfragger.gui.issue-tracker-dev";
  public static final String PROP_DOWNLOAD_MESSAGE = "msfragger.gui.download-message";
  public static final String PROP_IMPORTANT_UPDATES = "msfragger.gui.important-updates";
  public static final String PROP_CRITICAL_UPDATES = "msfragger.gui.critical-updates";

  private static final TreeMap<String, List<String>> CHANGELOG = new TreeMap<>(
      new VersionComparator());

  static {
    CHANGELOG.put("22.0", Arrays.asList(
        "diaTracer tool for generating pseudo-MS/MS spectra from diaPASEF data, enabling spectrum-centric, direct DIA analysis (including nonspecific and PTM searches).",
        "Integration of Skyline in FragPipe (supporting DIA, DDA, and DDA glycoproteomics workflows).",
        "Support for MSFragger DDA+ (full isolation window search) analysis of ddaPASEF data.",
        "Support in MSBooster for using the Koina server for deep-learning predictions. Requires specifying the Koina URL.",
        "Support user-specified glycans and glycan modifications for glycoproteomics searches.",
        "Implement DIA glycoproteomics workflows.",
        "Significantly faster loading of Thermo raw files in MSFragger and IonQuant.",
        "Calculate and report more information useful for localizing the sites of modifications identified in open and mass-offset searches.",
        "Add photo-affinity labeling (PAL) chemoproteomics workflow.",
        "For DIA analysis, propagate the site localization, protein start, and protein end information to msstats.csv file.",
        "For DIA analysis, propagate the site localization to the DIA-NN's reports.",
        "For DIA analysis, propagate `Proteotypic`, `AllMappedProteins`, and `AllMappedGenes` columns to the DIA-NN reports.",
        "Generate MSstatsPTM input files.",
        "Add `nocleavage` option to the enzyme panel in the MSFragger tab.",
        "Add `analyze filter` dropdown to the MSFragger tab to only search MS/MS scans of the specified type (ITMS or FTMS).",
        "Change the default value of the IonQuant's minions to 1 (MaxLFQ normalization).",
        "Overhaul MSFragger and IonQuant config panels. MSFragger, IonQuant, and diaTracer need to be in the same folder. Specify the folder in the Config tab.",
        "Update the tool download panel",
        "Bundle Philosopher",
        "Upgrade the bundled JRE to version 17",
        "Require Java 11+",
        "Require MSFragger 4.1+",
        "Require IonQuant 1.10.27+",
        "Require Python 3.9, 3.10, or 3.11",
        "Require EasyPQP 0.1.44+",
        "Upgrade Crystal-C to 1.5.6",
        "Upgrade MSBooster to 1.2.31",
        "Upgrade Percolator to 3.6.5",
        "Upgrade TMT-Integrator to 5.0.9",
        "Various bug fixes and improvements"
    ));


    CHANGELOG.put("21.1", Arrays.asList(
        "Fix a bug in TMT-Integrator that in the single/multi-site reports, the site, protein start, and protein end are off by 1",
        "Fix a bug in MSBooster that using `,` as the decimal points in some regions",
        "Require MSFragger 4.0+",
        "Require Philosopher 5.1.0+",
        "Require IonQuant 1.10.12+",
        "Require Python 3.9",
        "Require EasyPQP 0.1.41+",
        "Upgrade MSBooster to 1.1.28",
        "Upgrade TMT-Integrator to 5.0.7"
    ));

    CHANGELOG.put("21.0", Arrays.asList(
        "Add support for the new MSFragger-DDA+ search mode (introduced in MSFragger 4.0). Data acquired in DDA mode (including wide-window DDA data) can now be annotated as `DDA+` (Workflow tab). MSFragger will perform full isolation window search, identifying multiple co-fragmented peptides from chimeric DDA spectra. This mode significantly boosts the number of IDs compared to conventional DDA search. DDA+ mode is compatible with close search-based DDA workflows (e.g., Default, LFQ-MBR, etc.).",
        "Add support for the detailed mass offset search mode (introduced in MSFragger 4.0). Each specified mass offset can have their own modification sites, neutral losses, and diagnostic ions. The detailed mass offsets are loaded from an external file (MSFragger tab). A sample mass offset file (mass_offset_template.tsv file) is available in the FragPipe/tools folder.",
        "Add new workflow for DIA-based phosphoproteomics: `DIA_SpecLib_Quant_Phospho`. This workflow includes site localization analysis via PTMProphet. The site localization information is then propagated to the precursor-level quantification tables (additional columns with site localization added to the DIA-NN generated report.pr_matrix.tsv file).",
        "In isobaric quantification workflows, switch to IonQuant for extraction of reporter ion intensities, MS1 intensities, and precursor purity calculation (Quant(Isobaric) tab). These steps were previously done using Philosopher LabelQuant and FreeQuant modules (now available as an option). As an additional benefit of using IonQuant, isobaric quantification workflows can now be used with Thermo's RAW files (i.e., without first converting to mzML).",
        "Significantly improve the speed of spectral library building",
        "Significantly improve the timsTOF ddaPASEF .d folder loading speed",
        "Add new module for downstream analysis of FPOP data (Downstream tab)",
        "Add `MT16-ubiquitination-K_tmt_or_ubiq` and `TMT16-ubiquitination-K_tmt_plus_ubiq` workflows",
        "Adjust parameters in DIA workflows for improved analysis of Thermo Fisher's Astral narrow-window DIA data",
        "Enable MSBooster and Percolator in the analysis of endogenous peptidome data (nonspecific_peptidome workflow).",
        "Add the DIA-NN executable file box (Config tab) to support custom (user-downloaded) DIA-NN versions. If the path is left empty, FragPipe uses the default DIA-NN version 1.8.2_beta_8 included in the FragPipe.",
        "Generate MSstats formatted output files in DIA workflows",
        "Generate MSstatsPTM formatted files in DDA label-free-quantification workflows",
        "Rename DIA-NN output files (`diann-output` to `report`)",
        "Add support for DIA-NN's QuantUMS quantification option (DIA-NN tab)",
        "Add support for TMT-0 and TMT-2 isobaric labelling (Quant(Isobaric) tab)",
        "The isobaric quant annotation files can be in different folders",
        "Add SDRF table writer",
        "Add a button to load variable modifications specified in MSFragger to PTMProphet command box (Validation tab)",
        "Add a docker image: https://hub.docker.com/r/fcyucn/fragpipe",
        "Improve generation of the annotation files for FragPipe-Analyst: http://fragpipe-analyst.nesvilab.org/",
        "Remove `add top N complementary` option (MSFragger tab)",
        "Mark Philosopher FreeQuant as deprecated",
        "Require MSFragger 4.0+",
        "Require Philosopher 5.1.0+",
        "Require IonQuant 1.10.12+",
        "Require Python 3.9",
        "Require EasyPQP 0.1.41+",
        "Upgrade MSBooster to 1.1.27",
        "Upgrade Percolator to 3.6.4",
        "Upgrade PTMShepherd to 2.0.6",
        "Upgrade FragPipe-PDV to 1.1.5",
        "Upgrade O-Pair to 1.1.0",
        "Upgrade TMT-Integrator to 5.0.6",
        "Various bug fixes and improvements"
    ));

    CHANGELOG.put("20.0", Arrays.asList(
        "Require MSFragger 3.8+",
        "Require Philosopher 5.0.0+",
        "Require IonQuant 1.9.8+",
        "Require Python 3.9",
        "Require EasyPQP 0.1.34+",
        "Add MBR support for MS1-based label quantification (IonQuant in Quant(MS1) tab)",
        "Add support for fractionated data in MS1-based label quantification (IonQuant in Quant(MS1) tab)",
        "Add two-pass search support (checkbox in Run tab). Generate new calibrated mzML files with identified scans, i.e. those reported in the `PSM.tsv` file, removed. This option also saves Percolator model files from the first search to be used in the second search, as well as the \"second pass\" workflow file to be loaded and used for the second search.",
        "Add group FDR support based on the group label defined in the MSFragger tab. This complements the existing group FDR options in Philosopher for modification-based group FDR filtering in Philosopher (--mod and --delta command line options, Validation tab).",
        "Add plexDIA support (DIA-NN tab).",
        "Add `Require precursor` option for MSFragger-DIA (MSFragger tab)",
        "Add `Reuse DIA fragment peaks` option for MSFragger-DIA (MSFragger tab)",
        "Add report top-N options for DDA, DIA, and GPF-DIA, respectively (MSFragger tab)",
        "Adjust the the `spectral processing` panel (MSFragger tab)",
        "Add Oxonium ion filter (Glyco tab)",
        "Add an option to choose one of the built-in Glycan databases or load a custom list (Glyco tab)",
        "Add mass filtering option for Glycan database load when not using combinations (Glyco tab)",
        "Support parsing glycan databases in alternate (\"kind\") MetaMorpheus format (Glyco tab)",
        "Rename the old `TMT10-acetyl` and `TMT16-acetyl` workflows as `TMT10-acetyl-noloc` and `TMT16-acetyl-noloc`. Revise `TMT10-acetyl` and `TMT16-acetyl` workflows to enable PTM-Prophet site localization.",
        "Add new `TMT10-ubiquitination-K_tmt_or_ubiq`, and `TMT10-ubiquitination-K_tmt_plus_ubiq` workflows that use PTM-Prophet for localization",
        "Add new `Open-quickscan` workflow for faster (but less sensitive and less accurate) Open search",
        "Tuned parameters in some built-in workflows",
        "Faster processing speed and reduced memory usage due to major improvements in IO operations and database reading in Philosopher",
        "Generate a single combined msstats.csv file for the TMT workflows",
        "Generate `MSstatsTMT_annotation.csv` file for MSstatsTMT",
        "Improved parsing of samples/conditions when preparing `experiment_annotation.tsv` file for FragPipe-Analyst",
        "Upgrade Crystal-C to 1.5.2",
        "Upgrade Percolator Windows version to 3.06",
        "Upgrade PTMProphet to 6.3.2",
        "Upgrade PTMShepherd to 2.0.5",
        "Upgrade FragPipe-PDV to 1.1.1",
        "Upgrade O-Pair to 1.1-rc2",
        "Upgrade TMT-Integrator to 4.0.5",
        "Various bug fixes and improvements"
    ));

    CHANGELOG.put("19.1", Arrays.asList(
        "Require MSFragger 3.7+",
        "Require Philosopher 4.8.0+",
        "Require IonQuant 1.8.10+",
        "Require EasyPQP 0.1.34+",
        "Add 'write uncalibrated mgf' checkbox",
        "Add Add iTRAQ4-phospho workflow",
        "Update iTRAQ4 workflow",
        "Rename 'combined_annotation.tsv' to 'experiment_annotation.tsv'",
        "Update the format of `experiment_annotation.tsv`",
        "Allow MSBooster for mass-offset search",
        "In GUI mode, do not load the tools' config paths from the workflow file",
        "Upgrade Crystal-C to 1.5.1",
        "Upgrade MSBooster to 1.1.11",
        "Upgrade DIA-Umpire to 2.3.2",
        "Various minor bug fixes and improvements"
    ));

    CHANGELOG.put("19.0", Arrays.asList(
        "Require Java 9+",
        "Require MSFragger 3.6+",
        "Require Philosopher 4.6.0+",
        "Require IonQuant 1.8.9+",
        "Require EasyPQP 0.1.34+",
        "Add O-Pair for O-Glycan localization",
        "Add downstream analysis tool SAINT",
        "Support PTM localization using PTMProphet for timsTOF .d and Thermo .raw formats",
        "Add --config-ionquant flag in headless mode",
        "Add \"delete calibrated mzML\" and \"delete temp files\" checkboxes",
        "Add \"open FragPipe-Analyst\" button",
        "Add \"remove contaminants\" checkbox to the \"FDR filter and report\" panel",
        "Add \"neutral loss\" checkbox to the speclib tab",
        "Add glycan database loader to Glyco tab",
        "Add mass-offset list loader to MSFragger tab",
        "Put DIA-NN's output fies to \"diann-output\" folder",
        "Put EasyPQP output files to \"eaasypqp_files\" directory",
        "Rename \"Open visualization window\" button to \"Open FragPipe-PDV viewer\"",
        "Remove \"Close visualization window\" button",
        "Write all TMT annotations to a single combined_annotation.txt file",
        "Upgrade DIA-NN to 1.8.2 beta 8",
        "Upgrade PTM-Shepherd to 2.0.3",
        "Upgrade TMT-Integrator to 4.0.4",
        "Upgrade DIA-Umpire to 2.3.1",
        "Upgrade MSBooster to 1.1.10",
        "Upgrade FP-PDV to 1.0.5",
        "Add more workflows",
        "Various minor bug fixes and improvements"
    ));

    CHANGELOG.put("18.0", Arrays.asList(
        "Require Java 9+",
        "Require MSFragger 3.5+",
        "Require Philosopher 4.2.2+",
        "Require IonQuant 1.7.30+",
        "Add new FragPipe-PDV visualization module",
        "Add the IonQuant configuration panel",
        "Add more workflows",
        "Upgrade DIA-NN to 1.8.1",
        "Upgrade PTM-Shepherd to 2.0.0",
        "Upgrade TMT-Integrator to 3.3.3",
        "Upgrade DIA-Umpire to 2.2.8",
        "Upgrade MSBooster to 1.1.4",
        "Upgrade Crystal-C to 1.4.4",
        "Various minor bug fixes and improvements"
    ));

    CHANGELOG.put("17.1", Arrays.asList(
        "Require MSFragger 3.4+",
        "Require Philosopher 4.1.0+",
        "Add Pierce iRT file",
        "Add `contam_` prefix to contaminants in the downloaded database",
        "Add more workflows",
        "Upgrade PTM-Shepherd to 1.2.6",
        "Upgrade IonQuant to 1.7.17",
        "Upgrade TMT-Integrator to 3.2.1",
        "Upgrade MSBooster",
        "Various minor bug fixes and improvements"
    ));

    CHANGELOG.put("17.0", Arrays.asList(
        "Require MSFragger 3.4+",
        "Require Philosopher 4.1.0+",
        "Add MSBooster 1.0 for deep learning based rescoring (Validation)",
        "Add glycan assignment with FDR control (PTM)",
        "Add DIA quantification with DIA-NN 1.8 (DIA Quant tab)",
        "Support two enzymes and N-terminal cutting enzyme (MSFragger tab)",
        "Support DDA, DIA, GPF-DIA, and DIA-Quant data types",
        "Upgrade PTM-Shepherd to 1.2.5",
        "Upgrade DIA-Umpire to 2.2.3",
        "Upgrade IonQuant to 1.7.16",
        "Upgrade TMT-Integrator to 3.2.0",
        "Various minor bug fixes and improvements"
    ));

    CHANGELOG.put("16.0", Arrays.asList(
        "Require MSFragger 3.3+",
        "Require Philosopher 4.0.0+",
        "Upgrade Crystal-C to 1.4.2",
        "Upgrade IonQuant to 1.7.5",
        "Upgrade TMT-Integrator to 3.0.0",
        "Upgrade PTMShepherd to 1.1.0",
        "Upgrade batmass-io to 1.23.4",
        "Add Percolator as an alternative to PeptideProphet",
        "Change how files are passed to ProteinProphet, IonQuant, and EasyPQP to bypass the Windows command length limitation",
        "Various minor bug fixes and improvements"
    ));

    CHANGELOG.put("15.0", Arrays.asList(
        "Require MSFragger 3.2+.",
        "Upgrade IonQuant to 1.5.5.",
        "Upgrade TMT-Integrator to 2.4.0.",
        "Upgrade PTMShepherd to 1.0.0.",
        "Upgrade DIA-Umpire-SE to 2.2.1 (support mzML, mzXML, and raw formats natively)",
        "Support download MSFragger automatically.",
        "Add a button to install/upgrade EasyPQP.",
        "Remove 'process each experiment separately' checkbox.",
        "Don't allow space in the path of MSFragger and Philosopher.",
        "Follow XDG specification for Unix.",
        "Upgrade the bundled JRE to 11.",
        "Automatically detect data types for .d and .raw."
    ));

    CHANGELOG.put("14.0", Arrays.asList(
        "Add PTMProphet.",
        "Upgrade Crystal-C to 1.3.2.",
        "Upgrade IonQuant to 1.4.4.",
        "Upgrade TMT-Integrator to 2.1.4.",
        "Upgrade PTM-Shepherd to 0.4.0.",
        "Require MSFragger 3.1+.",
        "Retire MsAdjuster. Using MSFragger's built-in isotope error correction module.",
        "New button to add contaminants and decoys to a fasta.",
        "Correct the path in interact-*.pep.xml file.",
        "Print related references after the job."
    ));

    CHANGELOG.put("13.0", Arrays.asList(
        "Brand new fragpipe, rebuilt almost from ground up",
        "TMT-Integrator, PTM-Shepherd, IonQuant included",
        "Spectral library generation with EasyPQP",
        "Workflows for easier quick-start and sharing"
    ));
    CHANGELOG.put("12.2", Arrays.asList(
        "Check FASTA file for presence of decoys before running PeptideProphet or Report",
        "Fix some Philosopher workspace related bugs",
        "Spectral library generation update",
        "IMQuant is now IonQuant"
    ));
    CHANGELOG.put("12.1", Arrays.asList(
        "IMQuant fixes after initial release."
    ));
    CHANGELOG.put("12.0", Arrays.asList(
        "Add IMQuant - quantitation for timsTOF.",
        "PTMShepherd updates - in-depth PTM analysis and reporting.",
        "Fix running FragPipe without an internet connection.",
        "Choice of default enzyme specifications in MSFragger config."
    ));

    CHANGELOG.put("11.0", Arrays.asList(
        "Parallel execution engine. Used only for PeptideProphet now, utilizing all CPU cores.",
        "DIA-Umpire requires MSConvert from ProteoWizard on Linux."
    ));

    CHANGELOG.put("10.0", Arrays.asList(
        "Add PTMShepherd with UI.",
        "Thermo RAW files and Bruker TimsTOF .d directories are supported. They do require "
            + "new MSFragger with 'ext' directory for libraries and additional binaries.",
        "Fix for changes to modification tables not always being propagated to config files "
            + "if the user didn't leave the editing field.",
        "Saving all the FragPipe options before run + buttons to Save/Load configurations.",
        "Fix iProphet command threads and how it's invoked when multi-experiment is enabled."
    ));

    CHANGELOG
        .put("9.4", Arrays.asList("Fixes to MSFragger Split program for very large databases."));

    CHANGELOG.put("9.3", Arrays.asList(
        "Calibrate masses option in MSFragger",
        "Custom ion series option in MSFragger",
        "Turning off usage of protxml file in Filter command when ProteinProphet is not run",
        "Query user if protxml files exist while ProteinProphet is not run",
        "'Print Decoys' option/checkbox for Report command",
        "Update CrystalC-1.0.5",
        "Checkbox for generating report in mzID format",
        "UI for downloading protein databases via philosopher"));

    CHANGELOG.put("9.1", Arrays.asList(
        "Fix Abacus command bug with unrecognized options from Filter command being carried over."));

    CHANGELOG.put("9.0", Arrays.asList(
        "Downstream tab groups all downstream processing tools in one place.",
        "Simplified Config tab with more links and hint.",
        "Much improved saving and restoring of edited fields.",
        "Support MSFragger 20190222"));

    CHANGELOG.put("8.8", Arrays.asList(
        "iProphet for peptide level reports.",
        "Moved all downstream tools (Peptide/Protein Prophet, Crystal-C) to a single tab.",
        "Simplified reports tab.",
        "Ask twice about loading defaults automatically.",
        "Removed decoy tag specification from command line fields. It's now always added implicitly."));

    CHANGELOG.put("8.7", Arrays.asList(
        "Support for new Philosopher 20181119, Report Abacus --protein flag."));

    CHANGELOG.put("8.6", Arrays.asList(
        "Updated nonspecific search default options.",
        "Colorized console output.",
        "Multiple UI fixes and updates."));

    CHANGELOG.put("8.5", Arrays.asList(
        "Updated spectral library generation. LCMS files are copied and deleted from the right locations.",
        "Multi-experiment protein level report.",
        "Multi-experiment quantitation",
        "Allow separate processing of input files one-by-one."));

    CHANGELOG.put("8.4", Arrays.asList(
        "Combined protein report for multiple file groups."));

    CHANGELOG.put("8.3", Arrays.asList(
        "LCMS files can be processed in separate groups.",
        "Python detection on Windows via registry.",
        "Python binary location can be specified manually."));

    CHANGELOG.put("8.1", Arrays.asList(
        "DIA-Umpire SE added as an optional component. Mark the checkbox on Config panel to enable."));

    CHANGELOG.put("8.0", Arrays.asList(
        "Added MSAdjuster, Crystal-C. Both packaged with the release, no extra downloads."));

    CHANGELOG.put("7.2", Arrays.asList(
        "Added database slicing via a python script (Requires "
            + "Python 3, NumPy, Pandas)."));

    CHANGELOG.put("7.1", Arrays.asList(
        "Added label free quantitation."));

    CHANGELOG.put("7.0", Arrays.asList(
        "MFFragger-GUI is now calledFragPipe.",
        "Clear out Fragger modification tables when loading new parameter files to avoid ghost entries.",
        "Update the github urls for checking new versions."));

    CHANGELOG.put("6.0.1", Arrays.asList(
        "Allow loading of empty-valued parameters from fragger *.properties files.",
        "Comments in fragger.properties won't overwrite non-commented properties anymore."));

    CHANGELOG.put("6.0", Arrays.asList(
        "Automatic updates for MSFragger",
        "mass_offsets parameter for MSFragger",
        "Lower default number of fragments required in Closed search to 4",
        "Improved tooltips in MSFragger tab",
        "Initial defaults are loaded for Closed search now instead of Open"));

    CHANGELOG.put("5.4", Arrays.asList(
        "Restore last location of MSfragger params file save/load operation.",
        "Show errors from loading msfragger.params files"));

    CHANGELOG.put("5.3", Arrays.asList(
        "Button for auto-detection of decoy prefixes",
        "When sequence DB changes, display the number of proteins.",
        "Auto-detect buggy cached --clevel option and change it to -2."));

    CHANGELOG.put("5.2", Arrays.asList(
        "Revert PeptideProphet --clevel option default to '-2' for Open Searching."));

    CHANGELOG.put("5.1", Arrays.asList(
        "Bug fixes for cross-tool decoy tag updates."));

    CHANGELOG.put("5.0", Arrays.asList(
        "Separate tab for sequence database.",
        "Display info about known compatibility of newer versions of Philosopher."));

    CHANGELOG.put("4.9", Arrays.asList(
        "Stop execution of the pipeline if one of the processes returns non-zero exit code.",
        "Colorize console output a bit, red color for errors.",
        "Button that redirects to the issue tracker online for bug reporting."));

    CHANGELOG.put("4.8", Arrays.asList(
        "Introduce notifications about update contents",
        "User-message can now be shown without a newer version available",
        "Added export button and context menu item to the console to simplify bug reporting by users."));

    CHANGELOG.put("4.7", Arrays.asList(
        "Support new packaging of MSFragger jar with onejar."));

    CHANGELOG.put("4.6", Arrays.asList(
        "Fix mixed up order of philosopher calls."
    ));

    CHANGELOG.put("4.5", Arrays.asList(
        "Show parsed versions of tools in the UI.",
        "Print detected versions of tools to console before each run."
    ));

    CHANGELOG.put("4.4", Arrays.asList(
        "Only run Philosopher workspace --clean/--init once per analysis."
    ));

    CHANGELOG.put("4.3", Arrays.asList(
        "Locale dependency fix for MSfragger parameters panel.",
        "Philosopher checks version comparing to GitHub at startup."
    ));

    CHANGELOG.put("4.2", Arrays.asList(
        "Fix the issue that Fragger panel constructor could cause IOException and prevent the whole app from loading."
    ));

    CHANGELOG.put("4.1", Arrays.asList(
        "Variable mod site definition warning text if cached from older versions.",
        "Java 9 warning for Fragger.",
        "Non-symmetric precursor mass tolerance and detection of \"[*\" for var mods.",
        "Cache fragger params after a dry-run or a real run."
    ));

    CHANGELOG.put("4.0", Arrays.asList(
        "Added version check for MSFragger-GUI itself, comparing to GitHub.",
        "Two way sync of decoy/tag prefix used by PeptideProphet and Philosopher Report.",
        "Fix how msfragger jar is auto-found.",
        "Added version to msfragger properties.",
        "MSFragger version check."
    ));
  }

  public static Map<String, List<String>> getChangelog() {
    return Collections.unmodifiableMap(CHANGELOG);
  }

  /**
   *
   */
  public static List<String> updatesSinceCurrentVersion(String versionList) {
    if (StringUtils.isNullOrWhitespace(versionList)) {
      return Collections.emptyList();
    }

    VersionComparator vc = new VersionComparator();
    String[] split = versionList.trim().split("\\s*,\\s*");
    List<String> res = new ArrayList<>();
    for (String updateVersion : split) {
      if (vc.compare(version(), updateVersion) < 0) {
        res.add(updateVersion);
      }
    }
    return res;
  }

  public static boolean isDevBuild() {
    return version().toLowerCase().contains("build");
  }

  public static String version() {
    return version(false);
  }

  public static String version(boolean includeProgramTitle) {
    Properties p = loadPropertiesWithIdeDebugHack();
    if (!p.containsKey(Version.PROP_VER)) {
      throw new IllegalStateException(String.format("Key '%s' not found in bundle '%s'",
          Version.PROP_VER, PATH_BUNDLE));
    }
    String v = p.getProperty(Version.PROP_VER);
    return includeProgramTitle ? String.format("%s v%s", PROGRAM_TITLE, v) : v;
  }

  private static String chop(String original, String toChop) {
    return original.endsWith(toChop) ?
        original.substring(0, original.length() - toChop.length()) :
        original;
  }

  /**
   * To print changelog using just the jar file use:<br/>
   * <code>java -cp ./build/install/fragpipe/lib/* com.dmtavt.fragpipe.Version true 2</code>
   *
   * @param args The 1st param is a boolean whether to print GitHub release info preamble or not.
   *             Use true, to indicate "yes", any other string for "no". The 2nd parameter is an
   *             integer how many versions back worth of changelog to print.
   */
  public static void main(String[] args) {
    int maxVersionsToPrint = 0;
    boolean printGihubPreamble = true;
    if (args != null && args.length > 0) {
      printGihubPreamble = Boolean.parseBoolean(args[0]);

      if (args.length > 1) {
        try {
          long num = Long.parseLong(args[1]);
          maxVersionsToPrint = num > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) num;
        } catch (Exception e) {
          System.err.println("Unrecognized 2nd parameter. "
              + "Should be no params or an integer for how many versions"
              + " back to print changelog for.");
        }
      }
    }

    Properties props = loadPropertiesWithIdeDebugHack();

    if (!props.containsKey(PROP_DOWNLOAD_URL)) {
      throw new IllegalStateException(String.format("Didn't find '%s' in "
          + "FragPipe Bundle file", PROP_DOWNLOAD_URL));
    }
    String url = props.getProperty(PROP_DOWNLOAD_URL);
    String latest = "latest";
    url = chop(url, "/");
    url = chop(url, latest);
    url = chop(url, "/");

    final String version = version();
    if (printGihubPreamble) {
      final String zipFn = String.format("%s-%s.zip", PROGRAM_TITLE, version);
      final String zipUrl = String.format("%s/download/%s/%s", url, version, zipFn);
      final String zipFnWithJre = String.format("%s-jre-%s.zip", PROGRAM_TITLE, version);
      final String zipUrlWithJre = String.format("%s/download/%s/%s", url, version, zipFnWithJre);
      String githubReleaseMessage =
          "## Downloading\n"
          + "- The zip (<a href='" + zipUrl + "' target='_blank'>" + zipFn + "</a>) doesn't contain Java, you will need Java 9+ to run.\n"
          + "- The other zip with `-jre-` in its name (<a href='" + zipUrlWithJre + "' target='_blank'>" + zipFnWithJre + "</a>) contains a Java runtime **for Windows only**.\n"
          + "- The docker image is available at <a href='https://hub.docker.com/r/fcyucn/fragpipe' target='_blank'>https://hub.docker.com/r/fcyucn/fragpipe</a>\n"
          + "## Running\n"
          + "- Unzip the file\n"
          + "- In `/bin` subdirectory you will find a `shell script for Linux`, `bat file for Windows`, and an `exe file for Windows`\n";

      System.out.println(githubReleaseMessage);
      System.out.println();
      System.out.println();

    }

    StringBuilder sb = new StringBuilder();
    sb.append("### ").append("Changelog:\n");
    int cnt = 0;
    for (Map.Entry<String, List<String>> e : CHANGELOG.descendingMap().entrySet()) {
      if (maxVersionsToPrint > 0 && ++cnt > maxVersionsToPrint) {
        break;
      }
      String ver = e.getKey();
      sb.append("\nv").append(ver).append(":\n");
      for (String change : e.getValue()) {
        sb.append(" - ").append(change).append("\n");
      }
    }
    System.out.println(sb.toString());
  }

  private static Properties loadPropertiesWithIdeDebugHack() {
    Properties props = new Properties();
    try {
      ResourceBundle bundle = ThisAppProps.getLocalBundle();
      bundle.keySet().forEach(k -> props.setProperty(k, bundle.getString(k)));
    } catch (Exception e) {
      log.debug("Could not load local Bundle for fragpipe, retrying with properties", e);
      try {
        Properties p = new Properties();
        Path path = Paths
            .get(".").toAbsolutePath().resolve("src").resolve(PATH_BUNDLE + ".properties");
        p.load(Files.newBufferedReader(path));
        p.stringPropertyNames().forEach(k -> props.setProperty(k, p.getProperty(k)));
      } catch (IOException ex) {
        log.error("Could not load local Bundle for fragpipe at all", ex);
      }
    }
    return props;
  }

  public static Properties loadPropertiesFromBundle() {
    try (InputStream is = Fragpipe.class.getResourceAsStream("Bundle.properties")) {
      if (is == null) {
        throw new IllegalStateException("Could not read Bundle.properties from the classpath");
      }
      Properties props = new Properties();
      props.load(is);
      return props;
    } catch (IOException e) {
      throw new IllegalStateException("Error reading Bundle.properties from the classpath");
    }
  }

  public static String loadPropFromBundle(String propName) {
    Properties props = loadPropertiesFromBundle();
    String value = props.getProperty(propName);
    if (value == null) {
      throw new IllegalStateException("Property " + propName
          + " was not found in Bundle.properties");
    }
    return value;
  }

  private static List<String> createGuiUpdateMessages(TreeSet<String> newerVersionStrings,
      Properties propsRemote) {
    List<String> messages = new ArrayList<>();
    for (String newerVersion : newerVersionStrings) {
      String verMsg = propsRemote
          .getProperty(Version.PROP_DOWNLOAD_MESSAGE + "." + newerVersion, "");
      if (StringUtils.isNullOrWhitespace(verMsg)) {
        continue;
      }
      messages.add(verMsg);
    }
    return messages;
  }

  public static void checkUpdatesOld() {
    Properties props = ThisAppProps.getRemoteProperties();
    if (props == null) {
      log.info("Didn't get update info from any of the sources");
      return;
    }

    String lastVerKey = PROP_LAST_RELEASE_VER;
    if (!props.stringPropertyNames().contains(lastVerKey)) {
      List<String> urls = Arrays.asList(
          "https://raw.githubusercontent.com/Nesvilab/FragPipe/master/MSFragger-GUI/src/"
              + PATH_BUNDLE + ".properties");
      props = PropertiesUtils.initProperties(urls);
      if (props == null) {
        log.debug("Didn't get dev update info");
        return;
      }
      if (!props.stringPropertyNames().contains(Version.PROP_VER)) {
        log.info("Release branch didn't contain version info in: {}", Version.PROP_VER);
        return;
      }
      lastVerKey = Version.PROP_VER;
    }

//      final URI tmp = URI.create("https://raw.githubusercontent.com/chhh/FragPipe/master/MSFragger-GUI/src/" + PATH_BUNDLE + ".properties");
//      final URI tmp = URI.create("https://raw.githubusercontent.com/chhh/FragPipe/fragger-advanced/MSFragger-GUI/src/" + PATH_BUNDLE + ".properties");
//      String githubProps = IOUtils.toString(tmp, StandardCharsets.UTF_8);
//      Properties props = new Properties();
//      props.load(new StringReader(githubProps));

    // this is used to test functionality without pushing changes to github
//                        props.put("msfragger.gui.version", "5.7");
//                        props.put("msfragger.gui.important-updates", "3.1,3.5,4.9,5.2");
//                        props.put("msfragger.gui.critical-updates", "2.0,3.0,4.6,5.0, 4.7");
//                        props.put("msfragger.gui.download-message", "Happy new year!");
//                        props.put("msfragger.gui.download-message.4.7", "Crit 4.7");
//                        props.put("msfragger.gui.download-message.2.0", "Crit 2.0");
//                        props.put("msfragger.gui.download-message.5.0", "Crit 4.7");
//                        props.put("msfragger.gui.download-message.5.0", "Crit 5.0");
//                        props.put("msfragger.gui.download-message.3.1", "Important 3.1");
//                        props.put("msfragger.gui.download-message.4.9", "Important 4.9");

    final StringBuilder sb = new StringBuilder();
    final VersionComparator vc = new VersionComparator();

    // add new versions notification
    final String githubVersion = props.getProperty(lastVerKey);
    final String localVersion = Version.version();
    if (githubVersion != null && vc.compare(localVersion, githubVersion) < 0) {
      if (sb.length() > 0) {
        sb.append("<br><br>");
      }
      final String defaultDlUrl = loadPropFromBundle(Version.PROP_DOWNLOAD_URL);
      final String dlUrl = props.getProperty(Version.PROP_DOWNLOAD_URL, defaultDlUrl);
      sb.append(String.format(Locale.ROOT,
          "Your %s version is [%s]<br>\n"
              + "There is a newer version of %s available [%s]).<br/>\n"
              + "Please <a href=\"%s\">click here</a> to download a newer one.<br/>",
          Version.PROGRAM_TITLE, localVersion, Version.PROGRAM_TITLE, githubVersion, dlUrl));

      // check for critical or important updates since the current version
      List<String> updatesImportant = Version.updatesSinceCurrentVersion(
          props.getProperty(Version.PROP_IMPORTANT_UPDATES, ""));
      List<String> updatesCritical = Version.updatesSinceCurrentVersion(
          props.getProperty(Version.PROP_CRITICAL_UPDATES, ""));

      if (!updatesCritical.isEmpty()) {
        TreeSet<String> newerVersions = new TreeSet<>(updatesCritical);
        List<String> messages = createGuiUpdateMessages(newerVersions, props);
        if (!messages.isEmpty()) {
          sb.append("<br/><br/><b>Critical updates:</b><br><ul>");
          for (String message : messages) {
            sb.append("<li>").append(message).append("</li>");
          }
          sb.append("</ul>");
        } else {
          sb.append("<br/><b>There have been critical updates.</b><br>");
        }
      }

      if (!updatesImportant.isEmpty()) {
        TreeSet<String> newerVersions = new TreeSet<>(updatesImportant);
        List<String> messages = createGuiUpdateMessages(newerVersions, props);
        if (!messages.isEmpty()) {
          sb.append("<br/>Important updates:<br><ul>");
          for (String message : messages) {
            sb.append("<li>").append(message).append("</li>");
          }
          sb.append("</ul>");
        } else {
          sb.append("<br/><br/>There have been important updates.<br>");
        }
      }
    }

    final String downloadMessage = props.getProperty(Version.PROP_DOWNLOAD_MESSAGE, "");
    if (!StringUtils.isNullOrWhitespace(downloadMessage)) {
      if (sb.length() > 0) {
        sb.append("<br><br><b>");
      }
      sb.append(downloadMessage).append("</b>");
    }

    if (sb.length() > 0) {
      Bus.post(new MessageFragpipeUpdate(Version.PROP_VER, sb.toString()));
    }
  }

}
