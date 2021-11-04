### Glycoproteomics with FragPipe

We have developed a set of tools for analyzing tandem mass spectra of intact glycopeptides 
Intact glycopeptide analysis involves several steps: 
Intact glycopeptide tandem mass spectrometry data can be challenging to analyze due to 
fragmentation of both peptide and glycan components. We have developed a special glyco 
search method in MSFragger and several supporting downstream tools for intact glycopeptide 
validation, analysis, and quantitation. This tutorial covers each step of the process starting
from raw data to validation and quantation of the results.

##### Tutorial contents
* [Add the data](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#add-the-data)
* [Load the appropriate glyco workflow](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#load-a-glyco-workflow)
* [Fetch a sequence database](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#fetch-a-sequence-database)
* [Customize the search settings in MSFragger and Philosopher](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#customize-the-search-settings)
* [Customize the glycan identification and FDR settings in PTM-Shepherd](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#glycan-identification-and-fdr-in-ptm-shepherd)
* [Set the output location and run](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#set-the-output-location-and-run)
* [Examine the results](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#examine-the-results)

### Open FragPipe
When you launch FragPipe, check that MSFragger and Philosopher are both configured. If you haven’t downloaded them yet, use their respective ‘Download / Update’ buttons. See [this page](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html) for more help. Python is not needed for glycoproteomics workflows.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/lfq-config.png)

<br>

### Add the data
Load the data files to analyze (drag and drop or browse). Specify experiments if needed for the quantitation being done (not needed for most workflows). See [this page](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html#select-workflow-and-add-spectral-files) for additional details on how to load and categorize experiment files. 

### Load a Glyco workflow
Select the appropriate glyco workflow from the dropdown menu and click 'Load'. There are pre-built workflows for N- and O-glycopeptide analyses with a variety of fragmentation and quantitation methods. See below for more details on the best workflow to choose. Loading a workflow sets the parameters to good base settings, but individual parameters may need to be updated for your analysis (described in [this section](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html#customize-the-search-settings-in-msfragger-and-philosopher))
Workflows:  
**glyco-N-HCD**: Glycopeptide identification for CID/HCD fragmentation of N-glycopeptides (no quant)  
*glyco-N-Hybrid*: Glycopeptide identification for hybrid fragmentation (EThcD, AI-ETD, etc.) of N-glycopeptides  
**glyco-N-TMT**: TMT quantitation of CID/HCD fragmented N-glycopeptides. Note: for TMT-quant of other fragmentation modes, start here and change fragmentation settings accordingly     
**glyco-N-LFQ**: Label-free (MS1) quantation of CID/HCD fragmented N-glycopeptides  
*glyco-N-open-HCD*: Open search (allowing unknown glycan masses) for CID/HCD fragmentation of N-glycopeptides  
*glyco-N-open-Hybrid*: Open search (allowing unknown glycan masses) for hybrid fragmentation of N-glycopeptides  
**glyco-O-HCD**: Glycopeptide identification for CID/HCD fragmentation of O-glycopeptides  
*glyco-O-Hybrid*: Glycopeptide identification for hybrid fragmentation (EThcD, AI-ETD, etc.) of O-glycopeptides  
*glyco-O-open-HCD*: Open search (allowing unknown glycan masses) for CID/HCD fragmentation of O-glycopeptides  
*glyco-O-open-Hybrid*: Open search (allowing unknown glycan masses) for hybrid fragmentation of O-glycopeptides  

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/Fragpipe-glyco_1.png)

<br>

### Fetch a sequence database
Now we need to select a protein sequence database. You can choose to download a readymade human .fas file from [here](https://www.dropbox.com/s/v8tlkwu96f3txfj/2021-05-07-decoys-reviewed-contam-UP000005640.fas?dl=0), or you can download one using FragPipe. Downloading is easy, so we could also choose to download one at this point. On the Database tab, click the ‘Download’ button. Follow the prompts to use the default settings (reviewed human sequences with common contaminants).

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/lfq-databaseoptions.png)

Click ‘Yes’ to download the database. When it’s finished, you should see that the FASTA file path now points to the new database.


<br>

### Customize the search settings
Glycoproteomics searches can require changes to parameters depending on the glycans being analyzed and 
fragmentation method used, in addition to the typical enzyme digestion/sample prep and instrument parameters
that a standard proteomics search has. The key parameter changes for glyco searches are listed below, but for
a full list/explanation of all parameters, see these pages: [msfragger](https://github.com/Nesvilab/MSFragger/wiki/Setting-the-Parameters), [philosopher](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html#validation)
  
**Basic Parameters - MSFragger**  
Set the mass tolerances, enzyme digestion, and variable and fixed modifications to appropriate values for your sample prep/acquisition. See the MSFragger parameter page [here](https://github.com/Nesvilab/MSFragger/wiki/Setting-the-Parameters) for details 
   
**Key Glyco Parameters**:

1. **mass_offsets (list of glycan masses to search)**: (value: 0/mass1/mass2/...) All glycan masses to consider in a search should be specified as mass offsets (separated by '/'). 
NOTE: the default list is for mouse N-glycans (182 masses). Depending on the type of data and search, the glycans considered can vary. The masses of all glycans of interest should be determined and converted to 
a list separated by '/' (see example parameter file). An arbitrary number of mass offsets can be searched, but search speed decreases with the 
number of masses used. If more than a few thousand masses are being considered, consider using an open search instead.
For open searches, do NOT set any mass offsets. 
2. **labile_search_mode**: *(possible values: nglycan, labile, off)* nglycan mode checks for N-glycosylation motif N-X-S/T (X is not P), but is otherwise identical to "labile" mode. Labile mode should be used for all labile modifications (other than N-glycans), including O-glycans. Specify the allowed residues in restrict_deltamass_to. "Off" results in a standard (non-glyco) MSFragger search, in which all mass offsets are assumed to remain intact during activation.  
3. **restrict_deltamass_to**: (overridden in nglycan mode). Specify which amino acids (single letter codes) are allowed to contain the 
glycan mass offsets. Allowed values are single letter amino acid codes. 
Default value: 'all'. For typical O-glycoproteomics searches, should be set to 'ST' or 'STY'. In nglycan mode, this parameter is overridden to use the N-X-S/T sequon.   
When labile_search_mode is 'off', '-' can be used to allow non-localized
matches (i.e. matches to labile mods that have dissociated), by setting the value to 'STY-', for example. It is not necessary to include '-' for labile and nglycan modes,
as labile modifications are considered by default.
4. **fragment_ion_series**: fragment types of interest should be specified here depending on the activation method and data. b~/y~ refer to b/y ions + HexNAc (common for N-glycans in CID/HCD). Recommendations:   
*CID/HCD/IRMPD*: 		b, y, b~, y~, Y (NGlycan or low energy) or b, y, Y (OGlycan or high energy)  
*Hybrid (EThcD/etc)*: 	b, y, c, z, Y  
*ETD/ECD*: 				c, z  
5. **localize_delta_mass**: (values: 0 or 1) specifies whether to search for shifted ions (i.e., peptide fragments containing intact or partial glycan). Required to be set (1) for most glyco searches to allow glycans to be localized.
6. **Y_type_masses**: (value: 0/mass1/mass2/...) Masses of Y-type ions (intact peptide plus partially fragmented glycan) to consider in search. Note that ALL Y masses are applied to ALL potential glycopeptides (regardless of the actual glycan), so including too many can reduce search performance. Can be used in non-glyco searches as well (any modification that partially fragments, leaving behind some mass can be considered). 
7. **diagnostic_fragments**: (value: mass1/mass2/...) m/z values of diagnostic fragment ions (e.g., Oxonium ions) that appear in spectra of peptides containing a mod of interest. If diagnostic_intensity_filter > 0, at least one of the masses provided here must be found at sufficient intensity for any mass offset to be searched for the spectrum. To disable this checking, change diagnostic_intensity_filter to 0.
8. **diagnostic_intensity_filter**: Minimum relative intensity (relative to base peak height) for the SUM of intensities of all diagnostic fragment ions found in the spectrum to consider this a potential glyco (or other labile mod) spectrum. Set to 0 to disable. A value of 0.1 means summed intensity must be 10% the height of the base peak in the MS/MS spectrum to be considered. 
9. **deisotope**: (values: 0 or 1) glycopeptides tend to be larger than tryptic peptides, making deisotoping very helpful. Recommended setting '1' for glyco data. 

**Validation Parameters - Philosopher**  
The parameters for validation and FDR filtering in Philosopher are generally the same as open/offset searches. NOTE: Percolator is not supported for glyco searches.
Key parameters that may change are below:   
 - PeptideProphet: the --masswidth parameter must be larger than the largest glycan mass being searched (e.g., --masswidth 4000 is a common setting)
 - PeptideProphet: specify "--glyc" flag for N-glycan searches to use N-X-S/T sequon in modeling  
 - ProteinProphet: set --maxppmdiff to a large value (e.g., 2000000) to prevent excluding proteins carrying glycans
<br>

### Glycan Identification and FDR in PTM-Shepherd
MSFragger and Philosopher together report glycopeptides as a peptide sequence and a mass shift, and ensure that
all such peptide-mass shift combinations reported pass the specified FDR levels. However, converting a mass shift 
to a specific glycan composition (or structure) is not always straightforward. In PTM-Shepherd, we have developed a 
glycan identification method that identifies the specific glycan composition in each glycopeptide-spectrum match and
performs FDR filtering on the identified glycans. 

Results will be written to the PSM table with the assigned glycan in the Observed Modifications column, score in the Glycan Score column, and q-value (FDR) in the Glycan q-value column. 
NOTE: PSMs that did not pass glycan FDR are still listed, and must be filtered by Glycan q-value less than the set FDR. Glycans are 
also written to the Assigned Modification and Modified Peptide columns if the PSM passed glycan FDR. 

Default parameters for N-glycan analysis are shown below along with a description of each parameters. 

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/glyco_PTMS_params_fragpipe.png)

1. Top bar. 
- The Run PTM-Shepherd box must be checked to enable glycan analysis. 
- Check the extended output box to save additional outputs, including the .rawglyco file that contains details of ions found and 
decoy glycans. 
2. Diagnostic Ion Discovery section. 
- This must be enabled to perform glycan analysis, even though results of diagnostic ion discovery will NOT affect the glycan identification or FDR. 
If diagnostic ion discovery is not needed, leave each of the three boxes blank to skip.
- Y Ion Masses: specify potential masses of "Y"-type ions (i.e., intact peptide plus partially fragmented modification) to look for. PSMs with such ions found will be listed in the .rawglyco file in extended output (if extended output is specified)
- Diagnostic Fragment Masses: specify potential masses of diagnostic ions (fragments of the modification found in the low m/z region, not attached to the peptide) to look for. PSMs with such ions found will be listed in the .rawglyco file in extended output (if extended output is specified)
- Remainder masses: specify potential masses of remainder ions (partially fragmented modification found on peptide fragment (b/y) ions) to look for. PSMs with such ions found will be listed in the .rawglyco file in extended output (if extended output is specified)
3. Glycan Assignment and FDR: basic parameters
- Check the box to enable glycan assignment. NOTE: this is currently only supported for mass offset searches, not for fully open searches.
- Glycan FDR: specify the desired glycan FDR. PSMs will have the resulting q-value written to the PSM table. *NOTE: PSMs that fail glycan FDR will NOT be removed from the PSM table, but can be filtered by looking for PSMs with "Glycan q-value" less than the FDR set here.* Glycan FDR filtering is performed after PSM/peptide/protein FDR (done in Philosopher). 
- Glycan mass tolerance (ppm): Tolerance for matching between the mass of a candidate glycan and the observed delta mass (ppm)
- Isotope Error Range: Precursor monoisotopic peak selection errors to consider when matching candidate glycans, ranging from min to max. Typically should be set wide (any possible error) as matches with unlikely errors are penalized in scoring and will not be chosen unless there is very strong evidence. 
- N-glycan mode: Check for N-glycans. Sets allowed positions to be N-X-S/T sequon only and sets default glycan database to N-glycan internal list. If unchecked, allowed site(s) are taken from the "Restrict localization to" box in the main PTM-Shepherd settings above. 
- Prep for IonQuant: If checked, removes the glycan delta mass from the PSM table report to allow IonQuant to trace peaks. Only needed if running IonQuant on the output. NOTE: delta mass removal means that the PSM table must be regenerated prior to re-running PTM-Shepherd.
- Max Adducts: If considering non-covalent adducts (e.g., ammonium), set the max number of adducts allowed on a glycopeptide here. Set to 0 to disallow adducts.
- Adduct Types: Allowed adduct types: NH3, Fe3, Fe2, Na, Ca, Al. Note that NH3 refers to ammonium adduct (NH4+). All adducts replace protons equivalent to their charge state (e.g., Fe3 replaces 3 protons as Fe 3+)
- Custom Glycan Database: Provide a custom list of glycan candidates to match as a .glyc file. File format: one glycan per line, listed as "Residue1-Number_Residue2-Number..." where Residue is one of HexNAc, Hex, Fuc, NeuAc, NeuGc, Phospho, or Sulfo, and Number is the total count of that residue in the glycan. 
4. Advanced Glycan Parameters:
- Check the box to enable editing advanced parameters. These are the likelihood ratios used to compute glycan scores and may need to be changed for fragmentation other than HCD or for O-glycans. 
- Oxonium ion ratios: For each category of oxonium ions (NeuAc, NeuGc, Fuc, Phospho, Sulfo), oxonium ions specific to glycans containing this residue increase the score by log of the first value if found, and decrease it (by adding the log) of the second value if not found. The first value thus should always be greater than 1 and the second value always less than 1 (but must be greater than 0). 
The third value is the "expected" intensity (relative to the base peak of the spectrum) of such oxonium ions, used to decrease the effect of oxonium ions from co-fragmentation of another glycopeptide. Oxonium ions found at values less than the expected intensity will have their positive contribution to score decreased, while ions found above the expected intensity will have their positive contribution increased. 
- Y ion ratios: Y-ions are divided into two categories: containing Fucose or not. Likelihood ratios are the same as for oxonium ions, but no expected intensity is used. 
- Decoy Type: Several methods of decoy generation are available, but the default (1) should be used in most situations. 0 means generate decoys with intact mass within +/- 3 Da of the target glycan mass, resulting in less strict FDR filtering (generally not advisable). 1 means generate a decoy mass within the provided mass tolerance of the target glycan mass, allowing a randomly selected isotope error. 2 is the same as 1, but without isotope error. 3 means decoy glycan masses are exactly the same as targets and the mass error is not used in distinguishing between targets and decoys. 
- Print Decoy Glycans: By default, if a PSM matches to a decoy glycan, the best target glycan is reported in the PSM table with a q-value of 1. To report the decoy glycan instead for diagnostics, check this box. 
### Set the output location and run
On the Run tab, make a new folder for the output files (e.g. ‘glyco_results’), then click ‘RUN’ and wait for the analysis to finish.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/open-run.png)


When the run is finished, ‘DONE’ will be printed at the end of the text in the console.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/tmt-2plexes-done.png)

<br>

### Examine the results
In the output location, you will see several output files including the "psm.tsv" table, which contains all PSMs
found in the analysis. For searches without quantitation, the PSM table is the best place to look for glycopeptide
information. Mass offset and open searches will generate PSMs with delta mass values (corresponding
to glycan masses in glyco searches). For each PSM, the glycan identified by PTM-Shepherd can be found in the "Observed Modifications" column, 
followed by the glycan score and q-value. The location of the glycan determined by MSFragger can be found in the 
"MSFragger localization" column, which prints the peptide sequence in capital letters with the determined location
as a lower case letter. If multiple locations are possible (ambiguous localization), multiple residues may be lower
case. NOTE: in cases of ambiguous localization, when writing glycans to assigned modifications for quantitation, the
first allowed site will be assigned.   

If performing TMT analysis, reports summarizing the TMT results can be found in the "tmt-report" folder. For glyco
searches, the multi-mass reports summarize the results by peptide sequence and glycan composition. All other reports
are as in typical searches (see [this page](https://fragpipe.nesvilab.org/docs/tutorial_tmt.html) for details)

<br>
<br>
<br>
<br>

#### [Back to FragPipe homepage](https://fragpipe.nesvilab.org/)