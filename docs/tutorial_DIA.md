## Building spectral libraries for DIA analysis

##### [FragPipe](https://fragpipe.nesvilab.org) can be downloaded [here](https://github.com/Nesvilab/FragPipe/releases). Follow the instructions on that same Releases page to launch the program. See [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html#configure-fragpipe) for help configuring FragPipe.

FragPipe has several options for building spectral libraries for DIA data analysis:

* **[Build a library from DDA data](https://fragpipe.nesvilab.org/docs/tutorial_DIA.html#build-a-library-from-dda-data)**
* **[Build a library from DIA data (direct DIA using DIA-Umpire)](https://fragpipe.nesvilab.org/docs/tutorial_DIA.html#build-a-library-directly-from-dia-data)**
* **[Build a library from combined DDA and DIA data](https://fragpipe.nesvilab.org/docs/tutorial_DIA.html#build-a-library-from-combined-dda-and-dia-data)**

FragPipe generated libraries can be used to [**quantify with DIA-NN**](https://fragpipe.nesvilab.org/docs/tutorial_DIA.html#quantify-with-dia-nn).

Skyline users may also choose to import interact-.pep.xml files into Skyline for spectral library building and further analysis of DIA experiments, see this [tutorial](https://fragpipe.nesvilab.org/docs/tutorial_skyline.html).

Please note that only the first option (library building from DDA) currently works with timsTOF data. In this case, timsTOF DDA-PASEF raw files (.d) can be used directly for spectral library building, as well as .mgf files converted by Bruker DataAnalysis. To use .mgf, each .mgf file needs to copied out of its .d folder to a new joint folder (FragPipe cannot "see" .mgf files inside of .d folders for now).
<br>
<br>

The dataset used below for illustration was downloaded from [PXD011691](http://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD011691). It includes 10 samples analyzed using DIA (10 mouse brain tissue, with UPS proteins spiked in at varying concentration). It also includes 6 DDA runs (pool of the same 10 brain tissues, with peptides fractionated into 6 fractions) collected for building a spectrum library. You can download a subset of the dataset (2 DIA files and 2 DDA files in mzML format) from Dropbox [here](https://www.dropbox.com/sh/tix2mbp95k0nxcs/AACoGPnptbjjKuLB2-yGPry4a?dl=0) to use for these demos.    

### Build a library from DDA data
1. In Workflow tab of FragPipe, Select the 'SpecLib' workflow from the dropdown menu and 'Load'.
2. Load DDA spectral files in mzML or raw format. (In this example, 6 DDA files corresponding to 6 fractionated peptide samples were loaded.)
3. In the 'Database' tab, download or select an existing database. (In this case, a mouse database was downloaded with reviewed sequences, decoys, common contaminants, and iRT peptides. UPS protein sequences were also added manually)

**Note**: If you're working with a non-human dataset, change the 'RT calibration' option on the 'Spec Lib' tab to 'iRT' if these peptides have been spiked-in. EasyPQP will use the ciRT option (Biognosys common HeLa peptides) by default.

4. On the 'Run' tab, choose the location to output the results and click 'RUN'.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/DIA-tutorial_DDALibOnly.png)
<br>


### Build a library directly from DIA data
1. Select the 'DIA-Umpire_SpecLib' workflow from the dropdown menu and 'Load'.
2. Load DIA spectral files in .mzML or .raw format. (In this example, 10 DIA runs were loaded.)
3. On the 'Umpire' tab, choose the appropriate settings:
 - Change 'Max Missed Scans' to 2 if building a library from DIA data only (slower run time but higher identification sensitivity).
 - Check 'Remove Background' if building a hybrid DDA+DIA library (see below) and if there are many DIA runs (fastest run time).
 - Uncheck 'Mass Defect Filter' if DIA data is generated on modification-enriched peptides (e.g. phospho), or if you're interested in extended PTM searches.
4. In the 'Database' tab, download or select an existing sequence database.
5. On the 'Run' tab, choose the location to output the results and click 'RUN'.

**Note**: If DIA-Umpire fails or is interrupted, temporary files will cause issues if the process runs again. Make sure to delete any temporary files that are generated alongside the raw/mzML files before re-running FragPipe.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/DIA-tutorial_DIAUmpireLib.png)
<br>


### Build a library from combined DDA and DIA data
This workflow is composed of two steps:

A) run just the DIA-Umpire step to extract pseudo-MS/MS spectra from DIA data, and

B) build the library from the pseudo-MS/MS DDA files and additional DDA files**


1. For **part A**, load the 'DIA_Umpire' workflow, add DIA mzML files, and adjust the DIA-Umpire parameters as needed (see above). Select the output destination and 'RUN'. When DIA-Umpire is finished, the output folder will contain three pseudo-MS/MS (pseudo-DDA) files for each input mzML, with the suffixes `_Q1.mzML`, `_Q2.mzML`, `_Q3.mzML`.
 
2. For **part B** (shown below), first load the 'SpecLib' workflow.
3. Clear the DIA files from part A.
4. Load DDA mzML files and also all pseudo-MS/MS mzML files generated by part A.
5. On the 'Run' tab, click 'RUN'.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/DIA-tutorial_CombinedLib.png)
<br>

### Quantify with DIA-NN
DIA-NN is available for download [here](https://github.com/vdemichev/DiaNN) 
<br>

1. Click 'Raw' and load mzML files (or RAW format if DIA-NN has been configured to read the RAW format).
2. Select the spectral library generated using FragPipe ('library.tsv' file).
3. Choose where to write the output and the file name (e.g. 'DIA-NN_DIALib.tsv').
4. Specify the number of threads to use.
5. Set 'Protein inference' to 'Off', and 'Quantification strategy' to 'Robust LC (high accuracy)' (recommended). In the FragPipe-generated spectral library, shared peptides are assigned to proteins with the most evidence (razor proteins).
6. Click 'Run'.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/DIA-tutorial_DIANN.png)
<br>

#### Key References
Tsou CC, Avtonomov D, Larsen B, Tucholska M, Choi H, Gingras AC, Nesvizhskii AI. [DIA-Umpire: comprehensive computational framework for data-independent acquisition proteomics](https://doi.org/10.1021/acs.analchem.9b04418), Nature Methods 12:258-64 (2015).

Demichev V, Yu F, Teo GC, Szyrwiel L, Rosenberger G, Decker J, Kaspar-Schoenefeld S, Lilley KS, Mülleder M, Nesvizhskii AI, Ralser M. [High sensitivity dia-PASEF proteomics with DIA-NN and FragPipe](https://www.biorxiv.org/content/10.1101/2021.03.08.434385v1.full), bioRxiv (2021).

<br>
<br>

#### [Back to FragPipe homepage](https://fragpipe.nesvilab.org/)
