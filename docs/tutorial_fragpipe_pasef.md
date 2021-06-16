## Using TIMS-TOF PASEF data in FragPipe

##### FragPipe can be downloaded [here](https://github.com/Nesvilab/FragPipe/releases). Follow the instructions on that same Releases page to launch the program.

__Bruker's native library needs [Visual C++ Redistributable for Visual Studio 2017](https://aka.ms/vs/16/release/VC_redist.x64.exe) in Windows.__ If you see an error saying cannot find Bruker native library, please try to install the Visual C++ redistibutable.

#### Configure FragPipe
When FragPipe launches, the first tab in the window ('Config') will be used to configure the program.
1. Connect FragPipe to a MSFragger .jar program file. **Make sure the original directory structure of the MSFragger download is maintained, where the .jar is in the same place as the 'ext' folder.**  If you have already downloaded MSFragger, use the 'Browse' button to select the .jar file or 'Update' to upgrade to the latest version. If you have not downloaded MSFragger before, use the 'Download' button. [(MSFragger installation help)](http://msfragger.nesvilab.org/tutorial_setup_fragpipe.html#install-update-or-use-an-already-downloaded-version-of-msfragger)
2. Connect FragPipe to a Philosopher program file. If you already have it downloaded, select 'Browse', otherwise select 'Download'. [(Philosopher installation help)](http://msfragger.nesvilab.org/tutorial_setup_fragpipe.html#install-update-or-use-an-already-downloaded-version-of-philosopher)
3. Optional: Python is needed to perform database splitting (necessary in complex searches/low memory situations) and spectral library generation. If you already have Python 3 or greater plus a few additional packages installed (**numpy**, **pandas**, **Cython**, and **msproteomicstools**) use 'Browse' to locate your python.exe file. [(Python installation help)](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html#optional-install-update-or-use-an-already-installed-version-of-python) 

For more help, see the full [tutorial on FragPipe configuration](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html).

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_pasef_1.png)
 
 <br>

#### Add input files
In the next tab, 'Select LC/MS Files', drag & drop raw PASEF files (.d extension, each data file is a folder) into the window or select 'Add files'. 'Add Folder Recursively' can also be used, make sure to remove any .d folders from the input list before continuing. If you have already run MSFragger on the .d files, make sure the .mzBIN files resulting from that analysis are in the same directory as the .d files to speed up the analysis.

Specify the appropriate labels for the replicates/fractions in your experiment.

**Note**: If you do not need to perform quantification, .mgf or \_calibrated.mgf files can be used instead of .d.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_pasef_2.png)

#### Group input files
In the 'Select LC/MS Files' tab, indicate how you'd like PSM/peptide/protein reports to be generated.

##### For a single set of reports (search results from all input files merged)
Leave the 'Experiment' and 'Replicate' fields blank, and ensure that the 'Multi-Experiment Report' box on the 'Report' tab is not checked.

##### For reports with results from different replicates shown in separate columns
Indicate the 'Experiment' and 'Replicate' for each input file as shown below, where there are three replicates for two experimental conditions. On the 'Report' tab, check 'Multi-Experiment Report'. 
![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/specify_replicates_pasef.png)

##### For reports with results from different fractionated replicates shown in separate columns
Indicate the 'Experiment' and 'Replicate' for each input file as shown below, where each replicate (rep1, rep2) of two experimental conditions is composed of two fractions. Different fractions (1 & 2) from the same sample should have the same 'Experiment'/'Replicate' name. On the 'Report' tab, check 'Multi-Experiment Report'.
![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/specify_fractions_pasef.png)
<br>

**Note:** for compatibility with REPRINT ([Resource for Evaluation of Protein Interaction Networks](https://reprint-apms.org/)), 'Experiment' names should be written as `gene_condition`, e.g. `HDAC8_control`.

<br>

#### Specify a protein sequence database
In the 'Database' tab,

If you haven't made a database file using FragPipe/Philosopher before, select 'Download' to fetch one from Uniprot. Then choose your options and select an organism (use the uniprot proteome ID to specify your own, e.g. 'UP000000625' for E. coli).

Use 'Browse' to select a FASTA file from a previous FragPipe/Philosopher analysis.

If you need to use a custom FASTA database, it must follow a certain format and contain decoy sequences. Click 'Browse' to navigate to your custom FASTA. If you select 'Try Auto-Detect', 50% of the entries should contain the decoy tag. For help adding decoys and database formatting, see the instructions on the 'Database' tab or [here](https://github.com/Nesvilab/philosopher/wiki/Database).


![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_pasef_3.png)

<br>

#### Set MSFragger search parameters
In the 'MSFragger' tab,
1. Select the type of database search you want to perform.

   **Closed search**: To perform a closed search (normal precursor mass tolerance), select 'Closed Search'. This will prompt you to also update the downstream parameters for closed searching, select 'Yes'.

   **Open search**: To perform an open search (large precursor mass tolerance, used for finding unspecified post translational modifications), select 'Open Search'. This will prompt you to also update the downstream parameters for open searching, select 'Yes'.

   **Non-specific search**: To perform a closed search where peptides are not required to have any enzymatic terminus, select 'Non-specific Search'. This will prompt you to also update the downstream parameters for non-specific search, select 'Yes'. 
   
**Note:** For non-specific searches or for searches with many variable modifications, you may need to use the database splitting option, which requires an installation of [Python](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html#optional-install-update-or-use-an-already-installed-version-of-python).
   
 2. Fill in the amount of memory (in GB) that FragPipe will be allowed to use. We recommend leaving this set to '0', which will allow MSFragger to analyze the TIMS-TOF data as quickly as possible.
 3. Specify the search parameters you want to use. For more information on these parameters, see the [MSFragger wiki page](https://github.com/Nesvilab/MSFragger/wiki/Setting-the-Parameters).
 

 
![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_pasef_4.png)
 
 
 <br>
 

#### Set downstream processing parameters
In the 'Downstream' tab,
1. Select 'Run PeptideProphet' to validate your search results. (More information about PeptideProphet can be found [here](http://peptideprophet.sourceforge.net/).
2. If you previously updated the downstream parameters when setting MSFragger search parameters, you can skip to the next section. You can also re-load default downstream processing parameters by selecting the appropriate 'Load defaults' button.
3. Select 'Run ProteinProphet' to validate your protein identifications. (More information about ProteinProphet [here](http://proteinprophet.sourceforge.net/)).


![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_pasef_5.png)

<br>

#### Set quantification
In the 'Report' tab,
1. Select 'Generate report' to output tab-delimited tables of the search results.
2. Select 'Run Quantification' to perform label-free quantification if desired. Make sure 'IonQuant' is selected. Boundaries for 3D LC-IM-MS feature detection can optionally be set here, we recommend using the defaults.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_pasef_6.png)

 <br>
 
#### Run FragPipe
1. Browse for the folder where you would like the search results to be written.
2. The 'Print Commands' button can be used to see every line of commands that will be executed without actually running them.
3. Press 'RUN' to begin the analysis!

For more information, see the MSFragger [wiki](https://github.com/Nesvilab/MSFragger/wiki) for technical documentation and the [FAQ](https://github.com/Nesvilab/MSFragger/wiki/Frequently-Asked-Questions).


![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_pasef_7.png)

