### Fast photochemical oxidation of proteins (FPOP) Data Analysis with FragPipe

### FPOP Overview
Structural information of proteins can be obtained by labelling solvent accessible regions under different environmental states. 
In FPOP, OH radicals are used to irreversibly oxidized solvent exposed residues of proteins undergoing structural changes due to perturbations such as ligand-binding or mutations. 
Some benefits of this information-rich technique include single-protein to proteome wide mode, irreversible labelling, and 19 of the 20 residues can be oxidized. 
However, data analysis challenges (including the fast search space expansion due to the large combinations of oxidation modifications possible) have precluded an easier adaption of FPOP. 
We have implemented an hybrid workflow in FragPipe to search the most common FPOP modifications as variable modifications, and the rest as [mass offsets](https://fragpipe.nesvilab.org/docs/tutorial_convert.html). 
This hybrid approach increased amount of FPOP PSMs found as well, as decreasing search space and analysis time. See **[publication](https://doi.org/10.1021/acs.analchem.3c02388)** for more details.  

### Tutorial Contents:
1.	FPOP Dataset
2.	Input data conversion
3.	Setting up FragPipe
4.	Running Worklfow
5.	Understanding Results
6.	Downstream Analysis


#### 1.	FPOP Dataset 
The dataset used for this tutorial is available under in ProteomeXchange with identifier PXD019290. 
In this [study](https://pubs.acs.org/doi/10.1021/acs.jproteome.0c00245) they performed FPOP inside live *C. Elegans* nematodes. 
In order to increase efficiency of In Vivo- FPOP labelling, chemical compounds capable of making the nematodes more permeable to OH radicals were used. 
Oxidation is already present at background levels in cells, therefore is a great importance to use several controls to ensure the increase in oxidation. 
It will be typical in FPOP experiments to see several replicates and types of controls to ensure that the increase in oxidation is due to a structural change of proteins and not oxidation from other sources. 
For training purposes, the tutorial dataset used is a fraction of the original dataset. The samples to be used correspond to experiments with/without the chemical enhancer 1pAZ and the respective with (FPOP) or without (no FPOP) laser controls.

##### Ways to obtain the tutorial dataset
Dataset can be accessed by [ProteomeXchange](https://dx.doi.org/10.6019/PXD019290) or going directly to [PRIDE.](https://www.ebi.ac.uk/pride/archive/projects/PXD019290)
Go to project [FTP](https://ftp.pride.ebi.ac.uk/pride/data/archive/2020/06/PXD019290/)
Only the following files are needed ( see path column below), where # stands for all the replicates (1 – 3). The other columns will be used in section III.:

|File                           |Experiment Name     |Bioreplicate |Mode         |
| :----------------------------  | :---------------: | -----------: | :-----------: |
| path/1pAZ_Control_BR1_L#.mzML | Sample_no1pAZ   | 1           | DDA         |
| path/1pAZ_Control_BR1_NL#.mzML| Control_no1pAZ  | 1           | DDA         |
| path/1pAZ_sample_BR1_L#.mzML  | Sample_1pAZ     | 1           | DDA         |
| path/1pAZ_sample_BR1_NL#.mzML | Control_1pAZ    | 1           | DDA         |


#### 2. Input Data Conversion
Convert .RAW to mzML file using MSConvert with the [recommended parameters](https://fragpipe.nesvilab.org/docs/tutorial_convert.html).
#### 3. Setting up FragPipe 
Follow FragPipe Tutorial for FragPipe configuration, adding data, and selecting a workflow [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html). 
To see how to divide the files into experiments see the manuscript file example in section *Ways to obtain the tutorial dataset* above.
Next, select FPOP Workflow among the list of built-in workflows.
#### 4. Running a FragPipe Workflow
For general instructions in how to run a FragPipe workflow and saving results, see [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html).
##### Key parameters
While the current FPOP workflow has been optimized for the current test dataset, the user is encouraged to select appropriate parameters for their data such as enzyme (default is stricttrypsin), as well as if new oxidation modifications are expected. 
In the figure below mass offset are separated by spaces, but they can also be separated by “/”.
<div align="center">
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/FPOP-MassOffsets.png" width="600px" align="middle"/>
</div>
<br>

Another point at which the workflow can be tailored for the user’s dataset is the assigned modifications in the Filter command. 
Due to the large difference in unmodified vs modified peptides (see results below), global FDR filtering will set a threshold that will not be appropriate for both groups. 
See **[publication](https://doi.org/10.1021/acs.analchem.3c02388)** for more details on Group-based FDR. By default, methionine oxidation and n-terminal acetylation are assigned (see figure below), but if another modification is abundant, it can be added as well.  

<div align="center">
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/FPOP-Group-basedFDR.png" width="600px" align="middle"/>
</div>
<br>

More specifically, not only the modification needs to present in approx. > 20% of the PSMs, but it also causes a large search space expansion (there more this modification will more likely be to produced decoys).
In our example, after some testing we discover that n-terminal acetylation only expanded our search space barely, therefore in our case it was not that abundant. 

 
#### 5. Understanding results
To understand FragPipe outputs in general please go [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe_outputs.html).
In our workflow mass offsets are reported as variable modifications, the user can determine if a PSM contains FPOP modification by looking at the “Assigned Modifications” column in psm.tsv files, as well in other FragPipe output files (if that column is included). 
Also, MSFragger will attempt to localize these offsets. If localization was successful, information will be found in the columsn MSFragger Localization, best score with/without Delta Mass.

<div align="center">
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/FPOP-Results.png" width="600px" align="middle"/>
</div>
<br>

#### 6. Downstream Analysis
Since FragPipe v20.1, it is possible to perform quantitative analysis on FPOP results. 
Make sure the FPOP Quant checkbox is active. 
Also, in order to subtract control oxidation the common label of control samples can be given, as well as the common label for FPOP samples. 
FragPipe will automatically match the rest of the samples labels so the right counterpart will be compared. 
Results come in both peptide-level and residue level. 
In FPOP_peptide.tsv all overlapping peptides resulting from different cleavage sites will be group together and reported under the longest peptide group along with all the modifications found.
Same approach is also done at the residue level (with the option to customize the site region size -see figure below). 
 

#### FPOP_peptides.tsv 

**Group Name**  Longest peptide cover by all overlapping peptides.

**Protein** protein sequence header corresponding to the identified peptide sequence; this will be the selected razor protein if the peptide maps to multiple proteins (in this case, other mapped proteins are listed in the ‘Mapped Proteins’ column).

**Protein ID** UniProt protein identifier (primary accession number).

**Entry Name** entry name for the selected protein.

**Gene** gene name for the selected protein.

**Protein Description** name of the selected protein.

**Mapped Proteins** additional proteins the identified peptide maps to (including any arising from I/L substitutions).

**Mapped Genes** additional genes the identified peptide may originate from (including any arising from I/L substitutions).

**Peptide Sequence** peptide amino acid sequence, no modifications included (‘stripped’ peptide sequence).

**Start** Residue number of the start of the peptide in the intact protein sequence.

**End** Residue number of the end of the peptide in the intact protein sequence.

**FPOP Mods** All FPOP modifications found in all the overlapping peptides.

**Other** Mods Other modifications found in the overlapping peptides (e.g C57).

**Spectral counts** per experiment number of corresponding PSMs.
<br>

#### FPOP_sites.tsv
Group Name UniProt protein identifier (primary accession number)_site location in protein sequence 
Protein protein sequence header corresponding to the identified peptide sequence; this will be the selected razor protein if the peptide maps to multiple proteins (in this case, other mapped proteins are listed in the ‘Mapped Proteins’ column)
Protein ID UniProt protein identifier (primary accession number)
Entry Name entry name for the selected protein
Gene gene name for the selected protein
Protein Description name of the selected protein
Mapped Proteins additional proteins the identified peptide maps to (including any arising from I/L substitutions)
Mapped Genes additional genes the identified peptide may originate from (including any arising from I/L substitutions)
Peptide Sequence peptides use for determination of the site
Start First position of the largest area cover by the overlapping peptides used to calculate the site
End Last position of the larges area cover by the overlapping peptides used to calculate the site
FPOP Mods All FPOP modifications found in all the overlapping peptides
Other Mods Other modifications found in the overlapping peptides (e.g C57)	
Spectral counts per experiment number of corresponding PSMs

