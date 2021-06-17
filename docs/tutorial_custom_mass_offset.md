### Using a custom offset workflow with FragPipe: RNA crosslinking analysis
[XRNAX](https://www.xrnax.com/) is a method for “unbiased” purification of protein-crosslinked RNA, which can be used to help probe and understand protein-RNA interactions. For this analysis, we will need to load a custom workflow. This workflow searches the database with mass offsets that correspond to crosslinked RNA fragments. One can alternatively perform a fully open search (with the mass window set to -150, +1000 Da) as was done in the publication (citation below). However, mass offset searches are faster and more sensitive than performing a full open search, with the limitation that you can only find peptides with mass shifts specified in the offset list. 

This tutorial will use a custom workflow to seach a single .mzML spectral file of RNA-crosslinked tryptic peptides from an MCF7 cell line, acquired on a QExactive HF. Original publication: Trendel, Jakob, et al. "The human RNA-binding proteome and its dynamics during translational arrest." Cell 176.1-2 (2019): 391-403. The files you will need for this tutorial can be found in this [Dropbox folder](https://www.dropbox.com/sh/biwqa6dw3ti4bfz/AADRvn5mRxA3ple9DAC7LMvka?dl=0).

##### Tutorial contents
* [Load a custom workflow](https://fragpipe.nesvilab.org/docs/tutorial_offset.html#load-a-custom-workflow)
* [Inspect the search parameters](https://fragpipe.nesvilab.org/docs/tutorial_offset.html#inspect-the-search-parameters)
* [Get a sequence database](https://fragpipe.nesvilab.org/docs/tutorial_offset.html#get-a-sequence-database)
* [Set the custom annotation file](https://fragpipe.nesvilab.org/docs/tutorial_offset.html#set-the-custom-annotation-file)
* [Pick an output location and run](https://fragpipe.nesvilab.org/docs/tutorial_offset.html#pick-an-output-location-and-run)
* [Examine the results](https://fragpipe.nesvilab.org/docs/tutorial_offset.html#examine-the-results)



### Load a custom workflow
Download the XRNAX-MassOffset.workflow file from [Dropbox](https://www.dropbox.com/sh/biwqa6dw3ti4bfz/AADRvn5mRxA3ple9DAC7LMvka?dl=0). Then in the fragpipe directory, locate the workflows subfolder, and copy the XRNAX-MassOffset.workflow file into this folder.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/custom-offset-workflow.PNG)

Close and then re-launch FragPipe to refresh the workflows. On the Workflow tab, select XRNAX-MassOffset from the workflow dropdown menu and press ‘Load’. Drag and drop or use ‘Add files’ to input the file 20160530_QE1_JT_XRNAXpep_Trp_hydro.mzML.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/custom-offset-workflowtab.PNG)

<br>

### Get a sequence database
You can choose to download a readymade human protein database file from [here](https://www.dropbox.com/s/v8tlkwu96f3txfj/2021-05-07-decoys-reviewed-contam-UP000005640.fas?dl=0), or you can download one using FragPipe. On the Database tab, click the ‘Download’ button. Follow the prompts to use the default settings (reviewed human sequences with common contaminants).

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/lfq-databaseoptions.png)

Click ‘Yes’ to download the database. When it’s finished, you should see that the `FASTA file path` now points to the new database.


### Inspect the search parameters
On the MSFragger tab, inspect the search parameters. Scroll down to the Mass Offsets box in the Advanced Options section-- this is where the crosslinked RNA fragment masses are specified.


### Set the custom annotation file
On the PTMs tab, we will add a custom annotation file to supply names for the mass offset values (as a list of name-mass pairs), download 'unimod_20191002_RNAadducts.txt' from [Dropbox](https://www.dropbox.com/sh/biwqa6dw3ti4bfz/AADRvn5mRxA3ple9DAC7LMvka?dl=0) if you haven't already. In the ‘Custom annotation file’ box, provide the file path to unimod_20191002_RNAadducts.txt. You can copy and paste the file path or ‘Browse’ to add it.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/custom-offset-ptmshepherd.PNG)

<br>

### Pick an output location and run
Now we can start the analysis from the ‘Run’ tab after specifying an output location, you can make a new folder: ‘fragpipe_RNA-crosslinking_results’.


### Examine the results
Inspect the global.profile.tsv file and compare it to the figure below (generated from the pepxml file with [DeltaMass](https://github.com/Nesvilab/deltamass)). This is a histogram of the full range of delta masses observed from all peptide-spectrum matches. Two of the most prominent peaks are labeled (U and cyclic-U), and the higher mass peaks, between 600-700 Da and 900-1015 Da) correspond to various combinations of two and three attached nucleotides, respectively. 

<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/custom-offset-deltamass.png" width="600px" align="middle"/>

You should observe the mass shifts discussed in the [manuscript](https://doi.org/10.1016/j.cell.2018.11.004) (Figure 2B, shown below). What are the mapped_mass_1 names given to the top two entries in the global.profile.tsv file? Do these two mass shifts each predominantly localize to a particular residue(s)? Does the annotation (mapped_mass_1 name) of the top mass shift show up elsewhere in the table?

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/custom-offset-histogram.PNG)

<br>
<br>
<br>
<br>

#### [Back to FragPipe homepage](https://fragpipe.nesvilab.org/)
