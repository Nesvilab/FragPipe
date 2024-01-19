## Perform two-pass search to analyze novel peptides and PTMs

##### [FragPipe](https://fragpipe.nesvilab.org) can be downloaded [here](https://github.com/Nesvilab/FragPipe/releases). Follow the instructions on that same Releases page to launch the program. See [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html#configure-fragpipe) for help configuring FragPipe.

FragPipe has the ability to perform the "two-pass search" to detect novel peptides and PTMs semi-automatically. It performs the following three steps.
1. Perform the first search to identify the canonical peptides and/or common PTMs
2. Generate new "sub mzML" files by excluding the scans having good matches
3. Perform the second search to identify the novel peptides and/or PTMs

#### Common issues of two-pass search approaches due to the limited high-quality scans in the second search
1. The spectra are not mass calibrated or the mass calibration does not work well
2. The rescoring tools, such as Percolator, cannot learn a good model in the second search

#### To address these issues, FragPipe 
1. performs mass calibration in the first search, and then generates sub mzML files using the calibrated spectra
2. writes the Percolator models (`*_percolator.weights` files) to the result directory after the first search. These models will be used in the second search.

##### _An alternaive approach to analyze novel/variant peptides_
FragPipe can also perform group FDR estimation for the peptides with different levels of evidences. Please check [Perform group FDR estimation for different identification groups](https://fragpipe.nesvilab.org/docs/tutorial_group_fdr.html) for details.

### The two-pass search approach supports the `Default`, `LFQ-MBR`, and TMT-related workflows.

### Perform two-pass search for the label-free quantification data
1. Load the `LFQ-MBR` workflow in the `Workflow` tab
2. Load the LC-MS files, assign experiments and bioreplicates (if applicable), and set the `Data type` to `DDA`
3. Load your FASTA file in the `Database` tab
4. Go to the `Run` tab, specify the output directory
5. **[Critical]** In the `Run` tab, enable the `Write sub mzML`. FragPipe will generate "sub mzML" files containing the scans do not pass the FDR filtering and have the probability smaller than or equal to the `Min probability threshold of excluding scans from sub mzML files`. 

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/two_pass_search_1.png)

6. Click run. FragPipe will perform the "first-pass search".
7. After FragPipe finish, go back to the `Workflow` tab
8. Select the `Custom` workflow and click `Load workflow`

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/two_pass_search_2.png)

9. Locate the `fragpipe-second-pass.workflow` file in the result directory and load it
10. Click `Clear files` to remove all LC-MS files used in the first-pass search
11. Click `Load manifest`, locate the `fragpipe-second-pass.manifest` file in the result directory, and load it

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/two_pass_search_3.png)

12. Go to the `Database` tab to specify the FASTA file for the second search.
13. Go to `Run` tab, specify the new output directory and click `Run`


### Perform two-pass search for the isobaric-labeling data
1. Load one of the TMT workflow in the `Workflow` tab
2. Load the LC-MS files, assign experiments and bioreplicates (if applicable), and set the `Data type` to `DDA`
3. Load your FASTA file in the `Database` tab
4. Go to the `Quant (Isobaric)` tab, configure the sample/channel annotations. Details can be found in the [other tutorial](https://fragpipe.nesvilab.org/docs/tutorial_tmt-2plexes.html)
5. Go to the `Run` tab, specify the output directory
6. **[Critical]** In the `Run` tab, enable the `Write sub mzML`. FragPipe will generate "sub mzML" files containing the scans do not pass the FDR filtering and have the probability smaller than or equal to the `Min probability threshold of excluding scans from sub mzML files`. 

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/two_pass_search_1.png)

7. Click run. FragPipe will perform the "first-pass search".
8. After FragPipe finish, go back to the `Workflow` tab
9. Select the `Custom` workflow and click `Load workflow`

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/two_pass_search_2.png)

10. Locate the `fragpipe-second-pass.workflow` file in the result directory and load it
11. Click `Clear files` to remove all LC-MS files used in the first-pass search
12. Click `Load manifest`, locate the `fragpipe-second-pass.manifest` file in the result directory, and load it

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/two_pass_search_3.png)

13. Go to the `Database` tab to specify the FASTA file for the second search.
14. Go to the `Quant (Isobaric)` tab to ensure that the sample/channel annotation is correct.
15. Go to `Run` tab, specify the new output directory and click `Run`



<br>
<br>


#### Key References
Desai H, Ofori S, Boatner L, Yu F, Villanueva M, Ung N, Nesvizhskii AI, Backus K. [Multi-omic stratification of the missense variant cysteinome](https://doi.org/10.1101/2023.08.12.553095), bioRxiv (2023).



<br>
<br>

#### [Back to FragPipe homepage](https://fragpipe.nesvilab.org/)
