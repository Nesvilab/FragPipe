## Label-free quantification with FragPipe

This tutorial demonstrates label-free quantification with match-between-runs using a dataset published in [Proteomics separates adult-type diffuse high-grade gliomas in metabolic subgroups independent of 1p/19q codeletion and across IDH mutational status](https://doi.org/10.1016/j.xcrm.2022.100877) (ProteomeXchange identifier [PXD024427](https://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD024427)). In this study, researchers studied high-grade adult-type diffuse gliomas are malignant neuroepithelial tumors with poor survival rates in combined chemoradiotherapy. They used MS1-based label-free quantification (LFQ) mass spectrometry to characterize 42 formalin-fixed, paraffin-embedded (FFPE) samples from IDH-wild-type (IDHwt) gliomas, IDH-mutant (IDHmut) gliomas, and non-neoplastic controls. 

In this tutorial, we will use just 6 samples, 3 IDHmut and 3 IDHwt. We will use mzML files, although Raw files can be used instead. The files can be downloaded from [here](https://www.dropbox.com/scl/fo/i8m5hb3vgonr8n0q8el8h/h?rlkey=oan5otg2ss07x9s3rqhny8hmb&dl=1)


### Tutorial contents
* [Open FragPipe](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#open-fragpipe)
* [Load the data](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#load-the-data)
* [Load the LFQ-MBR workflow](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#load-the-lfq-mbr-workflow)
* [Fetch a sequence database](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#fetch-a-sequence-database)
* [Inspect the search and quantification settings](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#inspect-the-search-and-quantification-settings)
* [Set the output location and run](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#set-the-output-location-and-run)
* [Inspect the results](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#inspect-the-results)


### Open FragPipe
When you launch FragPipe, check that MSFragger, IonQuant, and Philosopher are configured. (If you haven’t downloaded them yet, use their respective ‘Download / Update’ buttons. Please see the tutorials [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html#configure-fragpipe) and [here](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html). Python is not needed for these exercises.)

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/share-config.png)


### Load the data
On the ‘Workflow’ tab, drag and drop the six .raw spectral files or use the ‘Add files’ button to browse for them. We are using a subset of the full dataset with annotations shown below.

| Path                                                              | experiment | bioreplicate | data type |
|-------------------------------------------------------------------|------------|--------------|-----------|
| 20170612_QEP8_JaBA_SA_LT01_V1_LC12_8_2FX2D1mS123fM2_sample11.mzML | IDHmut     | 1            | DDA       |
| 20170612_QEP8_JaBA_SA_LT01_V1_LC12_8_2FX2D1mS123fM2_sample12.mzML | IDHmut     | 2            | DDA       |
| 20170612_QEP8_JaBA_SA_LT01_V1_LC12_8_2FX2D1mS123fM2_sample13.mzML | IDHmut     | 3            | DDA       |
| 20170612_QEP8_JaBA_SA_LT01_V1_LC12_8_2FX2D1mS123fM2_sample03.mzML | IDHwt      | 4            | DDA       |
| 20170612_QEP8_JaBA_SA_LT01_V1_LC12_8_2FX2D1mS123fM2_sample06.mzML | IDHwt      | 5            | DDA       |
| 20170612_QEP8_JaBA_SA_LT01_V1_LC12_8_2FX2D1mS123fM2_sample09.mzML | IDHwt      | 6            | DDA       |

Once you’ve added the files, you can annotate them by editing the ‘Experiment’ and ‘Bioreplicate’ fields manually or in batches with the ‘Custom’ button. The data type should be automatically detected as DDA.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/lfq-annotatefiles.png)


### Load the LFQ-MBR workflow

Still on the ‘Workflow’ tab, select the LFQ-MBR workflow from the dropdown menu, then click ‘Load’.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/lfq-workflow.png)

This sets all the analysis steps for a closed database search with MSFragger, rescoring with MSBooster and Percolator, protein grouping with ProteinProspector, and filtering with Philosopher, and label-free quantification with FDR-controlled match-between-runs with IonQuant.


### Fetch a sequence database
On the ‘Database’ tab, click ‘Download’, which will prompt you to first set the download options. We will keep the default options (human, reviewed sequences, add common contaminants) for this dataset.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/share-database-options.png)

Clicking 'OK', and then, it will show the dialog for choosing a file location to store the database. Once you’ve chosen a folder, click ‘Select directory’ to start the downloading. When it’s finished, you should see that the `FASTA file path` now points to the new database.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/share-database.png)


### Inspect the search and quantification settings
On the ‘MSFragger’ tab, you can see the parameters that have been set by loading the workflow.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/share-msfragger.png)

On the ‘Quant (MS1)’ tab, you can see the settings that will be used for label-free quantification. Note that IonQuant will be used and ‘Match between runs (MBR)’ is enabled. The 'MaxLFQ' quantification method is selected by default, and MaxLFQ values will be reported in addition to abundances calculated using the topN method.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/share-lfq.png)


### Set the output location and run
On the ‘Run’ tab, use ‘Browse’ to make a new folder for the output files. Then click the ‘RUN’ button to start the analysis.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/share-run.png)


When the run is finished, ‘DONE’ will be printed at the end of the text in the console.


### Inspect the results
In the output location, you will find combined reports (including the ‘MSstats.csv’ table, compatible with MSstats) as well as folders for each sample.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/lfq-results1.png)


A guide to output files, with descriptions of each column in the reports, can be found [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe_outputs.html).


## A more comprenehsived tutorial from the US HUPO 2023 short course
The tutorial file can be found from [here](https://docs.google.com/document/d/1Y3irUF1cPImOWdjvcQo1wZ5Pdqd4jg8bo_PSDOG173k/edit?usp=sharing)



## Key References
Yu, F., Haynes, S. E., Teo, G. C., Avtonomov, D. M., Polasky, D. A., & Nesvizhskii, A. I. (2020). [Fast quantitative analysis of timsTOF PASEF data with MSFragger and IonQuant](https://doi.org/10.1074/mcp.TIR120.002048). Molecular & Cellular Proteomics, 10(9), 1575-1585.

Yu, F., Haynes, S. E., & Nesvizhskii, A. I. (2021). [IonQuant enables accurate and sensitive label-free quantification with FDR-controlled match-between-runs](https://doi.org/10.1016/j.mcpro.2021.100077). Molecular & Cellular Proteomics, 20, 100077.


<br>
<br>
<br>

#### [Back to FragPipe homepage](https://fragpipe.nesvilab.org/)
