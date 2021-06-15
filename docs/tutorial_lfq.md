### Label-free quantification with FragPipe


##### Tutorial contents
* [Open FragPipe](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#open-fragpipe)
* [Load the data](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#load-the-data)
* [Load the LFQ-MBR workflow](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#load-the-lfq-mbr-workflow)
* [Fetch a sequence database](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#fetch-a-sequence-database)
* [Inspect the search and quantification settings](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#inspect-the-search-and-quantification-settings)
* [Set the output location and run](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#set-the-output-location-and-run)
* [Inspect the results](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html#inspect-the-results)


<br>

We will use a dataset (PRIDE/ProteomeXchange identifier PXD020556) in which HCT116 cells were treated with aspirin (acetylsalicylic acid) to investigate metabolic changes. Extracts from willows and other plants rich in acetylsalicylic acid have been used medicinally since Mesopotamian times. This tutorial will use a subset of the data to quantify the proteomes of aspirin treated and untreated (control) cells. These data were acquired with a Q Exactive HF-X.

Associated publication: Castoldi, Francesca, et al. "Autophagy-mediated metabolic effects of aspirin." Cell death discovery 6.1 (2020): 1-17.

To get the input data, download the 'lfq-raw.zip' file from [Dropbox](https://www.dropbox.com/sh/azdbblag7whv9v7/AACk8t4-6LowMYobKyXxUMS8a?dl=0) and extract the files.


### Open FragPipe
When you launch FragPipe, check that MSFragger and Philosopher are configured. (If you haven’t downloaded them yet, use their respective ‘Download / Update’ buttons. See [this page](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html) for more help, Python is not needed for these exercises.)

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-config.PNG)

<br>


### Load the data
On the ‘Workflow’ tab, drag and drop the six .raw spectral files or use the ‘Add files’ button to browse for them. We are using a subset of the full dataset with annotations shown below (the full list of file annotations can be found in the ‘Design.xls’ file).

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-rawfiles.png)

Once you’ve added the files, you can annotate them by editing the ‘Experiment’ and ‘Replicate’ fields manually or in batches with the ‘Set experiment/replicate’ button (shown below).

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-annotatefiles.png)


<br>

### Load the LFQ-MBR workflow

Still on the ‘Workflow’ tab, select the LFQ-MBR workflow from the dropdown menu, then click ‘Load’.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-workflow.png)

This sets all the analysis steps for a closed database search with MSFragger, validation and filtering with Philosopher (using PeptideProphet and ProteinProphet), and label-free quantification with FDR-controlled match-between-runs with IonQuant.

<br>

### Fetch a sequence database
On the ‘Database’ tab, click ‘Download’, which will prompt you to first choose a file location to store the database. (Alternatively, you can choose to use the database from [Dropbox](https://www.dropbox.com/s/v8tlkwu96f3txfj/2021-05-07-decoys-reviewed-contam-UP000005640.fas?dl=0).

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-database.png)

Once you’ve chosen a folder, click ‘Select directory’ to proceed to the database options. We will keep the default options (human, reviewed sequences, add common contaminants) for this dataset.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-databaseoptions.png)

Click ‘Yes’ to download the database. When it’s finished, you should see that the `FASTA file path` now points to the new database.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-databasepath.png)

<br>

### Inspect the search and quantification settings
On the ‘MSFragger’ tab, you can see the parameters that have been set by loading the workflow.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-search.png)

To save time in the search (at the expense of slightly lower sensitivity), you can optionally set ‘Calibration and Optimization’ to ‘None’ in the ‘Peak Matching’ section. We don’t need to change any other search settings for this analysis, but you could optionally add acetyl lysine as a variable modification by editing the allowed sites for +42.0106 (already included on protein N-term in the workflow) since one aim of the study was to examine changes in acetylation.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-searchvarmod.png)

On the ‘Quant (MS1)’ tab, you can see the settings that will be used for label-free quantification. Note that IonQuant will be used and ‘Match between runs (MBR)’ is enabled.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-quant.png)


<br>

### Set the output location and run
On the ‘Run’ tab, use ‘Browse’ to make a new folder for the output files (e.g. ‘my-lfq-results’). Then click the ‘RUN’ button to start the analysis.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-run.png)


When the run is finished, ‘DONE’ will be printed at the end of the text in the console.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/tmt-2plexes-done.png)

<br>

### Inspect the results
In the output location (‘my-lfq-results’ folder), you will find combined reports (including the ‘MSstats.csv’ table, compatible with MSstats) as well as folders for each sample.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/lfq-results1.png)

Inside each individual folder, a separate set of reports is created for just that sample.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/sample_results2.PNG)

A guide to output files, with descriptions of each column in the reports, can be found [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe_outputs.html).

<br>
<br>
<br>
<br>

#### [Back to FragPipe homepage](https://fragpipe.nesvilab.org/)
