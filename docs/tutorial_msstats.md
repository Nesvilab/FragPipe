## Running MSstats using MSstats.csv from IonQuant

IonQuant can generate a [MSstats](https://bioconductor.org/packages/release/bioc/html/MSstats.html) compatible file `MSstats.csv`.  
Given an experimental setup that looks like this in the 'Select LC/MS Files' tab of FragPipe:

<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_LCMS_msstats.png" width="450"/>

The `MSstats.csv` file output from IonQuant (via FragPipe) will look something like this:

<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/fragpipe_table_to_msstats.png" width="700"/>

<!---
| ProteinName | PeptideSequence             | PrecursorCharge | FragmentIon | ProductCharge | IsotopeLabelType | Condition | BioReplicate | Run                                                                        | Intensity |
|-------------|-----------------------------|-----------------|-------------|---------------|------------------|-----------|--------------|----------------------------------------------------------------------------|-----------|
| A0A024RBG1  | SEQEDEVLLVSSSR              | 2               | NA          | NA            | L                | 1         | 1            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_1_A1_01_2767 | 251.38202 |
| A0A024RBG1  | SEQEDEVLLVSSSR              | 2               | NA          | NA            | L                | 1         | 2            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_2_A1_01_2768 | 198.37898 |
| A0A024RBG1  | SEQEDEVLLVSSSR              | 2               | NA          | NA            | L                | 1         | 3            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_3_A1_01_2769 | 212.49734 |
| A0A024RBG1  | SEQEDEVLLVSSSR              | 2               | NA          | NA            | L                | 1         | 4            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_4_A1_01_2770 | 279.63782 |
| A0A024RBG1  | YPDQWIVPGGGMEPEEEPGGAAVR    | 2               | NA          | NA            | L                | 1         | 1            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_1_A1_01_2767 | 136.62593 |
| A0A024RBG1  | YPDQWIVPGGGMEPEEEPGGAAVR    | 2               | NA          | NA            | L                | 1         | 2            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_2_A1_01_2768 | 123.56099 |
| A0A024RBG1  | YPDQWIVPGGGMEPEEEPGGAAVR    | 2               | NA          | NA            | L                | 1         | 3            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_3_A1_01_2769 | 116.3815  |
| A0A024RBG1  | YPDQWIVPGGGMEPEEEPGGAAVR    | 2               | NA          | NA            | L                | 1         | 4            | 20180819_TIMS2_12-2_AnBr_SA_200ng_HeLa_50cm_120min_100ms_11CT_4_A1_01_2770 | 114.43072 |
-->

This `MSstats.csv` file can be read by MSstats without any conversion. The R command for installing `MSstats` is:

```shell
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("MSstats")
```

Below is an example R script that reads and analyzes `MSstats.csv` using `MSstats` (make sure to set `rootDir` to the directory containing your MSstats.csv file):

```R
rm(list = ls())
library(stringr)
library(readr)
library(MSstats)

rootDir <- "folder_with_MSstats.csv" # Specify the path of the directory containing MSstats.csv.
print(str_c("Using IonQuant's result from ", rootDir))

# Read MSstats.csv file.
raw <- read_csv(str_c(rootDir, "MSstats.csv"), na = c("", "NA", "0"))
raw$ProteinName <- factor(raw$ProteinName)
raw$PeptideSequence <- factor(raw$PeptideSequence)

# Change root directory for MSstats
print(str_c("Root DIR: ", rootDir))
setwd(rootDir)

# Processing the data using MSstats
processedData <- dataProcess(raw, logTrans = 10)

# Downstream analysis...

```

Details of the functions and options can be found [here](https://bioconductor.org/packages/release/bioc/manuals/MSstats/man/MSstats.pdf).

With `processedData`, users can perform various downstream analyses described in the [MSstats user manual](http://msstats.org/wp-content/uploads/2020/02/MSstats_v3.18.1_manual_2020Feb26-v2.pdf).
