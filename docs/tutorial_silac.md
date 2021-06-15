## Analyzing SILAC samples (or other MS1-labeled samples) with FragPipe

[MSFragger](https://msfragger.nesvilab.org/) can be used to identify labeled peptides, with [IonQuant](https://ionquant.nesvilab.org/) (inside FragPipe) to perform MS1 quantification of labeled samples. Below are step-by-step instructions to perform SILAC or other labeling-based MS1 quantification (such as dimethyl). See other FragPipe tutorials listed [here](https://fragpipe.nesvilab.org/).


#### 1) Load the `Default` workflow
Descriptions of built-in workflows can be found [here](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe_workflows.html)

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/silac_1.jpg)


#### 2) Specify or download a FASTA file
![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/silac_2.jpg)


#### 3) Select the appropriate variable modifications
This example specifies the medium and heavy SILAC labels (K4R6/K8R10) as variable modifications.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/silac_3.jpg)

If you are using another kind of labeling, fill in the corresponding sites and modification masses. For example, to specify light and heavy dimethyl labels, variable modification should be specified as shown below:

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/silac_3_2.jpg)


#### 4) Use IonQuant for MS1 quantification
Check `Run MS1 quant` and load the default settings. Then fill in the labels' masses with the format `<site>mass`, with multiple site-mass pairs separated by `;`. Below is an example using SILAC light, medium, and heavy labeling.

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/silac_4.jpg)

If you are using light and heavy dimethyl labeling, change these fields to:

![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/silac_4_2.jpg)

#### 5) Specify output location and run
![](https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/silac_5.jpg)


### Output
For each experiment, the results folder will contain multiple files, including `psm.tsv`, `ion.tsv`, `peptide.tsv`, and `protein.tsv` reports. Entries in these files have been filtered with the user-defined FDR threshold, with the SILAC (or similar) modifications shown as variable modifications. For SILAC (and similar) samples, these reports do not provide ratio abundances (e.g. heavy/light) at each (ion/peptide/protein) level. Instead, as for label-free data, each PSM/ion/peptide is considered independently, and the protein file shows combined protein intensity based on all peptides, regardless of the label/sample status.   

Thus, for SILAC (and related) data, IonQuant generates two additional files exclusively for MS1-labeled data: `ion_label_quant.tsv` and `peptide_label_quant.tsv`. Entries in these two files correspond to those in 'ion.tsv' and 'peptide.tsv' files, respectively, but are presented in a more useful ratio (e.g. medium/light, heavy/light, etc.) format. Currently, IonQuant does not generate a similar protein-level file (we are working to add this in a future release of IonQuant).

