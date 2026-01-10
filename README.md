![Release](https://img.shields.io/github/release/Nesvilab/FragPipe.svg) ![Downloads](https://img.shields.io/github/downloads/Nesvilab/FragPipe/total.svg) ![Docker pulls](https://img.shields.io/docker/pulls/fcyucn/fragpipe) ![Downloads](https://img.shields.io/github/downloads/Nesvilab/FragPipe/latest/total.svg)

<div align="center">
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/develop/images/fragpipe-01.png" width="350px"/>
</div>

FragPipe is a comprehensive computational platform designed for the analysis of mass spectrometry-based proteomics data. It includes a Graphical User Interface and pipeline wrapper code (FragPipe-GUI), distributed alongside various independent software tools and workflow files. FragPipe can be run using GUI or in the command line mode, on Windows, Linux, or in the cloud environment. It is powered by [MSFragger](https://msfragger.nesvilab.org/) - an ultrafast proteomic search engine suitable for both conventional and "open" (wide precursor mass tolerance) peptide identification. FragPipe includes Percolator and the [Philosopher](https://nesvilab.github.io/philosopher/) toolkit for downstream post-processing of MSFragger search results (PeptideProphet, iProphet, ProteinProphet), FDR filtering, and multi-experiment summary report generation. FragPipe includes MSBooster module for deep-learning based rescoring of peptide identifications. [Crystal-C](https://www.nesvilab.org/Crystal-C/) and [PTM-Shepherd](https://github.com/Nesvilab/PTM-Shepherd) are included to aid interpretation of results from "open" and "mass offset" searches for post-translational modifications (PTM). FragPipe supports all modes of quantification, including MS1-based isotope labeling (e.g. SILAC) or MS1-based label-free quantification with FDR-controlled match-between-run (LFQ-MBR) using IonQuant [IonQuant](http://ionquant.nesvilab.org/), TMT/iTRAQ isobaric labeling-based quantification using IonQuant and [TMT-Integrator](http://tmt-integrator.nesvilab.org/). FragPipe provides a comprehensive list of workflows for the analysis of data independent acquisition (DIA) data with MSFragger-DIA, DIA-Umpire/MSFragger, and diaTracer/MSFragger workflows for direct ("library-free") peptide identification from DIA files, spectral library building with EasyPQP, and extraction of quantification using DIA-NN and Skyline.     


### [Download](https://github.com/Nesvilab/FragPipe/releases)
#### [Docker image](https://hub.docker.com/r/fcyucn/fragpipe)

#### FragPipe tutorials
* [Using FragPipe](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html) (general tutorial covering all FragPipe modules)
* [Running FragPipe in command line interface](https://fragpipe.nesvilab.org/docs/tutorial_headless.html)
* [Running multiple FragPipe jobs in Batch](https://fragpipe.nesvilab.org/docs/tutorial_batch.html)
* [Pulling and running FragPipe using Docker or Singularity](https://fragpipe.nesvilab.org/docs/tutorial_docker.html)
* PTM discovery
  * [Open search](https://fragpipe.nesvilab.org/docs/tutorial_open.html)
  * [Mass offset search](https://fragpipe.nesvilab.org/docs/tutorial_offset.html)
  * [Labile PTM search](https://fragpipe.nesvilab.org/docs/tutorial_labile.html)
  * [Glycoproteomics search](https://fragpipe.nesvilab.org/docs/tutorial_glyco.html)
  * [Custom mass offset workflow (RNA crosslinking example)](https://fragpipe.nesvilab.org/docs/tutorial_custom_mass_offset.html) 
  * [Diagnostic ion mining](https://fragpipe.nesvilab.org/docs/tutorial_diagnostic_mining.html)
  * [FPOP](https://fragpipe.nesvilab.org/docs/tutorial_fpop.html)
* TMT/iTRAQ quantification
  * [Single plex](https://fragpipe.nesvilab.org/docs/tutorial_tmt.html)
  * [Multiple plexes with a pooled reference sample](https://fragpipe.nesvilab.org/docs/tutorial_tmt-2plexes.html)
  * [Streamlined activity-based protein profiling of reactive cysteines (SLC-ABPP)](https://fragpipe.nesvilab.org/docs/tutorial_abpp.html)
* [Label-free quantification](https://fragpipe.nesvilab.org/docs/tutorial_lfq.html)
* [SILAC (or other MS1-labeled) data](https://fragpipe.nesvilab.org/docs/tutorial_silac.html)
* [DIA analysis](https://fragpipe.nesvilab.org/docs/tutorial_DIA.html)
* [Novel/variant peptide detection using two-pass search](https://fragpipe.nesvilab.org/docs/tutorial_two_pass_search.html)
* [Group FDR estimation for novel/variant peptide analysis](https://fragpipe.nesvilab.org/docs/tutorial_group_fdr.html)
* [Use peptide prediction models from Koina for MSBooster feature generation](https://fragpipe.nesvilab.org/docs/tutorial_koina.html)

#### Resources
* [Interpreting output files](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe_outputs.html)
* [List of built-in workflows](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe_workflows.html)
* [FragPipe setup](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html)
* [Converting LC/MS data files to mzML](https://fragpipe.nesvilab.org/docs/tutorial_convert.html)
* [Setting up FragPipe on remote Linux server (with X forwarding)](https://fragpipe.nesvilab.org/docs/tutorial_setup_x_forwarding.html)

#### Using FragPipe with other tools
* [Running MSstats with IonQuant results](https://fragpipe.nesvilab.org/docs/tutorial_msstats.html)
* [Importing results into Skyline](https://fragpipe.nesvilab.org/docs/tutorial_skyline.html)
* [Importing results into Perseus](https://fragpipe.nesvilab.org/docs/tutorial_perseus.html)

#### Representative papers using FragPipe
* [Chemoproteomics](https://fragpipe.nesvilab.org/docs/chemoproteomics_reference.html)



#### Supported instruments and file formats  
The table below shows the compatibility of FragPipe workflow components with different spectral file formats.

_Bruker .d indicates ddaPASEF files from timsTOF, other Bruker .d files should be converted to .mzML. Please also note that timsTOF data requires [Visual C++ Redistributable for Visual Studio 2017](https://aka.ms/vs/16/release/VC_redist.x64.exe) in Windows. If you see an error saying cannot find Bruker native library, please try to install the Visual C++ redistibutable._

| Workflow Step                      | .mzML | Thermo (.raw) | Bruker (.d) |  .mgf |
|------------------------------------|:-----:|:-------------:|:-----------:|:-----:|
| DIA-Umpire pseudo-MS/MS generation | ✔     | ✔             |             |       | 
| diaTracer pseudo-MS/MS generation  |       |               | ✔           |       | 
| MSFragger search                   | ✔     | ✔             | ✔           | ✔     | 
| MSFragger-DIA                      | ✔     | ✔             |             |       | 
| Crystal-C artifact removal         | ✔     | ✔             |             |       | 
| PTMProphet localization            | ✔     | ✔             | ✔           |       | 
| PTM-Shepherd summarization         | ✔     | ✔             | ✔           |       | 
| Label-free quantification          | ✔     | ✔             | ✔           |       | 
| SILAC/dimethyl quantification      | ✔     | ✔             | ✔           |       | 
| TMT/iTRAQ quantification           | ✔     | ✔             |             |       | 
| Spectral library generation        | ✔     | ✔             | ✔           | ✔     | 
| DIA-NN quantification              | ✔     | ✔*            | ✔           |       | 

_DIA data acquired with overlapping/staggered windows must be [converted to mzML with demultiplexing](https://fragpipe.nesvilab.org/docs/tutorial_convert.html#convert-thermo-dia-raw-files-with-overlappingstaggered-windows)._
_Quantification from Thermo .raw files with DIA-NN requires installation of Thermo MS File Reader, see the [DIA-NN documentation](https://github.com/vdemichev/DiaNN#raw-data-formats) for details._


#### Additional Documentation
- [DIA-Umpire](https://diaumpire.nesvilab.org/)
- [diaTracer](https://diatracer.nesvilab.org/)
- [MSFragger wiki](https://github.com/Nesvilab/MSFragger/wiki)
- [Crystal-C](https://www.nesvilab.org/Crystal-C/)
- [MSBooster](https://github.com/Nesvilab/MSBooster)
- [Philosopher wiki](https://github.com/Nesvilab/philosopher/wiki)
- [PTM-Shepherd](https://ptmshepherd.nesvilab.org/)
- [IonQuant](http://ionquant.nesvilab.org/)
- [TMT-Integrator](http://tmt-integrator.nesvilab.org/)

#### Questions and Technical Support
View previous questions/bug reports in the
[FragPipe issue tracker](https://github.com/Nesvilab/FragPipe/issues).

For other tools developed by Nesvizhskii lab, visit our website 
[nesvilab.org](http://www.nesvilab.org)

#### How to Run
- **Windows**:
  - Install FragPipe by double-clicking the `FragPipe-x.x-Installer.exe`
  - Double click the `FragPipe-x.x.exe` at the Desktop
- **Linux**:
  - Run the `fragpipe` shell script (can double-click to run)  
- **Mac OS** (command line interface only):
  - Read [Pulling and running FragPipe using Docker](https://fragpipe.nesvilab.org/docs/tutorial_docker.html)

 
#### Integration
FragPipe is open source and the output is currently supported by the following software projects:
- [Skyline](https://skyline.ms/project/home/software/Skyline/begin.view)
- [AlphaPeptDeep](https://github.com/MannLabs/alphapeptdeep)
- [AlphaPeptStats](https://github.com/MannLabs/alphapeptstats)
- [AlphaMap](https://github.com/MannLabs/alphamap)
- [directLFQ](https://github.com/MannLabs/directlfq)
- [DIA-NN](https://github.com/vdemichev/DiaNN)
- [MSstats](http://msstats.org/)
- [picked_group_fdr](https://github.com/kusterlab/picked_group_fdr)
- [FragPipe-Analyst](http://fragpipe-analyst.nesvilab.org/)



#### Key references
##### Database search
- Kong, A. T., Leprevost, F. V., Avtonomov, D. M., Mellacheruvu, D., & Nesvizhskii, A. I. (2017). MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry–based proteomics. Nature Methods, 14(5), 513-520.
- Yu, F., Teo, G. C., Kong, A. T., Haynes, S. E., Avtonomov, D. M., Geiszler, D. J., & Nesvizhskii, A. I. (2020). Identification of modified peptides using localization-aware open search. Nature Communications, 11, 4065.
- Yu, F., Haynes, S. E., Teo, G. C., Avtonomov, D. M., Polasky, D. A., & Nesvizhskii, A. I. (2020). Fast quantitative analysis of timsTOF PASEF data with MSFragger and IonQuant. Molecular & Cellular Proteomics, 10(9), 1575-1585.
- Teo, G. C., Polasky, D. A., Yu, F., Nesvizhskii, A. I. (2020). A fast deisotoping algorithm and its implementation in the MSFragger search engine. Journal of Proteome Research, 20(1), 498-505.


##### Chimeric spectra search
- Yu, F., Deng, Y. & Nesvizhskii, A.I. (2025). MSFragger-DDA+ enhances peptide identification sensitivity with full isolation window search. Nature Communications, 16, 3329.


##### Glyco/Labile search
- Polasky, D. A., Yu, F., Teo, G. C., & Nesvizhskii, A. I. (2020). Fast and Comprehensive N-and O-glycoproteomics analysis with MSFragger-Glyco. Nature Methods, 17, 1125-1132.
- Polasky, D. A., Geiszler, D. J., Yu, F., & Nesvizhskii, A. I. (2022). Multiattribute Glycan Identification and FDR Control for Glycoproteomics. Molecular & Cellular Proteomics, 21(3), 100205.
- Polasky, D. A., Geiszler, D. J., Yu, F., Kai, Li., Teo, G. C., & Nesvizhskii, A. I. (2023). MSFragger-Labile: A Flexible Method to Improve Labile PTM Analysis in Proteomics. Molecular & Cellular Proteomics, 22(5), 100538.
- Polasky, D. A., Lu, L., Yu, F., Li, K., Shortreed, M. R., Smith, L. M., & Nesvizhskii, A. I. (2025). Quantitative proteome-wide O-glycoproteomics analysis with FragPipe. Analytical and Bioanalytical Chemistry, 417(5), 921-930.


##### PTM
- Chang, H. Y., Kong, A. T., da Veiga Leprevost, F., Avtonomov, D. M., Haynes, S. E., & Nesvizhskii, A. I. (2020). Crystal-C: A computational tool for refinement of open search results. Journal of Proteome Research, 19(6), 2511-2515.
- Geiszler, D. J., Kong, A. T., Avtonomov, D. M., Yu, F., da Veiga Leprevost, F., & Nesvizhskii, A. I. (2020). PTM-Shepherd: analysis and summarization of post-translational and chemical modifications from open search results. Molecular & Cellular Proteomics, 20, 100018.
- Geiszler, D. J., Polasky, D. A., Yu, F., & Nesvizhskii, A. I. (2023). Detecting diagnostic features in MS/MS spectra of post-translationally modified peptides. Nature Communications, 14, 4132.


##### DIA
- Tsou, C. C., Avtonomov, D., Larsen, B., Tucholska, M., Choi, H., Gingras, A. C., & Nesvizhskii, A. I. (2015). DIA-Umpire: comprehensive computational framework for data-independent acquisition proteomics. Nature methods, 12(3), 258-264.
- Yu, F, Teo, G. C., Kong, A. T., Fröhlich, K., Li, G. X. , Demichev, V, Nesvizhskii, A..I. (2023). Analysis of DIA proteomics data using MSFragger-DIA and FragPipe computational platform, Nature Communications 14, 4154.


##### DIA-PASEF
- Li, K., Teo, G. C., Yang, K. L., Yu, F., & Nesvizhskii, A. I. (2025). diaTracer enables spectrum-centric analysis of diaPASEF proteomics data. Nature Communications, 16, 95.


##### DDA quantification
- Yu, F., Haynes, S. E., & Nesvizhskii, A. I. (2021). IonQuant enables accurate and sensitive label-free quantification with FDR-controlled match-between-runs. Molecular & Cellular Proteomics, 20, 100077.


##### Miscellaneous

- da Veiga Leprevost, F., Haynes, S. E., Avtonomov, D. M., Chang, H. Y., Shanmugam, A. K., Mellacheruvu, D., Kong, A. T., & Nesvizhskii, A. I. (2020). Philosopher: a versatile toolkit for shotgun proteomics data analysis. Nature Methods, 17(9), 869-870.
- Yang, K. L., Yu, F., Teo, G. C., Kai, L., Demichev, V., Ralser, M., & Nesvizhskii, A. I. (2023). MSBooster: improving peptide identification rates using deep learning-based features. Nature Communications, 14, 4539.



#### Building from scratch

You don't need to have Gradle installed, the Gradle wrapper included in this repository will be used. From the root directory of the repository issue the following commands:

```bash
cd FragPipe-GUI
./gradlew makeReleaseInstaller # for Windows
./gradlew makeReleaseZipLinux # for Linux
```
    
The `FragPipe-x.x-Installer.exe` will be in the `FragPipe-GUI/build/installer` directory.
The `FragPipe-x.x-linux.zip` will be in the `FragPipe-GUI/build/github-release` directory.
