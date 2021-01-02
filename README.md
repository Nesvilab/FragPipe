![Release](https://img.shields.io/github/release/Nesvilab/FragPipe.svg) ![Downloads](https://img.shields.io/github/downloads/Nesvilab/FragPipe/total.svg)

<div align="center">
<img src="frag-pipe/images/fragpipe-01.png" width="350px"/>
</div>

FragPipe is a Java Graphical User Interface (GUI) for a suite of computational tools enabling comprehensive analysis of mass spectrometry-based proteomics data. It is powered by [MSFragger](https://msfragger.nesvilab.org/) - an ultrafast proteomic search engine suitable for both conventional and "open" (wide precursor mass tolerance) peptide identification. FragPipe includes the [Philosopher](https://nesvilab.github.io/philosopher/) toolkit for downstream post-processing of MSFragger search results (PeptideProphet, iProphet, ProteinProphet), FDR filtering, label-based quantification, and multi-experiment summary report generation. [Crystal-C](https://www.nesvilab.org/Crystal-C/) and [PTM-Shepherd](https://github.com/Nesvilab/PTM-Shepherd) are included to aid interpretation of open search results. Also included in FragPipe binary are [TMT-Integrator](http://tmt-integrator.nesvilab.org/) for TMT/iTRAQ isobaric labeling-based quantification, [IonQuant](http://ionquant.nesvilab.org/) for label-free quantification with match-between-run (MBR) functionality, SpectraST and EasyPQP spectral library building modules, and DIA-Umpire SE module for direct analysis of data independent acquisition (DIA) data. 


### [Download](https://github.com/Nesvilab/FragPipe/releases)

### Tutorials
- [FragPipe setup](https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html)
- [Using FragPipe](https://msfragger.nesvilab.org/tutorial_fragpipe.html) (most comprehensive tutorial covering all FragPipe modules)
- [Using FragPipe for SILAC (or other chemical) labelled data](https://msfragger.nesvilab.org/tutorial_silac.html)
- [FragPipe workflows](https://msfragger.nesvilab.org/tutorial_fragpipe_workflows.html)
- [Analyzing glycoproteomics data](https://msfragger.nesvilab.org/tutorial_glyco-fragger.html)
- [Converting LC/MS data files to mzML](https://msfragger.nesvilab.org/tutorial_convert.html)
- [Running MSstats on TIMS-TOF data](https://msfragger.nesvilab.org/tutorial_msstats.html)
- [Importing results from TIMS-TOF data to Skyline](https://msfragger.nesvilab.org/tutorial_pasef_skyline.html)

### Documentation
Complete MSFragger documentation can be found on the [MSFragger wiki](https://github.com/Nesvilab/MSFragger/wiki).
For documentation on the Philosopher toolkit see the [Philosopher wiki](https://github.com/Nesvilab/philosopher/wiki).

### Questions and Technical Support
See the MSFragger [wiki](https://github.com/Nesvilab/MSFragger/wiki) and [FAQ](https://github.com/Nesvilab/MSFragger/wiki/Frequently-Asked-Questions). View previous questions/bug reports in the
[FragPipe issue tracker](https://github.com/Nesvilab/FragPipe/issues). Please post any new questions/bug reports regarding FragPipe itself here as well.
For questions specific to individual components of FragPipe you can also
use [MSFragger issue tracker](https://github.com/Nesvilab/MSFragger/issues),
[Philosopher issue tracker](https://github.com/Nesvilab/philosopher/issues),
[IonQuant issue tracker](https://github.com/Nesvilab/IonQuant/issues).


For other tools developed by Nesvizhskii lab, visit our website 
[nesvilab.org](http://www.nesvilab.org)

### How to Run
- **Windows**:
  - Run the Windows executable (*.exe*) from the "bin" folder
  - Or start the `FragPipe.bat` from the *.zip* distribution  
  or execute one of the following commands:
  - `start javaw -jar FragPipe-x.x.jar`
  - `java -jar FragPipe-x.x.jar`
- **Linux**:
  - Either run the `FragPipe` shell script from *.zip* distribution  
  or execute the following command:
  - Or execute `java -jar FragPipe-x.x.jar`

### Cite
- Kong, A. T., Leprevost, F. V., Avtonomov, D. M., Mellacheruvu, D., & Nesvizhskii, A. I. (2017). MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry–based proteomics. Nature Methods, 14(5), 513-520.
- Yu, F., Teo, G. C., Kong, A. T., Haynes, S. E., Avtonomov, D. M., Geiszler, D. J., & Nesvizhskii, A. I. (2020). Identification of modified peptides using localization-aware open search. Nature Communications, 11(1), 1-9.
- Polasky, D. A., Yu, F., Teo, G. C., & Nesvizhskii, A. I. (2020). Fast and Comprehensive N-and O-glycoproteomics analysis with MSFragger-Glyco. Nature Methods, 17, 1125-1132.
- Chang, H. Y., Kong, A. T., da Veiga Leprevost, F., Avtonomov, D. M., Haynes, S. E., & Nesvizhskii, A. I. (2020). Crystal-C: A computational tool for refinement of open search results. Journal of Proteome Research, 19(6), 2511-2515.
- Geiszler, D. J., Kong, A. T., Avtonomov, D. M., Yu, F., da Veiga Leprevost, F., & Nesvizhskii, A. I. (2020). PTM-Shepherd: analysis and summarization of post-translational and chemical modifications from open search results. Molecular & Cellular Proteomics.
- da Veiga Leprevost, F., Haynes, S. E., Avtonomov, D. M., Chang, H. Y., Shanmugam, A. K., Mellacheruvu, D., Kong, A. T., & Nesvizhskii, A. I. (2020). Philosopher: a versatile toolkit for shotgun proteomics data analysis. Nature Methods, 17(9), 869-870.
- Yu, F., Haynes, S. E., Teo, G. C., Avtonomov, D. M., Polasky, D. A., & Nesvizhskii, A. I. (2020). Fast quantitative analysis of timsTOF PASEF data with MSFragger and IonQuant. Molecular & Cellular Proteomics.
- Tsou, C. C., Avtonomov, D., Larsen, B., Tucholska, M., Choi, H., Gingras, A. C., & Nesvizhskii, A. I. (2015). DIA-Umpire: comprehensive computational framework for data-independent acquisition proteomics. Nature methods, 12(3), 258-264.

### Building from scratch

1. Update build version:  
The version of the build is stored in 2 separate places:  
    - File: `MSFragger-GUI/src/umich/msfragger/gui/Bundle.properties`  
      Property: `msfragger.gui.version`
    - File: `MSFragger-GUI/build.gradle`  
      Property: `version`
2. Build:  
You don't need to have Gradle installed. Gradle wrapper included in this repository will be used. From the root directory of the repository issue the following commands:

    ```bash
    cd ./MSFragger-GUI
    ./gradlew prepareReleaseNoExe
    ```
3. Inspect the output in `MSFragger-GUI/build/github-release` directory.
4. If you want *.exe* file for Windows, then you have to build on Windows with [Launch4j](http://launch4j.sourceforge.net/) installed.
    ```bash
    ./gradlew prepareReleaseWithExe
    ```
