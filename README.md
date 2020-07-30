![Release](https://img.shields.io/github/release/Nesvilab/FragPipe.svg) ![Downloads](https://img.shields.io/github/downloads/Nesvilab/FragPipe/total.svg)

<div align="center">
<img src="frag-pipe/images/fragpipe-01.png" width="350px"/>
</div>

## FragPipe
FragPipe is a Java Graphical User Interface (GUI) for a suite of computational tools enabling comprehensive analysis of mass spectrometry-based proteomics data. It is powered by [MSFragger](https://msfragger.nesvilab.org/) - an ultrafast proteomic search engine suitable for both conventional and "open" (wide precursor mass tolerance) peptide identification. FragPipe includes the [Philosopher](https://nesvilab.github.io/philosopher/) toolkit for downstream post-processing of MSFragger search results (PeptideProphet, iProphet, ProteinProphet), FDR filtering, label-based quantification, and multi-experiment summary report generation. [Crystal-C](https://www.nesvilab.org/Crystal-C/) and [PTM-Shepherd](https://github.com/Nesvilab/PTM-Shepherd) are included to aid interpretation of open search results. Also included in FragPipe binary are [TMT-Integrator](http://tmt-integrator.nesvilab.org/) for TMT/iTRAQ isobaric labeling-based quantification, [IonQuant](http://ionquant.nesvilab.org/) for label-free quantification with match-between-run (MBR) functionality, SpectraST and EasyPQP spectral library building modules, and DIA-Umpire SE module for direct analysis of data independent acquisition (DIA) data. 


### [Download](https://github.com/Nesvilab/FragPipe/releases)

### Tutorials
- [FragPipe setup](https://msfragger.nesvilab.org/tutorial_setup_fragpipe.html)
- [Using FragPipe](https://msfragger.nesvilab.org/tutorial_fragpipe.html). This is the most comprehensive tutorial covering all FragPipe modules.
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
[Andy Kong, Felipe Leprevost, Dmitry Avtonomov, Dattatreya Mellacheruvu, Alexey Nesvizhskii. "MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry-based proteomics". Nat Meth, May 2017. DOI: 10.1038/nmeth.4256](http://dx.doi.org/10.1038/nmeth.4256)

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

[![Analytics](https://ga-beacon-nocache.appspot.com/UA-5572974-15/github/chhh/msfragger-gui/landing-page?flat&useReferer)](https://github.com/igrigorik/ga-beacon)
