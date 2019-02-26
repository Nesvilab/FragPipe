![Release](https://img.shields.io/github/release/Nesvilab/FragPipe.svg) ![Downloads](https://img.shields.io/github/downloads/Nesvilab/FragPipe/total.svg)

<div align="center">
<img src="frag-pipe/images/fragpipe-01.png" width="350px"/><br/>
<img src="logo/msfragger-logo.png" width="350px"/>
</div>

# FragPipe
FragPipe is a Java Graphical User Interface (GUI) for a suite of computational tools enabling comprehensive analysis of mass spectrometry-based proteomics data. It is powered by [MSFragger](https://msfragger.nesvilab.org/) - an ultrafast proteomic search engine suitable for both conventional and "open" (wide precursor mass tolerance) peptide identification. FragPipe also includes [Philosopher](https://nesvilab.github.io/philosopher/) toolkit for downstream post-processing of MSFragger search results (PeptideProphet, iProphet, ProteinProphet), FDR filtering, label-free quantification, and multi-experiment summary report generation. Also included in FragPipe binary are SpectraST-based spectral library building module, and DIA-Umpire SE module for direct analysis of data independent acquisition (DIA) data. 

## Download
Download precompiled binaries from the [Releases](https://github.com/Nesvilab/FragPipe/releases/) section of this repository.

MSFragger binary needs to be downloaded separately, the user interface has a link to the download location.
You can find detailed instructions on the [MSFragger website](https://msfragger.nesvilab.org/).

## Documentation
We are currently working on creating Tutorials for most commonly used workflows supported in FragPipe.

For MSFragger documentation (search parameters etc.) see [MSFragger Documentation Wiki page](https://github.com/Nesvilab/MSFragger/wiki).  
For documentation on Philosopher toolkit see [Philosopher site](http://philosopher.nesvilab.org/).

## Questions and Technical Support
Please post all questions/bug reports regarding FragPipe itself in the
[FragPipe issue tracker](https://github.com/Nesvilab/FragPipe/issues).  
For questions specific to individual components of FragPipe you can also
use [MSFragger issue tracker](https://github.com/Nesvilab/MSFragger/issues)
and [Philosopher issue tracker](https://github.com/Nesvilab/philosopher/issues).

For other tools developed by Nesvizhskii lab, visit our website 
[nesvilab.org](http://www.nesvilab.org)

## Running
- **Windows**:
  - Run the Windows executable (*.exe*)
  - Or start the `FragPipe.bat` from the *.zip* distribution  
  or execute one of the following commands:
  - `start javaw -jar FragPipe-x.x.jar`
  - `java -jar FragPipe-x.x.jar`
- **Linux/Mac**:
  - Either run the `FragPipe` shell script from *.zip* distribution  
  or execute the following command:
  - Or execute `java -jar FragPipe-x.x.jar`

## Citing the work
Please refer to the following paper:  
[Andy Kong, Felipe Leprevost, Dmitry Avtonomov, Dattatreya Mellacheruvu, Alexey Nesvizhskii. "MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry-based proteomics". Nat Meth, May 2017. DOI: 10.1038/nmeth.4256](http://dx.doi.org/10.1038/nmeth.4256)

## Building from scratch

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
    ./gradlew prepareRelease
    ```
3. Inspect the output in `MSFragger-GUI/build/github-release` directory.

[![Analytics](https://ga-beacon-nocache.appspot.com/UA-5572974-15/github/chhh/msfragger-gui/landing-page?flat&useReferer)](https://github.com/igrigorik/ga-beacon)
