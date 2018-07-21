# This repository has moved to [FragPipe](https://github.com/chhh/FragPipe)
Please use [the issue tracker](https://github.com/chhh/FragPipe/issues) in the new repository to ask questions/report 
issues/request new features.
**This repository is in a frozen state and won't be updated.**

<img src="frag-pipe/images/fragpipe-01.png" width="350px">
<img src="logo/msfragger-logo.png" width="350px">

## MSfragger GUI (now [FragPipe](https://github.com/chhh/FragPipe))
![Release](https://img.shields.io/github/release/chhh/FragPipe.svg) ![Downloads](https://img.shields.io/github/downloads/chhh/FragPipe/total.svg)

# FragPipe (previously - MSfragger GUI)
This is a very basic Java GUI wrapper for [MSFragger](http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html) - ultrafast proteomic search engine.  
It will help you launch MSFragger for Open and Closed searches and can also run post-processing of results with Peptide/Protein Prophets as well as generate tabular summary results using [Philosopher](https://github.com/prvst/philosopher).

## Download
Download precompiled binaries from the [Releases](https://github.com/chhh/FragPipe/releases) section of this repository

## Running
- on **Windows** use one of the following:
  - Use the Windows executable (.exe)
  - `start javaw -jar MSfragger-GUI.jar`
  - `java -jar MSfragger-GUI.jar`
- **Linux/MacOS**
  - Run the shell script included in the release zip file
  - `java -jar MSfragger-GUI.jar`


## Referencing the work
Please cite the following paper:  
[Andy Kong, Felipe Leprevost, Dmitry Avtonomov, Dattatreya Mellacheruvu, Alexey Nesvizhskii. "MSFragger: ultrafast and comprehensive peptide identification in mass spectrometry-based proteomics". Nat Meth, May 2017. DOI: 10.1038/nmeth.4256](http://dx.doi.org/10.1038/nmeth.4256)

## To build
Open the source project in NetBeans and do "Clean Build". You will get the jar in ./dist directory and a zip file with the current version in the file name and start scripts bundled.

## Updating build version
The version of the build is stored in 2 separate places:
 - MSFragger-GUI/src/umich/msfragger/Version.java
   - `VERSION` static field
 - MSFragger-GUI/src/umich/msfragger/gui/Bundle.properties
   - `msfragger.gui.version` property
 Both need to be modified to the same value, otherwise you'll get a popup warning at the start of the application.

[![Analytics](https://ga-beacon-nocache.appspot.com/UA-5572974-15/github/chhh/msfragger-gui/landing-page?flat&useReferer)](https://github.com/igrigorik/ga-beacon)
