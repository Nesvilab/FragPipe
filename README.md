![MSFragger logo](logo/msfragger-logo.png)

# MSfragger GUI
This is a very basic Java GUI wrapper for [MSFragger](http://www.nature.com/nmeth/journal/v14/n5/full/nmeth.4256.html) - ultrafast proteomic search engine.  
It will help you launch MSFragger for Open and Closed searches and can also run post-processing of results with Peptide/Protein Prophets as well as generate tabular summary results using [Philosopher](https://github.com/prvst/philosopher).

## Download
Download precompiled binaries from the [Releases](https://github.com/chhh/MSFragger-GUI/releases/latest) section of this repository

## Running
`java -jar MSfragger-GUI.jar`  
an alternative on Windows 
`start javaw -jar MSfragger-GUI.jar`  

## To build
Open the source project in NetBeans and do "Clean Build". You will get the jar in ./dist directory and a zip file with the current version in the file name and start scripts bundled.

## Updating build version
The version of the build is stored in 2 separate places:
 - MSFragger-GUI/src/umich/msfragger/Version.java
   - `VERSION` static field
 - MSFragger-GUI/src/umich/msfragger/gui/Bundle.properties
   - `msfragger.gui.version` property
 Both need to be modified to the same value, otherwise you'll get a popup warning at the start of the application.
