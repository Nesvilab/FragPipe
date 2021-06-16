# Converting raw files to mzML

FragPipe and MSFragger accept mzML files, which can be generated from your raw LC/MS data with MSConvert, part of the ProteoWizard software package. ProteoWizard can be downloaded [here](http://www.proteowizard.org/download.html). Once ProteoWizard is installed, launch MSConvert.

This page has instructions for using MSConvert to generate mzML files from both Thermo Orbitrap and Bruker timsTOF data formats, though we recommend using the raw .d format for Bruker data.

<br>

### Convert Thermo .raw files:
In the MSConvert window:
1. Browse for the raw files you want to convert.
2. Add them to the input list.
3. Select an output directory (where you want to mzML files to be generated).
4. Set the output file format to 'mzML', and check the boxes for 'Write index', 'TPP compatibility', and 'Use zlib compression'. **Note:** Do not select 'zlib compression' for use with Proteome Discoverer, which currently does not support this compression function.
5. Select any additional filters you want to apply to the conversion. We recommend using the default filters shown here. "peakPicking" (centroiding) must be the first filter.
6. Press 'Start' to begin converting your files.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/9.jpg)

<br>

### Convert Bruker timsTOF .d files (NOT RECOMMENDED. MSFragger and IonQuant can load .d directly.):
In the MSConvert window:
1. Browse for the raw files you want to convert.
2. Add them to the input list.
3. Select an output directory (where you want to mzML files to be generated).
4. Set the output file format to 'mzML', and check the box for '**Combine ion mobility scans**'. The boxes for 'Write index', 'TPP compatibility', and 'Use zlib compression' should also be checked.
5. Add '**scanSumming**' and '**threshold**'. To create the scanSumming filter, select 'scanSumming' from the drop down menu and specify 0.05 precursorTol, 5 scanTimeTol, and 0.1 for ionMobilityTol. Also add a 'threshold' scan using type 'count' for the 150 most-intense peaks. Remove the peakPicking filter so you have only the three filters shown in the image below.
6. Press 'Start' to begin converting your files.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/10.jpg)
