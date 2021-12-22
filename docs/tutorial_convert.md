# Converting raw files to mzML

FragPipe and MSFragger accept mzML files, which can be generated from your raw LC/MS data with MSConvert, part of the ProteoWizard software package. ProteoWizard can be downloaded [here](http://www.proteowizard.org/download.html). Once ProteoWizard is installed, launch MSConvert.

This page has instructions for using MSConvert to generate mzML files from both Thermo Orbitrap and Bruker timsTOF data formats, though we recommend using the raw .d format for Bruker data.

**Please note: The latest version of MSConvert should be used for TMT MS3 workflows.** Philosopher is not compatible with mzML files generated using some older MSConvert versions, and the Windows taskbar may open an earlier version of MSConvert even if a more recent one is installed. Start MSConvert by clicking on the Windows icon, scrolling down to the desired (latest) version of ProteoWizard, and launching  MSConvert program.

<br>

### Convert Thermo .raw files (DDA or DIA data with non-overlapping windows):
In the MSConvert GUI window:
1. Browse for the raw files you want to convert.
2. Add them to the input list.
3. Select an output directory (where you want to mzML files to be generated).
4. Set the output file format to 'mzML', and check the boxes for 'Write index', 'TPP compatibility', and 'Use zlib compression'. **Note:** Do not select 'zlib compression' for use with Proteome Discoverer, which currently does not support this compression function.
5. Select any additional filters you want to apply to the conversion. We recommend using the default filters shown here. "peakPicking" (centroiding) must be the first filter.
6. Press 'Start' to begin converting your files.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/msconvert.png)

<br>

Example command line options:

`msconvert.exe --mzML --64 --zlib --filter "peakPicking true 1-" *.raw`

<br>

### Convert Thermo DIA .raw files with overlapping/staggered windows:
Follow the steps above, with the addition of the "Demultiplex" filter in step 5.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/msconvert_staggered_DIA.png)

<br>


### Convert Bruker timsTOF .d files (NOT RECOMMENDED. MSFragger and IonQuant can load .d directly.):
In the MSConvert window:
1. Browse for the .d folders you want to convert.
2. Add them to the input list.
3. Select an output directory (where you want to mzML files to be generated).
4. Set the output file format to 'mzML', and check the box for '**Combine ion mobility scans**'. The boxes for 'Write index', 'TPP compatibility', and 'Use zlib compression' should also be checked.
5. Add '**peakPicking**' and '**scanSumming**' filters.
6. Press 'Start' to begin converting your files.

![](https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/10.jpg)
