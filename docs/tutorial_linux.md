# Running a FragPipe-equivalent workflow on Linux using command line

For most desktop users, we recommend using [FragPipe](http://fragpipe.nesvilab.org/). Users can also use FragPipe on remote server with X forwarding ([https://fragpipe.nesvilab.org/docs/tutorial_setup_x_forwarding.html](https://fragpipe.nesvilab.org/docs/tutorial_setup_x_forwarding.html)). However, if X forwarding cannot be enabled for any reason, one can use the following shell scripts or Philosopher [pipeline](https://github.com/Nesvilab/philosopher/wiki/Pipeline).

Example shell scripts for timsTOF PASEF data and non-ion mobility data are shown below, modify them to suit your configuration.
<br>

### timsTOF data:

```shell
#!/bin/bash

set -xe

# Specify paths of tools and files to be analyzed.
dataDirPath="data/"
fastaPath="2020-01-22-decoys-reviewed-contam-UP000005640.fas"
msfraggerPath="MSFragger.jar" # download from http://msfragger-upgrader.nesvilab.org/upgrader/
fraggerParamsPath="fragger.params"
philosopherPath="philosopher" # download from https://github.com/Nesvilab/philosopher/releases/latest
crystalcPath="CrystalC.jar" # download from https://github.com/Nesvilab/Crystal-C/releases/latest
crystalcParameterPath="crystalc.params"
ionquantPath="IonQuant.jar" # download from https://github.com/Nesvilab/IonQuant/releases/latest
decoyPrefix="rev_"

# Run MSFragger. Change the -Xmx value according to your computer's memory.
java -Xmx64G -jar $msfraggerPath $fraggerParamsPath $dataDirPath/<spectral files ending with .d>

# Move pepXML files to current directory.
mv $dataDirPath/*.pepXML ./

# Move MSFragger tsv files to current directory.
mv $dataDirPath/*.tsv ./ # Comment this line if localize_delta_mass = 0 in your fragger.params file.

# For open searches, run Crystal-C. Otherwise, don't run Crystal-C (comment this for-loop).
for myFile in ./*.pepXML
do
	java -Xmx64G -jar $crystalcPath $crystalcParameterPath $myFile
done

# Run PeptideProphet, ProteinProphet, and FDR filtering with Philosopher
$philosopherPath workspace --clean
$philosopherPath workspace --init
$philosopherPath database --annotate $fastaPath --prefix $decoyPrefix

# Pick one from the following three commands and comment the other two.
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --ppm --accmass --decoy $decoyPrefix --database $fastaPath ./*.pepXML # Closed search
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --masswidth 1000.0 --clevel -2 --decoy $decoyPrefix --combine --database $fastaPath ./*_c.pepXML # Open search if you ran Crystal-C
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --masswidth 1000.0 --clevel -2 --decoy $decoyPrefix --combine --database $fastaPath ./*.pepXML # Open search if you did NOT ran Crystal-C
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --ppm --accmass --nontt --decoy $decoyPrefix --database $fastaPath ./*.pepXML # Non-specific closed search

$philosopherPath proteinprophet --maxppmdiff 2000000 --output combined ./*.pep.xml

# Pick one from the following two commands and comment the other one.
$philosopherPath filter --sequential --razor --mapmods --tag $decoyPrefix --pepxml ./ --protxml ./combined.prot.xml # closed or non-specific closed search
$philosopherPath filter --sequential --razor --mapmods --tag $decoyPrefix --pepxml ./interact.pep.xml --protxml ./combined.prot.xml # Open search

# Generate reports.
$philosopherPath report
$philosopherPath workspace --clean

# Run IonQuant. Change the -Xmx value according to your computer's memory.
java -Xmx64G -jar $ionquantPath <options> <path to .pepXML>
```
**Please note: The [IonQuant.jar](https://github.com/Nesvilab/IonQuant/releases/latest) file must be in the same directory as the `ext` folder.** To see the IonQuant help, run `java -jar IonQuant.jar`.

<br>


### Non-ion mobility data:

```shell
#!/bin/bash

set -xe

# Specify paths of tools and files to be analyzed.
dataDirPath="data/"
fastaPath="2020-01-22-decoys-reviewed-contam-UP000005640.fas"
msfraggerPath="MSFragger.jar" # download from http://msfragger-upgrader.nesvilab.org/upgrader/
fraggerParamsPath="fragger.params"
philosopherPath="philosopher" # download from https://github.com/Nesvilab/philosopher/releases/latest
crystalcPath="CrystalC.jar" # download from https://github.com/Nesvilab/Crystal-C/releases/latest
crystalcParameterPath="crystalc.params"
ionquantPath="IonQuant.jar" # download from https://github.com/Nesvilab/IonQuant/releases/latest
decoyPrefix="rev_"

# Run MSFragger. Change the -Xmx value according to your computer's memory.
java -Xmx64G -jar $msfraggerPath $fraggerParamsPath $dataDirPath/<spectral files ending with .mzML or .raw>

# Move pepXML files to current directory.
mv $dataDirPath/*.pepXML ./

# Move MSFragger tsv files to current directory.
mv $dataDirPath/*.tsv ./ # Comment this line if localize_delta_mass = 0 in your fragger.params file.

# For open searches, run Crystal-C. Otherwise, don't run Crystal-C (comment this for-loop).
for myFile in ./*.pepXML
do
	java -Xmx64G -cp $crystalcPath Main $crystalcParameterPath $myFile
done

# Run PeptideProphet, ProteinProphet, and FDR filtering with Philosopher
$philosopherPath workspace --clean
$philosopherPath workspace --init
$philosopherPath database --annotate $fastaPath --prefix $decoyPrefix

# Pick one from the following three commands and comment the other two.
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --ppm --accmass --decoy $decoyPrefix --database $fastaPath ./*.pepXML # Closed search
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --masswidth 1000.0 --clevel -2 --decoy $decoyPrefix --combine --database $fastaPath ./*_c.pepXML # Open search if you ran Crystal-C
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --masswidth 1000.0 --clevel -2 --decoy $decoyPrefix --combine --database $fastaPath ./*.pepXML # Open search if you did NOT ran Crystal-C
$philosopherPath peptideprophet --nonparam --expectscore --decoyprobs --ppm --accmass --nontt --decoy $decoyPrefix --database $fastaPath ./*.pepXML # Non-specific closed search

$philosopherPath proteinprophet --maxppmdiff 2000000 --output combined ./*.pep.xml

# Pick one from the following two commands and comment the other one.
$philosopherPath filter --sequential --razor --mapmods --tag $decoyPrefix --pepxml ./ --protxml ./combined.prot.xml # closed or non-specific closed search
$philosopherPath filter --sequential --razor --mapmods --tag $decoyPrefix --pepxml ./interact.pep.xml --protxml ./combined.prot.xml # Open search

# Make reports.
$philosopherPath report
$philosopherPath workspace --clean

# Perform quantification.
java -Xmx64G -jar $ionquantPath <options> <path to .pepXML>
```
**Please note: The [IonQuant.jar](https://github.com/Nesvilab/IonQuant/releases/latest) file must be in the same directory as the `ext` folder.** To see the IonQuant help, run `java -jar IonQuant.jar`.
