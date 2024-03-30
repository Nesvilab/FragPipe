
## Setting up FragPipe

<br>
#### Install or update Java
FragPipe and MSFragger both require a 64-bit Java to run. Windows users can choose to download the `-jre-` version of FragPipe (see below) or install 64-bit Java [here](https://www.oracle.com/java/technologies/downloads/#jdk17). Launch the installer and follow the prompts. You may need to restart FragPipe after updating Java.

<br>
#### Install or update FragPipe
FragPipe can be downloaded [here](https://github.com/Nesvilab/FragPipe/releases). Follow the instructions on that same Releases page to launch the program.  When FragPipe launches, the first tab in the window ('Config') will be used to configure the program.

<br>
#### Install Visual C++ Redistributable for Visual Studio 2017
Bruker's native library needs [Visual C++ Redistributable for Visual Studio 2017](https://aka.ms/vs/16/release/VC_redist.x64.exe) in Windows. If you see an error that the Bruker native library cannot be found, please try to install the Visual C++ redistibutable.

<br>
#### Install Mono (required for Thermo .raw file reading on Linux)
Linux users need to have [Mono](https://www.mono-project.com/download/stable/#download-lin) installed to read Thermo .raw files.

<br>
#### Install, update, or use an already downloaded version of MSFragger
**Use an existing MSFragger .jar file:** If you already have the latest MSFragger release downloaded, use the 'Browse' button on the Config tab in FragPipe to select the .jar file.

**Download MSFragger:** In FragPipe, use the 'Download/Update' button to get the latest version of MSFragger. You will be prompted to fill out a short form before proceeding with the download.

<br>
#### Install, update, or use an already downloaded version of Philosopher
If you have already downloaded the latest Philosopher release, use the 'Browse' button in FragPipe to select the Philosopher executable file. To upgrade to the most recent release or to download for the first time, use the 'Download/Update' button.

<br>
#### Optional: install, update, or use an already installed version of Python
Database splitting (to reduce the size of the in-memory fragment ion index-- helpful for workstations with limited memory or for complex searches) and/or spectral library generation will require Python 3 or above.

**FragPipe requires Python 3.8, 3.9, or 3.10**

**If you already have Python 3.8, 3.9, or 3.10**, specify the python executable file path in the `Config` tab, click `Install/Upgrade EasyPQP`, and wait for a few minutes.

**If Python 3.8, 3.9, or 3.10 is not already installed**:
1) Download and install Python 3.9

Windows: download the installer from [here](https://www.python.org/ftp/python/3.9.13/python-3.9.13-amd64.exe)

Linux: different Linux distributions have different commands to install Python. Please figure it out by yourself. If you are not familiar with the command line interface, please switching to Windows.

2)  To enable spectral library building with EasyPQP (which also works for timsTOF data), click `Install/Upgrade EasyPQP` at the bottom of the `Config` tab and wait for a few minutes. FragPipe will froze during the installation.


3) In FragPipe -> Config -> Python, use the 'Browse' button to navigate to the installation location and select **python.exe**. When FragPipe refreshes, DB Splitting and Spectral Library Generation should now be enabled.


<br>
#### Next: see the [FragPipe usage tutorial](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html).

