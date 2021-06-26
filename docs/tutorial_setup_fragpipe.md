
## Setting up FragPipe

<br>
#### Install or update Java
FragPipe and MSFragger both require a 64-bit Java to run. Windows users can choose to download the `-jre-` version of FragPipe (see below) or install 64-bit Java [here](https://www.oracle.com/java/technologies/javase-jdk14-downloads.html) by selecting the **Windows x64 Installer**. Launch the installer and follow the prompts. You may need to restart FragPipe after updating Java.

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

**Download MSFragger:** In FragPipe, use the 'Download/Update' button to get the latest version of MSFragger.

<br>
#### Install, update, or use an already downloaded version of Philosopher
If you have already downloaded the latest Philosopher release, use the 'Browse' button in FragPipe to select the Philosopher executable file. To upgrade to the most recent release or to download for the first time, use the 'Download/Update' button.

<br>
#### Optional: install, update, or use an already installed version of Python
Database splitting (to reduce the size of the in-memory fragment ion index-- helpful for workstations with limited memory or for complex searches) and/or spectral library generation will require Python 3 or above.

**If you already have Python 3 or above**, make sure the following packages are installed: `numpy`, `pandas`, `matplotlib`, `cython`, and `msproteomicstools`. The `easypqp` package is also required to build spectral libraries from timsTOF data. Please note: if Python was installed through Anaconda, you will already have all of these packages except for `msproteomicstools` (version 0.8.0) and `easypqp`. In most cases, you can run `pip install [package name]` to install a missing package. To install `easypqp`, you will need to 1) [install Git](https://github.com/git-guides/install-git) if you don't already have it, then 2) open an Anaconda Prompt command line window and run these two commands:

`pip uninstall --yes easypqp`

`pip install git+https://github.com/grosenberger/easypqp.git@master`

**If Python 3 is not already installed**:
1) Click 'Download' in the Python section of the Config tab in FragPipe or [click here](https://www.anaconda.com/distribution/) to go to the Anaconda site, click 'Download', then select the latest Python version (3.7 or higher) and launch the installer.
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/anaconda_install.png" width="500px" align="middle"/>

2) Follow the prompts in the graphical installer. **Note the install location** that you choose and complete the installation. We do not recommend adding Anaconda to your PATH environment variable, but you can choose to register Anaconda as your default Python.
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/anaconda_install_path.png" width="500px" align="middle"/>

3) From the start menu, search for "Anaconda Prompt" and launch it.
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/anaconda_prompt_search.png" width="700px" align="middle"/>

4) In the Anaconda Prompt window that opens, type `pip install msproteomicstools==0.8.0` and hit enter to install the _msproteomicstools_ package. Repeat this for the remaining packages (`numpy`, `pandas`, `matplotlib`, `cython`).
<img src="https://raw.githubusercontent.com/Nesvilab/FragPipe/gh-pages/images/pip_install.png" width="700px" align="middle"/>
  To install `easypqp` for spectral library generation, you will need to 1) [install Git](https://github.com/git-guides/install-git) if you don't already have it, then 2) run these two commands:

`pip uninstall --yes easypqp`

`pip install git+https://github.com/grosenberger/easypqp.git@master`

5) In FragPipe -> Config -> Python, use the 'Browse' button to navigate to the installation location and select **python.exe**. When FragPipe refreshes, DB Splitting and Spectral Library Generation should now be enabled.


<br>
#### Next: see the [FragPipe usage tutorial](https://fragpipe.nesvilab.org/docs/tutorial_fragpipe.html).

