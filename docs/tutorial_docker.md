# Pulling and running FragPipe using Docker

- Install Docker
- Open terminal and pull the Docker image: `docker pull fcyucn/fragpipe`
- (optional) Check the pulled image: `docker image ls -a`
- Create a Docker container: `docker run -it -v <host directory>:<container directory> fcyucn/fragpipe /bin/bash`. `<host directory>` is the directory in your host machine, and `<container directory>` is the directory you want to map the host directory to container.
- Then, you have entered the Docker container, and FragPipe is located in `/fragpipe_bin`
- Go to `/fragpipe_bin/fragpipe-x.x/fragpipe-x.x/bin` directory and execute `./fragpipe --help` in the terminal
  
# Pulling and running FragPipe using Singularity

- Install Singularity
- Open terminal and pull the Docker image: `singularity pull docker://fcyucn/fragpipe:latest`
- Enter the image: `singularity shell --compat --bind /storage:/storage fragpipe_latest.sif /bin/bash`
- Then, you have entered the Singularity image, and FragPipe is located in `/fragpipe_bin`
- Go to `/fragpipe_bin/fragpipe-x.x/fragpipe-x.x/bin` directory and execute `./fragpipe --help` in the terminal

#### If you are using Apptainer, just replace `singularity` with `apptainer` in the commands.

### Read more: Check [Running FragPipe in command line interface](https://fragpipe.nesvilab.org/docs/tutorial_headless.html) for the instructions running FragPipe in command line interface.
