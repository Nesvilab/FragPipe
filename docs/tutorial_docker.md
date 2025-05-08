# Pulling and running FragPipe using Docker

- Install docker by following the [instruction](https://docs.docker.com/desktop/install/mac-install/)
- Open terminal and pull the docker image: `docker pull fcyucn/fragpipe`
- (optional) Check the pulled image: `docker image ls -a`
- Create a Docker container: `docker run -it -v <host directory>:<container directory> fcyucn/fragpipe /bin/bash`. `<host directory>` is the directory in your host machine, and `<container directory>` is the directory you want to map the host directory to container.
- Then, you have entered the docker, and FragPipe is located in `/fragpipe_bin`
- Go to `/fragpipe_bin/fragpipe-x.x/fragpipe-x.x/bin` directory and execute `./fragpipe --help` in the terminal

Read more: Check [Running FragPipe in command line interface](https://fragpipe.nesvilab.org/docs/tutorial_headless.html) for the instructions running FragPipe in command line interface.
