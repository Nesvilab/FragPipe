FROM ubuntu:22.04
USER root
ARG DEBIAN_FRONTEND=noninteractive

# update and upgrade packages
RUN apt-get -y update --fix-missing \
    && apt-get -y upgrade \
    && apt-get -y update \
    && apt-get -y autoremove

# install mono	
RUN apt-get -y install ca-certificates gnupg
RUN gpg --homedir /tmp --no-default-keyring --keyring /usr/share/keyrings/mono-official-archive-keyring.gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb [signed-by=/usr/share/keyrings/mono-official-archive-keyring.gpg] https://download.mono-project.com/repo/ubuntu stable-focal main" | tee /etc/apt/sources.list.d/mono-official-stable.list
RUN apt-get -y update
RUN apt-get -y install mono-devel

# install dependencies
RUN apt-get -y install \
    git \
    python3.9 \
    python3-pip \
    tar \
    unzip \
    wget \
    openjdk-11-jdk \
	vim \
	dotnet-runtime-6.0
	
# install python packages
RUN pip uninstall easypqp \
    && pip install git+https://github.com/Nesvilab/easypqp.git@master \
    && pip install lxml
	
# create a directory with 777 permission and set it to the work directory
RUN mkdir /fragpipe_bin
RUN chmod 777 /fragpipe_bin
WORKDIR /fragpipe_bin

# create directories
RUN mkdir tmp
RUN chmod 777 tmp
	
# download and install fragPipe
RUN wget https://github.com/Nesvilab/FragPipe/releases/download/21.1/FragPipe-21.1.zip -P fragPipe-21.1
RUN unzip fragPipe-21.1/FragPipe-21.1.zip -d fragPipe-21.1
RUN chmod -R 777 /fragpipe_bin

# set environment variables
ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64/
RUN export JAVA_HOME
