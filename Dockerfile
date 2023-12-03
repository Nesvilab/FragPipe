FROM ubuntu:22.04
USER root
ARG DEBIAN_FRONTEND=noninteractive

# update and upgrade packages
RUN apt-get -y update --fix-missing \
    && apt-get -y upgrade \
    && apt-get -y update \
    && apt-get -y autoremove

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
	
# set the working directory to the HOME of root
WORKDIR /root

# create directories
RUN mkdir tmp
	
# download and install fragPipe
RUN wget https://github.com/Nesvilab/FragPipe/releases/download/20.0/FragPipe-20.0.zip -P fragPipe-20.0
RUN unzip fragPipe-20.0/FragPipe-20.0.zip -d fragPipe-20.0
RUN chmod -R 770 fragPipe-20.0

# set environment variables
ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64/
RUN export JAVA_HOME
