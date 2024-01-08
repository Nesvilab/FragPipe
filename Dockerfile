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
	
# download and install fragPipe
RUN wget https://github.com/Nesvilab/FragPipe/releases/download/21.1/FragPipe-21.1.zip -P fragPipe-21.1
RUN unzip fragPipe-21.1/FragPipe-21.1.zip -d fragPipe-21.1
RUN chmod -R 770 fragPipe-21.1

# set environment variables
ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64/
RUN export JAVA_HOME
