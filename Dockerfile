FROM ubuntu:22.04
USER root
ARG DEBIAN_FRONTEND=noninteractive
# update and upgrade packages
RUN apt-get -y update --fix-missing \
    && apt-get -y upgrade \
    && apt-get -y install software-properties-common \
    && add-apt-repository ppa:deadsnakes/ppa \
    && apt-get -y update
# install dependencies
RUN apt-get -yqq install \
    fontconfig \
    git \
    libglib2.0-0 \                                                                      
    libsm6 \                                                                            
    libxrender1 \                                                                       
    libxext6 \
    libfreetype6 \                                                                      
    libgomp1 \
    python3.12 \                                                                        
    python3-pip \
    tar \
    unzip \ 
    wget
# install python packages
RUN pip uninstall easypqp \
    && pip install git+https://github.com/Nesvilab/easypqp.git@master \
    && pip install lxml \
    && pip install --force-reinstall numpy==1.25 \
    && pip install --force-reinstall pandas==1.5.3 \
    && pip install pyopenms
# create directories
RUN mkdir /home/tmp
# Install OpenJDK-8
RUN apt-get update && \
    apt-get install -y openjdk-18-jdk && \
    apt-get clean;
# download and install fragPipe
RUN wget https://github.com/Nesvilab/FragPipe/releases/download/20.0/FragPipe-20.0.zip -P /home/fragPipe-20.0
RUN unzip /home/fragPipe-20.0/FragPipe-20.0.zip -d /home/fragPipe-20.0
RUN chmod -R 770 /home/fragPipe-20.0
# set environment variables
ENV JAVA_HOME /usr/lib/jvm/java-18-openjdk-amd64/
RUN export JAVA_HOME
