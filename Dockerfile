FROM ubuntu:24.04
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

RUN apt install -y software-properties-common
RUN add-apt-repository ppa:deadsnakes/ppa
RUN add-apt-repository ppa:dotnet/backports
RUN apt-get -y update

# install dependencies
RUN apt-get -y install \
    git \
    python3.11 \
    python3-pip \
    tar \
    unzip \
    wget \
    openjdk-17-jdk \
    vim \
    dotnet-runtime-6.0

# install python packages
RUN pip uninstall --break-system-packages easypqp \
    && pip install --break-system-packages git+https://github.com/Nesvilab/easypqp.git@master \
    && pip install --break-system-packages lxml \
    && pip install --break-system-packages plotly \
    && pip install --break-system-packages kaleido \
    && pip install --break-system-packages narwhals \
    && pip install --break-system-packages pyarrow \
    && pip install --break-system-packages pypdf2

# create a directory with 777 permission and set it to the work directory
RUN mkdir /fragpipe_bin
RUN chmod 777 /fragpipe_bin
WORKDIR /fragpipe_bin

# create directories
RUN mkdir tmp
RUN chmod 777 tmp

# download and install fragPipe
RUN wget https://github.com/Nesvilab/FragPipe/releases/download/23.0/FragPipe-23.0-linux.zip -P fragpipe-23.0
RUN unzip fragpipe-23.0/FragPipe-23.0-linux.zip -d fragpipe-23.0
RUN chmod -R 777 /fragpipe_bin

# set environment variables
ENV JAVA_HOME /usr/lib/jvm/java-17-openjdk-amd64/
RUN export JAVA_HOME
