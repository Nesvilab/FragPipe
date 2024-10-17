# Running FragPipe using Docker and Singularity

This tutorial covers how to use **Docker** and **Singularity** to run FragPipe. We will run the `fcyucn/fragpipe:<version>` container, first with Docker, then converting it to a Singularity image.

## Prerequisites for Docker and Apptainer (Linux)

Before setting up Docker and Apptainer on Linux, ensure the following:

- **Operating System**: A Linux distribution (e.g., Ubuntu, CentOS, Debian).
- **Docker Installation**: Install Docker by following the [official guide for Linux](https://docs.docker.com/engine/install/).
- **Apptainer Installation**: Install Apptainer from the [Apptainer documentation](https://apptainer.org/docs/admin/main/installation.html).
- **Using Singularity Commands**: Apptainer supports Singularity commands. After installation, you can use `singularity` as the command-line interface (`apptainer` binary serves as `singularity`).

- **User Privileges**: Ensure you have `sudo` privileges for installation.

## Running FragPipe with Docker

1. **Pull and run the Docker container:**

   To run the FragPipe tool directly using Docker, execute:

   ```bash
   docker run fcyucn/fragpipe:<version> fragPipe-<version>/fragpipe/bin/fragpipe --help
   ```

   This command will:
   - Pull the `fcyucn/fragpipe:<version>` image from Docker Hub.
   - Run the `fragpipe` command inside the container, displaying the help options.

2. **Save the Docker image to a TAR file:**

   To convert the Docker container for use in Singularity, save the Docker image to a tarball:

   ```bash
   docker save -o fragPipe-<version>.tar fcyucn/fragpipe:<version>
   ```

   This command will export the Docker image into a file called `fragPipe-<version>.tar`.

## Running FragPipe with Singularity

1. **Convert the Docker image to a Singularity image:**

   Use the exported Docker tarball to create a Singularity image:

   ```bash
   singularity build fragPipe-<version>.img docker-archive://fragPipe-<version>.tar
   ```

   This will convert the Docker image into a Singularity image named `fragPipe-<version>.img`.

2. **Run the FragPipe tool with Singularity:**

   Once the Singularity image is built, you can execute the `fragpipe` command with:

   ```bash
   singularity exec fragPipe-<verison>.img /fragpipe_bin/FragPipe-<version>/fragpipe/bin/fragpipe --help
   ```

   This command runs the same `fragpipe` tool using the Singularity image, displaying the help options.

Read more: Check [Running FragPipe in command line interface](https://fragpipe.nesvilab.org/docs/tutorial_headless.html) for the instructions running FragPipe in command line interface.
