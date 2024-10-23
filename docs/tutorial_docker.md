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

2. **Mounting the Host Directory to the Docker Container:**

   One of the most useful features of Docker is the ability to mount a host directory inside the container. This is especially helpful when you need to access data files or output results from FragPipe.

   You can mount a local directory (e.g., `/path/to/your/data`) to the Docker container using the `-v` option:

   ```bash
   docker run -v /path/to/your/data:/mnt/data fcyucn/fragpipe:<version> fragPipe-<version>/fragpipe/bin/fragpipe --help
   ```

   - `/path/to/your/data`: The path on your host machine where your data is located.
   - `/mnt/data`: The directory inside the container where the host directory will be mounted.
   
   Now, any files placed in `/path/to/your/data` will be accessible from `/mnt/data` inside the Docker container. This allows you to read input files and save output directly from/to your local machine while running FragPipe in Docker.

3. **Running FragPipe Inside the Docker Container:**

   To run FragPipe inside the container, mount the required directories (for example, where your data or project files are located), then execute the FragPipe tool. You can use the following command:

   ```bash
   docker run -v /path/to/your/data:/mnt/data -it fcyucn/fragpipe:<version> /fragpipe_bin/FragPipe-<version>/fragpipe/bin/fragpipe -config /mnt/data/config_file.fpconfig
   ```

   - `-v /path/to/your/data:/mnt/data`: Mounts the host directory to the container.
   - `-it`: Enables interactive mode, allowing you to interact with the container.
   - `/fragpipe_bin/FragPipe-<version>/fragpipe/bin/fragpipe`: Executes FragPipe inside the container.
   - `-config /mnt/data/config_file.fpconfig`: Specifies the configuration file from the mounted directory for FragPipe.

   You can replace `/mnt/data/config_file.fpconfig` with the path to your actual configuration file located in the mounted directory. This way, FragPipe runs with your specific settings.

4. **Save the Docker image to a TAR file:**

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
   singularity exec fragPipe-<version>.img /fragpipe_bin/FragPipe-<version>/fragpipe/bin/fragpipe --help
   ```

   This command runs the same `fragpipe` tool using the Singularity image, displaying the help options.

3. **Mounting Host Directories with Singularity:**

   Similar to Docker, you can mount directories from your host system when using Singularity. Use the `-B` option to bind a directory from your host system to the Singularity container:

   ```bash
   singularity exec -B /path/to/your/data:/mnt/data fragPipe-<version>.img /fragpipe_bin/FragPipe-<version>/fragpipe/bin/fragpipe -config /mnt/data/config_file.fpconfig
   ```

   This command binds the host directory `/path/to/your/data` to `/mnt/data` inside the Singularity container and runs FragPipe using the specified configuration file.