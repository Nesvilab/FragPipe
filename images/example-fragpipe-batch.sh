#!/bin/bash
set -x

/mnt/c/FragPipe/FragPipe-23.0/bin/fragpipe.sh --headless --workflow /mnt/c/FragPipe/FragPipe-23.0/jobs/example-job-01.workflow --manifest /mnt/c/FragPipe/FragPipe-23.0/jobs/example-job-01.fp-manifest --workdir C:/Example/Output_01 --config-tools-folder /mnt/c/FragPipe/FragPipe-23.0/tools --ram 0 --threads 0
/mnt/c/FragPipe/FragPipe-23.0/bin/fragpipe.sh --headless --workflow /mnt/c/FragPipe/FragPipe-23.0/jobs/example-job-02.workflow --manifest /mnt/c/FragPipe/FragPipe-23.0/jobs/example-job-02.fp-manifest --workdir C:/Example/Output_02 --config-tools-folder /mnt/c/FragPipe/FragPipe-23.0/tools --ram 0 --threads 0
