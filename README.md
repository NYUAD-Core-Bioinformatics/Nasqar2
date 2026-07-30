# Nasqar2

### Install locally with Docker
Official Nasqar2 image is hosted in Dockerhub. Run Nasqar2 in a Docker container and access it at http://localhost:80.

Make sure Docker software is up and running.

**Pull the image:**
```
docker pull nyuadcorebio/nasqarall:latest
```

**Run on port 80:**
```
docker run -p 80:80 nyuadcorebio/nasqarall:latest
```

- If you run this service on a server, specify the (IP-address or hostname):80 on the browser.
- If you run this service on a standalone machine (e.g. laptop), specify localhost:80 on the browser.

To run Nasqar2 on another port, e.g. 8080:

```
docker run -p 8080:80 nyuadcorebio/nasqarall:latest
```
It can be accessed via http://localhost:8080

### Build a local image with Docker ( Optional )
If you want to customize the code and then build the docker image. Refer to below instructions.




Build the docker image as follows:-
Note:- Make sure you have sufficient space. It will be around 10GB and takes an hour to finish.
```
sh extract_data.sh  
docker build  --progress=plain  -t <specify-image-name> .
```

Verify the build image
```
docker image ls
```

To start Nasqar2 using the build image.
```
docker run --name <specify-name-of-container> -p 80:80 -it <specify-image-name>
```
It can be accessed via http://localhost:80

### Run NASQAR2 on a Slurm HPC system

NASQAR2 includes an unprivileged Singularity deployment for Slurm-based HPC
systems. The all-in-one image runs on an allocated compute node; users connect
through an SSH tunnel rather than exposing the service publicly.

The supplied job requests 16 CPU cores, 32 GB memory, and eight hours by
default. It provides:

- read-only access to an administrator-selected HPC data root;
- job-specific writable runtime and exchange directories;
- persistent project directories for DADA2, Seurat v5, and Monocle 3;
- persistent DESeq2Shiny state files and ATACseqQC caches; and
- a private writable R library for packages that must persist across jobs.

Build the Singularity image, copy it and the launcher files to the HPC system,
and submit the supplied Slurm script:

```bash
docker build -t nasqar2-local:bulk-gcm .
./hpc/build-sif.sh

rsync -ah hpc/build/nasqar2.sif \
  USER@LOGIN_HOST:/path/to/nasqar2/images/nasqar2.sif
rsync -ah hpc/bin/nasqar2-hpc hpc/slurm/nasqar2.sbatch \
  USER@LOGIN_HOST:/path/to/nasqar2/

ssh USER@LOGIN_HOST \
  'NASQAR2_SIF=/path/to/nasqar2/images/nasqar2.sif \
   NASQAR2_LAUNCHER=/path/to/nasqar2/nasqar2-hpc \
   sbatch /path/to/nasqar2/nasqar2.sbatch'
```

The job writes `connection.txt` in its runtime directory. Use the SSH tunnel
shown in that file, then open the reported `http://localhost:<port>` address.
Do not run the portal on an HPC login node. See
[`hpc/README.md`](hpc/README.md) for the complete configuration and Jubail
example.

