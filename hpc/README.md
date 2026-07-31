# NASQAR2 on Jubail HPC

For Open OnDemand deployment, including prefixed static assets and Shiny
WebSocket requirements, see [`ood/README.md`](ood/README.md). The OOD mode is
opt-in and does not change SSH-tunnel or standalone deployments.

This package runs the all-in-one NASQAR2 image as an unprivileged Singularity
container inside a Slurm compute allocation.

## Build the image

The local definition imports the current `nasqar2-local:bulk-gcm` Docker image:

```bash
./hpc/build-sif.sh
```

The default output is `hpc/build/nasqar2.sif`.
Building requires approximately 25 GB of temporary space. If `/tmp` is too
small, direct the temporary files and cache to a larger filesystem:

```bash
mkdir -p /data/nasqar2-build/{tmp,cache}
TMPDIR=/data/nasqar2-build/tmp \
SINGULARITY_TMPDIR=/data/nasqar2-build/tmp \
SINGULARITY_CACHEDIR=/data/nasqar2-build/cache \
./hpc/build-sif.sh /data/nasqar2-build/nasqar2.sif
```

## Copy to Jubail

```bash
ssh nr83@jubail.abudhabi.nyu.edu \
  'mkdir -p "$SCRATCH/nasqar2"/{images,runs,slurm}'
rsync -ah --partial hpc/build/nasqar2.sif \
  nr83@jubail.abudhabi.nyu.edu:/scratch/nr83/nasqar2/images/nasqar2.sif
rsync -ah hpc/slurm/nasqar2.sbatch \
  nr83@jubail.abudhabi.nyu.edu:/scratch/nr83/nasqar2/slurm/
```

## Submit

```bash
ssh nr83@jubail.abudhabi.nyu.edu \
  'cd "$SCRATCH/nasqar2/slurm" && sbatch nasqar2.sbatch'
```

The job output reports the allocated compute node, selected port, SSH tunnel
command, and browser URL. The port defaults to a job-specific value between
20000 and 39999.

```bash
ssh nr83@jubail.abudhabi.nyu.edu \
  'cat "$SCRATCH/nasqar2/runs/<job-id>/connection.txt"'
```

## Runtime data

The Slurm script binds this writable directory to `/runtime`:

```text
/scratch/nr83/nasqar2/runs/<job-id>/
```

It contains application logs, Nginx state, caches, temporary uploads,
cross-module exchange files, results, and `connection.txt`. The `.sif` remains
read-only.

The Slurm job also exposes `$SCRATCH` inside the container as a read-only data
root. In ATACseqQC, select **Use HPC scratch directory** and enter a directory
such as `/scratch/nr83/project/bams` to load matching BAM/BAI pairs without a
browser upload. Override the allowed root at submission time when needed:

```bash
NASQAR2_DATA_ROOT=/scratch/nr83/project sbatch nasqar2.sbatch
```

Runtime R packages are installed into the private, writable library
`$SCRATCH/nasqar2/r-library/R-4.3`. The library persists across jobs while the
SIF and input data remain read-only. Override it when a separate package
environment is required:

```bash
NASQAR2_R_LIBRARY=/scratch/nr83/project/r-library sbatch nasqar2.sbatch
```

## Resource defaults

The supplied job requests one task, 16 CPU cores, 32 GB RAM, and eight
hours. Adjust these values to the intended dataset. Do not start NASQAR2 on a
Jubail login node.

ATACseqQC caches chromosome-specific core, nucleosome, and footprint results
under `$SCRATCH/nasqar2/cache/ATACseqQC`. Cached results are keyed by the BAM
path, size, modification time, genome, chromosome, and motif. The application
uses up to three Slurm CPUs concurrently for PT, NFR, and TSSE scoring.

DESeq2Shiny can also save and restore its existing `.RData` application states
directly in `$SCRATCH/nasqar2/states/DESeq2Shiny`. The private directory
persists across Slurm jobs while browser download and upload remain available.

DADA2, Seurat v5, and Monocle 3 can read datasets directly from the read-only
HPC data root. Their generated files persist across jobs under
`$SCRATCH/nasqar2/projects/DADA2`, `$SCRATCH/nasqar2/projects/SeuratV5`, and
`$SCRATCH/nasqar2/projects/Monocle3`. They derive worker counts from
`SLURM_CPUS_PER_TASK`.

DADA2 performs filtering and trimming in node-local temporary storage. Final
results and parameter-aware caches persist in its project directory. Cache keys
include FASTQ paths, sizes, modification times, analysis parameters, and the
DADA2 package version.

## Open OnDemand

Jubail also provides Open OnDemand at
<https://ood.hpc.abudhabi.nyu.edu>. The Slurm launcher is the foundation for a
future NASQAR2 interactive-app form.
