# NASQAR2 on a Slurm cluster

This package runs the all-in-one NASQAR2 image as an unprivileged Singularity
(or Apptainer) container inside a Slurm compute allocation, on any cluster
that provides Slurm, Singularity and SSH access to a login node. Nothing in it
is specific to one site: every path is derived from one install directory,
`NASQAR2_HOME`, and every site-dependent value can be overridden at
submission.

For Open OnDemand deployment, including prefixed static assets and Shiny
WebSocket requirements, see [`ood/README.md`](ood/README.md). The OOD mode is
opt-in and does not change SSH-tunnel or standalone deployments.

The user-facing version of these instructions is the portal's
[HPC deployment](../uiApp/src/pages/quickstart/hpc.mdx) page.

## Build the image

```bash
docker build -t nasqar2-all:latest .
./hpc/build-sif.sh
```

The SIF is written to `hpc/build/nasqar2.sif` (pass another path as the
first argument). The build needs about 25 GB of temporary space under
`hpc/build`; set `NASQAR2_BUILD_DIR` to use a larger filesystem instead.

## Choose the install directory

`NASQAR2_HOME` is one persistent, writable directory on the cluster
filesystem that the compute nodes can see: a scratch area, a project or lab
allocation, or your home directory. It holds the image, the job script, the
per-job runtime directories, the private R library and every application's
projects (about 10 GB for the image plus whatever your projects produce).

The job script defaults to `$SCRATCH/nasqar2` on clusters that define a
`SCRATCH` variable and to `$HOME/nasqar2` otherwise. The examples below use
`/path/to/nasqar2`; replace it and `USER@LOGIN_HOST` with your own values.

## Copy to the cluster

```bash
ssh USER@LOGIN_HOST \
  'mkdir -p /path/to/nasqar2/{bin,images,runs,slurm}'
rsync -ah --partial hpc/build/nasqar2.sif \
  USER@LOGIN_HOST:/path/to/nasqar2/images/nasqar2.sif
rsync -ah hpc/bin/nasqar2-hpc \
  USER@LOGIN_HOST:/path/to/nasqar2/bin/
rsync -ah hpc/slurm/nasqar2.sbatch \
  USER@LOGIN_HOST:/path/to/nasqar2/slurm/
```

## Submit

```bash
ssh USER@LOGIN_HOST \
  'cd /path/to/nasqar2/slurm && NASQAR2_HOME=/path/to/nasqar2 sbatch nasqar2.sbatch'
```

`NASQAR2_HOME` can be omitted when the install lives at the default location.
The job script loads a module named `singularity`; set
`NASQAR2_SINGULARITY_MODULE` (for example `apptainer` or `singularity/4.2.0`)
when your cluster names it differently.

The job output reports the allocated compute node, selected port, SSH tunnel
command, and browser URL. The port defaults to a job-specific value between
20000 and 39999 (`NASQAR2_PORT` overrides it).

```bash
ssh USER@LOGIN_HOST \
  'cat /path/to/nasqar2/runs/<job-id>/connection.txt'
```

## Variables

All are optional; defaults derive from `NASQAR2_HOME`.

| Variable | Default | Purpose |
| --- | --- | --- |
| `NASQAR2_HOME` | `$SCRATCH/nasqar2` or `$HOME/nasqar2` | Install directory everything else derives from |
| `NASQAR2_SIF` | `$NASQAR2_HOME/images/nasqar2.sif` | Container image |
| `NASQAR2_LAUNCHER` | `$NASQAR2_HOME/bin/nasqar2-hpc` | In-container launcher script |
| `NASQAR2_RUNTIME_DIR` | `$NASQAR2_HOME/runs/<job-id>` | Writable per-job runtime, bound to `/runtime` |
| `NASQAR2_PROJECTS` | `$NASQAR2_HOME/projects` | Private-space projects of every application |
| `NASQAR2_R_LIBRARY` | `$NASQAR2_HOME/r-library/R-4.3` | Private, writable R library |
| `NASQAR2_DATA_ROOT` | directory containing `NASQAR2_HOME` | Input data readable in place (mounted read-only) |
| `NASQAR2_PRIVATE_ROOT` | the data root, when the projects live inside it | Root shown by the applications' Browse dialogs |
| `NASQAR2_PORT` | job-specific, 20000-39999 | Portal port on the compute node |
| `NASQAR2_SINGULARITY_MODULE` | `singularity` | Module to load |
| `NASQAR_BASE_PATH` | empty | URL prefix for Open OnDemand (see `ood/README.md`) |
| `NASQAR_VARIANT_ANNOTATION_CHUNK` | 50000 | Variant Annotation records annotated per chunk |

Example overriding the data root and sharing one R library between users:

```bash
NASQAR2_HOME=/path/to/nasqar2 \
NASQAR2_DATA_ROOT=/path/to/sequencing-data \
NASQAR2_R_LIBRARY=/path/to/shared/r-library \
sbatch nasqar2.sbatch
```

## Runtime data

The Slurm script binds the per-job runtime directory to `/runtime`:

```text
/path/to/nasqar2/runs/<job-id>/
```

It contains application logs, Nginx state, temporary uploads, cross-module
exchange files, and `connection.txt`. The `.sif` remains read-only. Node-local
temporary storage (`SLURM_TMPDIR` when the site provides it) is bound to
`/runtime/node-tmp` for the stages that benefit from fast local disk.

The data root is exposed inside the container, read-only, at the same path as
on the host, so a path pasted from the cluster works unchanged. In every
application that offers it, choose the "private space" input option and
browse or enter a directory or file under that root to load data without a
browser upload.

Runtime R packages (for example additional reference genomes for Variant
Annotation) are installed into the private R library, which persists across
jobs while the SIF and input data remain read-only. Compute nodes usually have
no internet access; the application shows a ready-to-run `singularity exec`
command to run once on a login node, which installs into the same library.

## Projects

Every application saves its work as a project under
`$NASQAR2_PROJECTS/<App>/<name>` (`DESeq2Shiny`, `DADA2`, `SeuratV5`,
`Monocle3`, `ATACseqQC`, `VariantAnnotation`, `ClusterProfShinyORA`,
`ClusterProfShinyGSEA`, `GeneCountMerger`, `mergeFPKMs`). A project holds the
application's outputs, its caches, every download the user made, and a
`state.rds`/`state.json` pair recording the inputs and settings, so it can be
restored from the application's project picker in a later job. Projects
persist across jobs; stopping a job (`scancel <job-id>`) closes the portal
without removing them.

Cache keys include input paths, sizes and modification times, the analysis
parameters and the package version, so a changed input or setting never
serves a stale result. DADA2 filters and trims in node-local storage; Variant
Annotation converts VCFs that are not bgzip-compressed and tabix-indexed once
into the project and streams records in chunks of
`NASQAR_VARIANT_ANNOTATION_CHUNK`, so memory is bounded whatever the cohort
size. DADA2, Seurat v5, Monocle 3 and Variant Annotation derive worker counts
from `SLURM_CPUS_PER_TASK`.

## Resource defaults

The supplied job requests one task, 16 CPU cores, 32 GB RAM, and eight
hours. Adjust the `#SBATCH` lines in `hpc/slurm/nasqar2.sbatch` to the
intended dataset and the site's scheduling policy.

## Open OnDemand

Clusters that provide Open OnDemand can expose the same Slurm launcher as an
interactive app; `ood/README.md` describes the prefixed-URL mode this needs.
