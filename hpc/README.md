# NASQAR2 on a Slurm cluster

This package runs the all-in-one NASQAR2 image as an unprivileged Singularity
(or Apptainer) container inside a Slurm compute allocation, on any cluster
that provides Slurm, Singularity and SSH access to a login node. Nothing in it
is specific to one site: every path is derived from one install directory,
`NASQAR_HOST_HOME`, and every site-dependent value can be overridden at
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

Run both commands from the repository root. The SIF is written inside the
repository, to `hpc/build/nasqar2.sif`, wherever the script is run from
(pass another path as the first argument to change that). The build needs
about 25 GB of temporary space, also under `hpc/build`; set
`NASQAR_HOST_BUILD_DIR` to use a larger filesystem instead.

## Choose the install directory

`NASQAR_HOST_HOME` is one persistent, writable directory on the cluster
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
rsync -ah hpc/bin/ \
  USER@LOGIN_HOST:/path/to/nasqar2/bin/
rsync -ah hpc/slurm/nasqar2.sbatch \
  USER@LOGIN_HOST:/path/to/nasqar2/slurm/
```

## Submit

```bash
ssh USER@LOGIN_HOST \
  'cd /path/to/nasqar2/slurm && NASQAR_HOST_HOME=/path/to/nasqar2 sbatch nasqar2.sbatch'
```

`NASQAR_HOST_HOME` can be omitted when the install lives at the default location.
The job script loads a module named `singularity`; set
`NASQAR_HOST_SINGULARITY_MODULE` (for example `apptainer` or `singularity/4.2.0`)
when your cluster names it differently.

The job output reports the allocated compute node, selected port, SSH tunnel
command, and browser URL. The port defaults to a job-specific value between
20000 and 39999 (`NASQAR_HOST_PORT` overrides it).

```bash
ssh USER@LOGIN_HOST \
  'cat /path/to/nasqar2/runs/<job-id>/connection.txt'
```

## Variables

The name says where a variable applies. `NASQAR_HOST_*` are settings of the
job script: paths and values on the cluster. `NASQAR_CONTAINER_*` are read by
the applications inside the container; set one at submission and the job
script passes it in. All are optional; defaults derive from `NASQAR_HOST_HOME`.

| Variable | Default | Purpose |
| --- | --- | --- |
| `NASQAR_HOST_HOME` | `$SCRATCH/nasqar2` or `$HOME/nasqar2` | Install directory everything else derives from |
| `NASQAR_HOST_SIF` | `$NASQAR_HOST_HOME/images/nasqar2.sif` | Container image |
| `NASQAR_HOST_LAUNCHER` | the launcher inside the image | Run another copy of the launcher script |
| `NASQAR_HOST_RUNTIME_DIR` | `$NASQAR_HOST_HOME/runs/<job-id>` | Writable per-job runtime, bound to `/runtime` |
| `NASQAR_HOST_PROJECT_ROOT` | `$NASQAR_HOST_HOME/projects` | Private-space projects of every application |
| `NASQAR_HOST_R_LIBRARY` | `$NASQAR_HOST_HOME/r-library/R-4.3` | Private, writable R library |
| `NASQAR_HOST_DATA_ROOT` | directory containing `NASQAR_HOST_HOME`; `NASQAR_HOST_HOME` itself when that directory is your home | Input data readable in place (mounted read-only) |
| `NASQAR_HOST_PORT` | job-specific, 20000-39999 | Portal port on the compute node |
| `NASQAR_HOST_SINGULARITY_MODULE` | `singularity` | Module to load |
| `NASQAR_CONTAINER_BASE_PATH` | empty | URL prefix for Open OnDemand (see `ood/README.md`) |
| `NASQAR_CONTAINER_VARIANT_ANNOTATION_CHUNK` | 50000 | Variant Annotation records annotated per chunk |

Inside the container each `NASQAR_HOST_*` folder appears at a fixed path,
which is what the applications see:

| Setting (path on the cluster) | Path inside the container | Variable the applications read |
| --- | --- | --- |
| `NASQAR_HOST_RUNTIME_DIR` | `/runtime` | (none) |
| `NASQAR_HOST_PROJECT_ROOT` | `/runtime/projects` | `NASQAR_CONTAINER_PROJECT_ROOT` |
| `NASQAR_HOST_R_LIBRARY` | `/runtime/R/library` | `R_LIBS_USER` |
| `NASQAR_HOST_DATA_ROOT` | the same path as on the cluster, read-only | `NASQAR_CONTAINER_DATA_ROOT` |

The container variables in this table are set by the job script and the
launcher. `NASQAR_HOST_R_LIBRARY` and `NASQAR_HOST_SIF` are also visible
inside, still holding the cluster path, for the one place an application has
to show it to the user.

Every `NASQAR_CONTAINER_*` variable set at submission is passed to the
applications. A home directory is never
mounted as a whole: with an install at `$HOME/nasqar2`, put input data inside
that directory or set `NASQAR_HOST_DATA_ROOT`.

Example overriding the data root and sharing one R library between users:

```bash
NASQAR_HOST_HOME=/path/to/nasqar2 \
NASQAR_HOST_DATA_ROOT=/path/to/sequencing-data \
NASQAR_HOST_R_LIBRARY=/path/to/shared/r-library \
sbatch nasqar2.sbatch
```

## Runtime data

The Slurm script binds the per-job runtime directory to `/runtime`:

```text
/path/to/nasqar2/runs/<job-id>/
```

It contains application logs, Nginx state, temporary uploads, cross-module
exchange files, and `connection.txt`. The `.sif` remains read-only.

The data root is exposed inside the container, read-only, at the same path as
on the host, so a path pasted from the cluster works unchanged. In every
application that offers it, choose the "private space" input option and
browse or enter a directory or file under that root to load data without a
browser upload.

Runtime R packages (for example additional reference genomes for Variant
Annotation) are installed into the private R library, which persists across
jobs while the SIF and input data remain read-only. Compute nodes usually have
no internet access, so install genomes from a login node with the helper
(`nasqar2-install-genome --list` shows the catalog):

```bash
/path/to/nasqar2/bin/nasqar2-install-genome mm39
```

## Projects

Every application saves its work as a project under
`$NASQAR_HOST_PROJECT_ROOT/<project>/<App>` (`DESeq2Shiny`, `DADA2`, `SeuratV5`,
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
`NASQAR_CONTAINER_VARIANT_ANNOTATION_CHUNK`, so memory is bounded whatever the cohort
size. DADA2, Seurat v5, Monocle 3 and Variant Annotation derive worker counts
from `SLURM_CPUS_PER_TASK`.

## Resource defaults

The supplied job requests one task, 16 CPU cores, 32 GB RAM, and eight
hours. Adjust the `#SBATCH` lines in `hpc/slurm/nasqar2.sbatch` to the
intended dataset and the site's scheduling policy.

## Open OnDemand

Clusters that provide Open OnDemand can expose the same Slurm launcher as an
interactive app; `ood/README.md` describes the prefixed-URL mode this needs.
