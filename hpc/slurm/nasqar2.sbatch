#!/usr/bin/env bash

#SBATCH --job-name=nasqar2
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=32G
#SBATCH --time=08:00:00
#SBATCH --output=nasqar2-%j.out
#SBATCH --error=nasqar2-%j.err

set -euo pipefail

module purge
# Name of the Singularity/Apptainer module on this cluster.
module load "${NASQAR2_SINGULARITY_MODULE:-singularity}"

# Everything NASQAR2 keeps on the cluster filesystem lives under one
# persistent, writable directory: NASQAR2_HOME. Choose your scratch, project
# or home space as the site provides; the default is $SCRATCH/nasqar2 where a
# SCRATCH variable exists and $HOME/nasqar2 otherwise.
nasqar2_home="${NASQAR2_HOME:-${SCRATCH:-$HOME}/nasqar2}"

sif="${NASQAR2_SIF:-$nasqar2_home/images/nasqar2.sif}"
launcher="${NASQAR2_LAUNCHER:-$nasqar2_home/bin/nasqar2-hpc}"
runtime_dir="${NASQAR2_RUNTIME_DIR:-$nasqar2_home/runs/$SLURM_JOB_ID}"
port="${NASQAR2_PORT:-$((20000 + SLURM_JOB_ID % 20000))}"
# Input files the applications may read in place (mounted read-only).
data_root="${NASQAR2_DATA_ROOT:-$(dirname "$nasqar2_home")}"
r_library="${NASQAR2_R_LIBRARY:-$nasqar2_home/r-library/R-4.3}"
# Root of the private-space projects of every application (projects/<App>/<name>)
projects_root="${NASQAR2_PROJECTS:-$nasqar2_home/projects}"
node_tmp="${SLURM_TMPDIR:-/tmp/$USER/nasqar2-$SLURM_JOB_ID}"
exchange_dir="$runtime_dir/exchange"

if [[ ! -f "$sif" ]]; then
    echo "NASQAR2 image not found: $sif" >&2
    exit 1
fi
if [[ ! -x "$launcher" ]]; then
    echo "NASQAR2 HPC launcher not found or not executable: $launcher" >&2
    exit 1
fi
if [[ ! -d "$data_root" ]]; then
    echo "NASQAR2 data root not found: $data_root" >&2
    exit 1
fi

mkdir -p "$runtime_dir/R/library" "$r_library" "$projects_root" \
    "$node_tmp/DADA2" "$exchange_dir"
chmod 700 "$r_library" "$projects_root" \
    "$exchange_dir"

singularity exec --cleanenv \
    --env "SLURM_JOB_ID=$SLURM_JOB_ID" \
    --env "SLURM_CPUS_PER_TASK=$SLURM_CPUS_PER_TASK" \
    --env "NASQAR_BASE_PATH=${NASQAR_BASE_PATH:-}" \
    --env "NASQAR_DATA_ROOT=$data_root" \
    --env "NASQAR_R_LIBRARY_HOST=$r_library" \
    --env "NASQAR_SIF_HOST=$sif" \
    --env "NASQAR_VARIANT_ANNOTATION_CHUNK=${NASQAR_VARIANT_ANNOTATION_CHUNK:-}" \
    --env "NASQAR_VARIANT_YIELD_SIZE=${NASQAR_VARIANT_YIELD_SIZE:-}" \
    --env "NASQAR_EXCHANGE_DIR=/runtime/exchange" \
    --env "NASQAR_PROJECT_ROOT=/runtime/projects" \
    --env "NASQAR_PRIVATE_ROOT=${NASQAR2_PRIVATE_ROOT:-$( [[ "$projects_root" == "$data_root"/* ]] && echo "$data_root" )}" \
    --env "NASQAR_DADA2_WORK_DIR=/runtime/node-tmp/DADA2" \
    --env "R_LIBS_USER=/runtime/R/library" \
    --bind "$runtime_dir:/runtime" \
    --bind "$projects_root:/runtime/projects" \
    --bind "$node_tmp:/runtime/node-tmp" \
    --bind "$r_library:/runtime/R/library" \
    --bind "$data_root:$data_root:ro" \
    --bind "$launcher:/tmp/nasqar2-hpc:ro" \
    "$sif" \
    /tmp/nasqar2-hpc \
    --port "$port" \
    --runtime /runtime
