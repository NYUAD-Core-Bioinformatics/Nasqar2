#!/usr/bin/env bash

#SBATCH --job-name=nasqar2
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=32G
#SBATCH --time=08:00:00
#SBATCH --output=nasqar2-%j.out
#SBATCH --error=nasqar2-%j.err

set -euo pipefail

# Naming: NASQAR_HOST_* are settings of this script, paths and values on the
# cluster. NASQAR_CONTAINER_* are read by the applications inside the
# container; any that is set at submission is passed in unchanged.
renamed=(
    NASQAR2_HOME=NASQAR_HOST_HOME NASQAR2_SIF=NASQAR_HOST_SIF
    NASQAR2_LAUNCHER=NASQAR_HOST_LAUNCHER NASQAR2_RUNTIME_DIR=NASQAR_HOST_RUNTIME_DIR
    NASQAR2_PROJECTS=NASQAR_HOST_PROJECT_ROOT NASQAR2_R_LIBRARY=NASQAR_HOST_R_LIBRARY
    NASQAR2_DATA_ROOT=NASQAR_HOST_DATA_ROOT NASQAR2_PORT=NASQAR_HOST_PORT
    NASQAR2_SINGULARITY_MODULE=NASQAR_HOST_SINGULARITY_MODULE
    NASQAR_BASE_PATH=NASQAR_CONTAINER_BASE_PATH
    NASQAR_VARIANT_ANNOTATION_CHUNK=NASQAR_CONTAINER_VARIANT_ANNOTATION_CHUNK
)
for pair in "${renamed[@]}"; do
    old="${pair%%=*}"
    if [[ -n "${!old:-}" ]]; then
        echo "$old is no longer read: set ${pair#*=} instead." >&2
        exit 1
    fi
done

module purge
# Name of the Singularity/Apptainer module on this cluster.
module load "${NASQAR_HOST_SINGULARITY_MODULE:-singularity}"

# Everything NASQAR2 keeps on the cluster filesystem lives under one
# persistent, writable directory: NASQAR_HOST_HOME. Choose your scratch, project
# or home space as the site provides; the default is $SCRATCH/nasqar2 where a
# SCRATCH variable exists and $HOME/nasqar2 otherwise.
nasqar2_home="${NASQAR_HOST_HOME:-${SCRATCH:-$HOME}/nasqar2}"

sif="${NASQAR_HOST_SIF:-$nasqar2_home/images/nasqar2.sif}"
runtime_dir="${NASQAR_HOST_RUNTIME_DIR:-$nasqar2_home/runs/$SLURM_JOB_ID}"
r_library="${NASQAR_HOST_R_LIBRARY:-$nasqar2_home/r-library/R-4.3}"
# Root of the private-space projects of every application (projects/<project>/<App>)
projects_root="${NASQAR_HOST_PROJECT_ROOT:-$nasqar2_home/projects}"

if [[ ! -f "$sif" ]]; then
    echo "NASQAR2 image not found: $sif" >&2
    exit 1
fi

mkdir -p "$runtime_dir" "$r_library" "$projects_root"
chmod 700 "$r_library" "$projects_root"

# Input files the applications may read in place (mounted read-only): the
# directory containing NASQAR_HOST_HOME. A home directory is never mounted as a
# whole; an install directly under it exposes only NASQAR_HOST_HOME itself.
if [[ -n "${NASQAR_HOST_DATA_ROOT:-}" ]]; then
    data_root="$NASQAR_HOST_DATA_ROOT"
elif [[ "$(dirname "$nasqar2_home")" == "$HOME" ]]; then
    data_root="$nasqar2_home"
else
    data_root="$(dirname "$nasqar2_home")"
fi
if [[ ! -d "$data_root" ]]; then
    echo "NASQAR2 data root not found: $data_root" >&2
    exit 1
fi

args=(
    --cleanenv
    --env "SLURM_JOB_ID=$SLURM_JOB_ID"
    --env "SLURM_CPUS_PER_TASK=${SLURM_CPUS_PER_TASK:-1}"
    # Two cluster paths the applications cannot see from inside; Variant
    # Annotation shows them in its login-node install command.
    --env "NASQAR_HOST_R_LIBRARY=$r_library"
    --env "NASQAR_HOST_SIF=$sif"
)
# Every NASQAR_CONTAINER_* variable set at submission reaches the
# applications, for example NASQAR_CONTAINER_BASE_PATH (Open OnDemand) or
# NASQAR_CONTAINER_VARIANT_ANNOTATION_CHUNK.
for name in "${!NASQAR_CONTAINER_@}"; do
    args+=(--env "$name=${!name}")
done
# Each folder is mounted as "path on the cluster : path inside the container".
# What NASQAR2 writes sits at a fixed path under /runtime, so the launcher and
# saved projects do not depend on the site; the data root keeps its cluster
# path, so a path copied from a terminal works unchanged in the applications.
args+=(
    --env "NASQAR_CONTAINER_DATA_ROOT=$data_root"
    --bind "$runtime_dir:/runtime"
    --bind "$projects_root:/runtime/projects"
    --bind "$r_library:/runtime/R/library"
    --bind "$data_root:$data_root:ro"
)

# The launcher is part of the image; NASQAR_HOST_LAUNCHER runs another copy.
launcher=/opt/nasqar2-hpc/bin/nasqar2-hpc
if [[ -n "${NASQAR_HOST_LAUNCHER:-}" ]]; then
    if [[ ! -x "$NASQAR_HOST_LAUNCHER" ]]; then
        echo "NASQAR2 HPC launcher not found or not executable: $NASQAR_HOST_LAUNCHER" >&2
        exit 1
    fi
    args+=(--bind "$NASQAR_HOST_LAUNCHER:/tmp/nasqar2-hpc:ro")
    launcher=/tmp/nasqar2-hpc
fi

# The launcher picks a job-specific port (20000-39999) unless one is given.
launcher_args=()
if [[ -n "${NASQAR_HOST_PORT:-}" ]]; then
    launcher_args+=(--port "$NASQAR_HOST_PORT")
fi

# (written so that an empty list is accepted by bash older than 4.4)
singularity exec "${args[@]}" "$sif" "$launcher" ${launcher_args[@]+"${launcher_args[@]}"}
