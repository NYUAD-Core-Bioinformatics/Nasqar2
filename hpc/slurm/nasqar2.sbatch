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
module load singularity/4.2.0

sif="${NASQAR2_SIF:-$SCRATCH/nasqar2/images/nasqar2.sif}"
launcher="${NASQAR2_LAUNCHER:-$SCRATCH/nasqar2/bin/nasqar2-hpc}"
runtime_dir="${NASQAR2_RUNTIME_DIR:-$SCRATCH/nasqar2/runs/$SLURM_JOB_ID}"
port="${NASQAR2_PORT:-$((20000 + SLURM_JOB_ID % 20000))}"
data_root="${NASQAR2_DATA_ROOT:-$SCRATCH}"
r_library="${NASQAR2_R_LIBRARY:-$SCRATCH/nasqar2/r-library/R-4.3}"
atac_cache="${NASQAR2_ATAC_CACHE:-$SCRATCH/nasqar2/cache/ATACseqQC}"
deseq2_states="${NASQAR2_DESEQ2_STATES:-$SCRATCH/nasqar2/states/DESeq2Shiny}"
dada2_projects="${NASQAR2_DADA2_PROJECTS:-$SCRATCH/nasqar2/projects/DADA2}"
seurat_projects="${NASQAR2_SEURAT_PROJECTS:-$SCRATCH/nasqar2/projects/SeuratV5}"
monocle3_projects="${NASQAR2_MONOCLE3_PROJECTS:-$SCRATCH/nasqar2/projects/Monocle3}"
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

mkdir -p "$runtime_dir/R/library" "$r_library" "$atac_cache" "$deseq2_states" \
    "$dada2_projects" "$seurat_projects" "$monocle3_projects" \
    "$node_tmp/DADA2" "$exchange_dir"
chmod 700 "$r_library" "$atac_cache" "$deseq2_states" \
    "$dada2_projects" "$seurat_projects" "$monocle3_projects" "$exchange_dir"

singularity exec --cleanenv \
    --env "SLURM_JOB_ID=$SLURM_JOB_ID" \
    --env "SLURM_CPUS_PER_TASK=$SLURM_CPUS_PER_TASK" \
    --env "NASQAR_BASE_PATH=${NASQAR_BASE_PATH:-}" \
    --env "NASQAR_DATA_ROOT=$data_root" \
    --env "NASQAR_EXCHANGE_DIR=/runtime/exchange" \
    --env "NASQAR_ATAC_CACHE_DIR=/runtime/cache/ATACseqQC" \
    --env "NASQAR_DESEQ2_STATE_DIR=/runtime/states/DESeq2Shiny" \
    --env "NASQAR_DADA2_PROJECT_DIR=/runtime/projects/DADA2" \
    --env "NASQAR_DADA2_WORK_DIR=/runtime/node-tmp/DADA2" \
    --env "NASQAR_SEURAT_PROJECT_DIR=/runtime/projects/SeuratV5" \
    --env "NASQAR_MONOCLE3_PROJECT_DIR=/runtime/projects/Monocle3" \
    --env "R_LIBS_USER=/runtime/R/library" \
    --bind "$runtime_dir:/runtime" \
    --bind "$atac_cache:/runtime/cache/ATACseqQC" \
    --bind "$deseq2_states:/runtime/states/DESeq2Shiny" \
    --bind "$dada2_projects:/runtime/projects/DADA2" \
    --bind "$seurat_projects:/runtime/projects/SeuratV5" \
    --bind "$monocle3_projects:/runtime/projects/Monocle3" \
    --bind "$node_tmp:/runtime/node-tmp" \
    --bind "$r_library:/runtime/R/library" \
    --bind "$data_root:$data_root:ro" \
    --bind "$launcher:/tmp/nasqar2-hpc:ro" \
    "$sif" \
    /tmp/nasqar2-hpc \
    --port "$port" \
    --runtime /runtime
