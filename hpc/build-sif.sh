#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output="${1:-$repo_root/hpc/build/nasqar2.sif}"
definition="$repo_root/hpc/singularity/nasqar2-local.def"

mkdir -p "$(dirname "$output")"

cd "$repo_root"
singularity build --fakeroot --no-setgroups --force "$output" "$definition"

echo "Built: $output"
singularity inspect "$output"
