#!/usr/bin/env bash
# Build the Singularity image (SIF) from the local nasqar2-all:latest Docker
# image.
#
#   ./hpc/build-sif.sh                      # -> hpc/build/nasqar2.sif
#   ./hpc/build-sif.sh /path/to/nasqar2.sif
#
# The build needs roughly 25 GB of temporary space. It is taken under
# hpc/build by default; set NASQAR2_BUILD_DIR to use another filesystem.
#
# The image is exported from the Docker daemon into a sandbox directory and
# the SIF is built from that, rather than letting Singularity pull from the
# daemon: the latter stages the whole image under /var/lib/docker/tmp, which
# is often too small, and fails with "no space left on device". The result
# is identical in content.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output="${1:-$repo_root/hpc/build/nasqar2.sif}"
build_dir="${NASQAR2_BUILD_DIR:-$repo_root/hpc/build}"
definition="$repo_root/hpc/singularity/nasqar2-local.def"
image="nasqar2-all:latest"

mkdir -p "$(dirname "$output")" "$build_dir"/{tmp,cache,sandbox}
export TMPDIR="$build_dir/tmp"
export SINGULARITY_TMPDIR="$build_dir/tmp"
export SINGULARITY_CACHEDIR="$build_dir/cache"
export APPTAINER_TMPDIR="$build_dir/tmp"
export APPTAINER_CACHEDIR="$build_dir/cache"

sandbox="$build_dir/sandbox"
sandbox_def="$build_dir/nasqar2-sandbox.def"
cleanup() { rm -rf "$sandbox" "$sandbox_def"; }
trap cleanup EXIT

echo "Exporting $image to $sandbox"
rm -rf "$sandbox" && mkdir -p "$sandbox"
cid="$(docker create "$image")"
docker export "$cid" | tar -x -C "$sandbox"
docker rm "$cid" >/dev/null

# Same definition, bootstrapped from the exported filesystem.
sed "s#^Bootstrap: docker-daemon\$#Bootstrap: localimage#; s#^From: .*#From: $sandbox#" \
    "$definition" > "$sandbox_def"

# Run from the repository root so the definition's %files entry resolves.
cd "$repo_root"
singularity build --fakeroot --no-setgroups --force "$output" "$sandbox_def"

echo "Built: $output"
singularity inspect "$output"
