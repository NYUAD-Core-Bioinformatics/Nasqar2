# Open OnDemand integration

NASQAR2 must be exposed through Open OnDemand's standard `/rnode` reverse
proxy. Do not place the included Nginx/Shiny stack behind an HTTP-only reverse
proxy: Shiny requires an HTTP/1.1 WebSocket upgrade.

## Administrator changes

1. Replace the NASQAR2 Batch Connect app's `before.sh.erb` and `after.sh.erb`
   with the examples in this directory, or make the equivalent changes in the
   existing templates. The `after.sh` check must not wait for `proxy_port`.
2. Write `app_port` directly to `connection.yml`. Do not allocate a second
   `proxy_port`, and do not start `authrevproxy.py`.
3. Export `NASQAR_CONTAINER_BASE_PATH=/rnode/${host}/${port}` into the job environment.
4. Start the container on that port. The simplest way is the supplied job
   script, which passes every `NASQAR_CONTAINER_*` variable into the container and sets
   up the private space:

   ```bash
   export NASQAR_HOST_SIF=/path/to/nasqar2.sif
   export NASQAR_HOST_HOME="$SCRATCH/nasqar2"
   export NASQAR_HOST_PORT="${port}"
   bash /path/to/nasqar2.sbatch
   ```

   A hand-written command needs these options at least; the launcher inside
   the image sets everything else:

   ```bash
   home="$SCRATCH/nasqar2"
   mkdir -p "$home/runs/$SLURM_JOB_ID" "$home/projects" "$home/r-library/R-4.3"
   singularity run --cleanenv \
     --env "SLURM_JOB_ID=$SLURM_JOB_ID" \
     --env "SLURM_CPUS_PER_TASK=$SLURM_CPUS_PER_TASK" \
     --env "NASQAR_CONTAINER_BASE_PATH=$NASQAR_CONTAINER_BASE_PATH" \
     --env "NASQAR_CONTAINER_DATA_ROOT=$SCRATCH" \
     --bind "$home/runs/$SLURM_JOB_ID:/runtime" \
     --bind "$home/projects:/runtime/projects" \
     --bind "$home/r-library/R-4.3:/runtime/R/library" \
     --bind "$SCRATCH:$SCRATCH:ro" \
     /path/to/nasqar2.sif --port "${port}"
   ```

   Without `NASQAR_CONTAINER_DATA_ROOT` the applications start as a public instance:
   browser upload only, no projects and no Browse button.

The launcher generates a session-specific UI under the writable runtime
directory. The immutable image and its root UI are not modified.

## Verification

After starting an OOD session, verify these URLs in the same browser tab:

- `/rnode/<node>/<port>/`
- `/rnode/<node>/<port>/_next/static/...`
- `/rnode/<node>/<port>/GeneCountMerger/`
- `/rnode/<node>/<port>/GeneCountMerger/websocket/` (HTTP 101 while connected)

The first page should be styled, documentation links must retain the `/rnode`
prefix, and Shiny pages must not display the gray disconnected overlay.

## Private Slurm and Docker runs

Do not set `NASQAR_CONTAINER_BASE_PATH` for SSH-tunnel, standalone Docker, or existing
private Slurm deployments. They continue to use `/` and the root UI exactly as
before.
