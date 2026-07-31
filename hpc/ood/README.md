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
3. Export `NASQAR_BASE_PATH=/rnode/${host}/${port}` into the job environment.
4. Pass it through Singularity/Apptainer:

   ```bash
   singularity exec --cleanenv \
     --env "NASQAR_BASE_PATH=${NASQAR_BASE_PATH}" \
     ... nasqar2.sif .../nasqar2-hpc --port "${port}" --runtime /runtime
   ```

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

Do not set `NASQAR_BASE_PATH` for SSH-tunnel, standalone Docker, or existing
private Slurm deployments. They continue to use `/` and the root UI exactly as
before.
