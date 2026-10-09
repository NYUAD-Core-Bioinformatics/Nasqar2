#!/usr/bin/env bash
# Run a single application from the nasqar2-all image, with this checkout's
# source bind-mounted, for development: no nginx, no other apps, and edits to
# ui.R / server-*.R are picked up on the next browser reload (global.R and
# package changes need a restart: run the script again).
#
#   ./run-app.sh deseq2shiny            # http://localhost:4001/
#   ./run-app.sh dada2Shiny 4010        # pick the host port
#   ./run-app.sh deseq2shiny --private  # with the private space of ~/nasqar2-private
#
# Stop with Ctrl-C (the container is removed), or `docker rm -f nasqar2-dev-<app>`.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
app="" port="" private=0
for arg in "$@"; do
    case "$arg" in
        --private) private=1 ;;
        -h|--help) sed -n '2,11p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        [0-9]*) port="$arg" ;;
        *) app="${arg%/}" ;;
    esac
done
[[ -n "$app" && -d "$repo_root/$app/src" ]] || { echo "usage: $0 <app> [port] [--private]" >&2; exit 1; }

# The app's port inside the image comes from supervisord.conf.
app_port="$(awk -v app="$app" '
    $0 == "[program:" app "]" { found = 1 }
    found && match($0, /port=([0-9]+)/) { print substr($0, RSTART + 5, RLENGTH - 5); exit }' \
    "$repo_root/supervisord.conf")"
[[ -n "$app_port" ]] || { echo "no [program:$app] with a port in supervisord.conf" >&2; exit 1; }
port="${port:-$app_port}"

name="nasqar2-dev-$app"
docker rm -f "$name" >/dev/null 2>&1 || true

mounts=(-v "$repo_root/$app/src:/srv/shiny-server/$app/src")
env=()
if [[ $private -eq 1 ]]; then
    private_root="${NASQAR_PRIVATE_ROOT:-$HOME/nasqar2-private}"
    mounts+=(-v "$private_root:/private")
    env+=(-e NASQAR_PRIVATE_ROOT=/private -e NASQAR_DATA_ROOT=/private/data -e NASQAR_PROJECT_ROOT=/private/projects)
fi

tty=(); [[ -t 0 ]] && tty=(-it)
echo "$app at http://localhost:$port/  (container $name; Ctrl-C to stop)"
exec docker run --rm "${tty[@]}" --name "$name" -p "127.0.0.1:$port:$app_port" "${mounts[@]}" "${env[@]}" \
    nasqar2-all:latest \
    /opt/conda/envs/merged_env/bin/R -e "shiny::runApp('/srv/shiny-server/$app/src', port=$app_port, host='0.0.0.0')"
