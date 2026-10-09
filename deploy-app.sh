#!/usr/bin/env bash
# Hot-deploy one or more applications into running nasqar2-all containers
# without rebuilding the image: copy the application directory from this
# checkout over /srv/shiny-server/<app> and restart only that app's
# supervisord program. Nginx and the other apps keep running.
#
#   ./deploy-app.sh deseq2shiny                      # all running nasqar2-all containers
#   ./deploy-app.sh deseq2shiny dada2Shiny -c nasqar2-private
#
# Only files git knows about (tracked, or untracked but not ignored) are
# shipped, so local junk such as .RData, *.bak or Rplots.pdf stays out, and
# runtime data unpacked inside the image (www/exampleData, taxonomy
# databases) is left untouched. Files deleted in the checkout are not
# removed from the container; rebuild the image for that. The image itself
# is not changed: recreating a container reverts to the image's copy.

set -euo pipefail

usage() { sed -n '2,15p' "$0" | sed 's/^# \{0,1\}//'; exit "${1:-0}"; }

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
apps=()
containers=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        -c|--container) containers+=("$2"); shift 2 ;;
        -h|--help) usage ;;
        -*) echo "unknown option: $1" >&2; usage 1 ;;
        *) apps+=("${1%/}"); shift ;;
    esac
done
[[ ${#apps[@]} -gt 0 ]] || usage 1

# Default: every running container started with `docker run nasqar2-all...`
# (Config.Image keeps that reference even after the tag moves to a rebuilt
# image, which makes `--filter ancestor` and `docker ps` image names miss
# containers from the previous build) that actually runs the stack (a scanner
# or a one-off `docker run` of the image has no supervisord and is skipped).
if [[ ${#containers[@]} -eq 0 ]]; then
    for name in $(docker ps -q | xargs -r docker inspect --format '{{.Name}} {{.Config.Image}}' |
                      awk '$2 ~ /^nasqar2-all(:|$)/ {sub("^/", "", $1); print $1}'); do
        docker exec "$name" test -S /var/run/supervisor.sock 2>/dev/null && containers+=("$name")
    done
    [[ ${#containers[@]} -gt 0 ]] || { echo "no running nasqar2-all container with supervisord; pass -c <name>" >&2; exit 1; }
fi

for app in "${apps[@]}"; do
    [[ -d "$repo_root/$app/src" ]] || { echo "not an application directory: $app" >&2; exit 1; }
    grep -q "^\[program:$app\]" "$repo_root/supervisord.conf" ||
        { echo "no [program:$app] in supervisord.conf" >&2; exit 1; }
done

bundle="$(mktemp -t nasqar2-deploy-XXXXXX.tar)"
trap 'rm -f "$bundle"' EXIT
(cd "$repo_root" && git ls-files -z --cached --others --exclude-standard -- "${apps[@]}" |
     tar --null -T - -cf "$bundle")

status=0
for container in "${containers[@]}"; do
    echo "== $container"
    docker cp "$bundle" "$container:/tmp/nasqar2-deploy.tar" || { status=1; continue; }
    docker exec "$container" sh -c '
        set -e
        tar -xf /tmp/nasqar2-deploy.tar -C /srv/shiny-server --no-same-owner
        rm -f /tmp/nasqar2-deploy.tar
        for app in "$@"; do
            chmod -R a+rX "/srv/shiny-server/$app"
            supervisorctl restart "$app"
        done
        sleep 8
        supervisorctl status "$@"' sh "${apps[@]}" || status=1
done
exit $status
