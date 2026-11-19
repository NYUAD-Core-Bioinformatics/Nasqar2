#!/usr/bin/env bash
# Download the built-in example data and reference files that the NASQAR2
# applications read from their own www/ directories. These files are kept out
# of git; this script puts them back, both in a fresh checkout and in the
# Docker image (the root Dockerfile runs it in its own cached layer).
#
#   common/fetch-app-data.sh                       # into this checkout
#   common/fetch-app-data.sh --dest /srv/shiny-server
#   common/fetch-app-data.sh --zip /path/data.zip  # use a local copy, no download
#   common/fetch-app-data.sh --skip-silva          # without the 216 MB taxonomy references
#
# Writes <dest>/<app>/src/www/exampleData (gene_names and tx2gene for Gene
# Count Merger) from the data.zip release asset, and the SILVA 138.1
# references into <dest>/dada2Shiny/src/www/taxonomy. Directories and files
# that are already present are left alone, so it is safe to run again.
#
# Sources
#   data.zip                github.com/NYUAD-Core-Bioinformatics/Nasqar2, release data-v1
#   SILVA 138.1 references  zenodo.org/records/4587955

set -euo pipefail

usage() { sed -n '2,15p' "$0" | sed 's/^# \{0,1\}//'; exit "${1:-0}"; }

dest="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
url="${NASQAR_DATA_URL:-https://github.com/NYUAD-Core-Bioinformatics/Nasqar2/releases/download/data-v1/data.zip}"
sha256="${NASQAR_DATA_SHA256:-ca37041d16855f61cc20ed1be5e5c8113bd12c9911463f5d5d68a831ebb034d7}"
zip="" with_silva=1
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dest) dest="$2"; shift 2 ;;
        --url) url="$2"; shift 2 ;;
        --sha256) sha256="$2"; shift 2 ;;
        --zip) zip="$2"; shift 2 ;;
        --skip-silva) with_silva=0; shift ;;
        -h|--help) usage ;;
        *) echo "unknown option: $1" >&2; usage 1 ;;
    esac
done
mkdir -p "$dest"
dest="$(cd "$dest" && pwd)"

silva_base="https://zenodo.org/records/4587955/files"
silva_files=(silva_nr99_v138.1_train_set.fa.gz silva_species_assignment_v138.1.fa.gz)

# <directory inside data.zip>:<application directory>:<entry that marks it as present>
apps=(
    "data/dada2Shiny:dada2Shiny:exampleData"
    "data/deseq2shiny:deseq2shiny:exampleData"
    "data/ClusterProfShinyORA:ClusterProfShinyORA:exampleData"
    "data/ClusterProfShinyGSEA:ClusterProfShinyGSEA:exampleData"
    "data/GeneCountMerger:GeneCountMerger:gene_names"
    "data/NASQAR/tsar_nasqar:tsar_nasqar:exampleData"
    "data/NASQAR/mergeFPKMs:mergeFPKMs:exampleData"
    "data/ATACseqQCShniy:ATACseqQCShniy:exampleData"
)

fetch() {  # fetch <url> <file>
    echo "  downloading $(basename "$2")"
    if command -v curl >/dev/null 2>&1; then
        curl -fL --retry 3 --retry-delay 5 -o "$2.part" "$1"
    else
        wget -q --tries=3 --waitretry=5 -O "$2.part" "$1"
    fi
    mv "$2.part" "$2"
}

extract() {  # extract <zip> <directory>
    if command -v unzip >/dev/null 2>&1; then
        unzip -q -o "$1" -d "$2"
    else
        python3 -m zipfile -e "$1" "$2"
    fi
}

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

missing=0
for entry in "${apps[@]}"; do
    IFS=: read -r _ app marker <<<"$entry"
    [[ -e "$dest/$app/src/www/$marker" ]] || missing=1
done

if [[ $missing -eq 1 ]]; then
    echo "== example data"
    if [[ -z "$zip" ]]; then
        zip="$work/data.zip"
        fetch "$url" "$zip"
        echo "$sha256  $zip" | sha256sum -c - >/dev/null ||
            { echo "checksum mismatch for $url" >&2; exit 1; }
    fi
    extract "$zip" "$work/unpacked"
    for entry in "${apps[@]}"; do
        IFS=: read -r src app _ <<<"$entry"
        mkdir -p "$dest/$app/src/www"
        for item in "$work/unpacked/$src/www"/*; do
            name="$(basename "$item")"
            if [[ -e "$dest/$app/src/www/$name" ]]; then
                echo "  have $app/src/www/$name"
            else
                cp -r "$item" "$dest/$app/src/www/$name"
                echo "  wrote $app/src/www/$name"
            fi
        done
    done
else
    echo "== example data: already present"
fi

if [[ $with_silva -eq 1 ]]; then
    echo "== SILVA 138.1 taxonomy references"
    mkdir -p "$dest/dada2Shiny/src/www/taxonomy"
    for f in "${silva_files[@]}"; do
        if [[ -s "$dest/dada2Shiny/src/www/taxonomy/$f" ]]; then
            echo "  have $f"
        else
            fetch "$silva_base/$f" "$dest/dada2Shiny/src/www/taxonomy/$f"
        fi
    done
fi

echo "done: $dest"
