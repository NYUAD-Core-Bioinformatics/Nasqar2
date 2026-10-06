#!/usr/bin/env bash
# Copy common/nasqar-project.R into every NASQAR2-owned application's src/.
# Per-application Docker images only see their own directory, so each app
# carries a copy; this script keeps them identical. Run after editing the
# common file; CI/tests can run it with --check to verify nothing drifted.

set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source="$repo_root/common/nasqar-project.R"
apps=(variantAnnotationShiny deseq2shiny dada2Shiny SeuratV5Shiny monocle3 ATACseqQCShniy
      ClusterProfShinyORA ClusterProfShinyGSEA GeneCountMerger mergeFPKMs)

status=0
for app in "${apps[@]}"; do
    target="$repo_root/$app/src/nasqar-project.R"
    if [[ "${1:-}" == "--check" ]]; then
        if ! cmp -s "$source" "$target"; then
            echo "out of sync: $target"
            status=1
        fi
    else
        cp "$source" "$target"
        echo "synced $target"
    fi
done
exit $status
