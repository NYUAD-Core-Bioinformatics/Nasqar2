#!/bin/sh
# Run inside the image after the R environment is built (see Dockerfile).
# Remove sourceMappingURL comments from minified JS/CSS whose map file is not
# shipped by its R package (pickr in colourpicker, numbro in rhandsontable),
# so browsers stop requesting maps that do not exist.
lib=/opt/conda/envs/merged_env/lib/R/library
find "$lib" -type f \( -name '*.min.js' -o -name '*.min.css' \) | while read -r f; do
    map=$(grep -o 'sourceMappingURL=[^ *]*' "$f" | tail -1 | cut -d= -f2)
    [ -n "$map" ] || continue
    [ -e "$(dirname "$f")/$map" ] && continue
    sed -i -e 's#//\# sourceMappingURL=[^ ]*##' -e 's#/\*\# sourceMappingURL=[^ ]* \*/##' "$f"
    echo "stripped: ${f#$lib/}"
done
