#!/usr/bin/env bash
# Populate a private data root with a small public example data set for every
# NASQAR2 application, so a freshly set up private instance (Docker or Slurm)
# has something to try in each "private space" input.
#
#   common/fetch-example-data.sh <data-root>                   # Docker: nasqar2-all:latest
#   common/fetch-example-data.sh <data-root> --sif nasqar2.sif # Singularity image
#   common/fetch-example-data.sh <data-root> --skip-silva      # without the 220 MB taxonomy references
#
# Writes <data-root>/examples/<App>/... (about 140 MB, 360 MB with SILVA) and
# an examples/README.md naming every source. Downloads come from the public
# archives listed below; files that Bioconductor packages ship as example
# data are taken from the packages inside the NASQAR2 image, and the derived
# files (DESeq2 results, a Seurat object) are computed inside the image.
# Already-present files are not downloaded again.
#
# Sources
#   pasilla (Brooks et al. 2011, Drosophila RNA-seq)  Bioconductor data package
#   pbmc3k (10x Genomics, 2,700 PBMCs)               cf.10xgenomics.com
#   MiSeq SOP (Schloss lab, mouse gut 16S)            mothur.org
#   SILVA 138.1 DADA2-formatted references            zenodo.org/records/4587955
#   chr22 VCF excerpt                                 VariantAnnotation package extdata
#   GL1-GL4 ATAC-seq BAMs                             ATACseqQC package extdata

set -euo pipefail

usage() { sed -n '2,8p' "$0" | sed 's/^# \{0,1\}//'; exit "${1:-0}"; }

data_root="" image="nasqar2-all:latest" sif="" with_silva=1
while [[ $# -gt 0 ]]; do
    case "$1" in
        --image) image="$2"; shift 2 ;;
        --sif) sif="$2"; shift 2 ;;
        --skip-silva) with_silva=0; shift ;;
        -h|--help) usage ;;
        -*) echo "unknown option: $1" >&2; usage 1 ;;
        *) data_root="$1"; shift ;;
    esac
done
[[ -n "$data_root" ]] || usage 1
mkdir -p "$data_root"
data_root="$(cd "$data_root" && pwd)"
dest="$data_root/examples"
work="$dest/.work"
mkdir -p "$dest" "$work"
trap 'rm -rf "$work"' EXIT

pasilla_version="1.40.0"
pasilla_url="https://bioconductor.org/packages/release/data/experiment/src/contrib/pasilla_${pasilla_version}.tar.gz"
pbmc_url="https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz"
miseq_url="https://mothur.s3.us-east-2.amazonaws.com/wiki/miseqsopdata.zip"
silva_train_url="https://zenodo.org/records/4587955/files/silva_nr99_v138.1_train_set.fa.gz"
silva_species_url="https://zenodo.org/records/4587955/files/silva_species_assignment_v138.1.fa.gz"

say() { printf '\n== %s\n' "$*"; }
fetch() {  # fetch <url> <file>
    [[ -s "$2" ]] && { echo "  have $(basename "$2")"; return; }
    echo "  downloading $(basename "$2")"
    curl -fL --retry 3 --retry-delay 5 -o "$2.part" "$1" && mv "$2.part" "$2"
}

# Run a shell command inside the NASQAR2 image with the examples directory
# mounted at /examples, as the invoking user so the files stay yours.
in_image() {
    if [[ -n "$sif" ]]; then
        singularity exec --cleanenv --bind "$dest:/examples" "$sif" sh -c "$1"
    else
        docker run --rm --user "$(id -u):$(id -g)" -e HOME=/tmp -v "$dest:/examples" "$image" sh -c "$1"
    fi
}
R=/opt/conda/envs/merged_env/bin/Rscript
Rlib=/opt/conda/envs/merged_env/lib/R/library

# ---------------------------------------------------------------- pasilla
say "Bulk RNA-seq: pasilla (DESeq2Shiny, GeneCountMerger, mergeFPKMs, ORA, GSEA)"
mkdir -p "$dest/DESeq2Shiny" "$dest/GeneCountMerger" "$dest/mergeFPKMs" "$dest/ClusterProfShinyORA" "$dest/ClusterProfShinyGSEA"
fetch "$pasilla_url" "$work/pasilla.tar.gz"
tar -xzf "$work/pasilla.tar.gz" -C "$work" pasilla/inst/extdata/pasilla_gene_counts.tsv pasilla/inst/extdata/pasilla_sample_annotation.csv
counts="$dest/DESeq2Shiny/pasilla_gene_counts.tsv"
cp "$work/pasilla/inst/extdata/pasilla_gene_counts.tsv" "$counts"
# Metadata in the layout DESeq2Shiny expects: sample names matching the count
# columns in column 1, then the factors.
awk -F, 'NR == 1 { print "sample,condition,type"; next }
         { gsub(/"/, "", $0); sub(/fb$/, "", $1); print $1 "," $2 "," $3 }' \
    "$work/pasilla/inst/extdata/pasilla_sample_annotation.csv" > "$dest/DESeq2Shiny/pasilla_sample_annotation.csv"
# One two-column file per sample, as htseq-count writes them.
awk -F'\t' 'NR == 1 { for (i = 2; i <= NF; i++) name[i] = $i; next }
            { for (i = 2; i <= NF; i++) print $1 "\t" $i > ("'"$dest/GeneCountMerger/"'" name[i] ".txt") }' "$counts"
# Toy FPKM tracking files (counts per million, since pasilla has no lengths):
# the format mergeFPKMs reads, not real FPKM values.
awk -F'\t' 'NR == 1 { for (i = 2; i <= NF; i++) name[i] = $i; next }
            { for (i = 2; i <= NF; i++) { total[i] += $i; id[NR] = $1; v[NR, i] = $i } }
            END { for (i = 2; i <= NF; i++) { f = "'"$dest/mergeFPKMs/"'" name[i] ".genes.fpkm_tracking"
                      print "tracking_id\tgene_id\tgene_short_name\tFPKM" > f
                      for (r = 2; r <= NR; r++) printf "%s\t%s\t%s\t%.4f\n", id[r], id[r], id[r], v[r, i] * 1e6 / total[i] > f } }' "$counts"
echo "  derived per-sample count files and toy FPKM files"

if [[ ! -s "$dest/ClusterProfShinyORA/pasilla_treated_vs_untreated.csv" ]]; then
    echo "  running DESeq2 on pasilla inside the image (treated vs untreated)"
    cat > "$work/deseq2.R" <<'EOF'
suppressPackageStartupMessages(library(DESeq2))
counts <- as.matrix(read.delim("/examples/DESeq2Shiny/pasilla_gene_counts.tsv", row.names = 1))
meta <- read.csv("/examples/DESeq2Shiny/pasilla_sample_annotation.csv", row.names = 1)
meta <- meta[colnames(counts), , drop = FALSE]
dds <- DESeqDataSetFromMatrix(counts, meta, ~ type + condition)
dds <- DESeq(dds[rowSums(counts(dds)) >= 10, ], quiet = TRUE)
res <- as.data.frame(results(dds, contrast = c("condition", "treated", "untreated")))
res <- res[order(res$padj), ]
out <- cbind(X = rownames(res), res)   # same layout as a DESeq2Shiny download
write.csv(out, "/examples/ClusterProfShinyORA/pasilla_treated_vs_untreated.csv", row.names = FALSE)
EOF
    cp "$work/deseq2.R" "$dest/.deseq2.R"
    in_image "$R /examples/.deseq2.R" && rm -f "$dest/.deseq2.R"
    cp "$dest/ClusterProfShinyORA/pasilla_treated_vs_untreated.csv" "$dest/ClusterProfShinyGSEA/"
else
    echo "  have DESeq2 results"
fi

# ---------------------------------------------------------------- pbmc3k
say "Single cell: pbmc3k (SeuratV5, Monocle3)"
mkdir -p "$dest/SeuratV5" "$dest/Monocle3"
fetch "$pbmc_url" "$work/pbmc3k.tar.gz"
if [[ ! -s "$dest/SeuratV5/pbmc3k/hg19/matrix.mtx" ]]; then
    tar -xzf "$work/pbmc3k.tar.gz" -C "$work"
    mkdir -p "$dest/SeuratV5/pbmc3k"
    cp -r "$work/filtered_gene_bc_matrices/hg19" "$dest/SeuratV5/pbmc3k/"
fi
echo "  10x directory: SeuratV5/pbmc3k/hg19"
if [[ ! -s "$dest/Monocle3/pbmc3k_seurat.rds" ]]; then
    echo "  building a Seurat object inside the image (a minute or two)"
    cat > "$dest/.seurat.R" <<'EOF'
suppressPackageStartupMessages(library(Seurat))
set.seed(1)
counts <- Read10X("/examples/SeuratV5/pbmc3k/hg19")
pbmc <- CreateSeuratObject(counts, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc <- NormalizeData(pbmc, verbose = FALSE)
pbmc <- FindVariableFeatures(pbmc, nfeatures = 2000, verbose = FALSE)
pbmc <- ScaleData(pbmc, verbose = FALSE)
pbmc <- RunPCA(pbmc, npcs = 30, verbose = FALSE)
pbmc <- FindNeighbors(pbmc, dims = 1:10, verbose = FALSE)
pbmc <- FindClusters(pbmc, resolution = 0.5, verbose = FALSE)
pbmc <- RunUMAP(pbmc, dims = 1:10, verbose = FALSE)
saveRDS(pbmc, "/examples/Monocle3/pbmc3k_seurat.rds")
EOF
    in_image "$R /examples/.seurat.R" && rm -f "$dest/.seurat.R"
else
    echo "  have Monocle3/pbmc3k_seurat.rds"
fi

# ---------------------------------------------------------------- 16S
say "Amplicon: MiSeq SOP subset (DADA2)"
mkdir -p "$dest/DADA2/MiSeq_SOP"
fetch "$miseq_url" "$work/miseqsopdata.zip"
# Three of the twenty samples keep the run short.
for s in F3D0_S188 F3D141_S207 F3D142_S208; do
    for r in R1 R2; do
        f="${s}_L001_${r}_001.fastq"
        [[ -s "$dest/DADA2/MiSeq_SOP/$f" ]] || unzip -q -o -j "$work/miseqsopdata.zip" "MiSeq_SOP/$f" -d "$dest/DADA2/MiSeq_SOP"
    done
done
echo "  3 paired samples in DADA2/MiSeq_SOP"
if [[ $with_silva -eq 1 ]]; then
    fetch "$silva_train_url" "$dest/DADA2/silva_nr99_v138.1_train_set.fa.gz"
    fetch "$silva_species_url" "$dest/DADA2/silva_species_assignment_v138.1.fa.gz"
else
    echo "  SILVA references skipped (--skip-silva)"
fi

# ------------------------------------------------- package example data
say "ATAC-seq BAMs and chr22 VCF from Bioconductor package data (ATACseqQC, VariantAnnotation)"
mkdir -p "$dest/ATACseqQC" "$dest/VariantAnnotation"
# ATACseqQC pairs sample.bam with sample.bai.
in_image "for n in 1 2 3 4; do cp -n $Rlib/ATACseqQC/extdata/GL\$n.bam /examples/ATACseqQC/GL\$n.bam; cp -n $Rlib/ATACseqQC/extdata/GL\$n.bam.bai /examples/ATACseqQC/GL\$n.bai; done 2>/dev/null;
          cp -n $Rlib/VariantAnnotation/extdata/chr22.vcf.gz $Rlib/VariantAnnotation/extdata/chr22.vcf.gz.tbi /examples/VariantAnnotation/ 2>/dev/null; true"
echo "  ATACseqQC/GL1-4.bam, VariantAnnotation/chr22.vcf.gz"

# ---------------------------------------------------------------- README
cat > "$dest/README.md" <<EOF
# NASQAR2 example data

Small public data sets, one directory per application, for trying the
"private space" inputs. Created by common/fetch-example-data.sh on $(date +%F).

| Directory | Use in | Data | Source |
| --- | --- | --- | --- |
| DESeq2Shiny/ | DESeq2Shiny: counts file + metadata file | pasilla gene counts (7 Drosophila samples, RNAi knockdown of pasilla; Brooks et al. 2011) | Bioconductor pasilla $pasilla_version |
| GeneCountMerger/ | Gene Count Merger: per-sample count files | the same counts, one htseq-count style file per sample | derived |
| mergeFPKMs/ | mergeFPKMs: *.genes.fpkm_tracking files | the same counts as counts-per-million in cufflinks layout (toy values, not real FPKM) | derived |
| ClusterProfShinyORA/, ClusterProfShinyGSEA/ | ORA / GSEA: DE results table (organism Drosophila, FLYBASE ids) | DESeq2 treated vs untreated on pasilla | derived in the NASQAR2 image |
| SeuratV5/pbmc3k/hg19 | Seurat v5: 10x directory | 2,700 PBMCs, Cell Ranger 1.1 filtered matrix | 10x Genomics pbmc3k |
| Monocle3/pbmc3k_seurat.rds | Monocle 3: Seurat object | pbmc3k after the standard Seurat pipeline (PCA, clusters, UMAP) | derived in the NASQAR2 image |
| DADA2/MiSeq_SOP | DADA2: FASTQ directory | 3 paired-end samples of the mothur MiSeq SOP (mouse gut 16S V4) | mothur.org |
| DADA2/silva_*.fa.gz | DADA2: taxonomy references | SILVA 138.1 train set and species assignment | zenodo.org/records/4587955 |
| ATACseqQC/ | ATACseqQC: BAM directory | GL1-GL4 (chr1 excerpts) | ATACseqQC package extdata |
| VariantAnnotation/chr22.vcf.gz | Variant Annotation: VCF (hg19) | 1000 Genomes chr22 excerpt | VariantAnnotation package extdata |
EOF

say "Done: $dest ($(du -sh "$dest" | cut -f1))"
