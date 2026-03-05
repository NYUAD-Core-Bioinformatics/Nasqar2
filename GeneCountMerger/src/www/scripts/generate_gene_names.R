# =============================================================================
# generate_gene_names.R
#
# Extract a gene ID → gene name lookup table from a GTF file.
#
# Two output files are written from the same data:
#   <output_prefix>.csv  – plain CSV for upload into the Gene Count Merger app
#   <output_prefix>.Rda  – R data file (data.frame named 'geneid2name')
#
# Both files contain two columns:
#   GENE_ID    – Ensembl gene ID   (e.g. ENSG00000139618)
#   GENE_NAME  – gene symbol       (e.g. BRCA2)
#
# Dependencies:
#   BiocManager, rtracklayer, tidyverse
#   Install once with:
#     install.packages("BiocManager")
#     BiocManager::install("rtracklayer")
#     install.packages("tidyverse")
#
# Usage:
#   Rscript generate_gene_names.R <input.gtf> <output_prefix>
#
# Example:
#   Rscript generate_gene_names.R Homo_sapiens.GRCh38.110.gtf Homo_sapiens.GRCh38.110
#   # produces: Homo_sapiens.GRCh38.110.csv
#   #           Homo_sapiens.GRCh38.110.Rda
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
    cat("Usage: Rscript generate_gene_names.R <input.gtf> <output_prefix>\n")
    cat("  input.gtf      – path to the GTF annotation file\n")
    cat("  output_prefix  – prefix for output files (without extension)\n")
    cat("                   produces <prefix>.csv and <prefix>.Rda\n")
    quit(status = 1)
}

gtf_file_path <- args[1]
out_prefix    <- args[2]

if (!file.exists(gtf_file_path)) {
    stop("GTF file not found: ", gtf_file_path)
}

out_dir <- dirname(out_prefix)
if (out_dir != "." && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# Install / load dependencies
# ---------------------------------------------------------------------------
if (!require("BiocManager",  quietly = TRUE)) install.packages("BiocManager")
if (!require("rtracklayer", quietly = TRUE)) BiocManager::install("rtracklayer")
if (!require("tidyverse",   quietly = TRUE)) install.packages("tidyverse")

library(rtracklayer)
library(tidyverse)

# ---------------------------------------------------------------------------
# Parse GTF → geneid2name data.frame
# ---------------------------------------------------------------------------
message("Reading: ", gtf_file_path)
gtf <- import(gtf_file_path)

geneid2name <- as.data.frame(mcols(gtf)[, c("gene_id", "gene_name")]) %>%
    na.omit() %>%
    unique()

colnames(geneid2name) <- c("GENE_ID", "GENE_NAME")

message("Unique gene id-name pairs: ", nrow(geneid2name))

# ---------------------------------------------------------------------------
# Write outputs
# ---------------------------------------------------------------------------
csv_path <- paste0(out_prefix, ".csv")
rda_path <- paste0(out_prefix, ".Rda")

write_csv(geneid2name, csv_path)
message("Saved csv: ", csv_path)

save(geneid2name, file = rda_path)
message("Saved Rda: ", rda_path)
