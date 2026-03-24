# =============================================================================
# generate_tx2gene.R
#
# Build a transcript-to-gene mapping file (tx2gene) from a GTF file.
# Uses GenomicFeatures::makeTxDbFromGFF() and AnnotationDbi::select() rather
# than manual GTF parsing, following the approach described at:
# https://aaronmitchd.github.io/RNA_Seq/kallisto_h5_to_tximport_to_DESeq2_eval.html
#
# Two output files are written from the same data:
#   <output_prefix>.Rda  – R data file for use with the Gene Count Merger app
#                          (data.frame named 'tx2gene')
#   <output_prefix>.csv  – plain CSV for inspection or custom upload into the app
#
# Both files contain two columns:
#   TX_NAME  – Ensembl transcript ID  (e.g. ENSMUST00000000001)
#   GENE_ID  – Ensembl gene ID        (e.g. ENSMUSG00000000001)
#
# Dependencies:
#   BiocManager, GenomicFeatures, AnnotationDbi
#   Install once with (conda):
#     conda install -c bioconda -c conda-forge bioconductor-genomicfeatures bioconductor-annotationdbi
#   Or with R/BiocManager:
#     install.packages("BiocManager")
#     BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))
#
# Usage:
#   Rscript generate_tx2gene.R <input.gtf> <output_prefix>
#
# Example:
#   Rscript generate_tx2gene.R Homo_sapiens.GRCh38.110.gtf Homo_sapiens.GRCh38.110
#   # produces: Homo_sapiens.GRCh38.110.Rda
#   #           Homo_sapiens.GRCh38.110.csv
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
    cat("Usage: Rscript generate_tx2gene.R <input.gtf> <output_prefix>\n")
    cat("  input.gtf      – path to the GTF annotation file\n")
    cat("  output_prefix  – prefix for output files (without extension)\n")
    cat("                   produces <prefix>.Rda and <prefix>.csv\n")
    quit(status = 1)
}

gtf_path   <- args[1]
out_prefix <- args[2]

if (!file.exists(gtf_path)) {
    stop("GTF file not found: ", gtf_path)
}

out_dir <- dirname(out_prefix)
if (out_dir != "." && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------------------------------------------------------------------------
# Install / load dependencies
# ---------------------------------------------------------------------------
if (!require("BiocManager",      quietly = TRUE)) install.packages("BiocManager")
if (!require("GenomicFeatures",  quietly = TRUE)) BiocManager::install("GenomicFeatures")
if (!require("AnnotationDbi",    quietly = TRUE)) BiocManager::install("AnnotationDbi")

library(GenomicFeatures)
library(AnnotationDbi)

# ---------------------------------------------------------------------------
# Build TxDb → extract tx2gene mapping
# ---------------------------------------------------------------------------
message("Building TxDb from: ", gtf_path)
txdb <- makeTxDbFromGFF(gtf_path)

k       <- keys(txdb, keytype = "TXNAME")
tx_map  <- AnnotationDbi::select(txdb, keys = k, columns = "GENEID", keytype = "TXNAME")

tx2gene <- tx_map
colnames(tx2gene) <- c("TX_NAME", "GENE_ID")
tx2gene <- unique(tx2gene)
tx2gene <- tx2gene[!is.na(tx2gene$TX_NAME) & tx2gene$TX_NAME != "" &
                   !is.na(tx2gene$GENE_ID)  & tx2gene$GENE_ID  != "", ]

message("Unique transcript-gene pairs: ", nrow(tx2gene))

# ---------------------------------------------------------------------------
# Write outputs
# ---------------------------------------------------------------------------
rda_path <- paste0(out_prefix, ".Rda")
csv_path <- paste0(out_prefix, ".csv")

save(tx2gene, file = rda_path)
message("Saved Rda: ", rda_path)

write.csv(tx2gene, file = csv_path, row.names = FALSE, quote = FALSE)
message("Saved csv: ", csv_path)
