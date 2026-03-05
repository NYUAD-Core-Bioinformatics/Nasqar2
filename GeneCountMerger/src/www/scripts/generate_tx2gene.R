# =============================================================================
# generate_tx2gene.R
#
# Build a transcript-to-gene mapping file (tx2gene) from a GTF file.
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
# No extra packages required – mapping is parsed directly from the GTF file.
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
# Parse GTF → tx2gene data.frame
# ---------------------------------------------------------------------------
message("Reading: ", gtf_path)
lines <- readLines(gtf_path, warn = FALSE)
lines <- lines[!startsWith(lines, "#")]        # drop comment lines
lines <- lines[grepl("\ttranscript\t", lines)] # keep transcript feature rows only
message("Transcript rows: ", length(lines))

extract_attr <- function(attr_str, key) {
    pattern <- paste0(key, ' "([^"]+)"')
    m <- regmatches(attr_str, regexpr(pattern, attr_str))
    ifelse(length(m) > 0, sub(paste0(key, ' "([^"]+)"'), "\\1", m), NA_character_)
}

attr_col <- sub(".*\t", "", lines)
TX_NAME  <- extract_attr(attr_col, "transcript_id")
GENE_ID  <- extract_attr(attr_col, "gene_id")

tx2gene <- data.frame(TX_NAME = TX_NAME, GENE_ID = GENE_ID, stringsAsFactors = FALSE)
tx2gene <- unique(tx2gene)
tx2gene <- tx2gene[!is.na(tx2gene$TX_NAME) & tx2gene$TX_NAME != "" &
                   !is.na(tx2gene$GENE_ID) & tx2gene$GENE_ID != "", ]

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
