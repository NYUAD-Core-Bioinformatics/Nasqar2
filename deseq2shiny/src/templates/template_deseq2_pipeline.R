################################################################################
# Complete DESeq2 Analysis Pipeline
# Generated from DESeq2Shiny
# 
# This script can be run independently by editing the configuration parameters below
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Data input files
COUNTS_FILE <- "{{counts_filename}}"
METADATA_FILE <- "{{metadata_filename}}"

# Design formula for DESeq2 analysis
DESIGN_FORMULA <- {{design_formula}}

# Gene prefiltering (matches Shiny app "Config & Prefilter" step)
PREFILTER_APPLIED <- {{prefilter_applied}}  # Whether prefiltering was applied in Shiny
PREFILTER_THRESHOLD <- {{prefilter_threshold}}  # Minimum total counts threshold (sum across all samples)

# Statistical significance threshold
ALPHA <- {{alpha}}

# Contrast definitions (list of contrasts to test)
# Format: list(c("factor", "numerator", "denominator"))
CONTRASTS <- {{contrasts}}

################################################################################
# Load Required Libraries
################################################################################

cat("Loading required libraries...\n")
suppressPackageStartupMessages({
  library(DESeq2)
  library(ggplot2)
  library(pheatmap)
  library(RColorBrewer)
  library(dplyr)
  library(tidyr)
})
cat("Libraries loaded successfully!\n\n")

################################################################################
# Step 1: Load Data
################################################################################

cat("Step 1: Loading count data and metadata...\n")

# Load count matrix (genes x samples)
counts_data_raw <- read.csv(COUNTS_FILE, check.names = FALSE, stringsAsFactors = FALSE)
cat("  - Loaded count data file:", nrow(counts_data_raw), "rows ×", ncol(counts_data_raw), "columns\n")

# Check if gene.names column exists and preserve it
if ("gene.names" %in% colnames(counts_data_raw)) {
  cat("  - Detected gene.names column - preserving gene annotations\n")
  gene_names <- counts_data_raw$gene.names
  gene_ids <- counts_data_raw[, 1]
  
  # Extract numeric count columns only (skip gene.id and gene.names)
  count_cols <- setdiff(colnames(counts_data_raw), c(colnames(counts_data_raw)[1], "gene.names"))
  counts_data <- counts_data_raw[, count_cols, drop = FALSE]
  rownames(counts_data) <- gene_ids
  
  # Store gene annotations for later use
  gene_annotations <- data.frame(
    gene.id = gene_ids,
    gene.names = gene_names,
    row.names = gene_ids
  )
  cat("  - Created gene annotations table with", nrow(gene_annotations), "entries\n")
} else {
  # Standard format: first column is gene ID, rest are counts
  rownames(counts_data_raw) <- counts_data_raw[, 1]
  counts_data <- counts_data_raw[, -1, drop = FALSE]
  gene_annotations <- NULL
}

cat("  - Count matrix:", nrow(counts_data), "genes ×", ncol(counts_data), "samples\n")

# Load sample metadata
metadata <- read.csv(METADATA_FILE, row.names = 1, check.names = FALSE)
cat("  - Loaded metadata for", nrow(metadata), "samples\n")
cat("  - Metadata columns:", paste(colnames(metadata), collapse = ", "), "\n")

# Verify sample names match
if (!all(colnames(counts_data) == rownames(metadata))) {
  stop("ERROR: Sample names in count matrix and metadata do not match!")
}
cat("  ✓ Sample names match between count matrix and metadata\n\n")

# Display data structure
cat("Count data preview:\n")
print(head(counts_data[, 1:min(5, ncol(counts_data))], 3))
if (!is.null(gene_annotations)) {
  cat("\nGene annotations preview:\n")
  print(head(gene_annotations, 3))
}
cat("\nMetadata preview:\n")
print(head(metadata, 3))
cat("\n")

################################################################################
# Step 2: Create DESeqDataSet Object
################################################################################

cat("Step 2: Creating DESeqDataSet object...\n")

# Ensure count matrix contains integers
counts_matrix <- as.matrix(counts_data)
storage.mode(counts_matrix) <- "integer"

# Create DESeqDataSet with design formula
dds <- DESeqDataSetFromMatrix(
  countData = counts_matrix,
  colData = metadata,
  design = DESIGN_FORMULA
)

cat("  ✓ DESeqDataSet created with design:", deparse(design(dds)), "\n")
cat("  - Total genes:", nrow(dds), "\n")
cat("  - Total samples:", ncol(dds), "\n\n")

################################################################################
# Step 3: Filter Low-Count Genes (Matches Shiny App Prefiltering)
################################################################################

cat("Step 3: Gene filtering (matches Shiny 'Config & Prefilter' step)...\n")

if (PREFILTER_APPLIED) {
  cat("  - Applying prefilter: Total counts across all samples >= ", PREFILTER_THRESHOLD, "\n")
  
  # Match Shiny app filtering: rowSums(dataCounts) >= minRowCount
  initial_genes <- nrow(dds)
  keep <- rowSums(counts(dds)) >= PREFILTER_THRESHOLD
  dds <- dds[keep, ]
  
  cat("  ✓ Retained", sum(keep), "genes after filtering\n")
  cat("  - Removed", initial_genes - sum(keep), "low-count genes\n\n")
} else {
  cat("  - No prefiltering was applied in Shiny app\n")
  cat("  - Keeping all", nrow(dds), "genes\n\n")
}

################################################################################
# Step 4: Run DESeq2 Analysis
################################################################################

cat("Step 4: Running DESeq2 normalization and differential expression...\n")
cat("  (This may take a few minutes for large datasets)\n\n")

# Run the DESeq2 pipeline
# This performs: estimation of size factors, estimation of dispersions, 
# negative binomial GLM fitting, and Wald statistics
dds <- DESeq(dds)

cat("  ✓ DESeq2 analysis complete!\n")
cat("  - Size factors calculated\n")
cat("  - Dispersion estimates computed\n")
cat("  - Statistical testing completed\n\n")

# Display size factors
cat("Size factors (normalization factors):\n")
print(sizeFactors(dds))
cat("\n")

################################################################################
# Step 5: Extract Differential Expression Results
################################################################################

cat("Step 5: Extracting differential expression results...\n\n")

# Extract results for each contrast
if (!is.null(CONTRASTS) && length(CONTRASTS) > 0) {
  # Process each contrast
  results_list <- list()
  
  for (i in seq_along(CONTRASTS)) {
    contrast <- CONTRASTS[[i]]
    contrast_name <- if (!is.null(names(CONTRASTS)[i]) && names(CONTRASTS)[i] != "") {
      names(CONTRASTS)[i]
    } else {
      paste0("contrast_", i)
    }
    
    cat("Extracting results for:", contrast_name, "\n")
    
    results_list[[i]] <- results(dds, contrast = contrast, alpha = ALPHA)
    results_list[[i]] <- as.data.frame(results_list[[i]])
    
    cat("  - Total genes:", nrow(results_list[[i]]), "\n")
    cat("  - Significant (padj <", ALPHA, "):", 
        sum(results_list[[i]]$padj < ALPHA, na.rm = TRUE), "\n")
    cat("    * Upregulated:", 
        sum(results_list[[i]]$padj < ALPHA & results_list[[i]]$log2FoldChange > 0, na.rm = TRUE), "\n")
    cat("    * Downregulated:", 
        sum(results_list[[i]]$padj < ALPHA & results_list[[i]]$log2FoldChange < 0, na.rm = TRUE), "\n\n")
  }
  
  # Set primary results object
  results_de <- results_list[[1]]
  
  # Name the list for easy access
  names(results_list) <- names(CONTRASTS)
  
} else {
  # Default: extract results from first coefficient
  cat("Extract results (using default contrast from design)\n")
  results_de <- results(dds, alpha = ALPHA)
  results_de <- as.data.frame(results_de)
  
  cat("  - Total genes:", nrow(results_de), "\n")
  cat("  - Significant (padj <", ALPHA, "):", sum(results_de$padj < ALPHA, na.rm = TRUE), "\n")
  cat("    * Upregulated:", 
      sum(results_de$padj < ALPHA & results_de$log2FoldChange > 0, na.rm = TRUE), "\n")
  cat("    * Downregulated:", 
      sum(results_de$padj < ALPHA & results_de$log2FoldChange < 0, na.rm = TRUE), "\n\n")
}

# Display top results
cat("Top 10 most significant genes:\n")
print(head(results_de[order(results_de$padj), ], 10))
cat("\n")

# Merge gene annotations into results if available
if (!is.null(gene_annotations)) {
  cat("Merging gene annotations into results...\n")
  
  # Add gene names to primary results
  results_de$gene.name <- gene_annotations[rownames(results_de), "gene.names"]
  cat("  - Added gene names to primary results:", sum(!is.na(results_de$gene.name)), "genes annotated\n")
  
  # Add gene names to all contrast results
  if (!is.null(CONTRASTS) && length(CONTRASTS) > 0) {
    for (i in seq_along(results_list)) {
      results_list[[i]]$gene.name <- gene_annotations[rownames(results_list[[i]]), "gene.names"]
    }
    cat("  - Added gene names to all", length(results_list), "contrast results\n")
  }
  cat("\n")
}

# Export results for each contrast (for use in subsequent plot sections)
if (!is.null(CONTRASTS) && length(CONTRASTS) > 0) {
  cat("Exporting results for each contrast...\n")
  for (i in seq_along(CONTRASTS)) {
    contrast_name <- if (!is.null(names(CONTRASTS)[i]) && names(CONTRASTS)[i] != "") {
      names(CONTRASTS)[i]
    } else {
      paste0("contrast_", i)
    }
    
    # Export individual contrast results as CSV (now includes gene.name column)
    result_file <- paste0(contrast_name, "_results.csv")
    write.csv(results_list[[i]], result_file, row.names = TRUE)
    cat("  - Exported:", result_file, "\n")
  }
  cat("\n")
}

################################################################################
# Step 6: Variance Stabilizing Transformation (VST)
################################################################################

cat("Step 6: Performing variance-stabilizing transformation...\n")

# VST is recommended for datasets with >= 30 samples
# For visualization and clustering
vsd <- vst(dds, blind = FALSE)
vst_mat <- assay(vsd)

cat("  ✓ VST transformation complete\n")
cat("  - Transformed matrix:", nrow(vst_mat), "genes ×", ncol(vst_mat), "samples\n\n")

################################################################################
# Step 7: Regularized Log Transformation (rlog)
################################################################################

cat("Step 7: Performing regularized log transformation...\n")

# rlog is recommended for datasets with < 30 samples
# Provides better variance stabilization for small sample sizes
rld <- rlog(dds, blind = FALSE)
rlog_mat <- assay(rld)

cat("  ✓ rlog transformation complete\n")
cat("  - Transformed matrix:", nrow(rlog_mat), "genes ×", ncol(rlog_mat), "samples\n\n")

################################################################################
# Step 8: Extract Normalized Counts
################################################################################

cat("Step 8: Extracting normalized counts...\n")

# Extract size-factor normalized counts
normalized_counts <- counts(dds, normalized = TRUE)

cat("  ✓ Normalized counts extracted\n")
cat("  - Matrix:", nrow(normalized_counts), "genes ×", ncol(normalized_counts), "samples\n\n")

################################################################################
# Pipeline Complete - Objects Created
################################################################################

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("PIPELINE COMPLETE!\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("The following R objects are now available for downstream analysis:\n\n")
cat("  📊 dds              - DESeqDataSet object (main analysis object)\n")
cat("  📈 results_de       - Differential expression results (data frame)\n")
cat("  📊 results_list     - List of all contrast results (named list)\n")
cat("  🔢 normalized_counts - Size-factor normalized counts (matrix)\n")
cat("  📉 vst_mat          - Variance-stabilized counts (matrix)\n")
cat("  📉 rlog_mat         - Regularized log counts (matrix)\n")
cat("  🧬 vsd              - VST DESeqTransform object\n")
cat("  🧬 rld              - rlog DESeqTransform object\n\n")
cat("You can now proceed with visualization and further analysis!\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")
