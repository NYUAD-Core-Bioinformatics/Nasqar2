################################################################################
# Publication-ready Volcano Plot (OPTIMIZED)
# Generated from DESeq2Shiny
#
# This script uses helper functions to reduce code duplication
# For standalone use, ensure create_volcano_plot() function is defined first
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Analysis name
COMPARISON_NAME <- "{{comparison_name}}"

# Significance thresholds
PADJ_THRESHOLD <- {{padj_threshold}}
LOG2FC_THRESHOLD <- {{log2fc_threshold}}

# Data file (for full mode)
DATA_FILE <- "{{data_file}}"

# Plot parameters
POINT_SIZE <- 2.0
POINT_ALPHA <- 0.5

# Gene labeling
USE_GENE_NAMES <- {{use_gene_names}}
MAX_LABELS <- 20  # Maximum number of gene labels to show (prevents overcrowding)

# Genes of interest to label (optional)
# Specify gene names (or IDs) separated by commas
# Example: GENES_OF_INTEREST <- c("Egfl6", "Cnn1", "Hoxc11", "Lrp3", "Nr1h3")
# Leave as NULL to auto-select top significant genes
GENES_OF_INTEREST <- {{genes_of_interest}}

################################################################################
# Load Required Libraries
################################################################################

library(EnhancedVolcano)  # BiocManager::install("EnhancedVolcano")
library(ggplot2)

################################################################################
# Load Data
################################################################################

{{DATA_LOAD_SECTION}}

################################################################################
# Create Volcano Plot Using Helper Function
################################################################################

cat("Generating volcano plot for:", COMPARISON_NAME, "\n")
cat("  - Thresholds: padj <", PADJ_THRESHOLD, ", |log2FC| >", LOG2FC_THRESHOLD, "\n")

# Use create_volcano_plot() helper function
# (Function should be defined in helper functions section)
volcano_plot <- create_volcano_plot(
  results_data = results_data,
  comparison_name = COMPARISON_NAME,
  padj_threshold = PADJ_THRESHOLD,
  log2fc_threshold = LOG2FC_THRESHOLD,
  point_size = POINT_SIZE,
  point_alpha = POINT_ALPHA,
  use_gene_names = USE_GENE_NAMES,
  genes_of_interest = GENES_OF_INTEREST,
  max_labels = MAX_LABELS,
  gene_annotations = if(exists("gene_annotations")) gene_annotations else NULL
)

# Display plot
print(volcano_plot)

cat("✓ Volcano plot created with clean, non-overlapping labels\n")

################################################################################
# Save High-Resolution Plots
################################################################################

# Save as PDF
pdf(paste0("volcano_plot_", COMPARISON_NAME, ".pdf"), width = 10, height = 8)
print(volcano_plot)
dev.off()

# Save as PNG
png(paste0("volcano_plot_", COMPARISON_NAME, ".png"), width = 3000, height = 2400, res = 300)
print(volcano_plot)
dev.off()

cat("✓ Volcano plots saved successfully!\n")
cat("  - volcano_plot_", COMPARISON_NAME, ".pdf\n")
cat("  - volcano_plot_", COMPARISON_NAME, ".png\n")
