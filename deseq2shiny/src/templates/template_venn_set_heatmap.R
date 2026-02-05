################################################################################
# Publication-ready Venn Set Operation Heatmap
# Generated from DESeq2Shiny
#
# This heatmap visualizes log2FoldChange values for genes from a Venn diagram
# set operation (e.g., A*B for intersection, A+B for union, A-B for difference).
#
# This script is designed to work together with the Venn diagram script.
# Run the Venn diagram script first to understand the set operations,
# then use this script to visualize the expression patterns of genes
# in the specific set of interest.
#
# This script can be run independently by editing the configuration parameters below
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Set expression (e.g., "A*B", "A+B", "A-B")
SET_EXPRESSION <- "{{set_expression}}"

# Comparison names
COMPARISONS <- {{comparisons}}

# Ensure COMPARISONS is a character vector (not a list)
if (is.list(COMPARISONS)) {
  COMPARISONS <- as.character(unlist(COMPARISONS))
}

# Filtering thresholds (from Shiny app settings - must match Venn diagram)
PADJ_THRESHOLD <- {{padj_threshold}}  # Adjusted p-value threshold
FC_THRESHOLD <- {{fc_threshold}}      # |log2FoldChange| threshold

# Number of genes to display
NUM_GENES <- {{num_genes}}

# Data files (for full mode)
EXPRESSION_MATRIX_FILE <- "{{expression_matrix_file}}"

# Plot parameters
FONTSIZE_ROW <- {{fontsize_row}}

# Brushed heatmap settings (preserve order if this is a brushed sub-heatmap)
IS_BRUSHED <- {{is_brushed}}  # TRUE if this is a brushed sub-heatmap
SAMPLE_ORDER <- {{sample_order}}  # Comparison order (NULL if not brushed)
COLOR_RANGE <- {{color_range}}  # Parent heatmap's data range for consistent colors

# Ensure defaults for backwards compatibility
if (!exists("IS_BRUSHED") || is.null(IS_BRUSHED) || length(IS_BRUSHED) == 0) {
  IS_BRUSHED <- FALSE
}
if (!exists("SAMPLE_ORDER") || is.null(SAMPLE_ORDER) || length(SAMPLE_ORDER) == 0) {
  SAMPLE_ORDER <- NULL
}
if (!exists("COLOR_RANGE") || is.null(COLOR_RANGE) || length(COLOR_RANGE) == 0) {
  COLOR_RANGE <- NULL
}

################################################################################
# Load Required Libraries
################################################################################

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

################################################################################
# Load Data
################################################################################

{{DATA_LOAD_SECTION}}

################################################################################
# Prepare Heatmap Matrix
################################################################################

# The matrix should have genes as rows and comparisons as columns
# Values are log2FoldChange for each gene in each comparison
plot_matrix <- set_expression_matrix

cat("Creating heatmap for", nrow(plot_matrix), "genes from set operation:", SET_EXPRESSION, "\n")
cat("Comparisons:", paste(COMPARISONS, collapse = ", "), "\n\n")

################################################################################
# Create Heatmap (using ComplexHeatmap like Shiny app)
################################################################################

# Create column annotation
comparison_names <- COMPARISONS
ha <- HeatmapAnnotation(
  Comparison = comparison_names,
  which = "column",
  show_annotation_name = TRUE,
  annotation_legend_param = list(
    Comparison = list(title = "Comparison")
  )
)

# Reorder comparisons if this is a brushed heatmap (preserve original order)
if (IS_BRUSHED && !is.null(SAMPLE_ORDER) && length(SAMPLE_ORDER) > 0) {
  cat("Brushed heatmap: Preserving original comparison order\n")
  valid_comparisons <- SAMPLE_ORDER[SAMPLE_ORDER %in% colnames(plot_matrix)]
  if (length(valid_comparisons) > 0) {
    plot_matrix <- plot_matrix[, valid_comparisons, drop = FALSE]
  }
}

# Create color function
# For brushed heatmaps, use parent heatmap's data range for consistent colors
if (IS_BRUSHED && !is.null(COLOR_RANGE) && length(COLOR_RANGE) == 2) {
  cat("Using parent heatmap color range:", COLOR_RANGE[1], "to", COLOR_RANGE[2], "\n")
  col_fun <- colorRamp2(
    c(COLOR_RANGE[1], 0, COLOR_RANGE[2]),
    c("#0000FF", "#FFFFFF", "#FF0000")  # Blue-white-red
  )
} else {
  # For non-brushed or if no range specified, let ComplexHeatmap auto-scale
  col_fun <- NULL
}

# Create heatmap using ComplexHeatmap (matches Shiny app)
set_heatmap_args <- list(
  plot_matrix,
  name = "log2FC"  # legend title
)

# Add color function if specified
if (!is.null(col_fun)) {
  set_heatmap_args$col <- col_fun
}

set_heatmap <- do.call(Heatmap, c(set_heatmap_args, list(
  
  # Clustering: For brushed heatmaps, preserve original order
  cluster_rows = if (length(IS_BRUSHED) > 0 && IS_BRUSHED) FALSE else TRUE,  # Don't re-cluster genes for brushed heatmaps
  cluster_columns = FALSE,  # Never cluster comparisons
  clustering_distance_rows = if (length(IS_BRUSHED) == 0 || !IS_BRUSHED) "euclidean" else NULL,
  clustering_method_rows = if (length(IS_BRUSHED) == 0 || !IS_BRUSHED) "complete" else NULL,
  
  # Display options
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = FONTSIZE_ROW),
  column_names_gp = gpar(fontsize = 10),
  
  # Annotations
  top_annotation = ha,
  
  # Title
  column_title = if (IS_BRUSHED) {
    paste("Brushed Venn Sub-Heatmap:", SET_EXPRESSION)
  } else {
    paste("Venn Set Operation:", SET_EXPRESSION)
  },
  column_title_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # Heatmap body
  border = TRUE,
  
  # Cell size
  row_names_side = "left",
  width = unit(length(COMPARISONS) * 2, "cm"),
  height = unit(min(nrow(plot_matrix) * 0.5, 20), "cm")
)))

# Display heatmap
draw(set_heatmap)

################################################################################
# Save High-Resolution Heatmap
################################################################################

# Calculate appropriate height based on number of genes
heatmap_height <- max(8, NUM_GENES * 0.15)
heatmap_height_px <- max(2400, NUM_GENES * 45)

# Clean the set expression for filename
set_expr_clean <- gsub("[^A-Za-z0-9]", "_", SET_EXPRESSION)

# Save as PDF
pdf(paste0("venn_set_heatmap_", set_expr_clean, ".pdf"), 
    width = 10, height = heatmap_height)
draw(set_heatmap)
dev.off()

# Save as PNG
png(paste0("venn_set_heatmap_", set_expr_clean, ".png"), 
    width = 3000, height = heatmap_height_px, res = 300)
draw(set_heatmap)
dev.off()

cat("\nVenn set operation heatmap saved successfully!\n")
cat("  - venn_set_heatmap_", set_expr_clean, ".pdf\n")
cat("  - venn_set_heatmap_", set_expr_clean, ".png\n")
cat("\nThis heatmap shows log2FC values for", NUM_GENES, "genes across", length(COMPARISONS), "comparisons.\n")
