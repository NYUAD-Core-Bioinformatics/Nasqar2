################################################################################
# Brushed Heatmap - Generated from DESeq2Shiny
################################################################################
#
# This script plots a brushed sub-heatmap from pre-processed data.
# All processing was done by Shiny - this just loads and plots the matrix.
#
# The colors will match the parent heatmap for visual consistency.
#
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
################################################################################

# Data files
MATRIX_FILE <- "{{matrix_file}}"
METADATA_FILE <- "{{metadata_file}}"

# Display options
SHOW_ROWNAMES <- TRUE
SHOW_COLNAMES <- TRUE
FONTSIZE_ROW <- {{fontsize_row}}

# Parent heatmap color range (for color consistency)
PARENT_COLOR_RANGE <- {{parent_color_range}}

################################################################################
# Load Required Libraries
################################################################################

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

################################################################################
# Load Data
################################################################################

# Load brushed matrix (already log2-transformed and gene-selected by Shiny)
plot_matrix <- as.matrix(read.csv(MATRIX_FILE, row.names = 1, check.names = FALSE))
cat("Loaded brushed matrix:", nrow(plot_matrix), "genes ×", ncol(plot_matrix), "samples\n")

# Load sample metadata
sample_metadata <- read.csv(METADATA_FILE, row.names = 1, check.names = FALSE)
cat("Loaded metadata for", nrow(sample_metadata), "samples\n")

################################################################################
# Create Sample Annotations
################################################################################

# Create sample annotations from metadata
if (nrow(sample_metadata) > 0) {
  # Remove non-informative columns
  annotation_cols <- setdiff(colnames(sample_metadata), c("sizeFactor", "replaceable"))
  if (length(annotation_cols) > 0) {
    annotation_data <- sample_metadata[colnames(plot_matrix), annotation_cols, drop = FALSE]
    ha <- HeatmapAnnotation(
      df = annotation_data,
      which = "column",
      show_annotation_name = TRUE
    )
  } else {
    ha <- NULL
  }
} else {
  ha <- NULL
}

################################################################################
# Create Color Scale (matching parent heatmap)
################################################################################

# Use parent heatmap's color range for consistency
if (!is.null(PARENT_COLOR_RANGE) && length(PARENT_COLOR_RANGE) == 2) {
  min_val <- PARENT_COLOR_RANGE[1]
  max_val <- PARENT_COLOR_RANGE[2]
  cat("Using parent heatmap color range: min =", round(min_val, 2), ", max =", round(max_val, 2), "\n")
  cat("(This ensures colors match the parent heatmap)\n")
} else {
  # Fallback to brushed data range if parent range not available
  min_val <- min(plot_matrix, na.rm = TRUE)
  max_val <- max(plot_matrix, na.rm = TRUE)
  cat("Parent color range not available - using brushed data range\n")
  cat("Data range: min =", round(min_val, 2), ", max =", round(max_val, 2), "\n")
}

# Create color function (Blue-Yellow-Red, same as parent)
color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)
breaks <- seq(min_val, max_val, length.out = 255)
col_fun <- colorRamp2(breaks, color_palette)

################################################################################
# Create Brushed Heatmap (no clustering - preserve brush order)
################################################################################

cat("\nCreating brushed heatmap (preserving selection order)...\n")

heatmap_plot <- Heatmap(
  plot_matrix,
  name = "Expression",
  
  # Color (matching parent)
  col = col_fun,
  
  # NO clustering - preserve the order from brushing
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  
  # Display
  show_row_names = SHOW_ROWNAMES,
  show_column_names = SHOW_COLNAMES,
  row_names_gp = gpar(fontsize = FONTSIZE_ROW),
  column_names_gp = gpar(fontsize = 8),
  
  # Annotations
  top_annotation = ha,
  
  # Title
  column_title = "Brushed Sub-Heatmap (Original Order Preserved)",
  column_title_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # Border
  border = TRUE
)

# Display
draw(heatmap_plot)

################################################################################
# Save High-Resolution Heatmap
################################################################################

# Calculate height based on number of genes
heatmap_height <- max(6, nrow(plot_matrix) * 0.15)
heatmap_height_px <- max(1800, nrow(plot_matrix) * 45)

# Save as PDF
pdf("brushed_heatmap.pdf", width = 10, height = heatmap_height)
draw(heatmap_plot)
dev.off()

# Save as PNG
png("brushed_heatmap.png", width = 3000, height = heatmap_height_px, res = 300)
draw(heatmap_plot)
dev.off()

cat("\n✓ Brushed heatmap saved successfully!\n")
cat("  - brushed_heatmap.pdf (", nrow(plot_matrix), " genes)\n")
cat("  - brushed_heatmap.png (high resolution)\n")
