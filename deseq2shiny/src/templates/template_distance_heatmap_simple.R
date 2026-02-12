################################################################################
# Sample Distance Heatmap - Generated from DESeq2Shiny
################################################################################
#
# This script creates distance heatmaps from pre-calculated distance matrix.
# All distance calculation was done by Shiny - this just loads and plots.
#
# Generates TWO versions:
#   • Interactive HTML (heatmaply) - Matches Shiny app exactly
#   • Static PDF/PNG (pheatmap) - For publications
#
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
################################################################################

# Transformation type
TRANSFORM_TYPE <- "{{transform_type}}"  # "vst" or "rlog"

# Data files
DISTANCE_MATRIX_FILE <- "{{distance_matrix_file}}"
METADATA_FILE <- "{{metadata_file}}"

################################################################################
# Load Required Libraries
################################################################################

library(heatmaply)  # Interactive heatmap (same as Shiny app!)
library(plotly)     # Required by heatmaply
library(htmlwidgets) # For saving interactive HTML
library(pheatmap)   # Static heatmap for PDF/PNG
library(viridis)    # Viridis color palette

################################################################################
# Load Data
################################################################################

# Load pre-calculated distance matrix
sample_dist_matrix <- as.matrix(read.csv(DISTANCE_MATRIX_FILE, row.names = 1, check.names = FALSE))
cat("Loaded distance matrix:", nrow(sample_dist_matrix), "×", ncol(sample_dist_matrix), "\n")

# Load sample metadata
sample_metadata <- read.csv(METADATA_FILE, row.names = 1, check.names = FALSE)
cat("Loaded metadata for", nrow(sample_metadata), "samples\n\n")

################################################################################
# Create Annotations
################################################################################

# Create annotation (exclude technical columns)
annotation_col <- sample_metadata[colnames(sample_dist_matrix), , drop = FALSE]
annotation_col <- annotation_col[, !colnames(annotation_col) %in% c("sizeFactor", "replaceable"), drop = FALSE]
annotation_row <- annotation_col

################################################################################
# OPTION 1: Interactive Heatmap (Using heatmaply - SAME as Shiny app!)
################################################################################

cat("\n========================================\n")
cat("OPTION 1: Interactive Heatmap (heatmaply)\n")
cat("========================================\n\n")

cat("Creating interactive distance heatmap...\n")

# Create interactive heatmap
interactive_heatmap <- heatmaply(
  sample_dist_matrix,
  colors = viridis(256),
  main = paste("Sample Distance Heatmap (", toupper(TRANSFORM_TYPE), ")", sep = ""),
  xlab = "Samples",
  ylab = "Samples",
  margins = c(100, 100, NA, 0)
)

# Display
print(interactive_heatmap)

# Save as HTML
saveWidget(interactive_heatmap, "distance_heatmap_interactive.html", selfcontained = TRUE)

cat("  ✓ Interactive heatmap saved as distance_heatmap_interactive.html\n")
cat("  → Open this file in a web browser for interactive exploration!\n\n")

################################################################################
# OPTION 2: Static Heatmap (Using pheatmap - For publications)
################################################################################

cat("========================================\n")
cat("OPTION 2: Static Heatmap (pheatmap)\n")
cat("========================================\n\n")

cat("Creating static publication-ready heatmap...\n")

# Open PDF device
pdf("distance_heatmap_static.pdf", width = 10, height = 10)

# Create static heatmap
pheatmap(
  sample_dist_matrix,
  color = viridis(256),
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  annotation_col = annotation_col,
  annotation_row = annotation_row,
  main = paste("Sample Distance Heatmap (", toupper(TRANSFORM_TYPE), ")", sep = ""),
  fontsize = 10
)

dev.off()

# Save PNG version
png("distance_heatmap_static.png", width = 1200, height = 1200, res = 150)

pheatmap(
  sample_dist_matrix,
  color = viridis(256),
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  annotation_col = annotation_col,
  annotation_row = annotation_row,
  main = paste("Sample Distance Heatmap (", toupper(TRANSFORM_TYPE), ")", sep = ""),
  fontsize = 10
)

dev.off()

cat("  ✓ Static heatmap saved\n")
cat("    - distance_heatmap_static.pdf\n")
cat("    - distance_heatmap_static.png\n\n")

################################################################################
# Summary
################################################################################

cat("\n========================================\n")
cat("Summary\n")
cat("========================================\n\n")

cat("Generated distance heatmaps from", toupper(TRANSFORM_TYPE), "transformed data\n")
cat("  - Interactive HTML: distance_heatmap_interactive.html\n")
cat("  - Static PDF: distance_heatmap_static.pdf\n")
cat("  - Static PNG: distance_heatmap_static.png\n\n")

cat("TIP: Use the interactive HTML for exploration, and the PDF for publications!\n")
