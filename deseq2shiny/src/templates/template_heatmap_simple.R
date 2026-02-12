################################################################################
# Heatmap - Generated from DESeq2Shiny
################################################################################
#
# This script plots a heatmap from pre-processed data exported by DESeq2Shiny.
# All data processing (gene selection, log2 transformation) was done by Shiny.
# This script just loads the matrix and creates the heatmap.
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

################################################################################
# Load Required Libraries
################################################################################

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

################################################################################
# Load Data
################################################################################

# Load pre-processed matrix (already log2-transformed and gene-selected by Shiny)
plot_matrix <- as.matrix(read.csv(MATRIX_FILE, row.names = 1, check.names = FALSE))
cat("Loaded matrix:", nrow(plot_matrix), "genes ×", ncol(plot_matrix), "samples\n")

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
# Create Color Scale
################################################################################

# Get data range
min_val <- min(plot_matrix, na.rm = TRUE)
max_val <- max(plot_matrix, na.rm = TRUE)
cat("Data range: min =", round(min_val, 2), ", max =", round(max_val, 2), "\n")

# Create color function (Blue-Yellow-Red)
color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)
breaks <- seq(min_val, max_val, length.out = 255)
col_fun <- colorRamp2(breaks, color_palette)

################################################################################
# Create Heatmap
################################################################################

cat("\nCreating heatmap...\n")

heatmap_plot <- Heatmap(
  plot_matrix,
  name = "Expression",
  
  # Color
  col = col_fun,
  
  # Clustering
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  clustering_distance_rows = "euclidean",
  clustering_distance_columns = "euclidean",
  clustering_method_rows = "complete",
  clustering_method_columns = "complete",
  
  # Display
  show_row_names = SHOW_ROWNAMES,
  show_column_names = SHOW_COLNAMES,
  row_names_gp = gpar(fontsize = FONTSIZE_ROW),
  column_names_gp = gpar(fontsize = 8),
  
  # Annotations
  top_annotation = ha,
  
  # Title
  column_title = "Expression Heatmap",
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
heatmap_height <- max(8, nrow(plot_matrix) * 0.15)
heatmap_height_px <- max(2400, nrow(plot_matrix) * 45)

# Save as PDF
pdf("heatmap.pdf", width = 10, height = heatmap_height)
draw(heatmap_plot)
dev.off()

# Save as PNG
png("heatmap.png", width = 3000, height = heatmap_height_px, res = 300)
draw(heatmap_plot)
dev.off()

cat("\n✓ Heatmap saved successfully!\n")
cat("  - heatmap.pdf (", nrow(plot_matrix), " genes)\n")
cat("  - heatmap.png (high resolution)\n")
