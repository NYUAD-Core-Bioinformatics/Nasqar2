################################################################################
# Venn Brushed Sub-Heatmap - Generated from DESeq2Shiny
################################################################################
#
# This script creates a heatmap from brushed/selected genes.
# All selection was done by Shiny - this just loads and plots the exact matrix.
#
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
################################################################################

# Set description
SET_DESCRIPTION <- "{{set_description}}"

# Data file
HEATMAP_MATRIX_FILE <- "{{heatmap_matrix_file}}"

# Heatmap display settings
FONTSIZE_ROW <- {{fontsize_row}}
NUM_GENES <- {{num_genes}}

################################################################################
# Load Required Libraries
################################################################################

library(ComplexHeatmap)
library(circlize)

################################################################################
# Load Data
################################################################################

# Load brushed heatmap matrix (exact matrix from brush selection)
heatmap_matrix <- as.matrix(read.csv(HEATMAP_MATRIX_FILE, row.names = 1, check.names = FALSE))

cat("Loaded brushed Venn heatmap matrix:\n")
cat("  -", SET_DESCRIPTION, "\n")
cat("  - Genes:", nrow(heatmap_matrix), "\n")
cat("  - Contrasts:", ncol(heatmap_matrix), "\n")
cat("  - Contrast names:", paste(colnames(heatmap_matrix), collapse = ", "), "\n\n")

################################################################################
# Create Heatmap
################################################################################

cat("Creating heatmap...\n")

# Create color function for log2 fold changes
# Symmetric around 0 (blue = downregulated, red = upregulated)
col_fun <- colorRamp2(
  c(min(heatmap_matrix, na.rm = TRUE), 0, max(heatmap_matrix, na.rm = TRUE)),
  c("blue", "white", "red")
)

# Create heatmap WITHOUT clustering (preserve brush selection order)
ht <- Heatmap(
  heatmap_matrix,
  name = "log2FC",
  col = col_fun,
  cluster_rows = FALSE,      # Don't cluster - preserve brush order
  cluster_columns = FALSE,   # Don't cluster - preserve contrast order
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = FONTSIZE_ROW),
  column_names_gp = gpar(fontsize = 10),
  column_title = SET_DESCRIPTION,
  column_title_gp = gpar(fontsize = 12, fontface = "bold"),
  heatmap_legend_param = list(
    title = "log2 Fold Change",
    legend_direction = "vertical",
    legend_width = unit(4, "cm")
  )
)

# Draw heatmap
draw(ht)

cat("✓ Heatmap created\n\n")

################################################################################
# Save High-Resolution Plots
################################################################################

cat("Saving plots...\n")

# Calculate height based on number of genes
plot_height <- max(6, min(20, 0.2 * NUM_GENES))

# Save as PDF
pdf("venn_brushed_heatmap.pdf", width = 10, height = plot_height)
draw(ht)
dev.off()

# Save as PNG
png("venn_brushed_heatmap.png", width = 1200, height = plot_height * 120, res = 150)
draw(ht)
dev.off()

cat("✓ Plots saved successfully!\n")
cat("  - venn_brushed_heatmap.pdf\n")
cat("  - venn_brushed_heatmap.png\n\n")

cat("Brushed heatmap:", SET_DESCRIPTION, "\n")
cat("Genes in heatmap:", NUM_GENES, "\n")
cat("\nNote: Row and column order preserved from your brush selection.\n")
cat("No clustering applied to maintain your selected view.\n")
