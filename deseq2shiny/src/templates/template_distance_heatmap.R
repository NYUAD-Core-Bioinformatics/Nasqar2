################################################################################
# Sample Distance Heatmap - Dual Output
# Generated from DESeq2Shiny
#
# Generates TWO versions:
#   • Interactive HTML (heatmaply) - Matches Shiny app exactly
#   • Static PDF/PNG (pheatmap) - For publications
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Transformation type
TRANSFORM_TYPE <- "{{transform_type}}"  # "vst" or "rlog"

# Data files (for full mode)
TRANSFORMED_DATA_FILE <- "{{transformed_data_file}}"
METADATA_FILE <- "{{metadata_file}}"

# Validate parameters
if (is.null(TRANSFORM_TYPE) || length(TRANSFORM_TYPE) == 0 || nchar(TRANSFORM_TYPE) == 0) {
  TRANSFORM_TYPE <- "vst"
  cat("WARNING: TRANSFORM_TYPE not specified, using default: vst\n")
}

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

{{DATA_LOAD_SECTION}}

################################################################################
# Calculate Sample Distances
################################################################################

cat("Calculating sample-to-sample distances...\n")

# Calculate sample distances
sample_dists <- dist(t(transformed_data))
sample_dist_matrix <- as.matrix(sample_dists)

cat("  ✓ Distance matrix calculated\n")
cat("  - Matrix size:", nrow(sample_dist_matrix), "×", ncol(sample_dist_matrix), "\n\n")

################################################################################
# Create Annotations
################################################################################

# Create annotation (if metadata available)
if (exists("sample_metadata") && !is.null(sample_metadata) && is.data.frame(sample_metadata) && nrow(sample_metadata) > 0) {
  annotation_col <- sample_metadata[colnames(sample_dist_matrix), , drop = FALSE]
  annotation_col <- annotation_col[, !colnames(annotation_col) %in% c("sizeFactor", "replaceable"), drop = FALSE]
  annotation_row <- annotation_col
} else {
  annotation_col <- NA
  annotation_row <- NA
}

################################################################################
# OPTION 1: Interactive Heatmap (Using heatmaply - SAME as Shiny app!)
################################################################################

cat("\n========================================\n")
cat("OPTION 1: Interactive Heatmap (heatmaply)\n")
cat("========================================\n\n")

cat("Creating interactive distance heatmap using heatmaply (same as Shiny app)...\n")

# Use heatmaply with the SAME parameters as Shiny app
# This ensures EXACT visual matching!
heatmaply_plot <- heatmaply(
  sample_dist_matrix,
  showticklabels = c(FALSE, TRUE),  # Same as Shiny: showticklabels = c(F, T)
  main = paste0("Sample Distance Heatmap (", toupper(TRANSFORM_TYPE), ")")
)

# Display interactive heatmap
print(heatmaply_plot)

cat("✓ Interactive heatmap created (matches Shiny app exactly!)\n\n")

################################################################################
# OPTION 2: Static Heatmap (Using pheatmap for PDF/PNG)
################################################################################

cat("========================================\n")
cat("OPTION 2: Static Heatmap (pheatmap)\n")
cat("========================================\n\n")

cat("Creating static distance heatmap using pheatmap (for PDF/PNG)...\n")

# Use Viridis color palette to match heatmaply's default
colors <- viridis(255, option = "D")

# Create static heatmap for publication-quality PDF/PNG
pheatmap_plot <- pheatmap(
  sample_dist_matrix,
  clustering_distance_rows = sample_dists,
  clustering_distance_cols = sample_dists,
  clustering_method = "complete",
  col = colors,
  annotation_col = annotation_col,
  annotation_row = annotation_row,
  main = paste0("Sample Distance Heatmap (", toupper(TRANSFORM_TYPE), ")"),
  fontsize = 10,
  border_color = NA,
  show_rownames = TRUE,
  show_colnames = TRUE
)

cat("✓ Static heatmap created (for PDF/PNG export)\n")
cat("  Note: Dendrogram orientation may differ from heatmaply (cosmetic only)\n\n")

################################################################################
# Save Both Versions
################################################################################

cat("\n========================================\n")
cat("Saving Heatmaps\n")
cat("========================================\n\n")

# 1. Save interactive HTML (heatmaply - matches Shiny exactly!)
html_file <- paste0("distance_heatmap_", TRANSFORM_TYPE, "_interactive.html")
htmlwidgets::saveWidget(heatmaply_plot, html_file, selfcontained = TRUE)
cat("✓ Interactive heatmap saved:", html_file, "\n")
cat("  → Matches Shiny app exactly! Open in browser.\n\n")

# 2. Save static PDF (pheatmap - for publications)
pdf_file <- paste0("distance_heatmap_", TRANSFORM_TYPE, "_static.pdf")
pdf(pdf_file, width = 10, height = 10)
print(pheatmap_plot)
dev.off()
cat("✓ Static PDF saved:", pdf_file, "\n")
cat("  → High-resolution PDF for publications.\n\n")

# 3. Save static PNG (pheatmap - for presentations)
png_file <- paste0("distance_heatmap_", TRANSFORM_TYPE, "_static.png")
png(png_file, width = 3000, height = 3000, res = 300)
print(pheatmap_plot)
dev.off()
cat("✓ Static PNG saved:", png_file, "\n")
cat("  → High-resolution PNG for presentations.\n\n")

# 4. Optional: Save PNG from interactive heatmaply (requires webshot)
if (requireNamespace("webshot", quietly = TRUE)) {
  heatmaply_png <- paste0("distance_heatmap_", TRANSFORM_TYPE, "_heatmaply.png")
  
  if (!is.null(tryCatch(webshot:::find_phantom(), error = function(e) NULL))) {
    webshot::webshot(html_file, heatmaply_png, vwidth = 1200, vheight = 900)
    cat("✓ Heatmaply PNG saved:", heatmaply_png, "\n")
    cat("  → Screenshot of interactive version.\n\n")
  } else {
    cat("Note: Install phantomjs for heatmaply PNG: webshot::install_phantomjs()\n\n")
  }
}

cat("========================================\n")
cat("Summary\n")
cat("========================================\n\n")
cat("Generated files:\n")
cat("  1.", html_file, "← MATCHES SHINY EXACTLY (open in browser)\n")
cat("  2.", pdf_file, "← For publications (high-res PDF)\n")
cat("  3.", png_file, "← For presentations (high-res PNG)\n")
cat("\n")
cat("Recommendation:\n")
cat("  • For exact Shiny match: Use the interactive HTML\n")
cat("  • For publications: Use the static PDF\n")
cat("  • For presentations: Use the static PNG\n")
cat("\n")
