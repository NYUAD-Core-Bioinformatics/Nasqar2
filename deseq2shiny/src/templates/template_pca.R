################################################################################
# Publication-ready PCA Plot
# Generated from DESeq2Shiny
#
# This script can be run independently by editing the configuration parameters below
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Transformation type
TRANSFORM_TYPE <- "{{transform_type}}"  # "vst" or "rlog"

# Grouping variable for coloring
INTGROUP <- "{{intgroup}}"

# Data files (for full mode)
TRANSFORMED_DATA_FILE <- "{{transformed_data_file}}"
METADATA_FILE <- "{{metadata_file}}"

# Validate parameters
if (is.null(TRANSFORM_TYPE) || length(TRANSFORM_TYPE) == 0 || nchar(TRANSFORM_TYPE) == 0) {
  TRANSFORM_TYPE <- "vst"
  cat("WARNING: TRANSFORM_TYPE not specified, using default: vst\n")
}
if (is.null(INTGROUP) || length(INTGROUP) == 0 || nchar(INTGROUP) == 0) {
  INTGROUP <- "Conditions"
  cat("WARNING: INTGROUP not specified, using default: Conditions\n")
}

################################################################################
# Load Required Libraries
################################################################################

library(DESeq2)
library(ggplot2)
library(matrixStats)  # For rowVars() function

################################################################################
# Load Data
################################################################################

{{DATA_LOAD_SECTION}}

################################################################################
# Perform PCA (Using DESeq2's method - matches Shiny app exactly!)
################################################################################

cat("Performing PCA using DESeq2's method...\n")

# IMPORTANT: DESeq2::plotPCA() selects top 500 most variable genes by default
# This matches the Shiny app's behavior exactly!
# Using all genes (with prcomp) gives different variance percentages

# Ensure data is a matrix (rowVars requires matrix, not data frame)
if (is.data.frame(transformed_data)) {
  transformed_data <- as.matrix(transformed_data)
}

# Select top 500 most variable genes (same as DESeq2::plotPCA default)
ntop <- 500
rv <- rowVars(transformed_data)
select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]

cat("  - Selected top", length(select), "most variable genes for PCA\n")

# Perform PCA on selected genes only (matches Shiny app)
pca_result <- prcomp(t(transformed_data[select, ]), scale = FALSE)

# Calculate variance explained
percentVar <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)

# Create PCA data frame
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  sample = rownames(pca_result$x)
)

# Add metadata
pca_data <- merge(pca_data, sample_metadata, by.x = "sample", by.y = "row.names")

cat("  ✓ PCA complete\n")
cat("  - PC1 explains", percentVar[1], "% variance\n")
cat("  - PC2 explains", percentVar[2], "% variance\n\n")

################################################################################
# Create PCA Plot
################################################################################

pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = .data[[INTGROUP]])) +
  geom_point(size = 4, alpha = 0.8) +
  labs(title = paste("PCA Plot (", toupper(TRANSFORM_TYPE), " Transformation)", sep = ""),
       x = paste0("PC1: ", percentVar[1], "% variance"),
       y = paste0("PC2: ", percentVar[2], "% variance"),
       color = INTGROUP) +
  theme_bw(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank())

# Optional: Add sample labels
# Uncomment to add sample names to plot
# pca_plot <- pca_plot + geom_text(aes(label = sample), vjust = -1, size = 3)

# Display plot
print(pca_plot)

################################################################################
# Save High-Resolution Plot
################################################################################

ggsave(paste0("pca_plot_", TRANSFORM_TYPE, ".pdf"), pca_plot, width = 8, height = 6, dpi = 300)
ggsave(paste0("pca_plot_", TRANSFORM_TYPE, ".png"), pca_plot, width = 8, height = 6, dpi = 300)
cat("PCA plot saved successfully!\n")
