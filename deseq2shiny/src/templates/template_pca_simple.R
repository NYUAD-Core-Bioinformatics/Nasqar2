################################################################################
# PCA Plot - Generated from DESeq2Shiny
################################################################################
#
# This script creates PCA plots from pre-calculated PC coordinates.
# All PCA calculation was done by Shiny - this just loads and plots.
#
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
################################################################################

# Transformation type
TRANSFORM_TYPE <- "{{transform_type}}"  # "vst" or "rlog"

# Grouping variable for coloring
INTGROUP <- "{{intgroup}}"

# Data files
PCA_DATA_FILE <- "{{pca_data_file}}"

# Variance explained by PC1 and PC2
PC1_VAR <- {{pc1_var}}
PC2_VAR <- {{pc2_var}}

################################################################################
# Load Required Libraries
################################################################################

library(ggplot2)

################################################################################
# Load Data
################################################################################

# Load pre-calculated PCA coordinates
pca_data <- read.csv(PCA_DATA_FILE, row.names = 1, check.names = FALSE)
cat("Loaded PCA coordinates for", nrow(pca_data), "samples\n")
cat("  - PC1 explains", PC1_VAR, "% variance\n")
cat("  - PC2 explains", PC2_VAR, "% variance\n\n")

################################################################################
# Create PCA Plot
################################################################################

cat("Creating PCA plot...\n")

pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = .data[[INTGROUP]])) +
  geom_point(size = 4, alpha = 0.8) +
  labs(
    title = paste("PCA Plot (", toupper(TRANSFORM_TYPE), " Transformation)", sep = ""),
    x = paste0("PC1: ", PC1_VAR, "% variance"),
    y = paste0("PC2: ", PC2_VAR, "% variance"),
    color = INTGROUP
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Display
print(pca_plot)

################################################################################
# Save High-Resolution Plots
################################################################################

# Save as PDF
ggsave("pca_plot.pdf", pca_plot, width = 8, height = 6, dpi = 300)

# Save as PNG
ggsave("pca_plot.png", pca_plot, width = 8, height = 6, dpi = 300)

cat("\n✓ PCA plot saved successfully!\n")
cat("  - pca_plot.pdf\n")
cat("  - pca_plot.png (high resolution)\n")
