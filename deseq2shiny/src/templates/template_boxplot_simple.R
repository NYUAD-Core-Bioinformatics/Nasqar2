################################################################################
# Gene Expression Boxplot - Generated from DESeq2Shiny
################################################################################
#
# This script creates boxplots from pre-processed data exported by DESeq2Shiny.
# All data processing (gene selection, log2 transformation) was done by Shiny.
# This script just loads the final data and creates the plot.
#
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
################################################################################

# Plot grouping
X_AXIS <- "{{x_axis}}"
FILL_BY <- "{{fill_by}}"

# Data files
LOG2_COUNTS_FILE <- "{{log2_counts_file}}"
METADATA_FILE <- "{{metadata_file}}"

# Plot customization
CUSTOM_COLORS <- {{custom_colors}}
NUM_COLS <- {{num_cols}}
NUM_GENES <- {{num_genes}}

################################################################################
# Load Required Libraries
################################################################################

library(ggplot2)
library(tidyr)
library(dplyr)

################################################################################
# Load Data
################################################################################

# Load log2-transformed counts (already processed by Shiny)
log2_counts <- read.csv(LOG2_COUNTS_FILE, row.names = 1, check.names = FALSE)
cat("Loaded log2-transformed counts for", nrow(log2_counts), "genes ×", ncol(log2_counts), "samples\n")

# Load sample metadata
sample_metadata <- read.csv(METADATA_FILE, row.names = 1, check.names = FALSE)
cat("Loaded metadata for", nrow(sample_metadata), "samples\n")

################################################################################
# Prepare Data for Plotting
################################################################################

# Convert to long format for ggplot
plot_data <- log2_counts %>%
  as.data.frame() %>%
  tibble::rownames_to_column("gene") %>%
  pivot_longer(cols = -gene, names_to = "sample", values_to = "expression")

# Add metadata
plot_data <- plot_data %>%
  left_join(
    sample_metadata %>% tibble::rownames_to_column("sample"),
    by = "sample"
  )

cat("Prepared data for", NUM_GENES, "genes\n")

################################################################################
# Create Boxplot
################################################################################

cat("\nCreating boxplot...\n")

# Create plot
p <- ggplot(plot_data, aes_string(x = X_AXIS, y = "expression", fill = FILL_BY)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Gene Expression Boxplot",
    x = X_AXIS,
    y = "log2(Normalized Counts + 0.5)",
    fill = FILL_BY
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Add custom colors if specified
if (!is.null(CUSTOM_COLORS) && length(CUSTOM_COLORS) > 0) {
  p <- p + scale_fill_manual(values = CUSTOM_COLORS)
}

# Facet by gene if multiple genes
if (NUM_GENES > 1) {
  p <- p + facet_wrap(~ gene, scales = "free_y", ncol = NUM_COLS)
}

# Display
print(p)

################################################################################
# Save High-Resolution Plots
################################################################################

# Calculate height based on number of genes
plot_height <- if (NUM_GENES > 1) {
  max(6, ceiling(NUM_GENES / NUM_COLS) * 4)
} else {
  6
}

# Save as PDF
ggsave("boxplot.pdf", p, width = 10, height = plot_height, dpi = 300)

# Save as PNG
ggsave("boxplot.png", p, width = 10, height = plot_height, dpi = 300)

cat("\n✓ Boxplot saved successfully!\n")
cat("  - boxplot.pdf (", NUM_GENES, " genes)\n")
cat("  - boxplot.png (high resolution)\n")
