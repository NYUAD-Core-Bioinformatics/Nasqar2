################################################################################
# Publication-ready Gene Expression Boxplot
# Generated from DESeq2Shiny
#
# This script can be run independently by editing the configuration parameters below
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Selected genes to plot
SELECTED_GENES <- {{selected_genes}}

# Plot grouping
X_AXIS <- "{{x_axis}}"
FILL_BY <- "{{fill_by}}"

# Data files (for full mode)
COUNTS_FILE <- "{{counts_file}}"
METADATA_FILE <- "{{metadata_file}}"

# Plot customization
USE_GENE_NAMES <- {{use_gene_names}}
CUSTOM_COLORS <- {{custom_colors}}
NUM_COLS <- {{num_cols}}

################################################################################
# Load Required Libraries
################################################################################

library(ggplot2)
library(tidyr)
library(dplyr)

################################################################################
# Load Data
################################################################################

{{DATA_LOAD_SECTION}}

################################################################################
# Prepare Data for Plotting
################################################################################

# Check if genes exist in rownames (could be Ensembl IDs or gene symbols)
if (all(SELECTED_GENES %in% rownames(counts_data))) {
  # Genes found directly in rownames
  gene_indices <- SELECTED_GENES
  cat("Using genes directly from rownames\n")
} else {
  # Try to find genes by matching - could be in a gene.names or symbol column
  # First check if there's a gene.names column in the count data itself
  if ("gene.names" %in% colnames(counts_data)) {
    # Match by gene name from the gene.names column
    matched_rows <- which(counts_data$gene.names %in% SELECTED_GENES)
    if (length(matched_rows) > 0) {
      gene_indices <- rownames(counts_data)[matched_rows]
      cat("Matched", length(matched_rows), "genes by name from gene.names column\n")
    } else {
      stop("Could not find selected genes in gene.names column. Please check gene names.")
    }
  } else if (exists("gene_annotations") && "gene.names" %in% colnames(gene_annotations)) {
    # Match by gene name from separate annotations object
    matched_indices <- match(SELECTED_GENES, gene_annotations$gene.names)
    if (any(!is.na(matched_indices))) {
      gene_indices <- rownames(counts_data)[matched_indices[!is.na(matched_indices)]]
      cat("Matched", sum(!is.na(matched_indices)), "genes by name from annotations\n")
    } else {
      stop("Could not find selected genes in data. Please check gene names.")
    }
  } else {
    # No annotation available - genes might already be in rownames
    cat("Warning: Some genes not found in rownames.\n")
    cat("Available rownames (first 10):", paste(head(rownames(counts_data), 10), collapse=", "), "\n")
    cat("Requested genes:", paste(SELECTED_GENES, collapse=", "), "\n")
    # Use only genes that exist
    gene_indices <- SELECTED_GENES[SELECTED_GENES %in% rownames(counts_data)]
    if (length(gene_indices) == 0) {
      stop("None of the selected genes found in count matrix. Check gene names/IDs.")
    }
    cat("Found", length(gene_indices), "out of", length(SELECTED_GENES), "genes\n")
  }
}

# Log2 transform counts (adding 0.5 pseudocount)
log2_counts <- log2(counts_data[gene_indices, , drop = FALSE] + 0.5)

# Use gene symbols for display if available, otherwise use IDs
# Check multiple sources for gene names
display_names <- if ("gene.names" %in% colnames(counts_data)) {
  # Use gene.names column from count data
  counts_data[gene_indices, "gene.names"]
} else if (exists("gene_annotations") && "gene.names" %in% colnames(gene_annotations)) {
  # Use separate annotations
  gene_annotations[gene_indices, "gene.names"]
} else {
  # Fallback to gene IDs
  gene_indices
}
rownames(log2_counts) <- display_names

# Reshape data for ggplot
plot_data <- as.data.frame(t(log2_counts))
plot_data$sample <- rownames(plot_data)

# Add metadata
plot_data <- merge(plot_data, sample_metadata, by.x = "sample", by.y = "row.names")

# Optional: Filter for specific samples or conditions
# Uncomment and modify to filter data:
# plot_data <- plot_data[plot_data[[X_AXIS]] %in% c("value1", "value2"), ]

# Convert to long format
plot_data_long <- plot_data %>%
  pivot_longer(cols = all_of(display_names), 
               names_to = "gene", 
               values_to = "expression")

################################################################################
# Create Boxplot
################################################################################

boxplot <- ggplot(plot_data_long, aes(x = .data[[X_AXIS]], y = expression, fill = .data[[FILL_BY]])) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5) +
  facet_wrap(~gene, scales = "free_y", ncol = NUM_COLS)

# Add custom colors if provided
if (!is.null(CUSTOM_COLORS) && length(CUSTOM_COLORS) > 0) {
  boxplot <- boxplot + scale_fill_manual(values = CUSTOM_COLORS)
}

# Add labels and theme
boxplot <- boxplot +
  labs(title = "Gene Expression Boxplot",
       x = X_AXIS,
       y = "log2(Normalized Counts + 0.5)",
       fill = FILL_BY) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.background = element_rect(fill = "lightblue"),
        panel.grid.minor = element_blank())

# Display plot
print(boxplot)

################################################################################
# Save High-Resolution Plot
################################################################################

plot_width <- max(8, length(SELECTED_GENES) * 2.5)
ggsave(paste0("boxplot_", X_AXIS, "_by_", FILL_BY, ".pdf"), boxplot, 
       width = plot_width, height = 6, dpi = 300)
ggsave(paste0("boxplot_", X_AXIS, "_by_", FILL_BY, ".png"), boxplot, 
       width = plot_width, height = 6, dpi = 300)
cat("Plots saved successfully!\n")
