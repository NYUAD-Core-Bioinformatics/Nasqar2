################################################################################
# Publication-ready Venn Diagram
# Generated from DESeq2Shiny
#
# This script can be run independently by editing the configuration parameters below
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Comparison names
COMPARISONS <- {{comparisons}}

# Number of sets
NUM_SETS <- {{num_sets}}

# Ensure NUM_SETS has a default value for backwards compatibility
if (!exists("NUM_SETS") || is.null(NUM_SETS) || length(NUM_SETS) == 0) {
  NUM_SETS <- length(COMPARISONS)
}

# Filtering thresholds (from Shiny app settings)
PADJ_THRESHOLD <- {{padj_threshold}}  # Adjusted p-value threshold
FC_THRESHOLD <- {{fc_threshold}}      # |log2FoldChange| threshold

# Colors for Venn diagram
VENN_COLORS <- {{venn_colors}}

# Data files (for full mode)
# Gene list files - one for each comparison
GENE_FILES <- {{gene_files}}

################################################################################
# Load Required Libraries
################################################################################

library(VennDiagram)

################################################################################
# Load Gene Lists
################################################################################

{{DATA_LOAD_SECTION}}

################################################################################
# Create Venn Diagram
################################################################################

# Note: gene_sets is a named list using letter labels (A, B, C, etc.)
# Each letter corresponds to a comparison from COMPARISONS in order

# Print mapping of letters to comparisons
cat("Venn diagram label mapping:\n")
for (i in 1:length(gene_sets)) {
  comparison_name <- sub("\\.csv$", "", COMPARISONS[i])
  cat("  ", names(gene_sets)[i], " = ", comparison_name, "\n", sep = "")
}
cat("\n")

cat("Creating Venn diagram for", NUM_SETS, "sets\n")
for (i in 1:length(gene_sets)) {
  cat("  Set", names(gene_sets)[i], ":", length(gene_sets[[i]]), "genes\n")
}
cat("\n")

# Create Venn diagram
venn_plot <- venn.diagram(
  x = gene_sets,
  filename = NULL,
  fill = VENN_COLORS,
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.2,
  cat.fontface = "bold",
  margin = 0.1
)

# Display
grid::grid.newpage()
grid::grid.draw(venn_plot)

################################################################################
# Save to File
################################################################################

pdf("venn_diagram.pdf", width = 8, height = 8)
grid::grid.draw(venn_plot)
dev.off()

png("venn_diagram.png", width = 2400, height = 2400, res = 300)
grid::grid.draw(venn_plot)
dev.off()

################################################################################
# Calculate Intersections and Set Operations
################################################################################

cat("\nGene counts:\n")
for (i in 1:length(gene_sets)) {
  cat(names(gene_sets)[i], ":", length(gene_sets[[i]]), "genes\n")
}

# Calculate intersections for 2-3 sets
if (length(NUM_SETS) > 0 && NUM_SETS == 2) {
  set_names <- names(gene_sets)
  intersection <- intersect(gene_sets[[1]], gene_sets[[2]])
  union_genes <- union(gene_sets[[1]], gene_sets[[2]])
  only_first <- setdiff(gene_sets[[1]], gene_sets[[2]])
  only_second <- setdiff(gene_sets[[2]], gene_sets[[1]])
  
  cat("\nSet operations:\n")
  cat("  ", set_names[1], "∩", set_names[2], ":", length(intersection), "genes\n")
  cat("  ", set_names[1], "∪", set_names[2], ":", length(union_genes), "genes\n")
  cat("  ", set_names[1], "-", set_names[2], ":", length(only_first), "genes\n")
  cat("  ", set_names[2], "-", set_names[1], ":", length(only_second), "genes\n")
  
  # Store results for potential downstream use
  venn_set_operations <- list(
    intersection = intersection,
    union = union_genes,
    only_first = only_first,
    only_second = only_second
  )
  
} else if (length(NUM_SETS) > 0 && NUM_SETS == 3) {
  set_names <- names(gene_sets)
  
  # All pairwise intersections
  int_12 <- intersect(gene_sets[[1]], gene_sets[[2]])
  int_13 <- intersect(gene_sets[[1]], gene_sets[[3]])
  int_23 <- intersect(gene_sets[[2]], gene_sets[[3]])
  
  # Three-way intersection
  int_123 <- Reduce(intersect, gene_sets)
  
  # Union
  union_genes <- Reduce(union, gene_sets)
  
  cat("\nSet operations:\n")
  cat("  ", set_names[1], "∩", set_names[2], ":", length(int_12), "genes\n")
  cat("  ", set_names[1], "∩", set_names[3], ":", length(int_13), "genes\n")
  cat("  ", set_names[2], "∩", set_names[3], ":", length(int_23), "genes\n")
  cat("  ", set_names[1], "∩", set_names[2], "∩", set_names[3], ":", length(int_123), "genes\n")
  cat("  All union:", length(union_genes), "genes\n")
  
  # Store results
  venn_set_operations <- list(
    int_12 = int_12,
    int_13 = int_13,
    int_23 = int_23,
    int_123 = int_123,
    union = union_genes
  )
}

cat("\nVenn diagram saved successfully!\n")
cat("\nTo create a heatmap for specific gene sets, use the venn_set_operations list.\n")
cat("Example: genes_of_interest <- venn_set_operations$intersection\n")
