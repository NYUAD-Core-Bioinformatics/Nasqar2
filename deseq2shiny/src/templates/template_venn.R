################################################################################
# Publication-ready UpSet Plot
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

# Data files (for full mode)
# Gene list files - one for each comparison
GENE_FILES <- {{gene_files}}

################################################################################
# Load Required Libraries
################################################################################

library(ComplexHeatmap)
library(grid)

################################################################################
# Load Gene Lists
################################################################################

cat("Loading gene sets for the UpSet plot...\n")
cat("  Thresholds: padj < ", PADJ_THRESHOLD, ", |log2FC| > ", FC_THRESHOLD, "\n\n", sep="")

{{DATA_LOAD_SECTION}}

# If gene_sets wasn't created by DATA_LOAD_SECTION, build from results_list
if (!exists("gene_sets")) {
  # Extract significant genes from results_list generated in Section 0
  # Verify results_list exists (created in Section 0)
  if (!exists("results_list")) {
    stop("\nERROR: results_list not found!\n",
         "  This script expects results_list to be created in Section 0 (DESeq2 pipeline).\n",
         "  Please ensure Section 0 has been run before this UpSet plot section.\n")
  }
  
  cat("  Available results:", paste(names(results_list), collapse=", "), "\n")
  
  gene_sets <- list()
  
  for (i in 1:length(COMPARISONS)) {
    # Clean comparison name (remove .csv extension)
    comparison_clean <- sub("\\.csv$", "", COMPARISONS[i])
    
    # Get results from results_list
    if (comparison_clean %in% names(results_list)) {
      res_data <- results_list[[comparison_clean]]
      
      # Filter by BOTH padj AND log2FoldChange thresholds (matches Shiny app)
      sig_genes <- rownames(res_data)[which(
        !is.na(res_data$padj) & 
        res_data$padj < PADJ_THRESHOLD & 
        abs(res_data$log2FoldChange) > FC_THRESHOLD
      )]
      
      # Use letter labels (A, B, C), as in the app's set expressions
      gene_sets[[LETTERS[i]]] <- sig_genes
      cat("  Set ", LETTERS[i], " (", comparison_clean, "): ", length(sig_genes), " genes\n", sep="")
    } else {
      # Comparison not found in results_list
      stop("\nERROR: Cannot find results for comparison '", comparison_clean, "'\n",
           "  Requested: ", comparison_clean, "\n",
           "  Available: ", paste(names(results_list), collapse=", "), "\n\n",
           "  This mismatch may be due to:\n",
           "  1. Section 0 didn't create results for this comparison\n",
           "  2. Comparison name mismatch between the UpSet parameters and Section 0 output\n",
           "  3. Section 0 encountered an error\n")
    }
  }
}

cat("\nGene sets created for the UpSet plot\n\n")

################################################################################
# Create UpSet Plot
################################################################################

# Note: gene_sets is a named list using letter labels (A, B, C, etc.)
# Each letter corresponds to a comparison from COMPARISONS in order

# Print mapping of letters to comparisons
cat("UpSet plot label mapping:\n")
for (i in 1:length(gene_sets)) {
  comparison_name <- sub("\\.csv$", "", COMPARISONS[i])
  cat("  ", names(gene_sets)[i], " = ", comparison_name, "\n", sep = "")
}
cat("\n")

cat("Creating UpSet plot for", NUM_SETS, "sets\n")
for (i in 1:length(gene_sets)) {
  cat("  Set", names(gene_sets)[i], ":", length(gene_sets[[i]]), "genes\n")
}
cat("\n")

# One column per non-empty region ("distinct" mode: the regions a Venn diagram
# would show), largest first, one row per comparison, labelled with its letter.
upset_colour <- "#2E7BD6"
upset_sets <- gene_sets
names(upset_sets) <- paste0(names(upset_sets), ": ", sub("\\.csv$", "", COMPARISONS))
combinations <- make_comb_mat(upset_sets, mode = "distinct")
combinations <- combinations[comb_size(combinations) > 0]
bar_style <- gpar(fill = upset_colour, col = NA)
upset_plot <- UpSet(
  combinations,
  set_order = seq_along(set_name(combinations)),
  comb_order = order(-comb_size(combinations), comb_degree(combinations)),
  comb_col = upset_colour,
  top_annotation = upset_top_annotation(combinations, add_numbers = TRUE, gp = bar_style),
  right_annotation = upset_right_annotation(combinations, add_numbers = TRUE, gp = bar_style)
)

# Display
draw(upset_plot, padding = unit(c(2, 14, 2, 2), "mm"))

################################################################################
# Save to File
################################################################################

pdf("upset_plot.pdf", width = 10, height = 8)
draw(upset_plot, padding = unit(c(2, 14, 2, 2), "mm"))
dev.off()

png("upset_plot.png", width = 3000, height = 2400, res = 300)
draw(upset_plot, padding = unit(c(2, 14, 2, 2), "mm"))
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

cat("\nUpSet plot saved successfully!\n")
cat("\nTo create a heatmap for specific gene sets, use the venn_set_operations list.\n")
cat("Example: genes_of_interest <- venn_set_operations$intersection\n")
