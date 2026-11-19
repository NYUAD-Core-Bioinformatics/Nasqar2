################################################################################
# UpSet Plot - Generated from DESeq2Shiny
################################################################################
#
# This script creates an UpSet plot from pre-calculated gene lists.
# All filtering (padj, log2FC thresholds) was done by Shiny.
# This script just loads the gene lists and draws the plot.
#
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
################################################################################

# Number of comparisons
NUM_SETS <- {{num_sets}}

# Comparison names
COMPARISON_NAMES <- {{comparison_names}}

# Gene list files
GENE_LIST_FILES <- {{gene_list_files}}

################################################################################
# Load Required Libraries
################################################################################

library(ComplexHeatmap)
library(grid)

################################################################################
# Load Gene Lists
################################################################################

cat("Loading gene lists...\n")

gene_lists <- list()
for (i in 1:NUM_SETS) {
  gene_list <- read.csv(GENE_LIST_FILES[i], header = TRUE, stringsAsFactors = FALSE)
  gene_lists[[i]] <- gene_list[[1]]  # First column contains gene IDs
  cat("  -", COMPARISON_NAMES[i], ":", length(gene_lists[[i]]), "genes\n")
}

# Name the lists with letters (A, B, C, etc.)
names(gene_lists) <- LETTERS[1:NUM_SETS]

cat("\nTotal genes per set:\n")
for (name in names(gene_lists)) {
  cat("  Set", name, ":", length(gene_lists[[name]]), "genes\n")
}

################################################################################
# Create UpSet Plot
################################################################################

cat("\nCreating UpSet plot...\n")

# One column per non-empty region ("distinct" mode: the regions a Venn diagram
# would show), largest first, one row per comparison, labelled with its letter.
upset_colour <- "#2E7BD6"
upset_sets <- gene_lists
names(upset_sets) <- paste0(names(upset_sets), ": ", sub("\\.csv$", "", COMPARISON_NAMES))
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

cat("✓ UpSet plot created\n\n")

################################################################################
# Save High-Resolution Plots
################################################################################

cat("Saving plots...\n")

# Save as PDF
pdf("upset_plot.pdf", width = 10, height = 8)
draw(upset_plot, padding = unit(c(2, 14, 2, 2), "mm"))
dev.off()

# Save as PNG
png("upset_plot.png", width = 1500, height = 1200, res = 150)
draw(upset_plot, padding = unit(c(2, 14, 2, 2), "mm"))
dev.off()

cat("✓ Plots saved successfully!\n")
cat("  - upset_plot.pdf\n")
cat("  - upset_plot.png\n\n")

################################################################################
# Summary
################################################################################

cat("UpSet Plot Summary\n")
cat("==================\n")
cat("Comparisons:", paste(COMPARISON_NAMES, collapse = ", "), "\n")
cat("Sets:", NUM_SETS, "\n")
for (i in 1:NUM_SETS) {
  cat("  ", LETTERS[i], ":", length(gene_lists[[i]]), "genes\n")
}
