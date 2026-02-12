################################################################################
# Venn Diagram - Generated from DESeq2Shiny
################################################################################
#
# This script creates a Venn diagram from pre-calculated gene lists.
# All filtering (padj, log2FC thresholds) was done by Shiny.
# This script just loads the gene lists and draws the diagram.
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

# Venn diagram colors
VENN_COLORS <- {{venn_colors}}

################################################################################
# Load Required Libraries
################################################################################

library(VennDiagram)
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
# Create Venn Diagram
################################################################################

cat("\nCreating Venn diagram...\n")

# Create Venn diagram
venn_plot <- venn.diagram(
  gene_lists,
  filename = NULL,
  fill = VENN_COLORS[1:NUM_SETS],
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.5,
  cat.fontface = "bold"
)

# Display
grid.newpage()
grid.draw(venn_plot)

cat("✓ Venn diagram created\n\n")

################################################################################
# Save High-Resolution Plots
################################################################################

cat("Saving plots...\n")

# Save as PDF
pdf("venn_diagram.pdf", width = 10, height = 10)
grid.newpage()
grid.draw(venn_plot)
dev.off()

# Save as PNG
png("venn_diagram.png", width = 1200, height = 1200, res = 150)
grid.newpage()
grid.draw(venn_plot)
dev.off()

cat("✓ Plots saved successfully!\n")
cat("  - venn_diagram.pdf\n")
cat("  - venn_diagram.png\n\n")

################################################################################
# Summary
################################################################################

cat("Venn Diagram Summary\n")
cat("====================\n")
cat("Comparisons:", paste(COMPARISON_NAMES, collapse = ", "), "\n")
cat("Sets:", NUM_SETS, "\n")
for (i in 1:NUM_SETS) {
  cat("  ", LETTERS[i], ":", length(gene_lists[[i]]), "genes\n")
}
