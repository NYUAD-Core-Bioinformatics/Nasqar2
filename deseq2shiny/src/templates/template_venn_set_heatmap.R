################################################################################
# Publication-ready Venn Set Operation Heatmap
# Generated from DESeq2Shiny
#
# This heatmap visualizes log2FoldChange values for genes from a Venn diagram
# set operation (e.g., A*B for intersection, A+B for union, A-B for difference).
#
# This script is designed to work together with the Venn diagram script.
# Run the Venn diagram script first to understand the set operations,
# then use this script to visualize the expression patterns of genes
# in the specific set of interest.
#
# This script can be run independently by editing the configuration parameters below
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Set expression (e.g., "A*B", "A+B", "A-B")
SET_EXPRESSION <- "{{set_expression}}"

# Comparison names
COMPARISONS <- {{comparisons}}

# Ensure COMPARISONS is a character vector (not a list)
if (is.list(COMPARISONS)) {
  COMPARISONS <- as.character(unlist(COMPARISONS))
}

# Filtering thresholds (from Shiny app settings - must match Venn diagram)
PADJ_THRESHOLD <- {{padj_threshold}}  # Adjusted p-value threshold
FC_THRESHOLD <- {{fc_threshold}}      # |log2FoldChange| threshold

# Gene name settings
USE_GENE_NAMES <- {{use_gene_names}}  # TRUE to use gene names, FALSE for gene IDs

# Number of genes to display
NUM_GENES <- {{num_genes}}

# Data files (for full mode)
EXPRESSION_MATRIX_FILE <- "{{expression_matrix_file}}"

# Plot parameters
FONTSIZE_ROW <- {{fontsize_row}}

# Brushed heatmap settings (preserve order if this is a brushed sub-heatmap)
IS_BRUSHED <- {{is_brushed}}  # TRUE if this is a brushed sub-heatmap
BRUSHED_GENES <- {{brushed_genes}}  # Vector of gene IDs (if brushed)
SAMPLE_ORDER <- {{sample_order}}  # Comparison order (NULL if not brushed)
COLOR_RANGE <- {{color_range}}  # Parent heatmap's data range for consistent colors

# Ensure defaults for backwards compatibility
if (!exists("IS_BRUSHED") || is.null(IS_BRUSHED) || length(IS_BRUSHED) == 0) {
  IS_BRUSHED <- FALSE
}
if (!exists("SAMPLE_ORDER") || is.null(SAMPLE_ORDER) || length(SAMPLE_ORDER) == 0) {
  SAMPLE_ORDER <- NULL
}
if (!exists("COLOR_RANGE") || is.null(COLOR_RANGE) || length(COLOR_RANGE) == 0) {
  COLOR_RANGE <- NULL
}

################################################################################
# Load Required Libraries
################################################################################

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

################################################################################
# Load Data and Build Expression Matrix
################################################################################

{{DATA_LOAD_SECTION}}

# If set_expression_matrix wasn't loaded by DATA_LOAD_SECTION, build it
if (!exists("set_expression_matrix")) {
  # Use gene sets from Venn diagram section above
  if (IS_BRUSHED && !is.null(BRUSHED_GENES) && length(BRUSHED_GENES) > 0) {
    #-----------------------------------------------------------------------------
    # BRUSHED HEATMAP: Use pre-selected genes
    #-----------------------------------------------------------------------------
    cat("Building expression matrix for BRUSHED genes from set operation...\n")
    
    # This is a brushed sub-heatmap - use specific brushed genes
    set_genes <- BRUSHED_GENES
    cat("Set expression: ", SET_EXPRESSION, "\n")
    cat("Using ", length(set_genes), " brushed genes (user selection)\n\n", sep="")
    
  } else {
    #-----------------------------------------------------------------------------
    # FULL HEATMAP: Evaluate set operation to get genes
    #-----------------------------------------------------------------------------
    cat("Building expression matrix from set operation results...\n")
    
    # Evaluate set expression to get genes
    # Set operations: * = intersection, + = union, - = setdiff
    set_operation <- SET_EXPRESSION
    letters_in_expr <- unique(unlist(strsplit(gsub("[^A-Z]", "", set_operation), "")))
    cat("Set operation: ", set_operation, "\n", sep="")
    cat("Letters: ", paste(letters_in_expr, collapse=", "), "\n", sep="")
    cat("Thresholds: padj < ", PADJ_THRESHOLD, ", |log2FC| > ", FC_THRESHOLD, "\n\n", sep="")
    
    # Get genes from each set (using same filtering as Venn diagram)
    all_gene_sets <- list()
    for (i in seq_along(COMPARISONS)) {
      comparison_clean <- sub("\\.csv$", "", COMPARISONS[i])
      if (exists("results_list") && comparison_clean %in% names(results_list)) {
        res_data <- results_list[[comparison_clean]]
        # Filter by BOTH padj AND log2FoldChange thresholds (matches Shiny app)
        sig_genes <- rownames(res_data)[which(
          !is.na(res_data$padj) & 
          res_data$padj < PADJ_THRESHOLD & 
          abs(res_data$log2FoldChange) > FC_THRESHOLD
        )]
        all_gene_sets[[LETTERS[i]]] <- sig_genes
        cat("  Set ", LETTERS[i], " (", comparison_clean, "): ", length(sig_genes), " genes\n", sep="")
      } else {
        stop("\nERROR: Cannot find results for comparison '", comparison_clean, "'\n",
             "  results_list exists: ", exists("results_list"), "\n",
             "  Available: ", if(exists("results_list")) paste(names(results_list), collapse=", ") else "N/A", "\n\n",
             "  This script requires results_list from Section 0 (DESeq2 pipeline).\n",
             "  If running standalone, ensure you're using full export mode with data files.\n")
      }
    }
    
    # Evaluate set operation
    if (grepl("\\*", set_operation) && !grepl("[+-]", set_operation)) {
      # Pure intersection
      set_genes <- Reduce(intersect, all_gene_sets)
    } else if (grepl("\\+", set_operation) && !grepl("[*-]", set_operation)) {
      # Pure union
      set_genes <- Reduce(union, all_gene_sets)
    } else if (grepl("-", set_operation)) {
      # Difference: A-B means genes in A but not in B
      parts <- strsplit(set_operation, "-")[[1]]
      set_genes <- all_gene_sets[[parts[1]]]
      for (i in 2:length(parts)) {
        set_genes <- setdiff(set_genes, all_gene_sets[[parts[i]]])
      }
    } else {
      # Default: use all genes from first set
      set_genes <- all_gene_sets[[1]]
    }
    
    cat("\nSet operation result: ", length(set_genes), " genes\n\n", sep="")
  }
  
  #-----------------------------------------------------------------------------
  # Create expression matrix with log2FoldChange for each comparison
  #-----------------------------------------------------------------------------
  set_expression_matrix <- data.frame(row.names = set_genes)
  
  # Verify results_list exists before extracting log2FC values
  if (!exists("results_list")) {
    stop("\nERROR: results_list not found!\n",
         "  This script expects results_list to be created in Section 0 (DESeq2 pipeline).\n",
         "  Please ensure Section 0 has been run before this Venn Set Heatmap section.\n")
  }
  
  cat("Building expression matrix columns...\n")
  for (i in seq_along(COMPARISONS)) {
    comparison_clean <- sub("\\.csv$", "", COMPARISONS[i])
    cat("  Processing comparison", i, ":", comparison_clean, "... ")
    
    if (comparison_clean %in% names(results_list)) {
      res_data <- results_list[[comparison_clean]]
      # Extract log2FC for genes in set
      fc_values <- res_data[set_genes, "log2FoldChange"]
      set_expression_matrix[, COMPARISONS[i]] <- fc_values
      cat("✓ Added (", length(fc_values), " values)\n", sep="")
    } else {
      cat("✗ SKIPPED - Not found in results_list\n")
      cat("    Available in results_list: ", paste(names(results_list), collapse=", "), "\n")
    }
  }
  
  cat("\nExpression matrix created: ", nrow(set_expression_matrix), " genes x ", 
      ncol(set_expression_matrix), " comparisons\n", sep="")
  
  # Verify we have at least one comparison column
  if (ncol(set_expression_matrix) == 0) {
    stop("\nERROR: Expression matrix has 0 columns!\n",
         "  Failed to match any comparisons between requested and available results.\n",
         "  Requested comparisons: ", paste(COMPARISONS, collapse=", "), "\n",
         "  Cleaned names: ", paste(sapply(COMPARISONS, function(x) sub("\\.csv$", "", x)), collapse=", "), "\n",
         "  Available in results_list: ", paste(names(results_list), collapse=", "), "\n\n",
         "  This is likely a name mismatch issue. Check that comparison names in COMPARISONS\n",
         "  match the keys in results_list after removing .csv extension.\n")
  }
  
  cat("\n")
} else {
  # Matrix was loaded from file via DATA_LOAD_SECTION
  cat("Using pre-loaded expression matrix: ", nrow(set_expression_matrix), " genes x ", 
      ncol(set_expression_matrix), " comparisons\n\n", sep="")
}

# Convert gene IDs to gene names if requested
if (USE_GENE_NAMES && exists("gene_annotations") && !is.null(gene_annotations)) {
  # Get gene names for the rownames
  gene_ids <- rownames(set_expression_matrix)
  gene_names <- gene_annotations[gene_ids, "gene.names"]
  
  # Use gene names where available, keep IDs where not
  gene_names[is.na(gene_names)] <- gene_ids[is.na(gene_names)]
  rownames(set_expression_matrix) <- gene_names
  
  cat("Converted rownames to gene names\n\n")
} else if (USE_GENE_NAMES) {
  cat("Note: USE_GENE_NAMES is TRUE but gene annotations not available\n")
  cat("      Using gene IDs instead\n\n")
}

################################################################################
# Prepare Heatmap Matrix
################################################################################

# The matrix should have genes as rows and comparisons as columns
# Values are log2FoldChange for each gene in each comparison
plot_matrix <- set_expression_matrix

cat("Creating heatmap for ", nrow(plot_matrix), " genes from set operation: ", SET_EXPRESSION, "\n", sep="")
cat("Comparisons: ", paste(COMPARISONS, collapse = ", "), "\n\n", sep="")

################################################################################
# Create Heatmap (using ComplexHeatmap like Shiny app)
################################################################################

# Create column annotation
comparison_names <- COMPARISONS
ha <- HeatmapAnnotation(
  Comparison = comparison_names,
  which = "column",
  show_annotation_name = TRUE,
  annotation_legend_param = list(
    Comparison = list(title = "Comparison")
  )
)

# Reorder comparisons if this is a brushed heatmap (preserve original order)
if (IS_BRUSHED && !is.null(SAMPLE_ORDER) && length(SAMPLE_ORDER) > 0) {
  cat("Brushed heatmap: Preserving original comparison order\n")
  valid_comparisons <- SAMPLE_ORDER[SAMPLE_ORDER %in% colnames(plot_matrix)]
  if (length(valid_comparisons) > 0) {
    plot_matrix <- plot_matrix[, valid_comparisons, drop = FALSE]
  }
}

# Create color function
# For brushed heatmaps, use parent heatmap's data range for consistent colors
if (IS_BRUSHED && !is.null(COLOR_RANGE) && length(COLOR_RANGE) == 2) {
  cat("Using parent heatmap color range: ", COLOR_RANGE[1], " to ", COLOR_RANGE[2], "\n", sep="")
  col_fun <- colorRamp2(
    c(COLOR_RANGE[1], 0, COLOR_RANGE[2]),
    c("#0000FF", "#FFFFFF", "#FF0000")  # Blue-white-red
  )
} else {
  # For non-brushed or if no range specified, let ComplexHeatmap auto-scale
  col_fun <- NULL
}

# Create heatmap using ComplexHeatmap (matches Shiny app)
set_heatmap_args <- list(
  plot_matrix,
  name = "log2FC"  # legend title
)

# Add color function if specified
if (!is.null(col_fun)) {
  set_heatmap_args$col <- col_fun
}

set_heatmap <- do.call(Heatmap, c(set_heatmap_args, list(
  
  # Clustering: For brushed heatmaps, preserve original order
  cluster_rows = if (length(IS_BRUSHED) > 0 && IS_BRUSHED) FALSE else TRUE,  # Don't re-cluster genes for brushed heatmaps
  cluster_columns = FALSE,  # Never cluster comparisons
  clustering_distance_rows = if (length(IS_BRUSHED) == 0 || !IS_BRUSHED) "euclidean" else NULL,
  clustering_method_rows = if (length(IS_BRUSHED) == 0 || !IS_BRUSHED) "complete" else NULL,
  
  # Display options
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = FONTSIZE_ROW),
  column_names_gp = gpar(fontsize = 10),
  
  # Annotations
  top_annotation = ha,
  
  # Title
  column_title = if (IS_BRUSHED) {
    paste("Brushed Venn Sub-Heatmap:", SET_EXPRESSION)
  } else {
    paste("Venn Set Operation:", SET_EXPRESSION)
  },
  column_title_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # Heatmap body
  border = TRUE,
  
  # Cell size
  row_names_side = "left",
  width = unit(length(COMPARISONS) * 2, "cm"),
  height = unit(min(nrow(plot_matrix) * 0.5, 20), "cm")
)))

# Display heatmap
draw(set_heatmap)

################################################################################
# Save High-Resolution Heatmap
################################################################################

# Calculate appropriate height based on number of genes
heatmap_height <- max(8, NUM_GENES * 0.15)
heatmap_height_px <- max(2400, NUM_GENES * 45)

# Clean the set expression for filename
set_expr_clean <- gsub("[^A-Za-z0-9]", "_", SET_EXPRESSION)

# Save as PDF
pdf(paste0("venn_set_heatmap_", set_expr_clean, ".pdf"), 
    width = 10, height = heatmap_height)
draw(set_heatmap)
dev.off()

# Save as PNG
png(paste0("venn_set_heatmap_", set_expr_clean, ".png"), 
    width = 3000, height = heatmap_height_px, res = 300)
draw(set_heatmap)
dev.off()

cat("\nVenn set operation heatmap saved successfully!\n")
cat("  - venn_set_heatmap_", set_expr_clean, ".pdf\n", sep="")
cat("  - venn_set_heatmap_", set_expr_clean, ".png\n", sep="")
cat("\nThis heatmap shows log2FC values for ", NUM_GENES, " genes across ", length(COMPARISONS), " comparisons.\n", sep="")
