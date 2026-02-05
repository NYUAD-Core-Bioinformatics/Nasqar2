################################################################################
# Publication-ready Heatmap
# Generated from DESeq2Shiny
#
# This script can be run independently by editing the configuration parameters below
#
# NOTE: This is a STATIC heatmap for publication/reports.
# The Shiny app provides INTERACTIVE heatmaps with:
#   - Real-time brushing to select genes
#   - Click interactions
#   - Zooming and panning
# 
# For interactive exploration, use the Shiny app.
# For publication-quality static heatmaps, use this script.
################################################################################

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Gene selection
NUM_GENES <- {{num_genes}}
SELECTED_GENES <- {{selected_genes}}

# Data files (for full mode)
COUNTS_FILE <- "{{counts_file}}"
METADATA_FILE <- "{{metadata_file}}"

# Plot parameters
USE_GENE_NAMES <- {{use_gene_names}}
SCALE_ROWS <- {{scale_rows}}
SHOW_ROWNAMES <- TRUE
SHOW_COLNAMES <- TRUE
FONTSIZE_ROW <- {{fontsize_row}}

# Brushed heatmap settings (preserve original clustering)
IS_BRUSHED_HEATMAP <- {{is_brushed_heatmap}}  # TRUE if this is a brushed sub-heatmap
SAMPLE_ORDER <- {{sample_order}}  # Sample order from original heatmap (NULL if not brushed)
IS_VENN_HEATMAP <- {{is_venn_heatmap}}  # TRUE if data is from Venn diagram (already log2FoldChange)
COLOR_RANGE <- {{color_range}}  # Color range from parent heatmap (NULL if not brushed)

# Ensure defaults for backwards compatibility
if (!exists("IS_VENN_HEATMAP") || is.null(IS_VENN_HEATMAP) || length(IS_VENN_HEATMAP) == 0) {
  IS_VENN_HEATMAP <- FALSE
}
if (!exists("IS_BRUSHED_HEATMAP") || is.null(IS_BRUSHED_HEATMAP) || length(IS_BRUSHED_HEATMAP) == 0) {
  IS_BRUSHED_HEATMAP <- FALSE
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
# Load Data
################################################################################

{{DATA_LOAD_SECTION}}

################################################################################
# Select Genes for Heatmap
################################################################################

# First, separate numeric data from gene.names column if present
if ("gene.names" %in% colnames(normalized_counts)) {
  cat("Separating gene names from numeric count data...\n")
  gene_names_col <- normalized_counts[, "gene.names"]
  names(gene_names_col) <- rownames(normalized_counts)
  
  # Check if gene names are valid (not all NA)
  if (all(is.na(gene_names_col))) {
    warning("gene.names column contains all NA values. Gene names may not display correctly.")
    cat("WARNING: gene.names column is empty (all NA). Using gene IDs instead.\n")
    gene_names_col <- NULL  # Treat as if no gene names available
  } else {
    cat("Found", sum(!is.na(gene_names_col)), "gene names out of", length(gene_names_col), "genes\n")
  }
  
  # Keep only numeric columns for the heatmap
  numeric_cols <- setdiff(colnames(normalized_counts), "gene.names")
  normalized_counts_numeric <- normalized_counts[, numeric_cols, drop = FALSE]
} else {
  normalized_counts_numeric <- normalized_counts
  gene_names_col <- NULL
}

if (!is.null(SELECTED_GENES) && length(SELECTED_GENES) > 0) {
  # Use specified genes
  cat("Using", length(SELECTED_GENES), "specified genes\n")
  
  # For Venn heatmaps, rownames are already in the correct format (no mapping needed)
  if (IS_VENN_HEATMAP) {
    cat("Venn heatmap: Using gene identifiers directly (no mapping)\n")
    # SELECTED_GENES are already in the correct format (gene IDs or gene names)
    # Just subset directly
    plot_matrix <- normalized_counts_numeric[SELECTED_GENES, , drop = FALSE]
  } else if (USE_GENE_NAMES && !is.null(gene_names_col)) {
    # Regular heatmap: Map gene names to gene IDs for subsetting
    cat("Mapping gene names to gene IDs for subsetting...\n")
    # Create a mapping from gene names to gene IDs
    gene_name_to_id <- names(gene_names_col)
    names(gene_name_to_id) <- gene_names_col
    
    cat("Looking for", length(SELECTED_GENES), "selected gene names in data...\n")
    cat("First few selected genes:", head(SELECTED_GENES, 3), "\n")
    cat("First few available gene names:", head(na.omit(gene_names_col), 3), "\n")
    
    # Find gene IDs corresponding to the selected gene names
    gene_ids_to_use <- gene_name_to_id[SELECTED_GENES]
    
    # Remove any NAs (genes not found)
    genes_found <- !is.na(gene_ids_to_use)
    gene_ids_to_use <- gene_ids_to_use[genes_found]
    
    if (length(gene_ids_to_use) == 0) {
      stop(paste0("None of the selected gene names were found in the data!\n",
                  "Selected genes: ", paste(head(SELECTED_GENES, 5), collapse = ", "), "...\n",
                  "Available gene names: ", paste(head(na.omit(gene_names_col), 5), collapse = ", "), "...\n",
                  "Hint: Check if gene names column was exported correctly."))
    }
    
    if (length(gene_ids_to_use) < length(SELECTED_GENES)) {
      missing_genes <- SELECTED_GENES[!genes_found]
      cat("Warning:", length(missing_genes), "gene names not found in data\n")
      cat("Missing genes:", paste(head(missing_genes, 5), collapse = ", "), "\n")
    }
    
    # Subset by gene IDs (using numeric data only)
    plot_matrix <- normalized_counts_numeric[gene_ids_to_use, , drop = FALSE]
  } else {
    # SELECTED_GENES contains gene IDs, subset directly (using numeric data only)
    plot_matrix <- normalized_counts_numeric[SELECTED_GENES, , drop = FALSE]
  }
} else {
  # Select top genes by variance (using numeric data only)
  cat("Selecting top", NUM_GENES, "genes by variance\n")
  gene_variance <- apply(normalized_counts_numeric, 1, var)
  top_genes <- names(sort(gene_variance, decreasing = TRUE)[1:NUM_GENES])
  plot_matrix <- normalized_counts_numeric[top_genes, , drop = FALSE]
}

################################################################################
# Map Gene Names for Display (if requested)
################################################################################

# Replace rownames with gene symbols for display
if (IS_VENN_HEATMAP) {
  # For Venn heatmaps, rownames are already in the correct format
  cat("Venn heatmap: Using existing row labels (no display mapping needed)\n")
} else if (USE_GENE_NAMES) {
  if (!is.null(gene_names_col)) {
    # Gene names were separated earlier
    gene_ids <- rownames(plot_matrix)
    gene_names <- gene_names_col[gene_ids]
    # Remove NA values and keep gene IDs where gene names are missing
    gene_names[is.na(gene_names)] <- gene_ids[is.na(gene_names)]
    rownames(plot_matrix) <- gene_names
    cat("Mapped", sum(!is.na(gene_names)), "gene names for display\n")
  } else if (exists("gene_annotations") && "gene.names" %in% colnames(gene_annotations)) {
    # Gene names from separate annotations
    gene_ids <- rownames(plot_matrix)
    gene_names <- gene_annotations[gene_ids, "gene.names"]
    gene_names[is.na(gene_names)] <- gene_ids[is.na(gene_names)]
    rownames(plot_matrix) <- gene_names
    cat("Mapped", sum(!is.na(gene_names)), "gene names from annotations\n")
  }
}

################################################################################
# Transform Data
################################################################################

# Verify data is numeric before transformation
if (!is.numeric(plot_matrix)) {
  # Convert to matrix if it's a data frame
  if (is.data.frame(plot_matrix)) {
    plot_matrix <- as.matrix(plot_matrix)
  }
  # Check again
  if (!is.numeric(plot_matrix)) {
    stop("Error: plot_matrix contains non-numeric data. Please check your input files.")
  }
}

if (IS_VENN_HEATMAP) {
  cat("Data dimensions:", nrow(plot_matrix), "genes ×", ncol(plot_matrix), "comparisons\n")
  cat("Data type: log2FoldChange values from DESeq2 comparisons\n")
} else {
  cat("Data dimensions:", nrow(plot_matrix), "genes ×", ncol(plot_matrix), "samples\n")
}

cat("Data range before transformation: min =", round(min(plot_matrix, na.rm = TRUE), 2), 
    ", max =", round(max(plot_matrix, na.rm = TRUE), 2), "\n")

# Log2 transform (adding pseudocount)
# NOTE: Skip transformation for Venn heatmaps (already log2FoldChange)
if (!IS_VENN_HEATMAP) {
  # Assumes input data is RAW normalized counts (not already log-transformed)
  plot_matrix <- log2(plot_matrix + 0.5)
  cat("Data range after log2 transformation: min =", round(min(plot_matrix, na.rm = TRUE), 2), 
      ", max =", round(max(plot_matrix, na.rm = TRUE), 2), "\n")
} else {
  cat("Skipping log2 transformation (data already in log2 scale)\n")
}

# Z-score normalization (optional, for better visualization)
# Uncomment the next line to z-score normalize
if (SCALE_ROWS) {
  plot_matrix <- t(scale(t(plot_matrix)))
}

################################################################################
# Create Heatmap Annotations
################################################################################

# Create annotation (if metadata available)
if (IS_VENN_HEATMAP) {
  # For Venn heatmaps, metadata describes comparisons, not samples
  # Skip column annotation for Venn heatmaps
  annotation_col <- NA
} else {
  # For regular heatmaps, create sample annotations
  if (exists("sample_metadata") && !is.null(sample_metadata) && is.data.frame(sample_metadata) && nrow(sample_metadata) > 0) {
    # Select annotation columns (adjust as needed)
    annotation_col <- sample_metadata[colnames(plot_matrix), , drop = FALSE]
    # Remove non-informative columns
    annotation_col <- annotation_col[, !colnames(annotation_col) %in% c("sizeFactor", "replaceable"), drop = FALSE]
  } else {
    annotation_col <- NA
  }
}

################################################################################
# Create Heatmap (using ComplexHeatmap like Shiny app)
################################################################################

# Create column annotation if metadata available
if (is.data.frame(annotation_col) && nrow(annotation_col) > 0) {
  ha <- HeatmapAnnotation(
    df = annotation_col,
    which = "column",
    show_annotation_name = TRUE
  )
} else {
  ha <- NULL
}

# Reorder samples if this is a brushed heatmap (preserve original clustering)
if (IS_BRUSHED_HEATMAP && !is.null(SAMPLE_ORDER) && length(SAMPLE_ORDER) > 0) {
  cat("Brushed heatmap: Preserving original sample order from parent heatmap\n")
  # Reorder columns to match original heatmap's clustering
  valid_samples <- SAMPLE_ORDER[SAMPLE_ORDER %in% colnames(plot_matrix)]
  if (length(valid_samples) > 0) {
    plot_matrix <- plot_matrix[, valid_samples, drop = FALSE]
    if (!is.null(ha)) {
      # Also reorder annotation
      annotation_col <- annotation_col[valid_samples, , drop = FALSE]
      ha <- HeatmapAnnotation(
        df = annotation_col,
        which = "column",
        show_annotation_name = TRUE
      )
    }
  }
}

# Create color function for brushed heatmaps
# For brushed heatmaps, use parent heatmap's data range for consistent colors
col_fun <- NULL
if (IS_BRUSHED_HEATMAP && !is.null(COLOR_RANGE) && length(COLOR_RANGE) == 2) {
  cat("Using parent heatmap color range:", COLOR_RANGE[1], "to", COLOR_RANGE[2], "\n")
  # ComplexHeatmap's default uses RdYlBu (Red-Yellow-Blue) palette, NOT RdBu
  # This is reversed to go from Blue (low) -> Yellow (mid) -> Red (high)
  min_val <- COLOR_RANGE[1]
  max_val <- COLOR_RANGE[2]
  
  # Create smooth color gradient using RdYlBu (ComplexHeatmap's actual default)
  # with 11 colors reversed for Blue-Yellow-Red gradient
  color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)
  
  # Create breaks for the color function
  breaks <- seq(min_val, max_val, length.out = 255)
  col_fun <- colorRamp2(breaks, color_palette)
  
  cat("Color mapping: min=", min_val, ", max=", max_val, ", colors=255 (RdYlBu palette)\n")
}

# Create heatmap using ComplexHeatmap (matches Shiny app)
# Note: For non-brushed heatmaps, no color parameter specified for consistency with Shiny app
# ComplexHeatmap will auto-detect the data range and use appropriate colors
heatmap_args <- list(
  plot_matrix,
  name = if (length(IS_VENN_HEATMAP) > 0 && IS_VENN_HEATMAP) "log2FC" else "Expression",  # legend title
  
  # Clustering: For brushed heatmaps, preserve original order for both genes and samples
  cluster_rows = if (length(IS_BRUSHED_HEATMAP) > 0 && IS_BRUSHED_HEATMAP) FALSE else TRUE,  # Don't re-cluster genes for brushed heatmaps
  cluster_columns = if (length(IS_BRUSHED_HEATMAP) > 0 && IS_BRUSHED_HEATMAP) FALSE else TRUE,  # Don't re-cluster columns for brushed heatmaps
  clustering_distance_rows = if (length(IS_BRUSHED_HEATMAP) == 0 || !IS_BRUSHED_HEATMAP) "euclidean" else NULL,
  clustering_distance_columns = if (length(IS_BRUSHED_HEATMAP) == 0 || !IS_BRUSHED_HEATMAP) "euclidean" else NULL,
  clustering_method_rows = if (length(IS_BRUSHED_HEATMAP) == 0 || !IS_BRUSHED_HEATMAP) "complete" else NULL,
  clustering_method_columns = if (length(IS_BRUSHED_HEATMAP) == 0 || !IS_BRUSHED_HEATMAP) "complete" else NULL,
  
  # Display options
  show_row_names = SHOW_ROWNAMES,
  show_column_names = SHOW_COLNAMES,
  row_names_gp = gpar(fontsize = FONTSIZE_ROW),
  column_names_gp = gpar(fontsize = 8),
  
  # Annotations
  top_annotation = ha,
  
  # Title
  column_title = if (length(IS_VENN_HEATMAP) > 0 && IS_VENN_HEATMAP && length(IS_BRUSHED_HEATMAP) > 0 && IS_BRUSHED_HEATMAP) {
    "Brushed Venn Sub-Heatmap (log2FC Comparisons)"
  } else if (length(IS_BRUSHED_HEATMAP) > 0 && IS_BRUSHED_HEATMAP) {
    "Brushed Sub-Heatmap (Original Sample Order Preserved)"
  } else if (length(IS_VENN_HEATMAP) > 0 && IS_VENN_HEATMAP) {
    "Venn Set Heatmap (log2FC Comparisons)"
  } else {
    "Expression Heatmap"
  },
  column_title_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # Heatmap body
  border = TRUE
)

# Add color function if specified (for brushed heatmaps)
if (!is.null(col_fun)) {
  heatmap_args$col <- col_fun
}

heatmap_plot <- do.call(Heatmap, heatmap_args)

# Display heatmap
draw(heatmap_plot)

################################################################################
# Save High-Resolution Heatmap
################################################################################

# Calculate appropriate height based on number of genes
heatmap_height <- max(8, nrow(plot_matrix) * 0.15)
heatmap_height_px <- max(2400, nrow(plot_matrix) * 45)

# Save as PDF
pdf("heatmap.pdf", width = 10, height = heatmap_height)
draw(heatmap_plot)
dev.off()

# Save as PNG
png("heatmap.png", width = 3000, height = heatmap_height_px, res = 300)
draw(heatmap_plot)
dev.off()

cat("Heatmap saved successfully!\n")
cat("  - heatmap.pdf (", nrow(plot_matrix), " genes)\n")
cat("  - heatmap.png (high resolution)\n")
