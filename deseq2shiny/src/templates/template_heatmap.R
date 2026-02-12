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
SHOW_ROWNAMES <- TRUE
SHOW_COLNAMES <- TRUE
FONTSIZE_ROW <- {{fontsize_row}}

# Brushed heatmap settings (preserve original clustering)
IS_BRUSHED_HEATMAP <- {{is_brushed_heatmap}}  # TRUE if this is a brushed sub-heatmap
SAMPLE_ORDER <- {{sample_order}}  # Sample order from original heatmap (NULL if not brushed)
COLOR_RANGE <- {{color_range}}  # Color range from parent heatmap (NULL if not brushed)

# Ensure defaults for backwards compatibility
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
  
  if (USE_GENE_NAMES && !is.null(gene_names_col)) {
    # Map gene names to gene IDs for subsetting
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
  # Select top genes by standard deviation on log2-transformed counts
  # (matches Shiny app behavior)
  cat("Selecting top", NUM_GENES, "genes by SD (on log2-transformed counts)\n")
  
  # First log2-transform for gene selection (same as Shiny app)
  log_counts_for_selection <- log2(normalized_counts_numeric + 0.5)
  gene_sd <- apply(log_counts_for_selection, 1, sd)
  top_genes <- names(sort(gene_sd, decreasing = TRUE)[1:NUM_GENES])
  
  # Subset the RAW normalized counts (will be log2-transformed later)
  plot_matrix <- normalized_counts_numeric[top_genes, , drop = FALSE]
  cat("Selected genes based on highest variability in log2 space\n")
}

################################################################################
# Map Gene Names for Display (if requested)
################################################################################

# Replace rownames with gene symbols for display
if (USE_GENE_NAMES) {
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

cat("Data dimensions:", nrow(plot_matrix), "genes ×", ncol(plot_matrix), "samples\n")
cat("Data range before transformation: min =", round(min(plot_matrix, na.rm = TRUE), 2), 
    ", max =", round(max(plot_matrix, na.rm = TRUE), 2), "\n")

# Log2 transform (adding pseudocount)
# Assumes input data is RAW normalized counts (not already log-transformed)
plot_matrix <- log2(plot_matrix + 0.5)
cat("Data range after log2 transformation: min =", round(min(plot_matrix, na.rm = TRUE), 2), 
    ", max =", round(max(plot_matrix, na.rm = TRUE), 2), "\n")

# NOTE: Z-score normalization (row scaling) is NOT applied
# The Shiny app displays raw log2-transformed counts without scaling
# To match Shiny behavior, we skip scaling (no t(scale(t(plot_matrix))))

################################################################################
# Create Heatmap Annotations
################################################################################

# Create annotation (if metadata available)
if (exists("sample_metadata") && !is.null(sample_metadata) && is.data.frame(sample_metadata) && nrow(sample_metadata) > 0) {
  # Select annotation columns (adjust as needed)
  annotation_col <- sample_metadata[colnames(plot_matrix), , drop = FALSE]
  # Remove non-informative columns
  annotation_col <- annotation_col[, !colnames(annotation_col) %in% c("sizeFactor", "replaceable"), drop = FALSE]
} else {
  annotation_col <- NA
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

# Create color function for BOTH parent and brushed heatmaps
# This ensures consistent color mapping method between parent and brushed
col_fun <- NULL

if (IS_BRUSHED_HEATMAP && !is.null(COLOR_RANGE) && length(COLOR_RANGE) == 2) {
  # BRUSHED HEATMAP: Use parent heatmap's data range
  cat("Brushed heatmap: Using parent heatmap color range:", COLOR_RANGE[1], "to", COLOR_RANGE[2], "\n")
  min_val <- COLOR_RANGE[1]
  max_val <- COLOR_RANGE[2]
} else {
  # PARENT HEATMAP: Use its own data range
  min_val <- min(plot_matrix, na.rm = TRUE)
  max_val <- max(plot_matrix, na.rm = TRUE)
  cat("Parent heatmap: Using auto-detected data range:", round(min_val, 2), "to", round(max_val, 2), "\n")
}

# Create smooth color gradient using RdYlBu (ComplexHeatmap's default palette)
# Reversed for Blue (low) -> Yellow (mid) -> Red (high)
color_palette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(255)

# Create breaks for the color function
breaks <- seq(min_val, max_val, length.out = 255)
col_fun <- colorRamp2(breaks, color_palette)

cat("Color mapping: min=", round(min_val, 2), ", max=", round(max_val, 2), ", colors=255 (RdYlBu palette)\n")

# Create heatmap using ComplexHeatmap (matches Shiny app)
heatmap_plot <- Heatmap(
  plot_matrix,
  name = "Expression",  # legend title
  
  # Color scale
  col = col_fun,
  
  # Clustering
  cluster_rows = !IS_BRUSHED_HEATMAP,  # Don't re-cluster genes for brushed heatmaps
  cluster_columns = !IS_BRUSHED_HEATMAP,  # Don't re-cluster samples for brushed heatmaps
  clustering_distance_rows = "euclidean",
  clustering_distance_columns = "euclidean",
  clustering_method_rows = "complete",
  clustering_method_columns = "complete",
  
  # Display options
  show_row_names = SHOW_ROWNAMES,
  show_column_names = SHOW_COLNAMES,
  row_names_gp = gpar(fontsize = FONTSIZE_ROW),
  column_names_gp = gpar(fontsize = 8),
  
  # Annotations
  top_annotation = ha,
  
  # Title
  column_title = if (IS_BRUSHED_HEATMAP) {
    "Brushed Sub-Heatmap (Original Sample Order Preserved)"
  } else {
    "Expression Heatmap"
  },
  column_title_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # Heatmap border
  border = TRUE
)

# Display heatmap
draw(heatmap_plot)

################################################################################
# Save High-Resolution Heatmap
################################################################################

# Calculate appropriate height based on number of genes
heatmap_height <- max(8, nrow(plot_matrix) * 0.15)
heatmap_height_px <- max(2400, nrow(plot_matrix) * 45)

# Use different filenames for brushed vs non-brushed heatmaps
if (IS_BRUSHED_HEATMAP) {
  pdf_filename <- "heatmap_brushed.pdf"
  png_filename <- "heatmap_brushed.png"
} else {
  pdf_filename <- "heatmap.pdf"
  png_filename <- "heatmap.png"
}

# Save as PDF
pdf(pdf_filename, width = 10, height = heatmap_height)
draw(heatmap_plot)
dev.off()

# Save as PNG
png(png_filename, width = 3000, height = heatmap_height_px, res = 300)
draw(heatmap_plot)
dev.off()

cat("Heatmap saved successfully!\n")
cat("  -", pdf_filename, "(", nrow(plot_matrix), "genes)\n")
cat("  -", png_filename, "(high resolution)\n")
