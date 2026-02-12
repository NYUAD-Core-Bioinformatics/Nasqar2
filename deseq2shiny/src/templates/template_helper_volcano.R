################################################################################
# VOLCANO PLOT HELPER FUNCTIONS
################################################################################

#' Prepare Gene Labels
#'
#' Prepare gene labels, preferring gene names over gene IDs
#'
#' @param results_data Data frame with DESeq2 results (should include gene.name column if available)
#' @param use_gene_names Logical, whether to use gene names
#' @return Character vector of gene labels
prepare_gene_labels <- function(results_data, use_gene_names = TRUE) {
  # Prepare gene labels - PREFER gene names over gene IDs
  # Note: gene.name column should already be in results_data from export
  if ("gene.name" %in% colnames(results_data) && use_gene_names) {
    gene_labels <- results_data$gene.name
    # Replace NA with gene IDs
    gene_labels[is.na(gene_labels)] <- rownames(results_data)[is.na(gene_labels)]
  } else {
    gene_labels <- rownames(results_data)
  }
  
  return(gene_labels)
}

#' Select Genes to Label
#'
#' Select genes for labeling based on user specification or significance
#'
#' @param results_data Data frame with DESeq2 results
#' @param gene_labels Character vector of gene labels
#' @param genes_of_interest Character vector of specific genes to label (optional)
#' @param padj_threshold Numeric, adjusted p-value threshold
#' @param log2fc_threshold Numeric, log2 fold change threshold
#' @param max_labels Integer, maximum number of labels if auto-selecting
#' @return Character vector of row names to label
select_genes_to_label <- function(results_data, gene_labels, genes_of_interest = NULL,
                                   padj_threshold, log2fc_threshold, max_labels = 20) {
  if (!is.null(genes_of_interest) && length(genes_of_interest) > 0) {
    # User specified genes of interest
    genes_found <- c()
    for (gene in genes_of_interest) {
      if (gene %in% gene_labels) {
        gene_idx <- which(gene_labels == gene)
        genes_found <- c(genes_found, rownames(results_data)[gene_idx])
      } else if (gene %in% rownames(results_data)) {
        genes_found <- c(genes_found, gene)
      }
    }
    return(genes_found)
  } else {
    # Auto-select top most significant genes
    # NOTE: For labeling, we select based on padj only (like Shiny app does)
    # We don't require FC threshold for labels - that's already shown by colors
    sig_genes <- results_data[!is.na(results_data$padj) & results_data$padj < padj_threshold, ]
    
    if (!is.null(sig_genes) && is.data.frame(sig_genes) && nrow(sig_genes) > 0) {
      sig_genes_sorted <- sig_genes[order(sig_genes$padj), ]
      return(rownames(sig_genes_sorted)[1:min(max_labels, nrow(sig_genes_sorted))])
    } else {
      # If no significant genes, return empty to let EnhancedVolcano decide
      return(c())
    }
  }
}

#' Create Volcano Plot
#'
#' Generate a publication-ready volcano plot using EnhancedVolcano
#'
#' @param results_data Data frame with DESeq2 results
#' @param comparison_name String, name of the comparison for plot title
#' @param padj_threshold Numeric, adjusted p-value threshold
#' @param log2fc_threshold Numeric, log2 fold change threshold
#' @param point_size Numeric, size of points (default: 2.0)
#' @param point_alpha Numeric, transparency of points (default: 0.5)
#' @param use_gene_names Logical, whether to use gene names for labels
#' @param genes_of_interest Character vector of specific genes to label (optional)
#' @param max_labels Integer, maximum number of labels if auto-selecting
#' @return EnhancedVolcano plot object
create_volcano_plot <- function(results_data, comparison_name, padj_threshold, 
                                log2fc_threshold, point_size = 2.0, point_alpha = 0.5,
                                use_gene_names = TRUE, genes_of_interest = NULL, 
                                max_labels = 20) {
  
  # Prepare gene labels
  gene_labels <- prepare_gene_labels(results_data, use_gene_names)
  
  # Count significant genes
  sig_up <- sum(results_data$padj < padj_threshold & 
                results_data$log2FoldChange > log2fc_threshold, na.rm = TRUE)
  sig_down <- sum(results_data$padj < padj_threshold & 
                  results_data$log2FoldChange < -log2fc_threshold, na.rm = TRUE)
  cat("  - Volcano Plot:", comparison_name, "- Up:", sig_up, "Down:", sig_down, "\n")
  
  # Select genes to label (returns rownames of genes)
  top_gene_rownames <- select_genes_to_label(results_data, gene_labels, genes_of_interest,
                                             padj_threshold, log2fc_threshold, max_labels)
  
  # Convert rownames to labels for selectLab
  selectLab_genes <- NULL
  if (length(top_gene_rownames) > 0) {
    # Find positions of selected genes in results_data
    gene_positions <- match(top_gene_rownames, rownames(results_data))
    # Get corresponding labels
    selectLab_genes <- gene_labels[gene_positions]
  }
  
  # Load EnhancedVolcano if not already loaded
  if (!requireNamespace("EnhancedVolcano", quietly = TRUE)) {
    stop("Package 'EnhancedVolcano' is required. Install it with: BiocManager::install('EnhancedVolcano')")
  }
  
  # Create Enhanced Volcano Plot
  volcano_plot <- EnhancedVolcano::EnhancedVolcano(results_data,
      lab = gene_labels,
      x = 'log2FoldChange',
      y = 'padj',
      
      # Select only top genes for labeling (NULL = let EnhancedVolcano auto-select)
      selectLab = selectLab_genes,
      
      # Title and axis labels
      title = paste("Volcano Plot:", comparison_name),
      subtitle = if (!is.null(genes_of_interest) && length(genes_of_interest) > 0) {
        paste0("Up: ", sig_up, " | Down: ", sig_down, " genes (labeling ", 
               length(top_gene_rownames), " genes of interest)")
      } else if (length(top_gene_rownames) > 0) {
        paste0("Up: ", sig_up, " | Down: ", sig_down, " genes (labeling top ", 
               length(top_gene_rownames), " most significant)")
      } else {
        paste0("Up: ", sig_up, " | Down: ", sig_down, " genes")
      },
      caption = paste0("Thresholds: padj < ", padj_threshold, 
                      ", |log2FC| > ", log2fc_threshold),
      xlab = bquote(~Log[2]~ "fold change"),
      ylab = bquote(~-Log[10]~adjusted~italic(P)),
      
      # Thresholds
      pCutoff = padj_threshold,
      FCcutoff = log2fc_threshold,
      
      # Point aesthetics
      pointSize = point_size,
      colAlpha = point_alpha,
      
      # Custom colors for up/down regulation
      colCustom = {
        keyvals <- ifelse(
          results_data$padj < padj_threshold & abs(results_data$log2FoldChange) > log2fc_threshold,
          ifelse(results_data$log2FoldChange > 0, 'red2', 'royalblue'),
          'grey50'
        )
        keyvals[is.na(keyvals)] <- 'grey50'
        names(keyvals)[keyvals == 'red2'] <- 'Upregulated'
        names(keyvals)[keyvals == 'royalblue'] <- 'Downregulated'
        names(keyvals)[keyvals == 'grey50'] <- 'Not significant'
        keyvals
      },
      
      # Label aesthetics
      labSize = 4.5,
      labCol = 'black',
      labFace = 'bold',
      boxedLabels = TRUE,
      parseLabels = FALSE,
      drawConnectors = TRUE,
      widthConnectors = 0.75,
      colConnectors = 'black',
      max.overlaps = 20,
      min.segment.length = 0,
      
      # Legend
      legendPosition = 'right',
      legendLabSize = 12,
      legendIconSize = 4.0,
      
      # Grid and borders
      gridlines.major = TRUE,
      gridlines.minor = FALSE,
      border = 'partial',
      borderWidth = 1.0,
      borderColour = 'black'
  )
  
  return(volcano_plot)
}

cat("✓ Volcano plot helper functions loaded\n")
cat("  - prepare_gene_labels()\n")
cat("  - select_genes_to_label()\n")
cat("  - create_volcano_plot()\n")
