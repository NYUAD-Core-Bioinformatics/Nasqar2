################################################################################
# HELPER FUNCTIONS
# Reusable functions to eliminate code duplication across plots
# Generated from DESeq2Shiny
################################################################################

cat("\n")
cat("=", rep("=", 78), "\n", sep = "")
cat("HELPER FUNCTIONS: Reusable code for efficient plot generation\n")
cat("=", rep("=", 78), "\n", sep = "")

#' Create MA Plot
#'
#' Generate a publication-ready MA plot for differential expression results
#'
#' @param results_data Data frame with DESeq2 results (must have baseMean, log2FoldChange, padj)
#' @param comparison_name String, name of the comparison for plot title
#' @param alpha Numeric, adjusted p-value threshold (default: 0.1)
#' @param ylim Numeric, y-axis limits (default: 2)
#' @return ggplot object
create_ma_plot <- function(results_data, comparison_name, alpha = 0.1, ylim = 2) {
  # Remove rows with NA values
  plot_data <- results_data[!is.na(results_data$padj) & !is.na(results_data$baseMean), ]
  plot_data <- plot_data[plot_data$baseMean > 0, ]  # Remove zero counts
  
  # Add significance classification
  plot_data$significant <- ifelse(plot_data$padj < alpha, "Significant", "Not Significant")
  plot_data$significant[is.na(plot_data$significant)] <- "Not Significant"
  
  # Count significant genes
  sig_count <- sum(plot_data$significant == "Significant")
  cat("  - MA Plot:", comparison_name, "- Significant genes:", sig_count, "\n")
  
  # Create MA Plot
  ma_plot <- ggplot(plot_data, aes(x = log10(baseMean), y = log2FoldChange)) +
    geom_point(aes(color = significant), alpha = 0.5, size = 1) +
    scale_color_manual(values = c("Not Significant" = "gray", "Significant" = "red"),
                       name = paste0("padj < ", alpha)) +
    geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
    coord_cartesian(ylim = c(-ylim, ylim)) +
    labs(title = paste("MA Plot:", comparison_name),
         subtitle = paste0("Significant genes: ", sig_count),
         x = "log10(Mean Expression)",
         y = "log2 Fold Change") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())
  
  return(ma_plot)
}

#' Prepare Gene Labels
#'
#' Prepare gene labels, preferring gene names over gene IDs
#'
#' @param results_data Data frame with DESeq2 results
#' @param use_gene_names Logical, whether to use gene names
#' @param gene_annotations Data frame with gene annotations (optional)
#' @return Character vector of gene labels
prepare_gene_labels <- function(results_data, use_gene_names = TRUE, gene_annotations = NULL) {
  # Merge gene names from pipeline if available
  if (use_gene_names && !is.null(gene_annotations)) {
    results_data$gene.name <- gene_annotations[rownames(results_data), "gene.names"]
  }
  
  # Prepare gene labels - PREFER gene names over gene IDs
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
    sig_genes <- results_data[!is.na(results_data$padj) & 
                               results_data$padj < padj_threshold & 
                               abs(results_data$log2FoldChange) > log2fc_threshold, ]
    
    if (!is.null(sig_genes) && is.data.frame(sig_genes) && nrow(sig_genes) > 0) {
      sig_genes_sorted <- sig_genes[order(sig_genes$padj), ]
      return(rownames(sig_genes_sorted)[1:min(max_labels, nrow(sig_genes_sorted))])
    } else {
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
#' @param gene_annotations Data frame with gene annotations (optional)
#' @return EnhancedVolcano plot object
create_volcano_plot <- function(results_data, comparison_name, padj_threshold, 
                                log2fc_threshold, point_size = 2.0, point_alpha = 0.5,
                                use_gene_names = TRUE, genes_of_interest = NULL, 
                                max_labels = 20, gene_annotations = NULL) {
  
  # Prepare gene labels
  gene_labels <- prepare_gene_labels(results_data, use_gene_names, gene_annotations)
  
  # Count significant genes
  sig_up <- sum(results_data$padj < padj_threshold & 
                results_data$log2FoldChange > log2fc_threshold, na.rm = TRUE)
  sig_down <- sum(results_data$padj < padj_threshold & 
                  results_data$log2FoldChange < -log2fc_threshold, na.rm = TRUE)
  cat("  - Volcano Plot:", comparison_name, "- Up:", sig_up, "Down:", sig_down, "\n")
  
  # Select genes to label
  top_gene_indices <- select_genes_to_label(results_data, gene_labels, genes_of_interest,
                                            padj_threshold, log2fc_threshold, max_labels)
  
  # Load EnhancedVolcano if not already loaded
  if (!requireNamespace("EnhancedVolcano", quietly = TRUE)) {
    stop("Package 'EnhancedVolcano' is required. Install it with: BiocManager::install('EnhancedVolcano')")
  }
  
  # Create Enhanced Volcano Plot
  volcano_plot <- EnhancedVolcano::EnhancedVolcano(results_data,
      lab = gene_labels,
      x = 'log2FoldChange',
      y = 'padj',
      
      # Select only top genes for labeling (prevents overcrowding)
      selectLab = if (length(top_gene_indices) > 0) gene_labels[top_gene_indices] else NULL,
      
      # Title and axis labels
      title = paste("Volcano Plot:", comparison_name),
      subtitle = if (!is.null(genes_of_interest) && length(genes_of_interest) > 0) {
        paste0("Up: ", sig_up, " | Down: ", sig_down, " genes (labeling ", 
               length(top_gene_indices), " genes of interest)")
      } else {
        paste0("Up: ", sig_up, " | Down: ", sig_down, " genes (labeling top ", 
               length(top_gene_indices), " most significant)")
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

cat("✓ Helper functions loaded successfully!\n")
cat("  - create_ma_plot(): Generate MA plots\n")
cat("  - create_volcano_plot(): Generate volcano plots\n")
cat("  - prepare_gene_labels(): Handle gene name/ID mapping\n")
cat("  - select_genes_to_label(): Smart gene label selection\n\n")
