################################################################################
# MA PLOT HELPER FUNCTION
################################################################################

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

cat("✓ MA plot helper function loaded\n")
