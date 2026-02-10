################################################################################
#                                                                              #
#       COMPREHENSIVE DESEQ2 ANALYSIS - FROM RAW DATA TO PLOTS                #
#       Generated from DESeq2Shiny                              #
#       {{timestamp}}                                           #
#                                                                              #
################################################################################

# This script performs complete DESeq2 differential expression analysis from raw count data,
# generating {{num_plots}} publication-quality plots with full reproducibility.


# Set working directory to the location of this script and data files
# setwd("/path/to/extracted/folder")

# Install required packages if needed:
# if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# BiocManager::install("DESeq2")
# BiocManager::install("EnhancedVolcano")
# install.packages(c('ggplot2', 'pheatmap', 'RColorBrewer', 'ggrepel', 'tidyr', 'dplyr'))


{{HELPER_FUNCTIONS_SECTION}}

{{DESEQ2_PIPELINE_SECTION}}

{{PLOT_SECTIONS}}

################################################################################
#  SESSION INFORMATION
################################################################################

cat("\n=== Analysis Complete ===")
cat("\nAll plots have been generated and saved.")
cat("\nCheck the current directory for PDF and PNG files.\n\n")
sessionInfo()
