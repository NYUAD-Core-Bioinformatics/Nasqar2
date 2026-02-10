################################################################################
#                                                                              #
#       COMPREHENSIVE DESEQ2 ANALYSIS EXPORT                                  #
#       FULLY REPRODUCIBLE FROM RAW DATA                                      #
#                                                                              #
################################################################################

Generated from DESeq2Shiny on {{timestamp}}

This export contains everything you need to reproduce your DESeq2 analysis from scratch!

##############################################################################
# WHAT'S INCLUDED
##############################################################################

📜 MAIN R SCRIPT:
✅ {{script_filename}} → Complete analysis script

{{DATA_FILES_LIST}}

##############################################################################
# HOW TO USE THIS EXPORT
##############################################################################

1️⃣  EXTRACT FILES:
   Extract this ZIP file to a clean directory on your computer.

2️⃣  INSTALL PACKAGES (first time only):
   Open R or RStudio and run:
   
   if (!require("BiocManager", quietly = TRUE)) {
       install.packages("BiocManager")
   }
   BiocManager::install("DESeq2")
   BiocManager::install("EnhancedVolcano")
   install.packages(c('ggplot2', 'pheatmap', 'RColorBrewer', 
                      'ggrepel', 'tidyr', 'dplyr', 'ComplexHeatmap',
                      'circlize', 'heatmaply', 'plotly', 'htmlwidgets',
                      'VennDiagram'))

3️⃣  RUN THE SCRIPT:
   In R/RStudio, navigate to the extracted directory:
   
   setwd("/path/to/extracted/folder")
   source("{{script_filename}}")
   
   The script will:
   • Load your raw data (counts + metadata)
   • Create DESeq2 object and run differential expression analysis
   • Generate variance-stabilizing and rlog transformations
   • Create {{num_plots}} publication-quality plots (PDF + PNG)
   • Save all results to the current directory

##############################################################################
# PLOTS GENERATED ({{num_plots}} total)
##############################################################################

{{PLOT_LIST}}

##############################################################################
# KEY FEATURES OF THIS EXPORT
##############################################################################

✅ FULLY REPRODUCIBLE:
   Everything regenerated from raw counts. No intermediate files needed!
   You get the exact same results as the Shiny app.

✅ OPTIMIZED CODE:
   Uses reusable helper functions to reduce code duplication by 70-80%.
   Cleaner, more maintainable, easier to customize.

✅ PUBLICATION-READY:
   All plots saved in high-resolution PDF and PNG formats.
   Ready for journals, presentations, reports.

✅ COMPLETELY SELF-CONTAINED:
   No dependency on the Shiny app. Run anywhere R is installed.
   Perfect for sharing with collaborators or for long-term archival.

✅ CUSTOMIZABLE:
   All parameters clearly documented at the top of each section.
   Easy to modify thresholds, colors, gene selections, etc.

##############################################################################
# ADVANCED: RUNNING INDIVIDUAL SECTIONS
##############################################################################

The main script is divided into numbered sections. You can run specific
sections individually:

1. Open {{script_filename}} in RStudio
2. Run Section 0 first (DESeq2 Pipeline) to create base objects
3. Then run any individual plot section you want

Each section is clearly marked with:
   ################################################################################
   #  N. PLOT NAME
   ################################################################################

##############################################################################
# TROUBLESHOOTING
##############################################################################

❌ "Error: cannot open file 'raw_counts...csv'"
   → Make sure you've set the working directory to the extracted folder
   → Use setwd("/full/path/to/folder") or RStudio's Session > Set Working Directory

❌ "Error: package 'DESeq2' is not available"
   → Install Bioconductor packages using BiocManager (see step 2 above)

❌ "Error: object 'dds' not found"
   → You must run Section 0 (DESeq2 Pipeline) before running individual plots

❌ Plots look different from Shiny app
   → Ensure you're using the same R/package versions
   → Run sessionInfo() to check your environment

##############################################################################
# CITATION
##############################################################################

If you use DESeq2 in published research, please cite:

  Love, M.I., Huber, W., Anders, S. (2014)
  "Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2"
  Genome Biology, 15:550.
  https://doi.org/10.1186/s13059-014-0550-8

For EnhancedVolcano:

  Blighe, K., Rana, S., Lewis, M. (2018)
  "EnhancedVolcano: publication-ready volcano plots with enhanced colouring and labeling"
  https://github.com/kevinblighe/EnhancedVolcano

##############################################################################
# QUESTIONS OR ISSUES?
##############################################################################

For questions about the generated code or this export:
• Check the DESeq2 vignette: https://bioconductor.org/packages/DESeq2/
• DESeq2Shiny documentation: [Add your documentation URL here]

##############################################################################

Enjoy your analysis! 🧬📊
