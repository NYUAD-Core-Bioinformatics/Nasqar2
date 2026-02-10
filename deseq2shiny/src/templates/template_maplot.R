################################################################################
# Publication-ready MA Plot (OPTIMIZED)
# Generated from DESeq2Shiny
################################################################################

################################################################################
# HELPER FUNCTIONS
################################################################################

# Load helper functions for MA plot generation
{{HELPER_FUNCTIONS}}

################################################################################
# CONFIGURATION PARAMETERS
# Edit these parameters to customize your analysis
################################################################################

# Analysis name
COMPARISON_NAME <- "{{comparison_name}}"

# Significance threshold
ALPHA <- {{alpha}}

# Y-axis limits
YLIM <- {{ylim}}

# Data file (for full mode)
DATA_FILE <- "{{data_file}}"

################################################################################
# Load Required Libraries
################################################################################

library(ggplot2)

################################################################################
# Load Data
################################################################################

{{DATA_LOAD_SECTION}}

################################################################################
# Create MA Plot Using Helper Function
################################################################################

cat("Generating MA plot for:", COMPARISON_NAME, "\n")

# Use create_ma_plot() helper function
# (Function should be defined in helper functions section)
ma_plot <- create_ma_plot(
  results_data = results_data,
  comparison_name = COMPARISON_NAME,
  alpha = ALPHA,
  ylim = YLIM
)

# Display plot
print(ma_plot)

################################################################################
# Save High-Resolution Plots
################################################################################

ggsave(paste0("ma_plot_", COMPARISON_NAME, ".pdf"), ma_plot, width = 8, height = 6, dpi = 300)
ggsave(paste0("ma_plot_", COMPARISON_NAME, ".png"), ma_plot, width = 8, height = 6, dpi = 300)
cat("✓ MA plots saved successfully!\n")
