# State Management Feature

## Overview

The DESeq2 Shiny application now includes comprehensive state saving and loading functionality, allowing you to save your analysis progress and resume it later.

## Features

### Save Application State
- Save all your analysis data, settings, and results as an R object (.RData file)
- Includes all important components:
  - Raw count data and metadata
  - Design conditions and formula
  - DESeq2 objects (dds, rld, vsd)
  - Analysis results
  - Gene information and annotations
  - Transformation matrices
  - SVA/batch effect correction results

### 🆕 Enhanced Plot State Management
- **Volcano Plots**: Saves different datasets you've analyzed, threshold settings, and selected genes
- **Venn Diagrams**: Preserves multiple dataset comparisons, gene intersections, and selected expressions
- **Heatmaps**: Saves gene count settings (100 genes), subset selections, custom gene lists, and gene type preferences
- **Gene Boxplots**: Maintains selected genes, grouping variables (x-axis, fill), custom color schemes (#0000FF), and gene search preferences

### Load Previous State
- Load a previously saved state file to continue your analysis
- Automatically restores UI elements and tab visibility
- Navigates to the appropriate analysis step

## How to Use

### Saving State
1. Click on the "Save/Load State" dropdown in the top navigation bar
2. Click "Save State (.RData)" button (only available when you have loaded data)
3. Choose where to save the file - it will be named with timestamp: `deseq2shiny_state_YYYYMMDD_HHMMSS.RData`

### Loading State
1. Click on the "Save/Load State" dropdown in the top navigation bar
2. Use the file input to select a previously saved .RData file
3. The application will automatically load all your previous data and navigate to the appropriate tab
4. You can continue your analysis from where you left off

## State Components Saved

The following components are automatically saved:

**Core Data:**
- `dataCounts`: Raw count matrix
- `fileContent`: Original uploaded file content
- `DF`: Sample metadata and conditions
- `conditions`: Condition definitions

**Gene Information:**
- `geneids`: Gene identifiers
- `genenames`: Gene names/symbols
- `selected_genes`: Currently selected genes

**DESeq2 Objects:**
- `dds`: Main DESeqDataSet object
- `ddsSva`: SVA-corrected DESeqDataSet
- `ddsAddSV`: DESeqDataSet with surrogate variables

**Transformation Results:**
- `rld`: rlog transformation object
- `rlogMat`: rlog transformation matrix
- `vsd`: VST transformation object
- `vstMat`: VST transformation matrix
- `vsdSva`: SVA-corrected VST object

**Analysis Results:**
- `vsResults`: Differential expression results
- `status`: Analysis status information

**Plot-Specific Data:**
- `filelist_file_list`: Saved volcano plot and Venn diagram datasets from different analyses
- `custom_colors_colors`: Custom color schemes for boxplots
- `custom_colors_globalcolors`: Global color palettes for different conditions
- `selected_matrix_matrix`: Selected gene expression matrices from Venn diagram intersections
- `saved_inputs`: All plot parameters including:
  - Volcano plot thresholds and settings
  - Venn diagram comparison parameters
  - **Boxplot settings**: Selected genes, x-axis grouping, fill grouping, gene search type, custom colors
  - **Heatmap settings**: Number of genes (100), subset selection checkbox, custom gene lists, gene type selection

**Metadata:**
- `save_timestamp`: When the state was saved
- `app_version`: Application version

## Use Cases

1. **Long Analysis Sessions**: Save your progress during lengthy analyses and resume later
2. **Sharing Results**: Share your complete analysis state with collaborators
3. **Backup**: Create backups of important analyses
4. **Comparison**: Save different analysis versions for comparison
5. **Teaching**: Save example analyses for demonstration purposes
6. **🆕 Multi-Dataset Analysis**: Save volcano plots and Venn diagrams comparing multiple datasets
7. **🆕 Custom Visualizations**: Preserve your custom gene selections, color schemes, and plot parameters
8. **🆕 Publication Ready**: Save final plot configurations with selected genes for publications

## Technical Notes

- State files are saved in R's native .RData format
- Files can be quite large depending on your dataset size
- The state includes all computed transformations and results, so loading is much faster than recomputing
- State files are portable between different R sessions and computers (as long as required packages are installed)

## File Naming Convention

Saved files follow this pattern:
```
deseq2shiny_state_YYYYMMDD_HHMMSS.RData
```

Example: `deseq2shiny_state_20240924_143052.RData`

This ensures unique filenames and makes it easy to identify when analyses were saved.

## Using State Files in R

You can load and analyze your saved state files directly in R outside of the Shiny application! This gives you full programmatic access to all your data and results.

### Quick Start in R

```r
# Load the viewing script
source("view_saved_state.R")

# Load and explore your state file
state <- load_and_explore_state("deseq2shiny_state_20240924_143052.RData")

# Extract specific data
counts <- extract_data(state, "counts")
results <- extract_data(state, "results")
volcano_datasets <- extract_data(state, "volcano_data")

# Create quick plots
create_quick_plots(state)
```

### Advanced Analysis in R

```r
# Load the analysis script
source("quick_analysis_from_state.R")

# Load state for interactive analysis
analysis <- analyze_saved_state("your_state_file.RData")

# Get complete overview
get_analysis_summary(analysis)

# Recreate all your plots
volcano_data <- recreate_volcano_plot(analysis)
heatmap_genes <- recreate_heatmap(analysis, top_n = 30)
recreate_boxplot(analysis, "GENE_NAME")
venn_genes <- explore_venn_intersections(analysis)

# Work with DESeq2 objects directly
dds <- analysis$dds
plotCounts(dds, gene = "GENE1", intgroup = "condition")
```

### What You Can Access in R

- **All Raw Data**: Count matrices, metadata, gene annotations
- **DESeq2 Objects**: Full DESeqDataSet objects for further analysis
- **Plot Data**: All volcano plot datasets, Venn diagram selections
- **Custom Settings**: Color schemes, thresholds, gene selections
- **Results**: Complete differential expression results
- **Transformations**: VST and rlog normalized data

### Benefits of R Access

1. **Batch Processing**: Analyze multiple state files programmatically
2. **Custom Analysis**: Create new plots and analyses beyond the Shiny app
3. **Integration**: Combine with other R packages and workflows
4. **Publication**: Generate high-quality figures with full control
5. **Sharing**: Extract specific datasets to share with collaborators
6. **Automation**: Build automated reporting pipelines
