# R Code Export Module
# This module provides functions to generate publication-ready R code for all plot types


# Read a template file from the templates directory
readTemplate <- function(template_name, extension = ".R") {
  template_path <- file.path("templates", paste0(template_name, extension))
  
  # Check if file exists
  if (!file.exists(template_path)) {
    stop(paste("Template file not found:", template_path))
  }
  
  # Read template file
  template_lines <- readLines(template_path, warn = FALSE)
  # Add newline at end to ensure proper separation when concatenating
  template_text <- paste(c(template_lines, ""), collapse = "\n")
  
  return(template_text)
}

# Generate complete script using template wrapper
generateScriptWrapper <- function(helper_functions, deseq2_pipeline, plot_sections, timestamp, num_plots) {
  # Read wrapper template
  template <- readTemplate("template_script_wrapper", ".R")
  
  # Create plot sections with headers
  plot_sections_text <- ""
  for (i in seq_along(plot_sections)) {
    section <- plot_sections[[i]]
    plot_sections_text <- paste0(
      plot_sections_text,
      "################################################################################\n",
      "#  ", i, ". ", toupper(section$title), "\n",
      "################################################################################\n\n",
      section$code,
      "\n\n\n"
    )
  }
  
  # Replace template placeholders
  script <- template
  script <- gsub("{{timestamp}}", timestamp, script, fixed = TRUE)
  script <- gsub("{{num_sections}}", length(plot_sections), script, fixed = TRUE)
  script <- gsub("{{num_plots}}", num_plots, script, fixed = TRUE)
  script <- gsub("{{HELPER_FUNCTIONS_SECTION}}", helper_functions, script, fixed = TRUE)
  script <- gsub("{{DESEQ2_PIPELINE_SECTION}}", deseq2_pipeline, script, fixed = TRUE)
  script <- gsub("{{PLOT_SECTIONS}}", plot_sections_text, script, fixed = TRUE)
  
  return(script)
}

# Generate README using template
generateREADME <- function(timestamp, script_filename, data_files_list, plot_list, num_plots) {
  # Read README template
  template <- readTemplate("template_readme", ".txt")
  
  # Replace template placeholders
  readme <- template
  readme <- gsub("{{timestamp}}", timestamp, readme, fixed = TRUE)
  readme <- gsub("{{script_filename}}", script_filename, readme, fixed = TRUE)
  readme <- gsub("{{DATA_FILES_LIST}}", data_files_list, readme, fixed = TRUE)
  readme <- gsub("{{PLOT_LIST}}", plot_list, readme, fixed = TRUE)
  readme <- gsub("{{num_plots}}", num_plots, readme, fixed = TRUE)
  
  return(readme)
}

# Format R value for template substitution
# Note: For string values, we return the raw string without quotes
# because the template files already have quotes around string placeholders
formatRValue <- function(value) {
  if (is.null(value)) {
    return("NULL")
  } else if (is.character(value)) {
    # Handle character vectors
    if (length(value) == 0) {
      # Empty character vector
      return("NULL")
    } else if (length(value) > 1) {
      # Return as c("val1", "val2", ...) - templates don't have quotes for these
      return(paste0('c("', paste(value, collapse = '", "'), '")'))
    } else {
      # Single string - return without quotes (template already has them)
      # e.g., template has: NAME <- "{{name}}"
      # we replace {{name}} with: myname
      # result: NAME <- "myname"
      return(value)
    }
  } else if (is.logical(value)) {
    if (length(value) == 0) {
      return("NULL")
    }
    return(ifelse(value, "TRUE", "FALSE"))
  } else if (is.numeric(value)) {
    if (length(value) == 0) {
      return("NULL")
    }
    if (length(value) > 1) {
      # Multiple numeric values - format as c(val1, val2, ...)
      return(paste0("c(", paste(value, collapse = ", "), ")"))
    }
    return(as.character(value))
  } else if (is.list(value)) {
    # Handle lists - can be named or unnamed, with various element types
    if (length(value) == 0) {
      return("NULL")
    }
    if (!is.null(names(value)) && length(names(value)) > 0) {
      # Named list
      items <- sapply(names(value), function(n) {
        item_value <- value[[n]]
        if (is.character(item_value) && length(item_value) > 1) {
          # Character vector (e.g., contrast)
          paste0(n, ' = c("', paste(item_value, collapse = '", "'), '")')
        } else if (is.character(item_value)) {
          # Single string
          paste0(n, ' = "', item_value, '"')
        } else {
          # Other types
          paste0(n, ' = ', formatRValue(item_value))
        }
      })
      return(paste0("list(", paste(items, collapse = ", "), ")"))
    } else {
      # Unnamed list
      items <- sapply(value, function(item) {
        if (is.character(item)) {
          paste0('c("', paste(item, collapse = '", "'), '")')
        } else {
          formatRValue(item)
        }
      })
      return(paste0("list(", paste(items, collapse = ", "), ")"))
    }
  } else if (inherits(value, "formula")) {
    # Format formula objects - use deparse to get the formula as a string
    return(paste(deparse(value), collapse = " "))
  } else {
    # Default: convert to string
    return(as.character(value))
  }
}

# Replace template variables with actual values
replaceTemplateVars <- function(template, params) {
  # Replace each parameter
  for (param_name in names(params)) {
    placeholder <- paste0("{{", param_name, "}}")
    value <- formatRValue(params[[param_name]])
    template <- gsub(placeholder, value, template, fixed = TRUE)
  }
  
  return(template)
}

# Build data loading section based on mode
buildDataLoadSection <- function(mode, use_existing_objects, template_type, params) {
  # Validate inputs
  if (is.null(mode) || length(mode) == 0) mode <- "full"
  if (is.null(use_existing_objects) || length(use_existing_objects) == 0) use_existing_objects <- FALSE
  if (is.null(template_type) || length(template_type) == 0) template_type <- "unknown"
  if (is.null(params)) params <- list()
  
  if (use_existing_objects) {
    # Use objects from Section 0 (DESeq2 pipeline)
    if (template_type == "template_volcano" || template_type == "template_maplot") {
      # Load from the specific contrast's results file exported in Section 0
      data_file <- if (!is.null(params$data_file) && length(params$data_file) > 0) params$data_file else "results.csv"
      code <- paste0(
        "# Load results from Section 0 (DESeq2 pipeline)\n",
        "# Section 0 exported results for each contrast\n",
        "results_data <- read.csv(\"", data_file, "\", row.names = 1)\n",
        "cat(\"Loaded results from\", \"", data_file, "\"  , \":\", nrow(results_data), \"genes\\n\\n\")\n"
      )
    } else if (template_type == "template_boxplot") {
      code <- paste0(
        "# Use data from Section 0 (DESeq2 pipeline)\n",
        "counts_data <- normalized_counts  # Generated in Section 0\n",
        "sample_metadata <- as.data.frame(colData(dds))  # From Section 0\n",
        "cat(\"Using normalized counts from DESeq2 pipeline\\n\\n\")\n"
      )
    } else if (template_type == "template_heatmap") {
      code <- paste0(
        "# Use data from Section 0 (DESeq2 pipeline)\n",
        "# normalized_counts already generated in Section 0\n",
        "sample_metadata <- as.data.frame(colData(dds))  # From Section 0\n",
        "cat(\"Using normalized counts from DESeq2 pipeline\\n\\n\")\n"
      )
    } else if (template_type == "template_pca" || template_type == "template_distance_heatmap") {
      transform_type <- if (!is.null(params$transform_type) && length(params$transform_type) > 0) params$transform_type else "vst"
      code <- paste0(
        "# Use data from Section 0 (DESeq2 pipeline)\n",
        "transformed_data <- ", transform_type, "_mat  # Generated in Section 0\n",
        "sample_metadata <- as.data.frame(colData(dds))  # From Section 0\n",
        "cat(\"Using ", transform_type, " data from DESeq2 pipeline\\n\\n\")\n"
      )
    } else if (template_type == "template_venn_set_heatmap") {
      # For complete analysis: Load pre-computed matrix from Shiny (avoids NA/NaN/Inf issues)
      matrix_file <- if (!is.null(params$expression_matrix_file) && length(params$expression_matrix_file) > 0) {
        params$expression_matrix_file
      } else {
        "set_expression_matrix.csv"
      }
      code <- paste0(
        "# Load pre-computed Venn set matrix from Shiny (already filtered and clean)\n",
        "set_expression_matrix <- read.csv(\"", matrix_file, "\", row.names = 1, check.names = FALSE)\n",
        "cat(\"Loaded Venn set matrix from Shiny:\", nrow(set_expression_matrix), \"genes ×\", ncol(set_expression_matrix), \"comparisons\\n\\n\")\n"
      )
    } else if (template_type == "template_venn") {
      # Template now includes all data loading logic inline
      code <- paste0(
        "# Use objects from Section 0 (DESeq2 pipeline)\n",
        "# results_list already generated\n",
        "cat(\"Using results from DESeq2 pipeline for Venn diagram\\n\\n\")\n"
      )
    } else {
      code <- "# Data objects assumed to exist\n"
    }
  } else if (mode == "full") {
    # Load from files
    if (template_type == "template_volcano" || template_type == "template_maplot") {
      code <- paste0(
        "# Load DESeq2 results data\n",
        "results_data <- read.csv(DATA_FILE, row.names = 1)\n",
        "cat(\"Loaded results for\", nrow(results_data), \"genes\\n\\n\")\n"
      )
    } else if (template_type == "template_boxplot") {
      code <- paste0(
        "# Load count data and metadata\n",
        "counts_data <- read.csv(COUNTS_FILE, row.names = 1, check.names = FALSE)\n",
        "sample_metadata <- read.csv(METADATA_FILE, row.names = 1, check.names = FALSE)\n",
        "cat(\"Loaded data:\\n\")\n",
        "cat(\"  - Counts:\", nrow(counts_data), \"genes ×\", ncol(counts_data), \"samples\\n\")\n",
        "cat(\"  - Metadata:\", nrow(sample_metadata), \"samples\\n\\n\")\n"
      )
    } else if (template_type == "template_heatmap") {
      code <- paste0(
        "# Load normalized counts and metadata\n",
        "normalized_counts <- read.csv(COUNTS_FILE, row.names = 1, check.names = FALSE)\n",
        "sample_metadata <- read.csv(METADATA_FILE, row.names = 1, check.names = FALSE)\n",
        "cat(\"Loaded:\", nrow(normalized_counts), \"genes ×\", ncol(normalized_counts), \"samples\\n\\n\")\n"
      )
    } else if (template_type == "template_pca" || template_type == "template_distance_heatmap") {
      code <- paste0(
        "# Load transformed data and metadata\n",
        "transformed_data <- read.csv(TRANSFORMED_DATA_FILE, row.names = 1, check.names = FALSE)\n",
        "# Convert to matrix (required for rowVars and other matrix operations)\n",
        "transformed_data <- as.matrix(transformed_data)\n",
        "sample_metadata <- read.csv(METADATA_FILE, row.names = 1, check.names = FALSE)\n",
        "cat(\"Loaded transformed data:\", nrow(transformed_data), \"genes ×\", ncol(transformed_data), \"samples\\n\\n\")\n"
      )
    } else if (template_type == "template_venn") {
      code <- paste0(
        "# Load gene lists for each comparison\n",
        "gene_sets <- list()\n",
        "for (i in 1:length(GENE_FILES)) {\n",
        "  genes <- read.csv(GENE_FILES[i])$gene\n",
        "  # Use letter labels (A, B, C) for cleaner Venn diagram\n",
        "  comparison_clean <- sub(\"\\\\.csv$\", \"\", COMPARISONS[i])\n",
        "  gene_sets[[LETTERS[i]]] <- genes\n",
        "  cat(\"Loaded\", length(genes), \"genes for Set\", LETTERS[i], \"(\", comparison_clean, \")\\n\")\n",
        "}\n",
        "cat(\"\\n\")\n"
      )
    } else if (template_type == "template_venn_set_heatmap") {
      code <- paste0(
        "# Load expression matrix for set operation genes\n",
        "set_expression_matrix <- read.csv(EXPRESSION_MATRIX_FILE, row.names = 1, check.names = FALSE)\n",
        "cat(\"Loaded expression matrix:\", nrow(set_expression_matrix), \"genes ×\", ncol(set_expression_matrix), \"comparisons\\n\\n\")\n"
      )
    } else {
      code <- "# Load data files\n"
    }
  } else {
    # Code-only mode: assume objects exist
    if (template_type == "template_volcano" || template_type == "template_maplot") {
      code <- paste0(
        "# Assumes 'results_data' data frame exists with columns:\n",
        "# - log2FoldChange: log2 fold change values\n",
        "# - padj: adjusted p-values\n",
        "# - baseMean: mean normalized counts (for MA plot)\n",
        "# Example: results_data <- as.data.frame(results(dds, contrast = c('condition', 'treated', 'control')))\n\n"
      )
    } else if (template_type == "template_boxplot") {
      code <- paste0(
        "# Assumes the following objects exist:\n",
        "# - counts_data: normalized count matrix\n",
        "# - sample_metadata: data frame with sample information\n",
        "# Example: counts_data <- counts(dds, normalized = TRUE)\n",
        "#          sample_metadata <- as.data.frame(colData(dds))\n\n"
      )
    } else if (template_type == "template_heatmap") {
      code <- paste0(
        "# Assumes 'normalized_counts' matrix exists\n",
        "# Example: normalized_counts <- counts(dds, normalized = TRUE)\n\n"
      )
    } else if (template_type == "template_pca" || template_type == "template_distance_heatmap") {
      code <- paste0(
        "# Assumes transformed data exists\n",
        "# Example: vsd <- vst(dds, blind = FALSE)\n",
        "#          transformed_data <- assay(vsd)\n",
        "#          sample_metadata <- as.data.frame(colData(dds))\n\n"
      )
    } else if (template_type == "template_venn") {
      code <- paste0(
        "# Assumes gene_sets list exists with gene vectors for each comparison\n",
        "# Example: gene_sets <- list(\n",
        "#            \"Comparison1\" = c(\"gene1\", \"gene2\", ...),\n",
        "#            \"Comparison2\" = c(\"gene3\", \"gene4\", ...)\n",
        "#          )\n\n"
      )
    } else if (template_type == "template_venn_set_heatmap") {
      code <- paste0(
        "# Assumes set_expression_matrix exists\n",
        "# Matrix with genes as rows and comparisons as columns\n\n"
      )
    } else {
      code <- "# Assumes necessary data objects exist\n\n"
    }
  }
  
  return(code)
}

# Main function to generate code from template
generateCodeFromTemplate <- function(template_name, params, mode = "full", use_existing_objects = FALSE, include_helpers = TRUE) {
  # Validate inputs
  if (is.null(template_name) || length(template_name) == 0 || !is.character(template_name)) {
    stop("Invalid template_name: must be a non-empty character string")
  }
  if (is.null(params) || !is.list(params)) {
    stop("Invalid params: must be a non-null list")
  }
  if (is.null(mode) || length(mode) == 0 || !is.character(mode)) {
    mode <- "full"
  }
  if (is.null(use_existing_objects) || length(use_existing_objects) == 0) {
    use_existing_objects <- FALSE
  }
  if (is.null(include_helpers) || length(include_helpers) == 0) {
    include_helpers <- TRUE
  }
  
  # Read template
  template <- readTemplate(template_name)
  
  # Replace template variables
  code <- replaceTemplateVars(template, params)
  
  # Build and replace data loading section
  data_load_section <- buildDataLoadSection(mode, use_existing_objects, template_name, params)
  code <- gsub("{{DATA_LOAD_SECTION}}", data_load_section, code, fixed = TRUE)
  
  # Add helper functions if template uses them AND include_helpers is TRUE
  # Support both legacy {{HELPER_FUNCTIONS}} and specific helpers
  if (include_helpers) {
    if (grepl("{{HELPER_FUNCTIONS}}", code, fixed = TRUE)) {
      helper_functions <- readTemplate("template_helper_functions")
      code <- gsub("{{HELPER_FUNCTIONS}}", helper_functions, code, fixed = TRUE)
    }
    
    # MA plot specific helpers
    if (grepl("{{HELPER_FUNCTIONS_MA}}", code, fixed = TRUE)) {
      helper_functions_ma <- readTemplate("template_helper_ma")
      code <- gsub("{{HELPER_FUNCTIONS_MA}}", helper_functions_ma, code, fixed = TRUE)
    }
    
    # Volcano plot specific helpers
    if (grepl("{{HELPER_FUNCTIONS_VOLCANO}}", code, fixed = TRUE)) {
      helper_functions_volcano <- readTemplate("template_helper_volcano")
      code <- gsub("{{HELPER_FUNCTIONS_VOLCANO}}", helper_functions_volcano, code, fixed = TRUE)
    }
  } else {
    # Remove helper function sections if include_helpers is FALSE
    # Remove the entire HELPER FUNCTIONS section for MA plots
    code <- gsub("################################################################################\n# HELPER FUNCTIONS\n################################################################################\n\n# Load helper functions for MA plot generation\n{{HELPER_FUNCTIONS_MA}}\n\n", "", code, fixed = TRUE)
    
    # Remove the entire HELPER FUNCTIONS section for volcano plots
    code <- gsub("################################################################################\n# HELPER FUNCTIONS\n################################################################################\n\n# Load helper functions for volcano plot generation\n{{HELPER_FUNCTIONS_VOLCANO}}\n\n", "", code, fixed = TRUE)
    
    # Fallback: remove just the placeholders if the full sections aren't found
    code <- gsub("{{HELPER_FUNCTIONS}}", "", code, fixed = TRUE)
    code <- gsub("{{HELPER_FUNCTIONS_MA}}", "", code, fixed = TRUE)
    code <- gsub("{{HELPER_FUNCTIONS_VOLCANO}}", "", code, fixed = TRUE)
    
    # Also remove the "Publication-ready" header that's only needed for standalone scripts
    code <- gsub("################################################################################\n# Publication-ready MA Plot \\(OPTIMIZED\\)\n# Generated from DESeq2Shiny\n################################################################################\n\n", "", code)
    code <- gsub("################################################################################\n# Publication-ready Volcano Plot \\(OPTIMIZED\\)\n# Generated from DESeq2Shiny\n################################################################################\n\n", "", code)
  }
  
  # Only .R format is supported
  
  return(code)
}

# ============================================================================
# PLOT-SPECIFIC CODE GENERATORS
# ============================================================================
# These are wrapper functions around generateCodeFromTemplate() for each plot type

generateVolcanoCode <- function(params, mode = "full", use_existing_objects = FALSE, include_helpers = TRUE) {
  return(generateCodeFromTemplate("template_volcano", params, mode, use_existing_objects, include_helpers))
}

generateMAPlotCode <- function(params, mode = "full", use_existing_objects = FALSE, include_helpers = TRUE) {
  return(generateCodeFromTemplate("template_maplot", params, mode, use_existing_objects, include_helpers))
}

generateBoxplotCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  return(generateCodeFromTemplate("template_boxplot", params, mode, use_existing_objects))
}

generateHeatmapCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  return(generateCodeFromTemplate("template_heatmap", params, mode, use_existing_objects))
}

generateHeatmapCodeSimple <- function(params) {
  # Use simple template for individual heatmap exports
  # This template just loads the pre-processed matrix and plots it
  template <- readTemplate("template_heatmap_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

generateBrushedHeatmapCodeSimple <- function(params) {
  # Use simple template for brushed heatmap exports
  # This template loads the brushed matrix and plots with parent colors
  template <- readTemplate("template_brushed_heatmap_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

generateBoxplotCodeSimple <- function(params) {
  # Use simple template for boxplot exports
  # This template loads pre-subset normalized counts and creates boxplots
  template <- readTemplate("template_boxplot_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

generatePCACode <- function(params, mode = "full", use_existing_objects = FALSE) {
  return(generateCodeFromTemplate("template_pca", params, mode, use_existing_objects))
}

generatePCACodeSimple <- function(params) {
  # Use simple template for PCA exports
  # This template loads pre-calculated PC coordinates and creates plots
  template <- readTemplate("template_pca_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

generateDistHeatmapCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  return(generateCodeFromTemplate("template_distance_heatmap", params, mode, use_existing_objects))
}

generateDistHeatmapCodeSimple <- function(params) {
  # Use simple template for distance heatmap exports
  # This template loads pre-calculated distance matrix and creates heatmaps
  template <- readTemplate("template_distance_heatmap_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

generateVennCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  return(generateCodeFromTemplate("template_venn", params, mode, use_existing_objects))
}

generateVennCodeSimple <- function(params) {
  # Use simple template for Venn diagram exports
  # This template loads pre-filtered gene lists and draws the Venn
  template <- readTemplate("template_venn_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

generateVennSetHeatmapCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  return(generateCodeFromTemplate("template_venn_set_heatmap", params, mode, use_existing_objects))
}

generateVennSetHeatmapCodeSimple <- function(params) {
  # Use simple template for Venn set heatmap exports
  # This template loads the pre-processed matrix and plots
  template <- readTemplate("template_venn_set_heatmap_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

generateVennBrushedHeatmapCodeSimple <- function(params) {
  # Use simple template for Venn brushed heatmap exports
  # This template loads the brushed matrix and plots without clustering
  template <- readTemplate("template_venn_brushed_heatmap_simple")
  code <- replaceTemplateVars(template, params)
  return(code)
}

# ============================================================================
# SECTION 0: Complete DESeq2 Pipeline Code Generator
# ============================================================================
# This function generates a complete, reproducible DESeq2 analysis pipeline
# from raw count data through all transformations and differential expression

generateDESeq2PipelineCode <- function(params) {
  # Prepare template parameters with defaults
  # Match Shiny prefiltering approach instead of DESeq2-style filtering
  template_params <- list(
    counts_filename = if(!is.null(params$counts_filename) && length(params$counts_filename) > 0) params$counts_filename else "counts.csv",
    metadata_filename = if(!is.null(params$metadata_filename) && length(params$metadata_filename) > 0) params$metadata_filename else "metadata.csv",
    design_formula = params$design_formula,
    # Use Shiny prefilter parameters (sum-based filtering)
    prefilter_applied = if(!is.null(params$prefilter_applied) && length(params$prefilter_applied) > 0) params$prefilter_applied else FALSE,
    prefilter_threshold = if(!is.null(params$prefilter_threshold) && length(params$prefilter_threshold) > 0) params$prefilter_threshold else 0,
    alpha = if(!is.null(params$alpha) && length(params$alpha) > 0) params$alpha else 0.1,
    contrasts = params$contrasts
  )
  
  # Generate code from template (mode = "full" always for pipeline)
  code <- generateCodeFromTemplate("template_deseq2_pipeline", template_params, 
                                   mode = "full", use_existing_objects = FALSE)
  
  return(code)
}

# ============================================================================
# END OF CODE EXPORT MODULE
# ============================================================================
