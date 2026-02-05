# R Code Export Module
# This module provides functions to generate publication-ready R code for all plot types

# Helper function to create R Markdown YAML header
createRmdHeader <- function(title, author = "DESeq2Shiny User") {
  paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "author: \"", author, "\"\n",
    "date: \"", format(Sys.Date(), "%B %d, %Y"), "\"\n",
    "output:\n",
    "  html_document:\n",
    "    toc: true\n",
    "    toc_float: true\n",
    "    code_folding: show\n",
    "  pdf_document:\n",
    "    toc: true\n",
    "---\n\n"
  )
}

# Helper function to wrap code in R Markdown chunk
wrapInChunk <- function(code, chunk_name = "", message = FALSE, warning = FALSE) {
  paste0(
    "```{r ", chunk_name, 
    ", message=", ifelse(message, "TRUE", "FALSE"),
    ", warning=", ifelse(warning, "TRUE", "FALSE"), 
    "}\n",
    code,
    "\n```\n\n"
  )
}

# ============================================================================
# NEW TEMPLATE-BASED HELPER FUNCTIONS
# ============================================================================

# Read a template file from the templates directory
readTemplate <- function(template_name) {
  template_path <- file.path("templates", paste0(template_name, ".R"))
  
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
      # Need to create set_expression_matrix from results_list
      # Extract thresholds (should match Venn diagram thresholds)
      padj_threshold <- if(!is.null(params$padj_threshold)) params$padj_threshold else 0.1
      fc_threshold <- if(!is.null(params$fc_threshold)) params$fc_threshold else 0
      
      code <- paste0(
        "# Use gene sets from Venn diagram section above\n",
        "cat(\"Building expression matrix from set operation results...\\n\")\n\n",
        "# Evaluate set expression to get genes\n",
        "# Set operations: * = intersection, + = union, - = setdiff\n",
        "set_operation <- \"", if(!is.null(params$set_expression)) params$set_expression else "A*B", "\"\n",
        "letters_in_expr <- unique(unlist(strsplit(gsub(\"[^A-Z]\", \"\", set_operation), \"\")))\n",
        "cat(\"Set operation:\", set_operation, \"\\n\")\n",
        "cat(\"Letters:\", paste(letters_in_expr, collapse=\", \"), \"\\n\")\n",
        "cat(\"Thresholds: padj < ", padj_threshold, ", |log2FC| > ", fc_threshold, "\\n\\n\")\n\n",
        "# Get genes from each set (using same filtering as Venn diagram)\n",
        "all_gene_sets <- list()\n",
        "for (i in seq_along(COMPARISONS)) {\n",
        "  comparison_clean <- sub(\"\\\\.csv$\", \"\", COMPARISONS[i])\n",
        "  if (exists(\"results_list\") && comparison_clean %in% names(results_list)) {\n",
        "    res_data <- results_list[[comparison_clean]]\n",
        "    # Filter by BOTH padj AND log2FoldChange thresholds (matches Shiny app)\n",
        "    sig_genes <- rownames(res_data)[which(\n",
        "      !is.na(res_data$padj) & \n",
        "      res_data$padj < ", padj_threshold, " & \n",
        "      abs(res_data$log2FoldChange) > ", fc_threshold, "\n",
        "    )]\n",
        "    all_gene_sets[[LETTERS[i]]] <- sig_genes\n",
        "    cat(\"  Set\", LETTERS[i], \"(\", comparison_clean, \"):\", length(sig_genes), \"genes\\n\")\n",
        "  }\n",
        "}\n\n",
        "# Evaluate set operation\n",
        "if (grepl(\"\\\\*\", set_operation) && !grepl(\"[+-]\", set_operation)) {\n",
        "  # Pure intersection\n",
        "  set_genes <- Reduce(intersect, all_gene_sets)\n",
        "} else if (grepl(\"\\\\+\", set_operation) && !grepl(\"[*-]\", set_operation)) {\n",
        "  # Pure union\n",
        "  set_genes <- Reduce(union, all_gene_sets)\n",
        "} else if (grepl(\"-\", set_operation)) {\n",
        "  # Difference: A-B means genes in A but not in B\n",
        "  parts <- strsplit(set_operation, \"-\")[[1]]\n",
        "  set_genes <- all_gene_sets[[parts[1]]]\n",
        "  for (i in 2:length(parts)) {\n",
        "    set_genes <- setdiff(set_genes, all_gene_sets[[parts[i]]])\n",
        "  }\n",
        "} else {\n",
        "  # Default: use all genes from first set\n",
        "  set_genes <- all_gene_sets[[1]]\n",
        "}\n\n",
        "cat(\"\\nSet operation result:\", length(set_genes), \"genes\\n\\n\")\n\n",
        "# Create expression matrix with log2FoldChange for each comparison\n",
        "set_expression_matrix <- data.frame(row.names = set_genes)\n",
        "for (i in seq_along(COMPARISONS)) {\n",
        "  comparison_clean <- sub(\"\\\\.csv$\", \"\", COMPARISONS[i])\n",
        "  if (exists(\"results_list\") && comparison_clean %in% names(results_list)) {\n",
        "    res_data <- results_list[[comparison_clean]]\n",
        "    # Extract log2FC for genes in set\n",
        "    fc_values <- res_data[set_genes, \"log2FoldChange\"]\n",
        "    set_expression_matrix[, COMPARISONS[i]] <- fc_values\n",
        "  }\n",
        "}\n\n",
        "cat(\"Expression matrix created:\", nrow(set_expression_matrix), \"genes x\", \n",
        "    ncol(set_expression_matrix), \"comparisons\\n\\n\")\n"
      )
    } else if (template_type == "template_venn") {
      # Extract thresholds if provided
      padj_threshold <- if(!is.null(params$padj_threshold)) params$padj_threshold else 0.1
      fc_threshold <- if(!is.null(params$fc_threshold)) params$fc_threshold else 0
      
      code <- paste0(
        "# Extract significant genes from results_list generated in Section 0\n",
        "# Using thresholds from Shiny app settings\n",
        "cat(\"Extracting gene sets for Venn diagram...\\n\")\n",
        "cat(\"  Thresholds: padj < ", padj_threshold, ", |log2FC| > ", fc_threshold, "\\n\\n\")\n",
        "gene_sets <- list()\n\n",
        "for (i in 1:length(COMPARISONS)) {\n",
        "  # Clean comparison name (remove .csv extension)\n",
        "  comparison_clean <- sub(\"\\\\.csv$\", \"\", COMPARISONS[i])\n",
        "  \n",
        "  # Get results from results_list\n",
        "  if (exists(\"results_list\") && comparison_clean %in% names(results_list)) {\n",
        "    res_data <- results_list[[comparison_clean]]\n",
        "    \n",
        "    # Filter by BOTH padj AND log2FoldChange thresholds (matches Shiny app)\n",
        "    sig_genes <- rownames(res_data)[which(\n",
        "      !is.na(res_data$padj) & \n",
        "      res_data$padj < ", padj_threshold, " & \n",
        "      abs(res_data$log2FoldChange) > ", fc_threshold, "\n",
        "    )]\n",
        "    \n",
        "    # Use letter labels (A, B, C) for cleaner Venn diagram\n",
        "    gene_sets[[LETTERS[i]]] <- sig_genes\n",
        "    cat(\"  Set\", LETTERS[i], \"(\", comparison_clean, \"):\", length(sig_genes), \"genes\\n\")\n",
        "  } else {\n",
        "    # Fallback: use first results if available\n",
        "    sig_genes <- rownames(results_de)[which(\n",
        "      !is.na(results_de$padj) & \n",
        "      results_de$padj < ", padj_threshold, " & \n",
        "      abs(results_de$log2FoldChange) > ", fc_threshold, "\n",
        "    )]\n",
        "    gene_sets[[LETTERS[i]]] <- sig_genes\n",
        "    cat(\"  Set\", LETTERS[i], \"(default):\", length(sig_genes), \"genes\\n\")\n",
        "  }\n",
        "}\n",
        "cat(\"\\nGene sets created for Venn diagram\\n\\n\")\n"
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
generateCodeFromTemplate <- function(template_name, params, mode = "full", use_existing_objects = FALSE) {
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
  
  # Read template
  template <- readTemplate(template_name)
  
  # Replace template variables
  code <- replaceTemplateVars(template, params)
  
  # Build and replace data loading section
  data_load_section <- buildDataLoadSection(mode, use_existing_objects, template_name, params)
  code <- gsub("{{DATA_LOAD_SECTION}}", data_load_section, code, fixed = TRUE)
  
  # Only .R format is supported
  
  return(code)
}

# R Markdown format has been removed - only .R format is supported
# The following function is kept for reference but is no longer called
# convertToRMarkdown <- function(code, template_name, params) { ... }

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

# OLD IMPLEMENTATION (kept for reference, will be removed after testing)
generateDESeq2PipelineCode_OLD <- function(params, format = "r") {
  # Extract parameters
  design_formula <- params$design_formula
  contrasts <- params$contrasts  # List of contrasts to test
  filter_threshold <- if(!is.null(params$filter_threshold)) params$filter_threshold else 10
  filter_samples <- if(!is.null(params$filter_samples)) params$filter_samples else 3
  alpha <- if(!is.null(params$alpha)) params$alpha else 0.1
  counts_filename <- if(!is.null(params$counts_filename)) params$counts_filename else "counts.csv"
  metadata_filename <- if(!is.null(params$metadata_filename)) params$metadata_filename else "metadata.csv"
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Header
  header <- paste0(
    "################################################################################\n",
    "# SECTION 0: Complete DESeq2 Analysis Pipeline\n",
    "################################################################################\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "#\n",
    "# This script performs a complete RNA-seq differential expression analysis:\n",
    "#   1. Load raw count data and sample metadata\n",
    "#   2. Create DESeqDataSet object\n",
    "#   3. Filter low-count genes\n",
    "#   4. Run DESeq2 normalization and differential expression\n",
    "#   5. Extract results for specified contrasts\n",
    "#   6. Perform variance-stabilizing transformation (VST)\n",
    "#   7. Perform regularized log transformation (rlog)\n",
    "#   8. Extract normalized counts\n",
    "#\n",
    "# Design formula: ", design_formula, "\n",
    "# Filter threshold: >= ", filter_threshold, " counts in >= ", filter_samples, " samples\n",
    "# Significance level: ", alpha, "\n",
    "################################################################################\n\n"
  )
  
  # Load libraries
  libraries <- paste0(
    "# Load required libraries\n",
    "cat(\"Loading required libraries...\\n\")\n",
    "suppressPackageStartupMessages({\n",
    "  library(DESeq2)\n",
    "  library(ggplot2)\n",
    "  library(pheatmap)\n",
    "  library(RColorBrewer)\n",
    "  library(dplyr)\n",
    "  library(tidyr)\n",
    "})\n",
    "cat(\"Libraries loaded successfully!\\n\\n\")\n\n"
  )
  
  # Load data section
  data_load <- paste0(
    "################################################################################\n",
    "# Step 1: Load Data\n",
    "################################################################################\n\n",
    "cat(\"Step 1: Loading count data and metadata...\\n\")\n\n",
    "# Load count matrix (genes x samples)\n",
    "counts_data <- read.csv(\"", counts_filename, "\", row.names = 1, check.names = FALSE)\n",
    "cat(\"  - Loaded count matrix:\", nrow(counts_data), \"genes ×\", ncol(counts_data), \"samples\\n\")\n\n",
    "# Load sample metadata\n",
    "metadata <- read.csv(\"", metadata_filename, "\", row.names = 1, check.names = FALSE)\n",
    "cat(\"  - Loaded metadata for\", nrow(metadata), \"samples\\n\")\n",
    "cat(\"  - Metadata columns:\", paste(colnames(metadata), collapse = \", \"), \"\\n\")\n\n",
    "# Verify sample names match\n",
    "if (!all(colnames(counts_data) == rownames(metadata))) {\n",
    "  stop(\"ERROR: Sample names in count matrix and metadata do not match!\")\n",
    "}\n",
    "cat(\"  ✓ Sample names match between count matrix and metadata\\n\\n\")\n\n",
    "# Display data structure\n",
    "cat(\"Count data preview:\\n\")\n",
    "print(head(counts_data[, 1:min(5, ncol(counts_data))], 3))\n",
    "cat(\"\\nMetadata preview:\\n\")\n",
    "print(head(metadata, 3))\n",
    "cat(\"\\n\")\n\n"
  )
  
  # Create DESeqDataSet
  create_dds <- paste0(
    "################################################################################\n",
    "# Step 2: Create DESeqDataSet Object\n",
    "################################################################################\n\n",
    "cat(\"Step 2: Creating DESeqDataSet object...\\n\")\n\n",
    "# Ensure count matrix contains integers\n",
    "counts_matrix <- as.matrix(counts_data)\n",
    "storage.mode(counts_matrix) <- \"integer\"\n\n",
    "# Create DESeqDataSet with design formula\n",
    "dds <- DESeqDataSetFromMatrix(\n",
    "  countData = counts_matrix,\n",
    "  colData = metadata,\n",
    "  design = ", design_formula, "\n",
    ")\n\n",
    "cat(\"  ✓ DESeqDataSet created with design:\", deparse(design(dds)), \"\\n\")\n",
    "cat(\"  - Total genes:\", nrow(dds), \"\\n\")\n",
    "cat(\"  - Total samples:\", ncol(dds), \"\\n\\n\")\n\n"
  )
  
  # Filter low counts
  filter_code <- paste0(
    "################################################################################\n",
    "# Step 3: Filter Low-Count Genes\n",
    "################################################################################\n\n",
    "cat(\"Step 3: Filtering low-count genes...\\n\")\n",
    "cat(\"  - Threshold: >= ", filter_threshold, " counts in >= ", filter_samples, " samples\\n\")\n\n",
    "# Count how many samples have sufficient counts for each gene\n",
    "keep <- rowSums(counts(dds) >= ", filter_threshold, ") >= ", filter_samples, "\n",
    "dds <- dds[keep, ]\n\n",
    "cat(\"  ✓ Retained\", sum(keep), \"genes after filtering\\n\")\n",
    "cat(\"  - Removed\", sum(!keep), \"low-count genes\\n\\n\")\n\n"
  )
  
  # Run DESeq2
  run_deseq <- paste0(
    "################################################################################\n",
    "# Step 4: Run DESeq2 Analysis\n",
    "################################################################################\n\n",
    "cat(\"Step 4: Running DESeq2 normalization and differential expression...\\n\")\n",
    "cat(\"  (This may take a few minutes for large datasets)\\n\\n\")\n\n",
    "# Run the DESeq2 pipeline\n",
    "# This performs: estimation of size factors, estimation of dispersions, \n",
    "# negative binomial GLM fitting, and Wald statistics\n",
    "dds <- DESeq(dds)\n\n",
    "cat(\"  ✓ DESeq2 analysis complete!\\n\")\n",
    "cat(\"  - Size factors calculated\\n\")\n",
    "cat(\"  - Dispersion estimates computed\\n\")\n",
    "cat(\"  - Statistical testing completed\\n\\n\")\n\n",
    "# Display size factors\n",
    "cat(\"Size factors (normalization factors):\\n\")\n",
    "print(sizeFactors(dds))\n",
    "cat(\"\\n\")\n\n"
  )
  
  # Extract results
  # Generate code for each contrast
  results_code <- paste0(
    "################################################################################\n",
    "# Step 5: Extract Differential Expression Results\n",
    "################################################################################\n\n",
    "cat(\"Step 5: Extracting differential expression results...\\n\\n\")\n\n"
  )
  
  if (!is.null(contrasts) && length(contrasts) > 0) {
    # If specific contrasts provided
    for (i in seq_along(contrasts)) {
      contrast <- contrasts[[i]]
      contrast_name <- names(contrasts)[i]
      if (is.null(contrast_name) || contrast_name == "") {
        contrast_name <- paste0("contrast_", i)
      }
      
      results_code <- paste0(results_code,
        "# Contrast ", i, ": ", contrast_name, "\n",
        "cat(\"Extracting results for: ", contrast_name, "\\n\")\n",
        "results_", i, " <- results(dds, contrast = c(\"", paste(contrast, collapse = "\", \""), "\"), alpha = ", alpha, ")\n",
        "results_", i, " <- as.data.frame(results_", i, ")\n",
        "cat(\"  - Total genes:\", nrow(results_", i, "), \"\\n\")\n",
        "cat(\"  - Significant (padj < ", alpha, "):\", sum(results_", i, "$padj < ", alpha, ", na.rm = TRUE), \"\\n\")\n",
        "cat(\"    * Upregulated:\", sum(results_", i, "$padj < ", alpha, " & results_", i, "$log2FoldChange > 0, na.rm = TRUE), \"\\n\")\n",
        "cat(\"    * Downregulated:\", sum(results_", i, "$padj < ", alpha, " & results_", i, "$log2FoldChange < 0, na.rm = TRUE), \"\\n\\n\")\n\n"
      )
    }
    # Set primary results object
    results_code <- paste0(results_code,
      "# Set primary results object for downstream plotting\n",
      "results_de <- results_1  # Use first contrast as default\n\n"
    )
  } else {
    # Default: extract results from first coefficient
    results_code <- paste0(results_code,
      "# Extract results (using default contrast from design)\n",
      "results_de <- results(dds, alpha = ", alpha, ")\n",
      "results_de <- as.data.frame(results_de)\n\n",
      "cat(\"  - Total genes:\", nrow(results_de), \"\\n\")\n",
      "cat(\"  - Significant (padj < ", alpha, "):\", sum(results_de$padj < ", alpha, ", na.rm = TRUE), \"\\n\")\n",
      "cat(\"    * Upregulated:\", sum(results_de$padj < ", alpha, " & results_de$log2FoldChange > 0, na.rm = TRUE), \"\\n\")\n",
      "cat(\"    * Downregulated:\", sum(results_de$padj < ", alpha, " & results_de$log2FoldChange < 0, na.rm = TRUE), \"\\n\\n\")\n\n"
    )
  }
  
  results_code <- paste0(results_code,
    "# Display top results\n",
    "cat(\"Top 10 most significant genes:\\n\")\n",
    "print(head(results_de[order(results_de$padj), ], 10))\n",
    "cat(\"\\n\")\n\n"
  )
  
  # Transformations
  transformations <- paste0(
    "################################################################################\n",
    "# Step 6: Variance Stabilizing Transformation (VST)\n",
    "################################################################################\n\n",
    "cat(\"Step 6: Performing variance-stabilizing transformation...\\n\")\n\n",
    "# VST is recommended for datasets with >= 30 samples\n",
    "# For visualization and clustering\n",
    "vsd <- vst(dds, blind = FALSE)\n",
    "vst_mat <- assay(vsd)\n\n",
    "cat(\"  ✓ VST transformation complete\\n\")\n",
    "cat(\"  - Transformed matrix:\", nrow(vst_mat), \"genes ×\", ncol(vst_mat), \"samples\\n\\n\")\n\n",
    "################################################################################\n",
    "# Step 7: Regularized Log Transformation (rlog)\n",
    "################################################################################\n\n",
    "cat(\"Step 7: Performing regularized log transformation...\\n\")\n\n",
    "# rlog is recommended for datasets with < 30 samples\n",
    "# Provides better variance stabilization for small sample sizes\n",
    "rld <- rlog(dds, blind = FALSE)\n",
    "rlog_mat <- assay(rld)\n\n",
    "cat(\"  ✓ rlog transformation complete\\n\")\n",
    "cat(\"  - Transformed matrix:\", nrow(rlog_mat), \"genes ×\", ncol(rlog_mat), \"samples\\n\\n\")\n\n"
  )
  
  # Extract normalized counts
  normalized_counts_code <- paste0(
    "################################################################################\n",
    "# Step 8: Extract Normalized Counts\n",
    "################################################################################\n\n",
    "cat(\"Step 8: Extracting normalized counts...\\n\")\n\n",
    "# Extract size-factor normalized counts\n",
    "normalized_counts <- counts(dds, normalized = TRUE)\n\n",
    "cat(\"  ✓ Normalized counts extracted\\n\")\n",
    "cat(\"  - Matrix:\", nrow(normalized_counts), \"genes ×\", ncol(normalized_counts), \"samples\\n\\n\")\n\n"
  )
  
  # Summary
  summary_code <- paste0(
    "################################################################################\n",
    "# Pipeline Complete - Objects Created\n",
    "################################################################################\n\n",
    "cat(\"\\n\" , paste(rep(\"=\", 80), collapse = \"\"), \"\\n\")\n",
    "cat(\"PIPELINE COMPLETE!\\n\")\n",
    "cat(paste(rep(\"=\", 80), collapse = \"\"), \"\\n\\n\")\n\n",
    "cat(\"The following R objects are now available for downstream analysis:\\n\\n\")\n",
    "cat(\"  📊 dds              - DESeqDataSet object (main analysis object)\\n\")\n",
    "cat(\"  📈 results_de       - Differential expression results (data frame)\\n\")\n",
    "cat(\"  🔢 normalized_counts - Size-factor normalized counts (matrix)\\n\")\n",
    "cat(\"  📉 vst_mat          - Variance-stabilized counts (matrix)\\n\")\n",
    "cat(\"  📉 rlog_mat         - Regularized log counts (matrix)\\n\")\n",
    "cat(\"  🧬 vsd              - VST DESeqTransform object\\n\")\n",
    "cat(\"  🧬 rld              - rlog DESeqTransform object\\n\\n\")\n",
    "cat(\"You can now proceed with visualization and further analysis!\\n\")\n",
    "cat(paste(rep(\"=\", 80), collapse = \"\"), \"\\n\\n\")\n\n"
  )
  
  # Combine all sections
  full_code <- paste0(
    header,
    libraries,
    data_load,
    create_dds,
    filter_code,
    run_deseq,
    results_code,
    transformations,
    normalized_counts_code,
    summary_code
  )
  
  # Format as R Markdown if requested
  if (format == "rmd") {
    rmd_header <- createRmdHeader("Complete DESeq2 Analysis Pipeline")
    intro_text <- paste0(
      "## Overview\n\n",
      "This document contains a complete, reproducible DESeq2 RNA-seq analysis pipeline.\n\n",
      "**Design Formula:** `", design_formula, "`\n\n",
      "**Analysis Steps:**\n",
      "1. Load raw count data and sample metadata\n",
      "2. Create DESeqDataSet object\n",
      "3. Filter low-count genes\n",
      "4. Run DESeq2 normalization and differential expression\n",
      "5. Extract results for specified contrasts\n",
      "6. Perform variance-stabilizing transformation (VST)\n",
      "7. Perform regularized log transformation (rlog)\n",
      "8. Extract normalized counts\n\n",
      "---\n\n"
    )
    
    full_code <- paste0(
      rmd_header,
      intro_text,
      "## Load Libraries\n\n",
      wrapInChunk(libraries, "load-libraries", message = TRUE),
      "## Step 1: Load Data\n\n",
      wrapInChunk(data_load, "load-data", message = TRUE),
      "## Step 2: Create DESeqDataSet\n\n",
      wrapInChunk(create_dds, "create-dds", message = TRUE),
      "## Step 3: Filter Low Counts\n\n",
      wrapInChunk(filter_code, "filter-counts", message = TRUE),
      "## Step 4: Run DESeq2\n\n",
      wrapInChunk(run_deseq, "run-deseq", message = TRUE),
      "## Step 5: Extract Results\n\n",
      wrapInChunk(results_code, "extract-results", message = TRUE),
      "## Step 6-7: Transformations\n\n",
      wrapInChunk(transformations, "transformations", message = TRUE),
      "## Step 8: Normalized Counts\n\n",
      wrapInChunk(normalized_counts_code, "normalized-counts", message = TRUE),
      "## Summary\n\n",
      wrapInChunk(summary_code, "summary", message = TRUE),
      "## Session Information\n\n",
      wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# ============================================================================
# END SECTION 0 GENERATOR
# ============================================================================

# 1. Volcano Plot Code Generator
generateVolcanoCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Validate required parameters
  if (is.null(params$comparison_name) || length(params$comparison_name) == 0) {
    stop("comparison_name is required for volcano plot")
  }
  if (is.null(params$padj_threshold) || length(params$padj_threshold) == 0) {
    stop("padj_threshold is required for volcano plot")
  }
  if (is.null(params$log2fc_threshold) || length(params$log2fc_threshold) == 0) {
    stop("log2fc_threshold is required for volcano plot")
  }
  
  # Use provided data_file if available, otherwise generate default name
  data_file <- if (!is.null(params$data_file) && length(params$data_file) > 0) {
    params$data_file
  } else {
    paste0("volcano_data_", params$comparison_name, "_", timestamp, ".csv")
  }
  
  # Handle genes of interest parameter
  genes_of_interest <- if (!is.null(params$genes_of_interest) && length(params$genes_of_interest) > 0 && nchar(params$genes_of_interest) > 0) {
    # Parse comma-separated genes
    genes_vec <- strsplit(params$genes_of_interest, ",\\s*")[[1]]
    genes_vec <- trimws(genes_vec)
    genes_vec <- genes_vec[nchar(genes_vec) > 0]  # Remove empty strings
    if (length(genes_vec) > 0) {
      paste0("c(", paste0('"', genes_vec, '"', collapse = ", "), ")")
    } else {
      "NULL"
    }
  } else {
    "NULL"
  }
  
  # Prepare template parameters
  template_params <- list(
    comparison_name = params$comparison_name,
    padj_threshold = params$padj_threshold,
    log2fc_threshold = params$log2fc_threshold,
    use_gene_names = if(!is.null(params$use_gene_names) && length(params$use_gene_names) > 0) params$use_gene_names else FALSE,
    gene_label_column = if(!is.null(params$gene_type) && length(params$gene_type) > 0) params$gene_type else "gene.id",
    data_file = data_file,
    genes_of_interest = genes_of_interest
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_volcano", template_params, 
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generateVolcanoCode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  comparison_name <- params$comparison_name
  padj_threshold <- params$padj_threshold
  log2fc_threshold <- params$log2fc_threshold
  use_gene_names <- params$use_gene_names
  gene_type <- params$gene_type
  
  # Header comments
  header <- paste0(
    "# Publication-ready Volcano Plot\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Comparison: ", comparison_name, "\n",
    "# Adjusted p-value threshold: ", padj_threshold, "\n",
    "# Log2 fold change threshold: ", log2fc_threshold, "\n\n"
  )
  
  # Library loading
  libraries <- paste0(
    "# Load required libraries\n",
    "library(ggplot2)\n",
    "library(ggrepel)  # for gene labels (install if needed: install.packages('ggrepel'))\n\n"
  )
  
  # Data loading section
  if (use_existing_objects) {
    # Use objects from Section 0 (DESeq2 pipeline)
    data_load <- paste0(
      "# Use results from Section 0 (DESeq2 pipeline)\n",
      "results_data <- results_de  # Generated in Section 0\n",
      "cat(\"Using results from DESeq2 pipeline: \", nrow(results_data), \" genes\\n\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load data (exported from DESeq2Shiny)\n",
      "results_data <- read.csv(\"volcano_data_", comparison_name, "_", timestamp, ".csv\", row.names = 1)\n\n",
      "# Check data structure\n",
      "head(results_data)\n",
      "cat(\"Total genes:\", nrow(results_data), \"\\n\")\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes 'results_data' data frame exists with columns:\n",
      "# - log2FoldChange: log2 fold change values\n",
      "# - padj: adjusted p-values\n",
      "# - ", if(use_gene_names && gene_type == "gene.name") "gene.name" else "gene.id", ": gene identifiers\n\n",
      "# Example: results_data <- results(dds, contrast = c('condition', 'treated', 'control'))\n",
      "# results_data <- as.data.frame(results_data)\n\n"
    )
  }
  
  # Plot parameters
  plot_params <- paste0(
    "# Plot parameters (adjust as needed)\n",
    "padj_threshold <- ", padj_threshold, "\n",
    "log2fc_threshold <- ", log2fc_threshold, "\n",
    "point_size <- 2\n",
    "point_alpha <- 0.6\n",
    "sig_color <- \"red\"\n",
    "nonsig_color <- \"grey\"\n",
    "threshold_color <- \"blue\"\n\n"
  )
  
  # Add significance column
  sig_column <- paste0(
    "# Add significance classification\n",
    "results_data$significant <- (results_data$padj < padj_threshold & \n",
    "                              abs(results_data$log2FoldChange) > log2fc_threshold)\n",
    "results_data$significant[is.na(results_data$significant)] <- FALSE\n\n",
    "# Count significant genes\n",
    "sig_up <- sum(results_data$significant & results_data$log2FoldChange > 0, na.rm = TRUE)\n",
    "sig_down <- sum(results_data$significant & results_data$log2FoldChange < 0, na.rm = TRUE)\n",
    "cat(\"Significant genes - Up:\", sig_up, \"Down:\", sig_down, \"\\n\\n\")\n\n"
  )
  
  # Create plot
  gene_label <- if(use_gene_names && gene_type == "gene.name") "gene.name" else "rownames(results_data)"
  
  plot_code <- paste0(
    "# Create volcano plot\n",
    "volcano_plot <- ggplot(results_data, aes(x = log2FoldChange, y = -log10(padj))) +\n",
    "  # Plot all points\n",
    "  geom_point(aes(color = significant), alpha = point_alpha, size = point_size) +\n",
    "  \n",
    "  # Color scheme\n",
    "  scale_color_manual(values = c(\"FALSE\" = nonsig_color, \"TRUE\" = sig_color),\n",
    "                     labels = c(\"Not significant\", \"Significant\"),\n",
    "                     name = \"Significance\") +\n",
    "  \n",
    "  # Threshold lines\n",
    "  geom_hline(yintercept = -log10(padj_threshold), linetype = \"dashed\", \n",
    "             color = threshold_color, alpha = 0.7) +\n",
    "  geom_vline(xintercept = c(-log2fc_threshold, log2fc_threshold), \n",
    "             linetype = \"dashed\", color = threshold_color, alpha = 0.7) +\n",
    "  \n",
    "  # Labels and theme\n",
    "  labs(title = \"Volcano Plot: ", comparison_name, "\",\n",
    "       subtitle = paste0(\"Thresholds: padj < \", padj_threshold, \n",
    "                         \", |log2FC| > \", log2fc_threshold),\n",
    "       x = \"log2 Fold Change\",\n",
    "       y = \"-log10(adjusted p-value)\") +\n",
    "  theme_bw(base_size = 12) +\n",
    "  theme(legend.position = \"bottom\",\n",
    "        plot.title = element_text(hjust = 0.5, face = \"bold\"),\n",
    "        plot.subtitle = element_text(hjust = 0.5),\n",
    "        panel.grid.minor = element_blank())\n\n",
    "# Display plot\n",
    "print(volcano_plot)\n\n"
  )
  
  # Optional: Add labels for top genes
  label_code <- paste0(
    "# Optional: Add labels for top significant genes\n",
    "# Uncomment to label top 10 most significant genes\n",
    "# top_genes <- results_data[order(results_data$padj), ][1:10, ]\n",
    "# volcano_plot_labeled <- volcano_plot +\n",
    "#   geom_text_repel(data = top_genes,\n",
    "#                   aes(label = ", gene_label, "),\n",
    "#                   size = 3, box.padding = 0.5, max.overlaps = 20)\n",
    "# print(volcano_plot_labeled)\n\n"
  )
  
  # Save plot
  save_code <- paste0(
    "# Save high-resolution plots\n",
    "ggsave(\"volcano_plot_", comparison_name, ".pdf\", volcano_plot, \n",
    "       width = 8, height = 6, dpi = 300)\n",
    "ggsave(\"volcano_plot_", comparison_name, ".png\", volcano_plot, \n",
    "       width = 8, height = 6, dpi = 300)\n",
    "cat(\"Plots saved successfully!\\n\")\n"
  )
  
  # Combine all sections
  full_code <- paste0(header, libraries, data_load, plot_params, sig_column, 
                     plot_code, label_code, save_code)
  
  # Format for R Markdown if requested
  if (format == "rmd") {
    rmd_header <- createRmdHeader(paste("Volcano Plot:", comparison_name))
    intro_text <- paste0(
      "## Overview\n\n",
      "This document contains R code to generate a publication-ready volcano plot ",
      "from DESeq2 differential expression results.\n\n"
    )
    
    full_code <- paste0(
      rmd_header,
      intro_text,
      "## Load Libraries\n\n",
      wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Data\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Set Parameters\n\n",
      wrapInChunk(plot_params, "parameters"),
      "## Prepare Data\n\n",
      wrapInChunk(sig_column, "prep-data"),
      "## Create Volcano Plot\n\n",
      wrapInChunk(plot_code, "volcano-plot", message = TRUE),
      "## Optional Gene Labels\n\n",
      wrapInChunk(label_code, "gene-labels"),
      "## Save Plot\n\n",
      wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n",
      wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# 2. Boxplot Code Generator
generateBoxplotCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Validate required parameters
  if (is.null(params$selected_genes) || length(params$selected_genes) == 0) {
    stop("selected_genes is required for boxplot")
  }
  if (is.null(params$x_axis) || length(params$x_axis) == 0) {
    stop("x_axis is required for boxplot")
  }
  if (is.null(params$fill_by) || length(params$fill_by) == 0) {
    stop("fill_by is required for boxplot")
  }
  
  # Prepare template parameters
  template_params <- list(
    selected_genes = params$selected_genes,
    x_axis = params$x_axis,
    fill_by = params$fill_by,
    use_gene_names = if(!is.null(params$use_gene_names) && length(params$use_gene_names) > 0) params$use_gene_names else FALSE,
    custom_colors = params$custom_colors,
    num_cols = min(3, length(params$selected_genes)),
    counts_file = paste0("normalized_counts_", timestamp, ".csv"),
    metadata_file = paste0("metadata_", timestamp, ".csv")
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_boxplot", template_params,
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generateBoxplotCode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  selected_genes <- params$selected_genes
  x_axis <- params$x_axis
  fill_by <- params$fill_by
  factors <- params$factors
  use_gene_names <- params$use_gene_names
  custom_colors <- params$custom_colors
  
  header <- paste0(
    "# Publication-ready Gene Expression Boxplot\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Selected genes: ", paste(selected_genes, collapse = ", "), "\n",
    "# X-axis: ", x_axis, " | Fill by: ", fill_by, "\n\n"
  )
  
  libraries <- paste0(
    "# Load required libraries\n",
    "library(ggplot2)\n",
    "library(tidyr)\n",
    "library(dplyr)\n\n"
  )
  
  if (use_existing_objects) {
    # Use objects from Section 0
    data_load <- paste0(
      "# Use data from Section 0 (DESeq2 pipeline)\n",
      "counts_data <- normalized_counts  # Generated in Section 0\n",
      "sample_metadata <- as.data.frame(colData(dds))  # From Section 0\n",
      "cat(\"Using normalized counts from DESeq2 pipeline\\n\\n\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load data\n",
      "counts_data <- read.csv(\"normalized_counts_", timestamp, ".csv\", row.names = 1)\n",
      "sample_metadata <- read.csv(\"metadata_", timestamp, ".csv\", row.names = 1)\n\n",
      "# Check data structure\n",
      "head(counts_data)\n",
      "head(sample_metadata)\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes the following objects exist:\n",
      "# - dds: DESeqDataSet object\n",
      "# - sample_metadata: data frame with sample information\n\n",
      "# Extract normalized counts\n",
      "# counts_data <- counts(dds, normalized = TRUE)\n\n"
    )
  }
  
  # Prepare data for plotting
  prep_code <- paste0(
    "# Prepare data for plotting\n",
    "selected_genes <- c(", paste0("\"", selected_genes, "\"", collapse = ", "), ")\n\n",
    "# Check if genes exist in rownames (could be Ensembl IDs or gene symbols)\n",
    "if (all(selected_genes %in% rownames(counts_data))) {\n",
    "  # Genes found directly in rownames\n",
    "  gene_indices <- selected_genes\n",
    "  cat(\"Using genes directly from rownames\\n\")\n",
    "} else {\n",
    "  # Try to find genes by matching - could be in a gene_name or symbol column\n",
    "  # First check if there's annotation data\n",
    "  if (exists(\"gene_annotations\") && \"gene_name\" %in% colnames(gene_annotations)) {\n",
    "    # Match by gene name\n",
    "    matched_indices <- match(selected_genes, gene_annotations$gene_name)\n",
    "    if (any(!is.na(matched_indices))) {\n",
    "      gene_indices <- rownames(counts_data)[matched_indices[!is.na(matched_indices)]]\n",
    "      cat(\"Matched\", sum(!is.na(matched_indices)), \"genes by name\\n\")\n",
    "    } else {\n",
    "      stop(\"Could not find selected genes in data. Please check gene names.\")\n",
    "    }\n",
    "  } else {\n",
    "    # No annotation available - genes might already be in rownames\n",
    "    # Check if any partial matches exist\n",
    "    cat(\"Warning: Some genes not found in rownames.\\n\")\n",
    "    cat(\"Available rownames (first 10):\", paste(head(rownames(counts_data), 10), collapse=\", \"), \"\\n\")\n",
    "    cat(\"Requested genes:\", paste(selected_genes, collapse=\", \"), \"\\n\")\n",
    "    # Use only genes that exist\n",
    "    gene_indices <- selected_genes[selected_genes %in% rownames(counts_data)]\n",
    "    if (length(gene_indices) == 0) {\n",
    "      stop(\"None of the selected genes found in count matrix. Check gene names/IDs.\")\n",
    "    }\n",
    "    cat(\"Found\", length(gene_indices), \"out of\", length(selected_genes), \"genes\\n\")\n",
    "  }\n",
    "}\n\n",
    "# Log2 transform counts (adding 0.5 pseudocount)\n",
    "log2_counts <- log2(counts_data[gene_indices, , drop = FALSE] + 0.5)\n\n",
    "# Use gene symbols for display if available, otherwise use IDs\n",
    "display_names <- if (exists(\"gene_annotations\") && \"gene_name\" %in% colnames(gene_annotations)) {\n",
    "  gene_annotations[gene_indices, \"gene_name\"]\n",
    "} else {\n",
    "  gene_indices\n",
    "}\n",
    "rownames(log2_counts) <- display_names\n\n",
    "# Reshape data for ggplot\n",
    "plot_data <- as.data.frame(t(log2_counts))\n",
    "plot_data$sample <- rownames(plot_data)\n\n",
    "# Add metadata\n",
    "plot_data <- merge(plot_data, sample_metadata, by.x = \"sample\", by.y = \"row.names\")\n\n",
    "# Optional: Filter for specific samples or conditions\n",
    "# Uncomment and modify to filter data:\n",
    "# plot_data <- plot_data[plot_data$", x_axis, " %in% c(\"value1\", \"value2\"), ]\n\n",
    "# Convert to long format\n",
    "plot_data_long <- plot_data %>%\n",
    "  pivot_longer(cols = all_of(display_names), \n",
    "               names_to = \"gene\", \n",
    "               values_to = \"expression\")\n\n"
  )
  
  # Custom colors if provided
  color_code <- ""
  if (!is.null(custom_colors) && length(custom_colors) > 0) {
    color_vals <- paste0("\"", names(custom_colors), "\" = \"", unlist(custom_colors), "\"", collapse = ",\n    ")
    color_code <- paste0(
      "# Custom colors\n",
      "custom_colors <- c(\n    ",
      color_vals,
      "\n)\n\n"
    )
  }
  
  plot_code <- paste0(
    "# Create boxplot\n",
    "boxplot <- ggplot(plot_data_long, aes(x = ", x_axis, ", y = expression, fill = ", fill_by, ")) +\n",
    "  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5) +\n",
    "  facet_wrap(~gene, scales = \"free_y\", ncol = ", min(3, length(selected_genes)), ") +\n",
    if (color_code != "") "  scale_fill_manual(values = custom_colors) +\n" else "",
    "  labs(title = \"Gene Expression Boxplot\",\n",
    "       x = \"", x_axis, "\",\n",
    "       y = \"log2(Normalized Counts + 0.5)\",\n",
    "       fill = \"", fill_by, "\") +\n",
    "  theme_bw(base_size = 12) +\n",
    "  theme(axis.text.x = element_text(angle = 45, hjust = 1),\n",
    "        legend.position = \"bottom\",\n",
    "        plot.title = element_text(hjust = 0.5, face = \"bold\"),\n",
    "        strip.background = element_rect(fill = \"lightblue\"),\n",
    "        panel.grid.minor = element_blank())\n\n",
    "# Display plot\n",
    "print(boxplot)\n\n"
  )
  
  save_code <- paste0(
    "# Save high-resolution plot\n",
    "ggsave(\"boxplot_", x_axis, "_by_", fill_by, ".pdf\", boxplot, \n",
    "       width = ", max(8, length(selected_genes) * 2.5), ", height = 6, dpi = 300)\n",
    "ggsave(\"boxplot_", x_axis, "_by_", fill_by, ".png\", boxplot, \n",
    "       width = ", max(8, length(selected_genes) * 2.5), ", height = 6, dpi = 300)\n",
    "cat(\"Plots saved successfully!\\n\")\n"
  )
  
  full_code <- paste0(header, libraries, data_load, prep_code, color_code, plot_code, save_code)
  
  if (format == "rmd") {
    rmd_header <- createRmdHeader("Gene Expression Boxplot")
    intro_text <- "## Overview\n\nGene expression boxplot showing normalized counts across conditions.\n\n"
    
    full_code <- paste0(
      rmd_header, intro_text,
      "## Load Libraries\n\n", wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Data\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Prepare Data\n\n", wrapInChunk(paste0(prep_code, color_code), "prep-data"),
      "## Create Boxplot\n\n", wrapInChunk(plot_code, "boxplot", message = TRUE),
      "## Save Plot\n\n", wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n", wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# 3. Heatmap Code Generator
generateHeatmapCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Check if this is a brushed heatmap
  is_brushed <- if(!is.null(params$is_brushed_heatmap) && length(params$is_brushed_heatmap) > 0) params$is_brushed_heatmap else FALSE
  
  # Check if this is a Venn heatmap (log2FC data, no transformation needed)
  is_venn <- if(!is.null(params$is_venn_heatmap) && length(params$is_venn_heatmap) > 0) params$is_venn_heatmap else FALSE
  
  # Prepare sample order for brushed heatmaps
  sample_order_value <- if(!is.null(params$sample_order) && length(params$sample_order) > 0 && is_brushed) {
    paste0("c(", paste0('"', params$sample_order, '"', collapse = ", "), ")")
  } else {
    "NULL"
  }
  
  # Prepare color range for brushed heatmaps (to match parent heatmap colors)
  color_range_value <- if(!is.null(params$color_range) && length(params$color_range) == 2) {
    paste0("c(", params$color_range[1], ", ", params$color_range[2], ")")
  } else {
    "NULL"
  }
  
  # Prepare template parameters
  num_genes <- if(!is.null(params$num_genes) && length(params$num_genes) > 0) params$num_genes else 50
  
  # Use different file names for brushed vs regular heatmaps
  file_prefix <- if(is_brushed) "brushed_heatmap" else "heatmap"
  
  # Allow custom filenames to be passed (e.g., for Venn brushed heatmaps)
  counts_file <- if(!is.null(params$counts_file)) {
    params$counts_file
  } else {
    paste0(file_prefix, "_counts_", timestamp, ".csv")
  }
  
  metadata_file <- if(!is.null(params$metadata_file)) {
    params$metadata_file
  } else {
    paste0(file_prefix, "_metadata_", timestamp, ".csv")
  }
  
  template_params <- list(
    num_genes = num_genes,
    selected_genes = params$selected_genes,
    use_gene_names = if(!is.null(params$use_gene_names) && length(params$use_gene_names) > 0) params$use_gene_names else FALSE,
    scale_rows = FALSE,  # Default, can be changed in template
    fontsize_row = if(!is.null(num_genes) && length(num_genes) > 0 && num_genes > 50) 6 else 8,
    counts_file = counts_file,
    metadata_file = metadata_file,
    is_brushed_heatmap = is_brushed,
    is_venn_heatmap = is_venn,
    sample_order = sample_order_value,
    color_range = color_range_value
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_heatmap", template_params,
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generateHeatmapCode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  num_genes <- params$num_genes
  selected_genes <- params$selected_genes
  use_gene_names <- params$use_gene_names
  
  header <- paste0(
    "# Publication-ready Heatmap\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    if (!is.null(selected_genes)) paste0("# Selected genes: ", paste(selected_genes, collapse = ", "), "\n") else paste0("# Top ", num_genes, " genes by variance\n"),
    "\n"
  )
  
  libraries <- paste0(
    "# Load required libraries\n",
    "library(pheatmap)\n",
    "library(RColorBrewer)\n\n"
  )
  
  if (use_existing_objects) {
    # Use objects from Section 0
    data_load <- paste0(
      "# Use data from Section 0 (DESeq2 pipeline)\n",
      "# normalized_counts already generated in Section 0\n",
      "sample_metadata <- as.data.frame(colData(dds))  # From Section 0\n",
      "cat(\"Using normalized counts from DESeq2 pipeline\\n\\n\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load data\n",
      "normalized_counts <- read.csv(\"heatmap_counts_", timestamp, ".csv\", row.names = 1)\n",
      "sample_metadata <- read.csv(\"heatmap_metadata_", timestamp, ".csv\", row.names = 1)\n\n",
      "# Check data structure\n",
      "dim(normalized_counts)\n",
      "head(sample_metadata)\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes 'dds' DESeqDataSet object exists\n\n",
      "# Extract normalized counts\n",
      "# normalized_counts <- counts(dds, normalized = TRUE)\n\n"
    )
  }
  
  # Gene selection
  if (!is.null(selected_genes) && length(selected_genes) > 0) {
    gene_select <- paste0(
      "# Use specified genes\n",
      "selected_genes <- c(", paste0("\"", selected_genes, "\"", collapse = ", "), ")\n",
      "plot_matrix <- normalized_counts[selected_genes, ]\n\n"
    )
  } else {
    gene_select <- paste0(
      "# Select top ", num_genes, " genes by variance\n",
      "gene_variance <- apply(normalized_counts, 1, var)\n",
      "top_genes <- names(sort(gene_variance, decreasing = TRUE)[1:", num_genes, "])\n",
      "plot_matrix <- normalized_counts[top_genes, ]\n\n"
    )
  }
  
  prep_code <- paste0(
    "# Log2 transform (adding pseudocount)\n",
    "plot_matrix <- log2(plot_matrix + 0.5)\n\n",
    "# Z-score normalization (optional, for better visualization)\n",
    "# Uncomment the next line to z-score normalize\n",
    "# plot_matrix <- t(scale(t(plot_matrix)))\n\n"
  )
  
  plot_code <- paste0(
    "# Create annotation for samples (if metadata available)\n",
    "if (exists(\"metadata\") && nrow(metadata) > 0) {\n",
    "  # Select annotation columns (adjust as needed)\n",
    "  annotation_col <- metadata[colnames(plot_matrix), , drop = FALSE]\n",
    "  # Remove non-informative columns\n",
    "  annotation_col <- annotation_col[, !colnames(annotation_col) %in% c(\"sizeFactor\", \"replaceable\"), drop = FALSE]\n",
    "} else {\n",
    "  annotation_col <- NA\n",
    "}\n\n",
    "# Create heatmap\n",
    "heatmap_plot <- pheatmap(\n",
    "  plot_matrix,\n",
    "  scale = \"none\",  # already log2 transformed\n",
    "  clustering_distance_rows = \"euclidean\",\n",
    "  clustering_distance_cols = \"euclidean\",\n",
    "  clustering_method = \"complete\",\n",
    "  color = colorRampPalette(rev(brewer.pal(9, \"RdBu\")))(255),\n",
    "  annotation_col = annotation_col,\n",
    "  show_rownames = TRUE,\n",
    "  show_colnames = TRUE,\n",
    "  fontsize = 10,\n",
    "  fontsize_row = 8,\n",
    "  fontsize_col = 8,\n",
    "  main = \"Expression Heatmap\",\n",
    "  border_color = NA\n",
    ")\n\n",
    "# Display heatmap\n",
    "print(heatmap_plot)\n\n"
  )
  
  save_code <- paste0(
    "# Save high-resolution heatmap\n",
    "pdf(\"heatmap_", timestamp, ".pdf\", width = 10, height = ", max(8, num_genes * 0.15), ")\n",
    "print(heatmap_plot)\n",
    "dev.off()\n\n",
    "png(\"heatmap_", timestamp, ".png\", width = 3000, height = ", max(2400, num_genes * 45), ", res = 300)\n",
    "print(heatmap_plot)\n",
    "dev.off()\n\n",
    "cat(\"Heatmap saved successfully!\\n\")\n"
  )
  
  full_code <- paste0(header, libraries, data_load, gene_select, prep_code, plot_code, save_code)
  
  if (format == "rmd") {
    rmd_header <- createRmdHeader("Expression Heatmap")
    intro_text <- "## Overview\n\nHeatmap of gene expression across samples.\n\n"
    
    full_code <- paste0(
      rmd_header, intro_text,
      "## Load Libraries\n\n", wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Data\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Select Genes\n\n", wrapInChunk(gene_select, "select-genes"),
      "## Prepare Matrix\n\n", wrapInChunk(prep_code, "prep-data"),
      "## Create Heatmap\n\n", wrapInChunk(plot_code, "heatmap", message = TRUE),
      "## Save Plot\n\n", wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n", wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# 4. PCA Plot Code Generator
generatePCACode <- function(params, mode = "full", use_existing_objects = FALSE) {
  # Validate input parameters
  if (is.null(params) || !is.list(params)) {
    stop("Invalid params: must be a non-null list")
  }
  if (is.null(mode) || length(mode) == 0 || !is.character(mode)) {
    mode <- "full"
  }
  if (is.null(use_existing_objects) || length(use_existing_objects) == 0) {
    use_existing_objects <- FALSE
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Validate and set defaults with explicit checks
  intgroup <- "Conditions"  # Default
  if (!is.null(params$intgroup) && length(params$intgroup) > 0 && is.character(params$intgroup)) {
    intgroup <- params$intgroup[1]  # Take first element if vector
  }
  
  transform_type <- "vst"  # Default
  if (!is.null(params$transform_type) && length(params$transform_type) > 0 && is.character(params$transform_type)) {
    transform_type <- params$transform_type[1]  # Take first element if vector
  }
  
  # Prepare template parameters
  template_params <- list(
    intgroup = intgroup,
    transform_type = transform_type,
    transformed_data_file = paste0(transform_type, "_data_", timestamp, ".csv"),
    metadata_file = paste0("pca_metadata_", timestamp, ".csv")
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_pca", template_params, 
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generatePCACode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  intgroup <- params$intgroup
  transform_type <- params$transform_type  # "vst" or "rlog"
  
  header <- paste0(
    "# Publication-ready PCA Plot\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Transformation: ", transform_type, "\n",
    "# Group of interest: ", intgroup, "\n\n"
  )
  
  libraries <- paste0(
    "# Load required libraries\n",
    "library(DESeq2)\n",
    "library(ggplot2)\n\n"
  )
  
  if (use_existing_objects) {
    # Use objects from Section 0
    data_load <- paste0(
      "# Use data from Section 0 (DESeq2 pipeline)\n",
      "transformed_data <- ", transform_type, "_mat  # Generated in Section 0\n",
      "sample_metadata <- as.data.frame(colData(dds))  # From Section 0\n",
      "cat(\"Using ", transform_type, " data from DESeq2 pipeline\\n\\n\")\n\n"
    )
    
    pca_calc <- paste0(
      "# Perform PCA\n",
      "pca_result <- prcomp(t(transformed_data), scale = FALSE)\n\n",
      "# Calculate variance explained\n",
      "percentVar <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)\n\n",
      "# Create PCA data frame\n",
      "pca_data <- data.frame(\n",
      "  PC1 = pca_result$x[, 1],\n",
      "  PC2 = pca_result$x[, 2],\n",
      "  sample = rownames(pca_result$x)\n",
      ")\n\n",
      "# Add metadata\n",
      "pca_data <- merge(pca_data, sample_metadata, by.x = \"sample\", by.y = \"row.names\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load transformed data\n",
      "transformed_data <- read.csv(\"", transform_type, "_data_", timestamp, ".csv\", row.names = 1)\n",
      "sample_metadata <- read.csv(\"pca_metadata_", timestamp, ".csv\", row.names = 1)\n\n",
      "# Check data structure\n",
      "dim(transformed_data)\n",
      "head(sample_metadata)\n\n"
    )
    
    pca_calc <- paste0(
      "# Perform PCA\n",
      "pca_result <- prcomp(t(transformed_data), scale = FALSE)\n\n",
      "# Calculate variance explained\n",
      "percentVar <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)\n\n",
      "# Create PCA data frame\n",
      "pca_data <- data.frame(\n",
      "  PC1 = pca_result$x[, 1],\n",
      "  PC2 = pca_result$x[, 2],\n",
      "  sample = rownames(pca_result$x)\n",
      ")\n\n",
      "# Add metadata\n",
      "pca_data <- merge(pca_data, sample_metadata, by.x = \"sample\", by.y = \"row.names\")\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes 'dds' DESeqDataSet object exists\n\n",
      "# Perform ", toupper(transform_type), " transformation\n",
      if (transform_type == "vst") "# vst_data <- vst(dds, blind = FALSE)\n" else "# rld_data <- rlog(dds, blind = FALSE)\n",
      "\n"
    )
    
    pca_calc <- paste0(
      "# Perform PCA using DESeq2\n",
      "pca_data <- plotPCA(", if(transform_type == "vst") "vst_data" else "rld_data", 
      ", intgroup = \"", intgroup, "\", returnData = TRUE)\n",
      "percentVar <- round(100 * attr(pca_data, \"percentVar\"), 1)\n\n"
    )
  }
  
  plot_code <- paste0(
    "# Create PCA plot\n",
    "pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = ", intgroup, ")) +\n",
    "  geom_point(size = 4, alpha = 0.8) +\n",
    "  labs(title = \"PCA Plot (", toupper(transform_type), " Transformation)\",\n",
    "       x = paste0(\"PC1: \", percentVar[1], \"% variance\"),\n",
    "       y = paste0(\"PC2: \", percentVar[2], \"% variance\"),\n",
    "       color = \"", intgroup, "\") +\n",
    "  theme_bw(base_size = 12) +\n",
    "  theme(legend.position = \"right\",\n",
    "        plot.title = element_text(hjust = 0.5, face = \"bold\"),\n",
    "        panel.grid.minor = element_blank())\n\n",
    "# Optional: Add sample labels\n",
    "# pca_plot <- pca_plot + geom_text(aes(label = sample), vjust = -1, size = 3)\n\n",
    "# Display plot\n",
    "print(pca_plot)\n\n"
  )
  
  save_code <- paste0(
    "# Save high-resolution plot\n",
    "ggsave(\"pca_plot_", transform_type, ".pdf\", pca_plot, width = 8, height = 6, dpi = 300)\n",
    "ggsave(\"pca_plot_", transform_type, ".png\", pca_plot, width = 8, height = 6, dpi = 300)\n",
    "cat(\"PCA plot saved successfully!\\n\")\n"
  )
  
  full_code <- paste0(header, libraries, data_load, pca_calc, plot_code, save_code)
  
  if (format == "rmd") {
    rmd_header <- createRmdHeader(paste("PCA Plot -", toupper(transform_type)))
    intro_text <- paste0("## Overview\n\nPCA plot using ", toupper(transform_type), " transformed data.\n\n")
    
    full_code <- paste0(
      rmd_header, intro_text,
      "## Load Libraries\n\n", wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Data\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Calculate PCA\n\n", wrapInChunk(pca_calc, "pca-calc"),
      "## Create PCA Plot\n\n", wrapInChunk(plot_code, "pca-plot", message = TRUE),
      "## Save Plot\n\n", wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n", wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# 5. MA Plot Code Generator
generateMAPlotCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Validate required parameters
  if (is.null(params$comparison_name) || length(params$comparison_name) == 0) {
    stop("comparison_name is required for MA plot")
  }
  if (is.null(params$alpha) || length(params$alpha) == 0) {
    stop("alpha is required for MA plot")
  }
  
  # Use provided data_file if available, otherwise generate default name
  data_file <- if (!is.null(params$data_file) && length(params$data_file) > 0) {
    params$data_file
  } else {
    paste0("ma_plot_data_", params$comparison_name, "_", timestamp, ".csv")
  }
  
  # Prepare template parameters
  template_params <- list(
    comparison_name = params$comparison_name,
    alpha = params$alpha,
    ylim = if(!is.null(params$ylim) && length(params$ylim) > 0) params$ylim else 5,
    data_file = data_file
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_maplot", template_params,
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generateMAPlotCode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  comparison_name <- params$comparison_name
  alpha <- params$alpha
  ylim <- params$ylim
  
  header <- paste0(
    "# Publication-ready MA Plot\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Comparison: ", comparison_name, "\n",
    "# Alpha threshold: ", alpha, "\n\n"
  )
  
  libraries <- paste0(
    "# Load required libraries\n",
    "library(ggplot2)\n\n"
  )
  
  if (use_existing_objects) {
    # Use results from Section 0
    data_load <- paste0(
      "# Use results from Section 0 (DESeq2 pipeline)\n",
      "results_data <- results_de  # Generated in Section 0\n",
      "cat(\"Using results from DESeq2 pipeline: \", nrow(results_data), \" genes\\n\\n\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load DESeq2 results\n",
      "results_data <- read.csv(\"ma_plot_data_", comparison_name, "_", timestamp, ".csv\", row.names = 1)\n\n",
      "# Check data structure\n",
      "head(results_data)\n",
      "cat(\"Total genes:\", nrow(results_data), \"\\n\")\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes 'results_data' contains DESeq2 results as a data frame\n",
      "# Example: results_data <- as.data.frame(results(dds, contrast = c('condition', 'treated', 'control')))\n\n"
    )
  }
  
  prep_code <- paste0(
    "# Prepare data for plotting\n",
    "# Remove rows with NA values\n",
    "plot_data <- results_data[!is.na(results_data$padj) & !is.na(results_data$baseMean), ]\n",
    "plot_data <- plot_data[plot_data$baseMean > 0, ]  # Remove zero counts\n\n",
    "# Add significance classification\n",
    "plot_data$significant <- ifelse(plot_data$padj < ", alpha, ", \"Significant\", \"Not Significant\")\n",
    "plot_data$significant[is.na(plot_data$significant)] <- \"Not Significant\"\n\n",
    "# Count significant genes\n",
    "sig_count <- sum(plot_data$significant == \"Significant\")\n",
    "cat(\"Significant genes at alpha =\", ", alpha, ", \":\", sig_count, \"\\n\\n\")\n\n"
  )
  
  plot_code <- paste0(
    "# Create MA plot using ggplot2\n",
    "ma_plot <- ggplot(plot_data, aes(x = log10(baseMean), y = log2FoldChange)) +\n",
    "  geom_point(aes(color = significant), alpha = 0.5, size = 1) +\n",
    "  scale_color_manual(values = c(\"Not Significant\" = \"gray\", \"Significant\" = \"red\"),\n",
    "                     name = paste0(\"padj < \", ", alpha, ")) +\n",
    "  geom_hline(yintercept = 0, color = \"blue\", linetype = \"dashed\") +\n",
    "  coord_cartesian(ylim = c(-", ylim, ", ", ylim, ")) +\n",
    "  labs(title = \"MA Plot: ", comparison_name, "\",\n",
    "       subtitle = paste0(\"Significant genes: \", sig_count),\n",
    "       x = \"log10(Mean Expression)\",\n",
    "       y = \"log2 Fold Change\") +\n",
    "  theme_bw(base_size = 12) +\n",
    "  theme(legend.position = \"bottom\",\n",
    "        plot.title = element_text(hjust = 0.5, face = \"bold\"),\n",
    "        plot.subtitle = element_text(hjust = 0.5),\n",
    "        panel.grid.minor = element_blank())\n\n",
    "# Display plot\n",
    "print(ma_plot)\n\n"
  )
  
  save_code <- paste0(
    "# Save high-resolution plots\n",
    "ggsave(\"ma_plot_", comparison_name, ".pdf\", ma_plot, width = 8, height = 6, dpi = 300)\n",
    "ggsave(\"ma_plot_", comparison_name, ".png\", ma_plot, width = 8, height = 6, dpi = 300)\n",
    "cat(\"MA plots saved successfully!\\n\")\n"
  )
  
  full_code <- paste0(header, libraries, data_load, prep_code, plot_code, save_code)
  
  if (format == "rmd") {
    rmd_header <- createRmdHeader(paste("MA Plot:", comparison_name))
    intro_text <- "## Overview\n\nMA plot showing log2 fold changes versus mean expression.\n\n"
    
    full_code <- paste0(
      rmd_header, intro_text,
      "## Load Libraries\n\n", wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Data\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Prepare Data\n\n", wrapInChunk(prep_code, "prep-data"),
      "## Create MA Plot\n\n", wrapInChunk(plot_code, "ma-plot", message = TRUE),
      "## Save Plot\n\n", wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n", wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# 6. Venn Diagram Code Generator
generateVennCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Prepare template parameters
  comparisons <- params$comparisons
  
  # Validate comparisons
  if (is.null(comparisons) || length(comparisons) == 0) {
    stop("No comparisons specified for Venn diagram")
  }
  
  num_sets <- length(comparisons)
  
  # Generate colors based on number of sets
  if (num_sets == 2) {
    venn_colors <- c("#FF6B6B", "#4ECDC4")
  } else if (num_sets == 3) {
    venn_colors <- c("#FF6B6B", "#4ECDC4", "#95E1D3")
  } else {
    venn_colors <- rainbow(num_sets)
  }
  
  # Generate gene file names for full mode
  gene_files <- sapply(comparisons, function(comp) {
    paste0("venn_genes_", comp, "_", timestamp, ".csv")
  })
  
  template_params <- list(
    comparisons = comparisons,
    num_sets = num_sets,
    venn_colors = venn_colors,
    gene_files = gene_files,
    padj_threshold = if(!is.null(params$padj_threshold)) params$padj_threshold else 0.1,
    fc_threshold = if(!is.null(params$fc_threshold)) params$fc_threshold else 0
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_venn", template_params,
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generateVennCode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  comparisons <- params$comparisons
  num_sets <- length(comparisons)
  
  header <- paste0(
    "# Publication-ready Venn Diagram\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Comparisons: ", paste(comparisons, collapse = ", "), "\n\n"
  )
  
  libraries <- paste0(
    "# Load required libraries\n",
    "library(VennDiagram)\n\n"
  )
  
  if (use_existing_objects) {
    # Use results from Section 0
    data_load <- paste0(
      "# Extract significant genes from results generated in Section 0\n",
      "# results_de was generated in the DESeq2 pipeline\n",
      "significant_genes <- rownames(results_de)[which(results_de$padj < 0.1)]\n",
      "cat(\"Extracted\", length(significant_genes), \"significant genes from results\\n\")\n\n",
      "# Create gene sets list for Venn diagram\n",
      "# Note: This example uses the same gene set for all comparisons\n",
      "# In practice, you would run multiple contrasts to get different gene sets\n",
      "gene_sets <- list(\n",
      paste(sapply(1:num_sets, function(i) {
        paste0("  \"", comparisons[i], "\" = significant_genes")
      }), collapse = ",\n"),
      "\n)\n\n",
      "# For multi-comparison Venn, run additional contrasts:\n",
      "# Example for comparison 2:\n",
      "# results_2 <- results(dds, contrast = c('condition', 'treatment2', 'control'))\n",
      "# gene_sets[[\"", if(num_sets > 1) comparisons[2] else "comparison_2", "\"]] <- rownames(results_2)[which(results_2$padj < 0.1)]\n",
      if(num_sets > 2) {
        paste0("# results_3 <- results(dds, contrast = c('condition', 'treatment3', 'control'))\n",
               "# gene_sets[[\"", comparisons[3], "\"]] <- rownames(results_3)[which(results_3$padj < 0.1)]\n")
      } else {
        ""
      },
      "\ncat(\"Gene sets created for Venn diagram:\\n\")\n",
      "for (i in 1:length(gene_sets)) {\n",
      "  cat(names(gene_sets)[i], \":\", length(gene_sets[[i]]), \"genes\\n\")\n",
      "}\n",
      "cat(\"\\n\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load gene lists for each comparison\n",
      paste(sapply(1:num_sets, function(i) {
        paste0("genes_", i, " <- read.csv(\"venn_genes_", comparisons[i], "_", timestamp, ".csv\")$gene\n")
      }), collapse = ""),
      "\n# Create list of gene sets\n",
      "gene_sets <- list(\n",
      paste(sapply(1:num_sets, function(i) {
        paste0("  \"", comparisons[i], "\" = genes_", i)
      }), collapse = ",\n"),
      "\n)\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes gene lists exist for each comparison\n",
      "# Example:\n",
      "# genes_1 <- rownames(subset(results1, padj < 0.05))\n",
      "# genes_2 <- rownames(subset(results2, padj < 0.05))\n\n",
      "# Create list of gene sets\n",
      "gene_sets <- list(\n",
      paste(sapply(1:num_sets, function(i) {
        paste0("  \"", comparisons[i], "\" = genes_", i)
      }), collapse = ",\n"),
      "\n)\n\n"
    )
  }
  
  # Different Venn diagram code based on number of sets
  if (num_sets == 2) {
    plot_code <- paste0(
      "# Create Venn diagram for 2 sets\n",
      "venn_plot <- venn.diagram(\n",
      "  x = gene_sets,\n",
      "  filename = NULL,\n",
      "  fill = c(\"#FF6B6B\", \"#4ECDC4\"),\n",
      "  alpha = 0.5,\n",
      "  cex = 1.5,\n",
      "  cat.cex = 1.2,\n",
      "  cat.fontface = \"bold\",\n",
      "  margin = 0.1\n",
      ")\n\n",
      "# Display\n",
      "grid::grid.newpage()\n",
      "grid::grid.draw(venn_plot)\n\n"
    )
  } else if (num_sets == 3) {
    plot_code <- paste0(
      "# Create Venn diagram for 3 sets\n",
      "venn_plot <- venn.diagram(\n",
      "  x = gene_sets,\n",
      "  filename = NULL,\n",
      "  fill = c(\"#FF6B6B\", \"#4ECDC4\", \"#95E1D3\"),\n",
      "  alpha = 0.5,\n",
      "  cex = 1.5,\n",
      "  cat.cex = 1.2,\n",
      "  cat.fontface = \"bold\",\n",
      "  margin = 0.1\n",
      ")\n\n",
      "# Display\n",
      "grid::grid.newpage()\n",
      "grid::grid.draw(venn_plot)\n\n"
    )
  } else {
    plot_code <- paste0(
      "# Create Venn diagram for ", num_sets, " sets\n",
      "venn_plot <- venn.diagram(\n",
      "  x = gene_sets,\n",
      "  filename = NULL,\n",
      "  fill = rainbow(", num_sets, "),\n",
      "  alpha = 0.5,\n",
      "  cex = 1.5,\n",
      "  cat.cex = 1.2,\n",
      "  cat.fontface = \"bold\",\n",
      "  margin = 0.1\n",
      ")\n\n",
      "# Display\n",
      "grid::grid.newpage()\n",
      "grid::grid.draw(venn_plot)\n\n"
    )
  }
  
  save_code <- paste0(
    "# Save to file\n",
    "pdf(\"venn_diagram_", timestamp, ".pdf\", width = 8, height = 8)\n",
    "grid::grid.draw(venn_plot)\n",
    "dev.off()\n\n",
    "png(\"venn_diagram_", timestamp, ".png\", width = 2400, height = 2400, res = 300)\n",
    "grid::grid.draw(venn_plot)\n",
    "dev.off()\n\n",
    "# Calculate intersections\n",
    "cat(\"\\nGene counts:\\n\")\n",
    "for (i in 1:length(gene_sets)) {\n",
    "  cat(names(gene_sets)[i], \":\", length(gene_sets[[i]]), \"genes\\n\")\n",
    "}\n\n",
    "cat(\"\\nVenn diagram saved successfully!\\n\")\n"
  )
  
  full_code <- paste0(header, libraries, data_load, plot_code, save_code)
  
  if (format == "rmd") {
    rmd_header <- createRmdHeader("Venn Diagram - Gene Set Comparison")
    intro_text <- "## Overview\n\nVenn diagram comparing significant genes across multiple comparisons.\n\n"
    
    full_code <- paste0(
      rmd_header, intro_text,
      "## Load Libraries\n\n", wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Gene Sets\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Create Venn Diagram\n\n", wrapInChunk(plot_code, "venn-diagram", message = TRUE),
      "## Save Plot\n\n", wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n", wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# 6b. Venn Set Operation Heatmap Code Generator
generateVennSetHeatmapCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Validate required parameters
  if (is.null(params$set_expression) || length(params$set_expression) == 0 || nchar(params$set_expression) == 0) {
    stop("No set expression specified for Venn set heatmap")
  }
  
  if (is.null(params$comparisons) || length(params$comparisons) == 0) {
    stop("No comparisons specified for Venn set heatmap")
  }
  
  # Prepare template parameters
  num_genes <- if(!is.null(params$num_genes) && length(params$num_genes) > 0) params$num_genes else 50
  is_brushed <- if(!is.null(params$is_brushed) && length(params$is_brushed) > 0) params$is_brushed else FALSE
  
  # Prepare sample order for brushed heatmaps
  sample_order_value <- if(!is.null(params$sample_order) && length(params$sample_order) > 0 && is_brushed) {
    paste0("c(", paste0('"', params$sample_order, '"', collapse = ", "), ")")
  } else {
    "NULL"
  }
  
  # Prepare color range for brushed heatmaps
  color_range_value <- if(!is.null(params$color_range) && length(params$color_range) == 2) {
    paste0("c(", params$color_range[1], ", ", params$color_range[2], ")")
  } else {
    "NULL"
  }
  
  template_params <- list(
    set_expression = params$set_expression,
    comparisons = params$comparisons,
    num_genes = num_genes,
    fontsize_row = if(!is.null(num_genes) && length(num_genes) > 0 && num_genes > 50) 6 else 8,
    expression_matrix_file = if(!is.null(params$expression_matrix_file) && length(params$expression_matrix_file) > 0) {
      params$expression_matrix_file
    } else {
      paste0("venn_set_heatmap_data_", timestamp, ".csv")
    },
    is_brushed = is_brushed,
    sample_order = sample_order_value,
    color_range = color_range_value,
    padj_threshold = if(!is.null(params$padj_threshold)) params$padj_threshold else 0.1,
    fc_threshold = if(!is.null(params$fc_threshold)) params$fc_threshold else 0
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_venn_set_heatmap", template_params,
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generateVennSetHeatmapCode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  set_expression <- params$set_expression
  comparisons <- params$comparisons
  num_genes <- params$num_genes
  
  header <- paste0(
    "# Publication-ready Venn Set Operation Heatmap\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Set Expression: ", set_expression, "\n",
    "# Comparisons: ", paste(comparisons, collapse = ", "), "\n\n"
  )
  
  libraries <- paste0(
    "# Load required libraries\n",
    "library(pheatmap)\n",
    "library(RColorBrewer)\n\n"
  )
  
  
  if (use_existing_objects) {
    # Use objects from Section 0 (DESeq2 pipeline)
    data_load <- paste0(
      "# Use gene sets from Venn diagram section above\n",
      "# gene_sets was created in the Venn diagram code\n\n",
      "# Evaluate set expression to get genes for heatmap\n",
      "# Set operations: * = intersection, + = union, - = setdiff\n"
    )
    
    # Generate specific code based on the set expression pattern
    if (grepl("\\*", set_expression) && !grepl("[+-]", set_expression)) {
      # Pure intersection
      letters_in_expr <- unique(unlist(strsplit(gsub("[^A-Z]", "", set_expression), "")))
      data_load <- paste0(data_load,
        "# Intersection of ", paste(letters_in_expr, collapse = " and "), "\n",
        "set_genes <- Reduce(intersect, list(", paste0("gene_sets[[", 1:length(letters_in_expr), "]]", collapse = ", "), "))\n"
      )
    } else if (grepl("\\+", set_expression) && !grepl("[*-]", set_expression)) {
      # Pure union
      letters_in_expr <- unique(unlist(strsplit(gsub("[^A-Z]", "", set_expression), "")))
      data_load <- paste0(data_load,
        "# Union of ", paste(letters_in_expr, collapse = " and "), "\n",
        "set_genes <- Reduce(union, list(", paste0("gene_sets[[", 1:length(letters_in_expr), "]]", collapse = ", "), "))\n"
      )
    } else {
      # Complex expression - provide generic code
      data_load <- paste0(data_load,
        "# For complex expression '", set_expression, "', manually construct the set operation\n",
        "# This is a placeholder - adjust based on your specific expression\n",
        "set_genes <- gene_sets[[1]]  # Modify this line based on your set expression\n"
      )
    }
    
    data_load <- paste0(data_load,
      "cat(\"Set operation genes:\", length(set_genes), \"\\n\\n\")\n\n",
      "# Create expression matrix with log2FoldChange values for each comparison\n",
      "# Note: You need to have run the contrasts for each comparison\n",
      "# For now, using results_de from Section 0 for all comparisons\n",
      "set_expression_matrix <- data.frame(\n",
      paste(sapply(1:length(comparisons), function(i) {
        paste0("  ", LETTERS[i], " = results_de[set_genes, \"log2FoldChange\"]")
      }), collapse = ",\n"),
      "\n)\n",
      "rownames(set_expression_matrix) <- set_genes\n\n",
      "# For multiple comparisons, run additional contrasts and add columns:\n",
      "# Example:\n",
      "# results_2 <- results(dds, contrast = c('condition', 'treatment2', 'control'))\n",
      "# set_expression_matrix$B <- results_2[set_genes, \"log2FoldChange\"]\n\n",
      "cat(\"Expression matrix created:\", nrow(set_expression_matrix), \"genes x\", ncol(set_expression_matrix), \"comparisons\\n\\n\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load gene lists for each comparison\n",
      paste(sapply(1:length(comparisons), function(i) {
        paste0("genes_", LETTERS[i], " <- read.csv(\"venn_genes_", comparisons[i], "_", timestamp, ".csv\")$gene\n")
      }), collapse = ""),
      "\n# Load expression matrix for set operation genes\n",
      "set_expression_matrix <- read.csv(\"venn_set_heatmap_data_", timestamp, ".csv\", row.names = 1)\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes gene lists and expression matrix exist\n",
      "# Example:\n",
      paste(sapply(1:length(comparisons), function(i) {
        paste0("# genes_", LETTERS[i], " <- rownames(subset(results_", i, ", padj < 0.05))\n")
      }), collapse = ""),
      "# set_expression_matrix should contain log2FoldChange values for genes in the set\n\n"
    )
  }
  
  
  # Set evaluation code (only needed for non-use_existing_objects modes)
  set_eval_code <- ""
  
  if (!use_existing_objects) {
    set_eval_code <- paste0(
      "# Evaluate set expression: ", set_expression, "\n",
      "# Set operations: * = intersection, + = union, - = setdiff\n",
      "# Convert expression to R code\n",
      "set_expr_r <- gsub(\"\\\\*\", \",\", \"", set_expression, "\")  \n",
      "set_expr_r <- gsub(\"\\\\+\", \",\", set_expr_r)\n\n",
      
      "# Parse and evaluate the set expression\n",
      "# This is a simplified version - the actual app uses a postfix evaluator\n",
      "# For common operations:\n"
    )
    
    # Generate specific code based on the set expression pattern
    if (grepl("\\*", set_expression) && !grepl("[+-]", set_expression)) {
      # Pure intersection
      letters_in_expr <- unique(unlist(strsplit(gsub("[^A-Z]", "", set_expression), "")))
      set_eval_code <- paste0(set_eval_code,
        "# Intersection of ", paste(letters_in_expr, collapse = " and "), "\n",
        "set_genes <- Reduce(intersect, list(", paste0("genes_", letters_in_expr, collapse = ", "), "))\n",
        "cat(\"Set operation genes:\", length(set_genes), \"\\n\\n\")\n\n"
      )
    } else if (grepl("\\+", set_expression) && !grepl("[*-]", set_expression)) {
      # Pure union
      letters_in_expr <- unique(unlist(strsplit(gsub("[^A-Z]", "", set_expression), "")))
      set_eval_code <- paste0(set_eval_code,
        "# Union of ", paste(letters_in_expr, collapse = " and "), "\n",
        "set_genes <- Reduce(union, list(", paste0("genes_", letters_in_expr, collapse = ", "), "))\n",
        "cat(\"Set operation genes:\", length(set_genes), \"\\n\\n\")\n\n"
      )
    } else {
      # Complex expression - provide generic code
      set_eval_code <- paste0(set_eval_code,
        "# For complex expressions, manually construct the set operation\n",
        "# Example for '", set_expression, "':\n",
        "# set_genes <- ... # Define based on your specific set operation\n",
        "# For now, using the provided expression matrix genes\n",
        "set_genes <- rownames(set_expression_matrix)\n",
        "cat(\"Set operation genes:\", length(set_genes), \"\\n\\n\")\n\n"
      )
    }
  }
  
  
  plot_code <- paste0(
    "# Prepare heatmap matrix\n",
    "# The matrix should have genes as rows and comparisons as columns\n",
    "# Values are log2FoldChange for each gene in each comparison\n",
    "plot_matrix <- set_expression_matrix\n\n",
    
    "# Create column annotations\n",
    "comparison_names <- c(", paste0("\"", comparisons, "\"", collapse = ", "), ")\n",
    "annotation_col <- data.frame(\n",
    "  Comparison = comparison_names,\n",
    "  row.names = colnames(plot_matrix)\n",
    ")\n\n",
    
    "# Create heatmap\n",
    "set_heatmap <- pheatmap(\n",
    "  plot_matrix,\n",
    "  scale = \"none\",  # log2FC values, no additional scaling\n",
    "  clustering_distance_rows = \"euclidean\",\n",
    "  clustering_distance_cols = \"euclidean\",\n",
    "  clustering_method = \"complete\",\n",
    "  color = colorRampPalette(rev(brewer.pal(9, \"RdBu\")))(255),\n",
    "  annotation_col = annotation_col,\n",
    "  show_rownames = TRUE,\n",
    "  show_colnames = TRUE,\n",
    "  fontsize = 10,\n",
    "  fontsize_row = ", if(num_genes > 50) "6" else "8", ",\n",
    "  fontsize_col = 10,\n",
    "  main = \"Venn Set Operation Heatmap: ", set_expression, "\",\n",
    "  border_color = NA\n",
    ")\n\n",
    
    "# Display heatmap\n",
    "print(set_heatmap)\n\n"
  )
  
  save_code <- paste0(
    "# Save high-resolution heatmap\n",
    "pdf(\"venn_set_heatmap_", gsub("[^A-Za-z0-9]", "_", set_expression), "_", timestamp, ".pdf\", \n",
    "    width = 10, height = ", max(8, num_genes * 0.15), ")\n",
    "print(set_heatmap)\n",
    "dev.off()\n\n",
    
    "png(\"venn_set_heatmap_", gsub("[^A-Za-z0-9]", "_", set_expression), "_", timestamp, ".png\", \n",
    "    width = 3000, height = ", max(2400, num_genes * 45), ", res = 300)\n",
    "print(set_heatmap)\n",
    "dev.off()\n\n",
    
    "cat(\"Venn set operation heatmap saved successfully!\\n\")\n"
  )
  
  full_code <- paste0(header, libraries, data_load, set_eval_code, plot_code, save_code)
  
  if (format == "rmd") {
    rmd_header <- createRmdHeader(paste("Venn Set Operation Heatmap:", set_expression))
    intro_text <- paste0("## Overview\n\nHeatmap showing expression patterns for genes in the set operation: **", 
                         set_expression, "**\n\n")
    
    full_code <- paste0(
      rmd_header, intro_text,
      "## Load Libraries\n\n", wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Data\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Evaluate Set Expression\n\n", wrapInChunk(set_eval_code, "set-eval"),
      "## Create Heatmap\n\n", wrapInChunk(plot_code, "heatmap", message = TRUE),
      "## Save Plot\n\n", wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n", wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}

# 7. Distance Heatmap Code Generator
generateDistHeatmapCode <- function(params, mode = "full", use_existing_objects = FALSE) {
  # Validate input parameters
  if (is.null(params) || !is.list(params)) {
    stop("Invalid params: must be a non-null list")
  }
  if (is.null(mode) || length(mode) == 0 || !is.character(mode)) {
    mode <- "full"
  }
  if (is.null(use_existing_objects) || length(use_existing_objects) == 0) {
    use_existing_objects <- FALSE
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Validate and set defaults with explicit checks
  transform_type <- "vst"  # Default
  if (!is.null(params$transform_type) && length(params$transform_type) > 0 && is.character(params$transform_type)) {
    transform_type <- params$transform_type[1]  # Take first element if vector
  }
  
  # Prepare template parameters
  template_params <- list(
    transform_type = transform_type,
    transformed_data_file = paste0(transform_type, "_data_", timestamp, ".csv"),
    metadata_file = paste0("metadata_", timestamp, ".csv")
  )
  
  # Generate code from template
  code <- generateCodeFromTemplate("template_distance_heatmap", template_params, 
                                   mode = mode, use_existing_objects = use_existing_objects)
  
  return(code)
}

# OLD IMPLEMENTATION (kept for reference)
generateDistHeatmapCode_OLD <- function(params, mode = "full", format = "r", use_existing_objects = FALSE) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  transform_type <- params$transform_type
  
  header <- paste0(
    "# Publication-ready Sample Distance Heatmap\n",
    "# Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# Transformation: ", transform_type, "\n\n"
  )
  
  libraries <- paste0(
    "# Load required libraries\n",
    "library(pheatmap)\n",
    "library(RColorBrewer)\n\n"
  )
  
  if (use_existing_objects) {
    # Use objects from Section 0
    data_load <- paste0(
      "# Use data from Section 0 (DESeq2 pipeline)\n",
      "transformed_data <- ", transform_type, "_mat  # Generated in Section 0\n",
      "sample_metadata <- as.data.frame(colData(dds))  # From Section 0\n",
      "cat(\"Using ", transform_type, " data from DESeq2 pipeline\\n\\n\")\n\n"
    )
  } else if (mode == "full") {
    data_load <- paste0(
      "# Load transformed data\n",
      "transformed_data <- read.csv(\"", transform_type, "_data_", timestamp, ".csv\", row.names = 1)\n",
      "sample_metadata <- read.csv(\"metadata_", timestamp, ".csv\", row.names = 1)\n\n"
    )
  } else {
    data_load <- paste0(
      "# Assumes transformed data exists\n",
      "# transformed_data should be a matrix of ", toupper(transform_type), " values\n\n"
    )
  }
  
  plot_code <- paste0(
    "# Calculate sample distances\n",
    "sample_dists <- dist(t(transformed_data))\n",
    "sample_dist_matrix <- as.matrix(sample_dists)\n\n",
    "# Create annotation (if metadata available)\n",
    "if (exists(\"metadata\") && nrow(metadata) > 0) {\n",
    "  annotation_col <- metadata[colnames(sample_dist_matrix), , drop = FALSE]\n",
    "  annotation_col <- annotation_col[, !colnames(annotation_col) %in% c(\"sizeFactor\", \"replaceable\"), drop = FALSE]\n",
    "  annotation_row <- annotation_col\n",
    "} else {\n",
    "  annotation_col <- NA\n",
    "  annotation_row <- NA\n",
    "}\n\n",
    "# Create distance heatmap\n",
    "# Use RdYlBu palette (Red-Yellow-Blue) to match Shiny app colors\n",
    "colors <- colorRampPalette(rev(brewer.pal(11, \"RdYlBu\")))(255)\n",
    "distance_heatmap <- pheatmap(\n",
    "  sample_dist_matrix,\n",
    "  clustering_distance_rows = sample_dists,\n",
    "  clustering_distance_cols = sample_dists,\n",
    "  col = colors,\n",
    "  annotation_col = annotation_col,\n",
    "  annotation_row = annotation_row,\n",
    "  main = \"Sample-to-Sample Distance Heatmap\",\n",
    "  fontsize = 10\n",
    ")\n\n",
    "# Display\n",
    "print(distance_heatmap)\n\n"
  )
  
  save_code <- paste0(
    "# Save high-resolution plot\n",
    "pdf(\"distance_heatmap_", transform_type, ".pdf\", width = 10, height = 10)\n",
    "print(distance_heatmap)\n",
    "dev.off()\n\n",
    "png(\"distance_heatmap_", transform_type, ".png\", width = 3000, height = 3000, res = 300)\n",
    "print(distance_heatmap)\n",
    "dev.off()\n\n",
    "cat(\"Distance heatmap saved successfully!\\n\")\n"
  )
  
  full_code <- paste0(header, libraries, data_load, plot_code, save_code)
  
  if (format == "rmd") {
    rmd_header <- createRmdHeader("Sample Distance Heatmap")
    intro_text <- "## Overview\n\nHeatmap showing pairwise distances between samples.\n\n"
    
    full_code <- paste0(
      rmd_header, intro_text,
      "## Load Libraries\n\n", wrapInChunk(libraries, "libraries"),
      if(mode == "full") paste0("## Load Data\n\n", wrapInChunk(data_load, "load-data")) else "",
      "## Calculate Distances and Create Heatmap\n\n", wrapInChunk(plot_code, "dist-heatmap", message = TRUE),
      "## Save Plot\n\n", wrapInChunk(save_code, "save-plot", message = TRUE),
      "## Session Information\n\n", wrapInChunk("sessionInfo()", "session-info")
    )
  }
  
  return(full_code)
}
