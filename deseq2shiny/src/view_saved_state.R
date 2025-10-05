# View and Interact with Saved DESeq2 Shiny State Files in R
# This script shows how to load and explore saved state files outside the Shiny app

# Function to load and explore a saved state file
load_and_explore_state <- function(state_file_path) {
    cat("Loading state file:", state_file_path, "\n")
    cat("=" %R% 50, "\n\n")
    
    # Load the state file
    load(state_file_path)
    
    # Check if state_object exists
    if (!exists("state_object")) {
        stop("No 'state_object' found in the file. This may not be a valid DESeq2 Shiny state file.")
    }
    
    # Display basic information
    cat("📊 STATE FILE OVERVIEW\n")
    cat("=" %R% 30, "\n")
    cat("Save timestamp:", format(state_object$save_timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("App version:", state_object$app_version, "\n")
    cat("File size:", format(object.size(state_object), units = "MB"), "\n\n")
    
    # Show available components
    cat("🗂️  AVAILABLE COMPONENTS\n")
    cat("=" %R% 30, "\n")
    components <- names(state_object)
    for (comp in components) {
        if (!is.null(state_object[[comp]])) {
            cat("✓", comp, "\n")
        } else {
            cat("✗", comp, "(NULL)\n")
        }
    }
    cat("\n")
    
    # Data overview
    if (!is.null(state_object$dataCounts)) {
        cat("📈 COUNT DATA\n")
        cat("=" %R% 20, "\n")
        cat("Genes:", nrow(state_object$dataCounts), "\n")
        cat("Samples:", ncol(state_object$dataCounts), "\n")
        cat("Sample names:", paste(colnames(state_object$dataCounts)[1:min(5, ncol(state_object$dataCounts))], collapse = ", "))
        if (ncol(state_object$dataCounts) > 5) cat(" ...")
        cat("\n\n")
    }
    
    # Sample information
    if (!is.null(state_object$DF)) {
        cat("🧬 SAMPLE METADATA\n")
        cat("=" %R% 25, "\n")
        cat("Samples:", nrow(state_object$DF), "\n")
        cat("Conditions:", paste(colnames(state_object$DF), collapse = ", "), "\n")
        print(head(state_object$DF, 3))
        cat("\n")
    }
    
    # DESeq2 results
    if (!is.null(state_object$vsResults)) {
        cat("📊 DIFFERENTIAL EXPRESSION RESULTS\n")
        cat("=" %R% 40, "\n")
        cat("Total genes:", nrow(state_object$vsResults), "\n")
        sig_genes <- sum(state_object$vsResults$padj < 0.05, na.rm = TRUE)
        cat("Significant genes (padj < 0.05):", sig_genes, "\n")
        cat("Log2 fold change range:", 
            round(min(state_object$vsResults$log2FoldChange, na.rm = TRUE), 2), "to",
            round(max(state_object$vsResults$log2FoldChange, na.rm = TRUE), 2), "\n\n")
    }
    
    # Plot-specific data
    if (!is.null(state_object$filelist_file_list)) {
        cat("🌋 VOLCANO PLOT DATASETS\n")
        cat("=" %R% 30, "\n")
        cat("Number of saved datasets:", length(state_object$filelist_file_list), "\n")
        cat("Dataset names:", paste(names(state_object$filelist_file_list), collapse = ", "), "\n\n")
    }
    
    if (!is.null(state_object$custom_colors_colors)) {
        cat("🎨 CUSTOM COLORS\n")
        cat("=" %R% 20, "\n")
        cat("Color schemes saved for boxplots\n")
        print(state_object$custom_colors_colors)
        cat("\n")
    }
    
    if (!is.null(state_object$selected_matrix_matrix)) {
        cat("🔲 VENN DIAGRAM SELECTIONS\n")
        cat("=" %R% 30, "\n")
        cat("Selected genes from Venn intersections:", nrow(state_object$selected_matrix_matrix), "\n")
        cat("Samples:", ncol(state_object$selected_matrix_matrix), "\n\n")
    }
    
    # Saved plot parameters
    if (!is.null(state_object$saved_inputs)) {
        cat("⚙️  SAVED PLOT PARAMETERS\n")
        cat("=" %R% 30, "\n")
        inputs <- state_object$saved_inputs
        
        # Volcano plot parameters
        if (!is.null(inputs$volcano_significance_threshold)) {
            cat("🌋 Volcano Plot Settings:\n")
            cat("  - Significance threshold:", inputs$volcano_significance_threshold, "\n")
            cat("  - Log fold change threshold:", inputs$volcano_log_fold_change_threshold, "\n")
            cat("  - Selected dataset:", inputs$select_avo_de_file, "\n")
        }
        
        # Venn diagram parameters
        if (!is.null(inputs$venn_significance_threshold)) {
            cat("🔲 Venn Diagram Settings:\n")
            cat("  - Significance threshold:", inputs$venn_significance_threshold, "\n")
            cat("  - Log fold change threshold:", inputs$venn_log_fold_change_threshold, "\n")
            if (!is.null(inputs$select_avo_de_venn_files)) {
                cat("  - Selected files:", paste(inputs$select_avo_de_venn_files, collapse = ", "), "\n")
            }
        }
        
        # Boxplot parameters
        if (!is.null(inputs$sel_gene)) {
            cat("📊 Boxplot Settings:\n")
            cat("  - Selected gene:", inputs$sel_gene, "\n")
            cat("  - Fill variable:", inputs$boxplotFill, "\n")
        }
        cat("\n")
    }
    
    return(state_object)
}

# Function to extract specific data from state
extract_data <- function(state_object, data_type = "counts") {
    switch(data_type,
           "counts" = state_object$dataCounts,
           "metadata" = state_object$DF,
           "results" = state_object$vsResults,
           "vst" = state_object$vstMat,
           "rlog" = state_object$rlogMat,
           "dds" = state_object$dds,
           "volcano_data" = state_object$filelist_file_list,
           "venn_selection" = state_object$selected_matrix_matrix,
           "colors" = state_object$custom_colors_colors,
           stop("Unknown data type. Options: counts, metadata, results, vst, rlog, dds, volcano_data, venn_selection, colors")
    )
}

# Function to create basic plots from saved state
create_quick_plots <- function(state_object) {
    library(ggplot2)
    library(DESeq2)
    
    # PCA plot if VST data is available
    if (!is.null(state_object$vstMat) && !is.null(state_object$DF)) {
        cat("Creating PCA plot...\n")
        
        # Prepare data for PCA
        vst_data <- state_object$vstMat
        metadata <- state_object$DF
        
        # Perform PCA
        pca_result <- prcomp(t(vst_data), scale = FALSE)
        
        # Create PCA plot
        pca_df <- data.frame(
            PC1 = pca_result$x[,1],
            PC2 = pca_result$x[,2],
            Sample = rownames(pca_result$x)
        )
        
        # Add metadata
        pca_df <- merge(pca_df, metadata, by.x = "Sample", by.y = "row.names", all.x = TRUE)
        
        # Plot
        p1 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = get(colnames(metadata)[1]))) +
            geom_point(size = 3) +
            labs(title = "PCA Plot from Saved State",
                 color = colnames(metadata)[1]) +
            theme_minimal()
        
        print(p1)
    }
    
    # Volcano plot if results are available
    if (!is.null(state_object$vsResults)) {
        cat("Creating volcano plot...\n")
        
        results <- state_object$vsResults
        results$significant <- results$padj < 0.05 & abs(results$log2FoldChange) > 1
        
        p2 <- ggplot(as.data.frame(results), aes(x = log2FoldChange, y = -log10(padj))) +
            geom_point(aes(color = significant), alpha = 0.6) +
            scale_color_manual(values = c("grey", "red")) +
            labs(title = "Volcano Plot from Saved State",
                 x = "Log2 Fold Change",
                 y = "-log10(adjusted p-value)") +
            theme_minimal()
        
        print(p2)
    }
}

# Example usage function
example_usage <- function() {
    cat("EXAMPLE: How to use these functions\n")
    cat("="  %R% 40, "\n\n")
    
    cat("# 1. Load and explore a state file:\n")
    cat('state <- load_and_explore_state("deseq2shiny_state_20240924_143052.RData")\n\n')
    
    cat("# 2. Extract specific data:\n")
    cat('counts <- extract_data(state, "counts")\n')
    cat('results <- extract_data(state, "results")\n')
    cat('metadata <- extract_data(state, "metadata")\n\n')
    
    cat("# 3. Access volcano plot datasets:\n")
    cat('volcano_datasets <- extract_data(state, "volcano_data")\n')
    cat('dataset1 <- read.csv(volcano_datasets[["Dataset1"]])\n\n')
    
    cat("# 4. Create quick plots:\n")
    cat('create_quick_plots(state)\n\n')
    
    cat("# 5. Work with DESeq2 objects directly:\n")
    cat('dds <- extract_data(state, "dds")\n')
    cat('plotCounts(dds, gene = "GENE1", intgroup = "condition")\n\n')
    
    cat("# 6. Access custom colors and selections:\n")
    cat('colors <- extract_data(state, "colors")\n')
    cat('venn_genes <- extract_data(state, "venn_selection")\n\n')
}

# String repeat operator
`%R%` <- function(string, times) {
    paste(rep(string, times), collapse = "")
}

# Print example usage
example_usage()

cat("💡 TIP: Run this script in R/RStudio to interactively explore your saved DESeq2 Shiny states!\n")
