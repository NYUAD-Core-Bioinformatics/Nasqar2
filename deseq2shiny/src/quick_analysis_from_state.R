# Quick Analysis Script for DESeq2 Shiny State Files
# This script provides ready-to-use functions for analyzing saved state data

# Load required libraries
required_packages <- c("DESeq2", "ggplot2", "pheatmap", "dplyr", "VennDiagram")
for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        cat("Installing", pkg, "...\n")
        if (pkg %in% c("DESeq2")) {
            BiocManager::install(pkg)
        } else {
            install.packages(pkg)
        }
        library(pkg, character.only = TRUE)
    }
}

# Main function to load state and provide analysis options
analyze_saved_state <- function(state_file_path) {
    # Load the state
    load(state_file_path)
    
    if (!exists("state_object")) {
        stop("Invalid state file")
    }
    
    cat("🔬 INTERACTIVE ANALYSIS MENU\n")
    cat("="  %R% 35, "\n")
    cat("Loaded state from:", format(state_object$save_timestamp, "%Y-%m-%d %H:%M:%S"), "\n\n")
    
    # Create analysis object
    analysis <- list(
        state = state_object,
        counts = state_object$dataCounts,
        metadata = state_object$DF,
        dds = state_object$dds,
        results = state_object$vsResults,
        vst = state_object$vstMat,
        rlog = state_object$rlogMat
    )
    
    return(analysis)
}

# 1. Recreate volcano plots from saved data
recreate_volcano_plot <- function(analysis, dataset_name = NULL, 
                                 padj_cutoff = 0.05, lfc_cutoff = 1) {
    
    if (is.null(analysis$state$filelist_file_list)) {
        # Use main results if no volcano datasets
        if (is.null(analysis$results)) {
            stop("No volcano plot data or results available")
        }
        
        results_df <- as.data.frame(analysis$results)
        title <- "Main DESeq2 Results"
        
    } else {
        # Use saved volcano datasets
        datasets <- analysis$state$filelist_file_list
        
        if (is.null(dataset_name)) {
            dataset_name <- names(datasets)[1]
            cat("Using first dataset:", dataset_name, "\n")
        }
        
        if (!dataset_name %in% names(datasets)) {
            stop("Dataset not found. Available:", paste(names(datasets), collapse = ", "))
        }
        
        # Read the dataset
        results_df <- read.csv(datasets[[dataset_name]])
        title <- paste("Volcano Plot -", dataset_name)
    }
    
    # Prepare data
    results_df$significant <- results_df$padj < padj_cutoff & 
                             abs(results_df$log2FoldChange) > lfc_cutoff
    
    results_df$regulation <- ifelse(results_df$log2FoldChange > lfc_cutoff & 
                                   results_df$padj < padj_cutoff, "Up",
                            ifelse(results_df$log2FoldChange < -lfc_cutoff & 
                                   results_df$padj < padj_cutoff, "Down", "Not Sig"))
    
    # Create volcano plot
    p <- ggplot(results_df, aes(x = log2FoldChange, y = -log10(padj))) +
        geom_point(aes(color = regulation), alpha = 0.6, size = 1) +
        scale_color_manual(values = c("Up" = "red", "Down" = "blue", "Not Sig" = "grey")) +
        geom_vline(xintercept = c(-lfc_cutoff, lfc_cutoff), linetype = "dashed", alpha = 0.5) +
        geom_hline(yintercept = -log10(padj_cutoff), linetype = "dashed", alpha = 0.5) +
        labs(title = title,
             x = "Log2 Fold Change",
             y = "-log10(adjusted p-value)",
             color = "Regulation") +
        theme_minimal() +
        theme(legend.position = "bottom")
    
    # Add summary
    up_genes <- sum(results_df$regulation == "Up", na.rm = TRUE)
    down_genes <- sum(results_df$regulation == "Down", na.rm = TRUE)
    
    cat("📊 VOLCANO PLOT SUMMARY\n")
    cat("Up-regulated genes:", up_genes, "\n")
    cat("Down-regulated genes:", down_genes, "\n")
    cat("Total significant:", up_genes + down_genes, "\n\n")
    
    print(p)
    return(results_df)
}

# 2. Recreate heatmaps with saved gene selections
recreate_heatmap <- function(analysis, gene_list = NULL, top_n = 50) {
    
    if (is.null(analysis$vst)) {
        stop("No VST data available for heatmap")
    }
    
    vst_data <- analysis$vst
    metadata <- analysis$metadata
    
    # Determine genes to plot
    if (is.null(gene_list)) {
        if (!is.null(analysis$results)) {
            # Use top variable genes from results
            results_df <- as.data.frame(analysis$results)
            results_df <- results_df[!is.na(results_df$padj), ]
            top_genes <- rownames(results_df[order(results_df$padj)[1:top_n], ])
            cat("Using top", length(top_genes), "significant genes\n")
        } else {
            # Use most variable genes
            gene_vars <- apply(vst_data, 1, var)
            top_genes <- names(sort(gene_vars, decreasing = TRUE)[1:top_n])
            cat("Using top", length(top_genes), "variable genes\n")
        }
    } else {
        top_genes <- intersect(gene_list, rownames(vst_data))
        cat("Using", length(top_genes), "provided genes\n")
    }
    
    # Create heatmap
    heatmap_data <- vst_data[top_genes, ]
    
    # Annotation
    if (!is.null(metadata)) {
        annotation_df <- metadata
        rownames(annotation_df) <- rownames(metadata)
    } else {
        annotation_df <- NULL
    }
    
    # Plot
    pheatmap(heatmap_data,
             annotation_col = annotation_df,
             scale = "row",
             clustering_distance_rows = "correlation",
             clustering_distance_cols = "correlation",
             main = paste("Heatmap -", length(top_genes), "genes"))
    
    return(top_genes)
}

# 3. Recreate boxplots with saved selections
recreate_boxplot <- function(analysis, gene_name, group_by = NULL) {
    
    if (is.null(analysis$dds)) {
        stop("No DESeq2 object available for boxplot")
    }
    
    if (is.null(group_by) && !is.null(analysis$metadata)) {
        group_by <- colnames(analysis$metadata)[1]
        cat("Using grouping variable:", group_by, "\n")
    }
    
    # Get normalized counts
    normalized_counts <- counts(analysis$dds, normalized = TRUE)
    
    if (!gene_name %in% rownames(normalized_counts)) {
        stop("Gene not found:", gene_name)
    }
    
    # Prepare data
    plot_data <- data.frame(
        count = normalized_counts[gene_name, ],
        sample = colnames(normalized_counts)
    )
    
    # Add metadata
    if (!is.null(analysis$metadata) && !is.null(group_by)) {
        plot_data[[group_by]] <- analysis$metadata[[group_by]][match(plot_data$sample, rownames(analysis$metadata))]
    }
    
    # Create boxplot
    if (!is.null(group_by)) {
        p <- ggplot(plot_data, aes_string(x = group_by, y = "count", fill = group_by)) +
            geom_boxplot(alpha = 0.7) +
            geom_jitter(width = 0.2, alpha = 0.8)
        
        # Use saved colors if available
        if (!is.null(analysis$state$custom_colors_colors)) {
            colors <- unlist(analysis$state$custom_colors_colors)
            if (length(colors) > 0) {
                p <- p + scale_fill_manual(values = colors)
            }
        }
    } else {
        p <- ggplot(plot_data, aes(x = sample, y = count)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    p <- p + 
        labs(title = paste("Expression of", gene_name),
             y = "Normalized Count") +
        theme_minimal()
    
    print(p)
    return(plot_data)
}

# 4. Explore Venn diagram intersections
explore_venn_intersections <- function(analysis) {
    
    if (is.null(analysis$state$selected_matrix_matrix)) {
        cat("No Venn diagram intersections saved\n")
        return(NULL)
    }
    
    venn_matrix <- analysis$state$selected_matrix_matrix
    
    cat("🔲 VENN DIAGRAM INTERSECTIONS\n")
    cat("=" %R% 35, "\n")
    cat("Selected genes:", nrow(venn_matrix), "\n")
    cat("Samples:", ncol(venn_matrix), "\n\n")
    
    # Show top genes
    cat("Top genes from intersection:\n")
    print(head(rownames(venn_matrix), 10))
    
    # Create heatmap of intersection
    if (nrow(venn_matrix) > 1) {
        pheatmap(venn_matrix,
                 scale = "row",
                 main = "Venn Intersection Genes",
                 show_rownames = nrow(venn_matrix) < 50)
    }
    
    return(rownames(venn_matrix))
}

# 5. Get summary statistics
get_analysis_summary <- function(analysis) {
    
    cat("📈 ANALYSIS SUMMARY\n")
    cat("=" %R% 25, "\n\n")
    
    # Basic data info
    if (!is.null(analysis$counts)) {
        cat("📊 Count Data:\n")
        cat("  Genes:", nrow(analysis$counts), "\n")
        cat("  Samples:", ncol(analysis$counts), "\n")
        cat("  Total counts:", format(sum(analysis$counts), big.mark = ","), "\n\n")
    }
    
    # Results summary
    if (!is.null(analysis$results)) {
        cat("🔬 Differential Expression:\n")
        sig_001 <- sum(analysis$results$padj < 0.01, na.rm = TRUE)
        sig_005 <- sum(analysis$results$padj < 0.05, na.rm = TRUE)
        sig_01 <- sum(analysis$results$padj < 0.1, na.rm = TRUE)
        
        cat("  Significant (padj < 0.01):", sig_001, "\n")
        cat("  Significant (padj < 0.05):", sig_005, "\n")
        cat("  Significant (padj < 0.1):", sig_01, "\n\n")
    }
    
    # Plot data info
    if (!is.null(analysis$state$filelist_file_list)) {
        cat("🌋 Volcano Plot Datasets:\n")
        for (name in names(analysis$state$filelist_file_list)) {
            cat("  -", name, "\n")
        }
        cat("\n")
    }
    
    # Saved plot parameters
    if (!is.null(analysis$state$saved_inputs)) {
        cat("⚙️  Saved Plot Settings:\n")
        inputs <- analysis$state$saved_inputs
        if (!is.null(inputs$volcano_significance_threshold)) {
            cat("  🌋 Volcano - Significance threshold:", inputs$volcano_significance_threshold, "\n")
        }
        if (!is.null(inputs$select_avo_de_file)) {
            cat("  🌋 Volcano - Selected dataset:", inputs$select_avo_de_file, "\n")
        }
        if (!is.null(inputs$sig_genes_selection)) {
            gene_types <- c("1" = "All significant", "2" = "Up regulated", "3" = "Down regulated")
            cat("  🌋 Volcano - Gene selection:", gene_types[inputs$sig_genes_selection], "\n")
        }
        
        # Venn diagram parameters
        if (!is.null(inputs$select_avo_de_venn_files)) {
            cat("  🔲 Venn - Selected datasets:", paste(inputs$select_avo_de_venn_files, collapse = ", "), "\n")
        }
        if (!is.null(inputs$venn_sig_genes_selection)) {
            venn_types <- c("1" = "All significant", "2" = "Up regulated", "3" = "Down regulated")
            cat("  🔲 Venn - Gene selection:", venn_types[inputs$venn_sig_genes_selection], "\n")
        }
        if (!is.null(inputs$select_expression)) {
            cat("  🔲 Venn - Set expression:", inputs$select_expression, "\n")
        }
        
        # Boxplot parameters
        if (!is.null(inputs$sel_gene)) {
            cat("  📊 Boxplot - Selected genes:", paste(inputs$sel_gene, collapse = ", "), "\n")
        }
        if (!is.null(inputs$boxplotFill)) {
            cat("  📊 Boxplot - Fill grouping:", inputs$boxplotFill, "\n")
        }
        if (!is.null(inputs$levelColor)) {
            cat("  📊 Boxplot - Custom color:", inputs$levelColor, "\n")
        }
        
        # Heatmap parameters  
        if (!is.null(inputs$numGenes)) {
            cat("  🔥 Heatmap - Number of genes:", inputs$numGenes, "\n")
        }
        if (!is.null(inputs$subsetGenes) && inputs$subsetGenes) {
            cat("  🔥 Heatmap - Using gene subset:", inputs$subsetGenes, "\n")
            if (!is.null(inputs$listPasteGenes) && inputs$listPasteGenes != "") {
                gene_count <- length(unlist(strsplit(inputs$listPasteGenes, "[,\\s]+")))
                cat("  🔥 Heatmap - Custom genes provided:", gene_count, "genes\n")
            }
        }
        cat("\n")
    }
}

# Helper function
`%R%` <- function(string, times) {
    paste(rep(string, times), collapse = "")
}

# Example workflow
example_workflow <- function(state_file) {
    cat("🚀 EXAMPLE WORKFLOW\n")
    cat("=" %R% 25, "\n\n")
    
    cat("# Load your saved state\n")
    cat('analysis <- analyze_saved_state("', state_file, '")\n\n', sep = "")
    
    cat("# Get overview\n")
    cat("get_analysis_summary(analysis)\n\n")
    
    cat("# Recreate volcano plot\n")
    cat("volcano_data <- recreate_volcano_plot(analysis)\n\n")
    
    cat("# Create heatmap with top 30 genes\n")
    cat("heatmap_genes <- recreate_heatmap(analysis, top_n = 30)\n\n")
    
    cat("# Create boxplot for specific gene\n")
    cat('recreate_boxplot(analysis, "GENE_NAME")\n\n')
    
    cat("# Explore Venn intersections\n")
    cat("venn_genes <- explore_venn_intersections(analysis)\n\n")
}

cat("✅ Loaded DESeq2 Shiny State Analysis Functions!\n\n")
cat("📝 QUICK START:\n")
cat('analysis <- analyze_saved_state("your_state_file.RData")\n')
cat("get_analysis_summary(analysis)\n\n")

cat("🔧 AVAILABLE FUNCTIONS:\n")
cat("- analyze_saved_state()     # Load state file\n")
cat("- get_analysis_summary()    # Overview of data\n")
cat("- recreate_volcano_plot()   # Recreate volcano plots\n")
cat("- recreate_heatmap()        # Recreate heatmaps\n")
cat("- recreate_boxplot()        # Recreate boxplots\n")
cat("- explore_venn_intersections() # View Venn selections\n\n")
