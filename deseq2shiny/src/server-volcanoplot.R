output$select_ui <- renderUI({
    selectInput("select_avo_de_file",
        label = h5("Select DE data"), selected = NULL,
        choices = c(c("Select data"), names(filelist$file_list))
    )
})

avo_data <- reactive({
    print("avao_dataaaa")
    req(input$select_avo_de_file)

    print("avao_dataaaa2")




    req(input$select_avo_de_file != "Select data")

    df <- read.csv(filelist$file_list[[input$select_avo_de_file]])



    df <- na.omit(df)
    zero_padj_row <- df$padj < 1e-310
    print(sum(zero_padj_row))

    if (sum(zero_padj_row) > 0) {
        df[zero_padj_row, ]$padj <- 1e-310
    }


    withProgress(message = "Making plot", value = 0, {
        incProgress(0.3, detail = paste("fetching gene sysmbols"))


        # df <- df[(df$pvalue < 1 / 10^as.numeric(input$significance_threshold)) & abs(df$log2FoldChange) > as.numeric(input$log_fold_change_threshold), ]

        df$minus_10_log_padj <- -log10(df$padj)

        colnames(df)[1] <- "gene.id"

        if (input$gene_alias == "included") {
            genes <- myValues$genenames[df$gene.id, ]
            gene.name <- genes
            df <- cbind(df, gene.name)
        }
    })



    return(df)
})


all_genes <- reactive({
    df <- avo_data()
    print(as.numeric(input$log_fold_change_threshold))
    
    # Use either the slider threshold or direct padj threshold based on user selection
    if (input$volcano_threshold_type == "slider") {
        df <- df[(df$padj < 1 / 10^as.numeric(input$significance_threshold)) & abs(df$log2FoldChange) > as.numeric(input$log_fold_change_threshold), ]
    } else {
        df <- df[(df$padj < as.numeric(input$volcano_direct_padj)) & abs(df$log2FoldChange) > as.numeric(input$log_fold_change_threshold), ]
    }
    
    # print(df)
    return(df)
})

high_sig_genes <- reactive({
    df <- all_genes()
    # print(df)
    return(df[df$log2FoldChange > as.numeric(input$log_fold_change_threshold), ])
})

low_sig_genes <- reactive({
    df <- all_genes()
    return(df[df$log2FoldChange < as.numeric(input$log_fold_change_threshold), ])
})


sig_genes <- reactive({
    if (input$sig_genes_selection == "1") {
        print("All genes")
        return(all_genes())
    }
    if (input$sig_genes_selection == "2") {
        print("High genes")
        return(high_sig_genes())
    }

    print("Low genes")
    return(low_sig_genes())
})


output$sig_gene_table <- renderDataTable(
    {
        fileUrl <- UUIDgenerate()
        fileUrl <- paste0(tempdir(), "/", fileUrl, ".csv")
        genes_df <- sig_genes()

        if (input$gene_alias == "included") {
            gene_names <- genes_df$gene.name
            if (input$volcano_sel_gene_type == "gene.id") {
                gene_names <- genes_df$gene.id
            }
            colnames <- colnames(genes_df)
            col_to_move <-  which(colnames == 'gene.name')
            index <- c(1:length(colnames))
            index <- index[-col_to_move]

            index <- c(index[1], col_to_move, index[2:length(index)])


            # Reorder the columns
            genes_df <- genes_df[index]
        } else {
            gene_names <- genes_df$gene.id
        }



        # write.csv(gene_names, file = fileUrl, row.names = FALSE)


        updateTextAreaInput(session, "volcano_gene_list", value = paste(gene_names, collapse = input$volcano_input_genes_sep))

        # output[["volcano_gene_list_ouput"]] <- renderUI({
        #           textAreaInput("volcano_gene_list", 'GeneList', rows = 3)
        # })

        # output[["enrichGo_volcano"]] <-   renderUI({

        #       tags$div(class = "BoxArea7", style = "text-align: center;",
        #                     p(strong("ClusterProfShinyORA")),
        #                     a("goEnrich", href=paste0("/ClusterProfShinyORA?gene_names=", encryptUrlParam(fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
        #                     )
        # })


        return(genes_df)
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

output$sig_genes_header <- renderText({
    req(input$select_avo_de_file != "Select data")
    # print(length(all_genes()))
    if (input$sig_genes_selection == "1") {
        return(paste0("All significant genes: ", nrow(all_genes())))
    }
    if (input$sig_genes_selection == "2") {
        return(paste0("Up regulated genes: ", nrow(high_sig_genes())))
    }

    print("Low genes")
    return(paste0("Down regulated genes: ", nrow(low_sig_genes())))
})



output$curve_plot <- renderPlot(
    {
        print("curve_plot")

        df <- avo_data()

        print(df)

        # Determine the padj cutoff based on user's selection
        pCutoff <- if (input$volcano_threshold_type == "slider") {
            1 / 10^as.numeric(input$significance_threshold)
        } else {
            as.numeric(input$volcano_direct_padj)
        }
        
        # Get log2FC threshold
        fcCutoff <- as.numeric(input$log_fold_change_threshold)
        
        # Create custom colors: RED (up), BLUE (down), GREY (not sig)
        # Same color scheme as exported volcano plots
        keyvals <- ifelse(
            df$padj < pCutoff & abs(df$log2FoldChange) > fcCutoff,
            ifelse(df$log2FoldChange > 0, 'red2', 'royalblue'),
            'grey50'
        )
        keyvals[is.na(keyvals)] <- 'grey50'
        names(keyvals)[keyvals == 'red2'] <- 'Upregulated'
        names(keyvals)[keyvals == 'royalblue'] <- 'Downregulated'
        names(keyvals)[keyvals == 'grey50'] <- 'Not significant'
        
        # Count significant genes for subtitle
        sig_up <- sum(df$padj < pCutoff & df$log2FoldChange > fcCutoff, na.rm = TRUE)
        sig_down <- sum(df$padj < pCutoff & df$log2FoldChange < -fcCutoff, na.rm = TRUE)
        
        # Handle genes of interest for labeling
        selectLab_genes <- NULL
        label_mode <- "auto"
        
        if (!is.null(input$volcano_genes_of_interest) && nchar(trimws(input$volcano_genes_of_interest)) > 0) {
            # User specified genes of interest
            genes_vec <- strsplit(input$volcano_genes_of_interest, ",\\s*")[[1]]
            genes_vec <- trimws(genes_vec)
            genes_vec <- genes_vec[nchar(genes_vec) > 0]
            
            if (length(genes_vec) > 0) {
                selectLab_genes <- genes_vec
                label_mode <- "custom"
            }
        }
        
        subtitle_text <- if (label_mode == "custom") {
            paste0("Up: ", sig_up, " | Down: ", sig_down, " genes (labeling ", length(selectLab_genes), " genes of interest)")
        } else {
            paste0("Up: ", sig_up, " | Down: ", sig_down, " genes")
        }

        if (input$gene_alias == "included") {
            return(EnhancedVolcano(df,
                title = paste0("DESeq2 results of ", input$select_avo_de_file),
                subtitle = subtitle_text,
                lab = df$gene.name,
                x = "log2FoldChange",
                y = "padj",
                selectLab = selectLab_genes,  # NULL for auto-select, or custom genes
                pCutoff = pCutoff,
                FCcutoff = fcCutoff,
                colCustom = keyvals,
                colAlpha = 0.5,
                pointSize = 2.0,
                labSize = 4.0,
                boxedLabels = TRUE,
                drawConnectors = TRUE,
                widthConnectors = 0.75,
                colConnectors = 'black',
                max.overlaps = 20,
                legendPosition = 'right',
                legendLabSize = 12,
                legendIconSize = 4.0
            ))
        } else {
            return(EnhancedVolcano(df,
                title = paste0("DESeq2 results of ", input$select_avo_de_file),
                subtitle = subtitle_text,
                lab = df$gene.id,
                x = "log2FoldChange",
                y = "padj",
                selectLab = selectLab_genes,  # NULL for auto-select, or custom genes
                pCutoff = pCutoff,
                FCcutoff = fcCutoff,
                colCustom = keyvals,
                colAlpha = 0.5,
                pointSize = 2.0,
                labSize = 4.0,
                boxedLabels = TRUE,
                drawConnectors = TRUE,
                widthConnectors = 0.75,
                colConnectors = 'black',
                max.overlaps = 20,
                legendPosition = 'right',
                legendLabSize = 12,
                legendIconSize = 4.0
            ))
        }
    },
    height = 600
)

# Download volcano plot as image with custom size and format
output$download_volcano_plot <- downloadHandler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- input$volcano_plot_format
        paste0("volcano_plot_", input$select_avo_de_file, "_", timestamp, ".", ext)
    },
    content = function(file) {
        req(input$select_avo_de_file != "Select data")
        
        # Get plot parameters
        df <- avo_data()
        plot_width <- input$volcano_plot_width
        plot_height <- input$volcano_plot_height
        plot_format <- input$volcano_plot_format
        plot_dpi <- input$volcano_plot_dpi
        
        # Determine the padj cutoff based on user's selection
        pCutoff <- if (input$volcano_threshold_type == "slider") {
            1 / 10^as.numeric(input$significance_threshold)
        } else {
            as.numeric(input$volcano_direct_padj)
        }
        
        # Get log2FC threshold
        fcCutoff <- as.numeric(input$log_fold_change_threshold)
        
        # Create custom colors: RED (up), BLUE (down), GREY (not sig)
        keyvals <- ifelse(
            df$padj < pCutoff & abs(df$log2FoldChange) > fcCutoff,
            ifelse(df$log2FoldChange > 0, 'red2', 'royalblue'),
            'grey50'
        )
        keyvals[is.na(keyvals)] <- 'grey50'
        names(keyvals)[keyvals == 'red2'] <- 'Upregulated'
        names(keyvals)[keyvals == 'royalblue'] <- 'Downregulated'
        names(keyvals)[keyvals == 'grey50'] <- 'Not significant'
        
        # Count significant genes for subtitle
        sig_up <- sum(df$padj < pCutoff & df$log2FoldChange > fcCutoff, na.rm = TRUE)
        sig_down <- sum(df$padj < pCutoff & df$log2FoldChange < -fcCutoff, na.rm = TRUE)
        
        # Handle genes of interest for labeling
        selectLab_genes <- NULL
        label_mode <- "auto"
        
        if (!is.null(input$volcano_genes_of_interest) && nchar(trimws(input$volcano_genes_of_interest)) > 0) {
            genes_vec <- strsplit(input$volcano_genes_of_interest, ",\\s*")[[1]]
            genes_vec <- trimws(genes_vec)
            genes_vec <- genes_vec[nchar(genes_vec) > 0]
            
            if (length(genes_vec) > 0) {
                selectLab_genes <- genes_vec
                label_mode <- "custom"
            }
        }
        
        subtitle_text <- if (label_mode == "custom") {
            paste0("Up: ", sig_up, " | Down: ", sig_down, " genes (labeling ", length(selectLab_genes), " genes of interest)")
        } else {
            paste0("Up: ", sig_up, " | Down: ", sig_down, " genes")
        }
        
        # Create the plot
        if (input$gene_alias == "included") {
            p <- EnhancedVolcano(df,
                title = paste0("DESeq2 results of ", input$select_avo_de_file),
                subtitle = subtitle_text,
                lab = df$gene.name,
                x = "log2FoldChange",
                y = "padj",
                selectLab = selectLab_genes,
                pCutoff = pCutoff,
                FCcutoff = fcCutoff,
                colCustom = keyvals,
                colAlpha = 0.5,
                pointSize = 2.0,
                labSize = 4.0,
                boxedLabels = TRUE,
                drawConnectors = TRUE,
                widthConnectors = 0.75,
                colConnectors = 'black',
                max.overlaps = 20,
                legendPosition = 'right',
                legendLabSize = 12,
                legendIconSize = 4.0
            )
        } else {
            p <- EnhancedVolcano(df,
                title = paste0("DESeq2 results of ", input$select_avo_de_file),
                subtitle = subtitle_text,
                lab = df$gene.id,
                x = "log2FoldChange",
                y = "padj",
                selectLab = selectLab_genes,
                pCutoff = pCutoff,
                FCcutoff = fcCutoff,
                colCustom = keyvals,
                colAlpha = 0.5,
                pointSize = 2.0,
                labSize = 4.0,
                boxedLabels = TRUE,
                drawConnectors = TRUE,
                widthConnectors = 0.75,
                colConnectors = 'black',
                max.overlaps = 20,
                legendPosition = 'right',
                legendLabSize = 12,
                legendIconSize = 4.0
            )
        }
        
        # Save plot with specified format and dimensions
        if (plot_format == "pdf") {
            pdf(file, width = plot_width, height = plot_height)
            print(p)
            dev.off()
        } else if (plot_format == "png") {
            png(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi)
            print(p)
            dev.off()
        } else if (plot_format == "jpeg") {
            jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, quality = 95)
            print(p)
            dev.off()
        } else if (plot_format == "tiff") {
            tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw")
            print(p)
            dev.off()
        }
    }
)

# Export R code for volcano plot
output$download_code_volcano <- downloadHandler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        if (export_mode == "full") {
            paste0("volcano_plot_export_", timestamp, ".zip")
        } else {
            ext <- ".R"  # Only .R format supported
            paste0("volcano_plot_", input$select_avo_de_file, "_", timestamp, ext)
        }
    },
    content = function(file) {
        req(input$select_avo_de_file != "Select data")
        
        # Safe access to export mode/format with defaults
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        # Get current parameters
        padj_threshold <- if (input$volcano_threshold_type == "slider") {
            1 / 10^as.numeric(input$significance_threshold)
        } else {
            as.numeric(input$volcano_direct_padj)
        }
        
        # Remove .csv extension if present to avoid double extension
        comparison_name <- gsub("\\.csv$", "", input$select_avo_de_file)
        
        # Prepare data filename for full mode
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        data_filename <- if (export_mode == "full") {
            paste0("volcano_data_", comparison_name, "_", timestamp, ".csv")
        } else {
            NULL
        }
        
        params <- list(
            comparison_name = comparison_name,
            padj_threshold = padj_threshold,
            log2fc_threshold = input$log_fold_change_threshold,
            use_gene_names = (input$gene_alias == "included"),
            gene_type = if(exists("input$volcano_sel_gene_type")) input$volcano_sel_gene_type else "gene.id",
            data_file = data_filename,  # Add data filename for full mode
            genes_of_interest = if (!is.null(input$volcano_genes_of_interest)) trimws(input$volcano_genes_of_interest) else ""
        )
        
        # Generate R code
        r_code <- generateVolcanoCode(params, 
                                     mode = export_mode)
        
        # If full mode, export data and create ZIP
        if (export_mode == "full") {
            temp_dir <- tempdir()
            export_dir <- file.path(temp_dir, paste0("volcano_plot_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Export data
            df <- avo_data()
            data_file <- file.path(export_dir, data_filename)
            write.csv(df, data_file, row.names = TRUE)
            
            # Also export original raw counts and metadata if available
            # Use fileContent to preserve gene.name column
            if (!is.null(myValues$fileContent)) {
                raw_counts_file <- file.path(export_dir, paste0("raw_counts_", timestamp, ".csv"))
                write.csv(myValues$fileContent, raw_counts_file, row.names = FALSE)  # First column is gene.id
            } else if (!is.null(myValues$dataCounts)) {
                raw_counts_file <- file.path(export_dir, paste0("raw_counts_", timestamp, ".csv"))
                write.csv(myValues$dataCounts, raw_counts_file, row.names = TRUE)
            }
            if (!is.null(myValues$coldata)) {
                metadata_file <- file.path(export_dir, paste0("original_metadata_", timestamp, ".csv"))
                write.csv(myValues$coldata, metadata_file, row.names = TRUE)
            }
            
            # Write R code
            code_filename <- paste0("volcano_plot_", input$select_avo_de_file, "_", timestamp, 
                                   ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            readme_files_list <- paste0(
                "- ", code_filename, " : R code to generate the volcano plot\n",
                "- ", data_filename, " : DESeq2 results data for this comparison\n"
            )
            if (!is.null(myValues$dataCounts)) {
                readme_files_list <- paste0(readme_files_list,
                    "- raw_counts_", timestamp, ".csv : Original raw count matrix\n")
            }
            if (!is.null(myValues$coldata)) {
                readme_files_list <- paste0(readme_files_list,
                    "- original_metadata_", timestamp, ".csv : Original sample metadata\n")
            }
            readme_files_list <- paste0(readme_files_list, "- README.txt : This file\n")
            
            readme_text <- paste0(
                "Volcano Plot R Code Export\n",
                "===========================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Comparison: ", comparison_name, "\n",
                "Threshold: padj < ", padj_threshold, ", |log2FC| > ", input$log_fold_change_threshold, "\n\n",
                "Files included:\n",
                readme_files_list,
                "\nInstructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script in RStudio\n",
                "3. Run the script to generate the volcano plot\n",
                "4. Customize colors, labels, and other parameters as needed\n",
                "5. The plot will be saved as PDF and PNG files\n\n",
                "Note: The raw counts and metadata files are included for reference\n",
                "and reproducibility. They are not used directly by the volcano plot script.\n"
            )
            writeLines(readme_text, readme_file)
            
            # Create ZIP from the directory
            zip_file <- file.path(temp_dir, paste0("volcano_plot_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            
            # Copy to output
            file.copy(zip_file, file)
        } else {
            # Just write the R code
            writeLines(r_code, file)
        }
    }
)
