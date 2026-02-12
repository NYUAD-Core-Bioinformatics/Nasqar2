observe({
    # updateSelectizeInput(session,'sel_gene',
    #                      choices= rownames(myValues$dataCounts),
    #                      server=TRUE)
    # browser()
    if (!is.null(myValues$DF) && ncol(myValues$DF) > 0) {
        tmpgroups <- colnames(myValues$DF)
        tmpgroups <- unlist(lapply(tmpgroups, function(x) {
            if (x %in% colnames(myValues$DF)) {
                levels(myValues$DF[, x])
            } else {
                NULL
            }
        }))
        # Remove NULL values and strip attributes
        tmpgroups <- tmpgroups[!is.null(tmpgroups)]
        tmpgroups <- as.vector(tmpgroups)  # CRITICAL: Strip all attributes to ensure it's a simple vector

        updateSelectizeInput(session, "heat_group",
            choices = tmpgroups, selected = tmpgroups, server = T
        )
    }
})

observe({
    req(input$genHeatmap > 0)
    logNormCounts <- heatmapReactive()$logNormCounts
     ht <- Heatmap(logNormCounts)
})

heatmapReactive <- reactive({
    cat("DEBUG: heatmapReactive triggered, genHeatmap value:", input$genHeatmap, "\n")
    if (input$genHeatmap > 0) {
        cat("DEBUG: genHeatmap > 0, proceeding with heatmap generation\n")
        isolate({

        updateActionButton(session, "genHeatmap",
      label = "Generating heatmap...")
         
        shinyjs::disable("genHeatmap")
        

            logNormCounts <- log2((counts(myValues$dds, normalized = TRUE, replaced = FALSE) + .5))
            # %>%
            #   gather(gene, expression, (ncol(.)-length(input$sel_gene)+1):ncol(.))



            # vst = myValues$vstMat

            # selGroupSamples = row.names(myValues$DF[myValues$DF$Conditions %in% input$heat_group,])
            # logNormCounts = logNormCounts[,selGroupSamples]

            # vst = vst[,input$heat_group]

            myValues$heatmap_path <- paste0(tempdir(), "/", "heatmap-highres.pdf")

            if (!input$subsetGenes) {
                tmpsd <- apply(logNormCounts, 1, sd)

                selectGenes <- rownames(logNormCounts)[order(tmpsd, decreasing = TRUE)]
                selectGenes <- head(selectGenes, input$numGenes)

                genesNotFound <- NULL
            } else {
                genes <- unlist(strsplit(input$listPasteGenes, ","))

                genes <- gsub("^\\s+|\\s+$", "", genes)
                genes <- gsub("\\n+$|^\\n+", "", genes)
                genes <- gsub("^\"|\"$", "", genes)

                genes <- genes[genes != ""]

                print("heatmappp")
                print(genes)

                req(input$gene_alias, input$heatmap_sel_gene_type)
                if (input$gene_alias == "included" && input$heatmap_sel_gene_type == "gene.name") {
                    # get gene.ids from gene.name (geneids is a data frame)
                    genes <- myValues$geneids[genes, 1]
                }
                print(genes)
                genesNotFound <- genes[!(genes %in% rownames(logNormCounts))]

                genes <- genes[!(genes %in% genesNotFound)]

                selectGenes <- genes
            }



            logNormCounts <- logNormCounts[selectGenes, ]
            if (input$gene_alias == "included" && input$heatmap_sel_gene_type == "gene.name") {
                # get gene.ids from gene.name (genenames is a data frame)
                rownames(logNormCounts) <- myValues$genenames[selectGenes, 1]
            }
            # print(logNormCounts)

            ht <- Heatmap(logNormCounts)
            ht <- draw(ht)
            makeInteractiveComplexHeatmap(input, output, session, ht, 
                click_action = heat_map_click_action, brush_action = heat_map_brush_action,"ht2"
            )
           

            #generateHeatmapPdf(logNormCounts, Rowv, annLegend, annCol)
            #  shinyjs::enable("genHeatmap")
            cat("DEBUG: Heatmap generation completed successfully\n")
            return(list("logNormCounts" = logNormCounts, "genesNotFound" = genesNotFound))
        })
    } else {
        cat("DEBUG: genHeatmap <= 0, returning NULL\n")
        return(NULL)
    }

    #
})

output$heatmapPlot <- renderPlot({
    print("heatmapPlot")
    if (!is.null(heatmapReactive())) {
        print("logNormCounts")
        logNormCounts <- heatmapReactive()$logNormCounts
        genesNotFound <- heatmapReactive()$genesNotFound

        validate(
            # need( is.null(genesNotFound) || length(genesNotFound) < 1, message = "Some genes were not found!"),
            need(nrow(logNormCounts) > 1, message = "Need atleast 2 genes to plot!")
        )


        coldata <- colData(myValues$dds)
        coldata$sizeFactor <- NULL
        coldata$replaceable <- NULL

        if (input$no_replicates) {
            annLegend <- F
            Rowv <- NA
            annCol <- NULL
        } else {
            annLegend <- T
            Rowv <- NA

            annCols <- colnames(coldata)[!colnames(coldata) %in% c("sizeFactor", "replaceable")]
            annCol <- as.data.frame(coldata[, annCols])
        }

       
        # dev.off()

        print("aheatmap")
        print(annCol)

        p <- pheatmap(logNormCounts)
        # p <- aheatmap(logNormCounts,scale = "none",
        #               revC=TRUE,
        #               fontsize = 10,
        #               cexRow = 1.2,
        #               Rowv = Rowv,
        #               annLegend = annLegend,
        #               #color = colorRampPalette( rev(brewer.pal(9, "Blues")) )(255),
        #               annCol = annCol
        #  )

        # print(p)
        return(p)
    }
})

heat_map_click_action <- function(df, output) {
    print('heat_map_click_action')
    shinyjs::enable("genHeatmap")
    updateActionButton(session, "genHeatmap",
      label = "Generate Plot")
    output[["heatmap_click"]] <- renderUI({
        if (!is.null(df)) {
            HTML(qq("<p style='background-color:#FF8080;color:white;padding:5px;'>You have clicked on heatmap @{df$heatmap}, row @{df$row_index}, column @{df$column_index}</p>"))
        }
    })
}

heat_map_brush_action <- function(df, output) {
    input$evaluateExpression

    row_index <- unique(unlist(df$row_index))
    column_index <- unique(unlist(df$column_index))
    
    # Store brushed genes for export
    if (!is.null(heatmapReactive()) && length(row_index) > 0) {
        matrix <- heatmapReactive()$logNormCounts
        m <- matrix[row_index, column_index, drop = FALSE]
        
        # Store the parent heatmap's full data range for consistent color scaling
        myValues$heatmap_data_range <- c(min(matrix, na.rm = TRUE), max(matrix, na.rm = TRUE))
        cat("Stored parent heatmap data range:", myValues$heatmap_data_range[1], "to", myValues$heatmap_data_range[2], "\n")
        
        # Store in myValues for export
        myValues$brushed_heatmap_data <- m
        myValues$brushed_genes <- rownames(m)  # These might be gene names or gene IDs depending on display mode
        myValues$brushed_samples <- colnames(m)
        
        # IMPORTANT: Store the original gene IDs for data extraction AND preserve order
        # If rownames are gene names, convert back to gene IDs
        gene_type <- tryCatch(input$heatmap_sel_gene_type, error = function(e) NULL)
        
        if (!is.null(gene_type) && gene_type == "gene.name" && !is.null(myValues$geneids)) {
            # Rownames are gene names, convert to gene IDs while preserving order
            gene_ids_mapped <- myValues$geneids[rownames(m), 1]
            # Keep all genes, even if mapping returns NA (preserve order!)
            myValues$brushed_gene_ids <- gene_ids_mapped
            myValues$brushed_gene_order <- rownames(m)  # Store original names for order reference
            cat("Brushed", length(row_index), "genes (stored as gene names, mapped to gene IDs with order preserved)\n")
        } else {
            # Rownames are gene IDs already (or fallback)
            myValues$brushed_gene_ids <- rownames(m)
            myValues$brushed_gene_order <- rownames(m)  # Same as IDs
            cat("Brushed", length(row_index), "genes (stored as gene IDs)\n")
        }
    }

    # if (!is.null(heatmapReactive())) {
    #     matrix <- heatmapReactive()$logNormCounts
    #     m <- matrix[row_index, column_index, drop = FALSE]
    #        output[["heatmap_matrix_table"]] <- DT::renderDataTable(
    #     {
    #         gene.id <- rownames(m)
    #         genes <- gene.id



    #         if (input$gene_alias == "included") {
    #             genes <- myValues$genenames[gene.id, ]
    #             gene.name <- genes
    #             m <- cbind(m, gene.name)
    #         }

    #         if (input$venn_sel_gene_type == "gene.id") {
    #             genes <- gene.id
    #         }
    #         isolate({
    #             myValues$selected_genes <- myValues$selected_genes + 1
    #             print("myValues$selected_genes")
    #             print(myValues$selected_genes)
    #         })

    #         updateTextAreaInput(session, "venn_gene_list", value = paste(genes, collapse = input$venn_input_genes_sep))

    #         return(m)
    #     },
    #     options = list(scrollX = TRUE, pageLength = 50)
    # )

            
    # }



   
 


}


## Output for brushed heatmap availability
output$brushedHeatmapAvailable <- reactive({
    !is.null(myValues$brushed_heatmap_data) && nrow(myValues$brushed_heatmap_data) > 0
})
outputOptions(output, "brushedHeatmapAvailable", suspendWhenHidden = FALSE)

## Output for brushed genes count
output$brushed_genes_count <- renderText({
    if (!is.null(myValues$brushed_genes)) {
        paste("Selected:", length(myValues$brushed_genes), "genes ×", 
              length(myValues$brushed_samples), "samples")
    } else {
        ""
    }
})

## Download handler for brushed heatmap data
output$download_brushed_heatmap_csv <- downloadHandler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("brushed_heatmap_data_", timestamp, ".csv")
    },
    content = function(file) {
        req(myValues$brushed_heatmap_data)
        write.csv(myValues$brushed_heatmap_data, file, row.names = TRUE)
    }
)

## Download handler for brushed heatmap R code
output$download_code_brushed_heatmap <- downloadHandler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        if (export_mode == "full") {
            paste0("brushed_heatmap_export_", timestamp, ".zip")
        } else {
            ext <- ".R"  # Only .R format supported
            paste0("brushed_heatmap_", timestamp, ext)
        }
    },
    content = function(file) {
        req(myValues$brushed_heatmap_data, myValues$brushed_genes)
        
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        # Get current parameters
        gene_type <- tryCatch(input$heatmap_sel_gene_type, error = function(e) NULL)
        use_gene_names_val <- (!is.null(gene_type) && gene_type == "gene.name")
        
        # If full mode, prepare data export FIRST to get correct rownames
        if (get_export_mode(input) == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- tempdir()
            export_dir <- file.path(temp_dir, paste0("brushed_heatmap_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Export the BRUSHED matrix that Shiny is displaying (already log2-transformed!)
            # No need to re-do transformation - just export what user brushed
            brushed_matrix <- myValues$brushed_heatmap_data
            
            if (is.null(brushed_matrix)) {
                stop("Brushed heatmap data not available. Please brush/select genes on the heatmap and try again.")
            }
            
            cat("Exporting brushed heatmap matrix (already processed by Shiny)\n")
            cat("Matrix dimensions:", nrow(brushed_matrix), "×", ncol(brushed_matrix), "\n")
            cat("Data range: min =", round(min(brushed_matrix, na.rm = TRUE), 2), 
                ", max =", round(max(brushed_matrix, na.rm = TRUE), 2), "\n")
            
            # Get parent heatmap color range for consistent color scaling
            if (is.null(myValues$heatmap_data_range)) {
                warning("Parent heatmap color range not found. Using auto-scaling.")
                parent_range <- NULL
            } else {
                parent_range <- myValues$heatmap_data_range
                cat("Using parent heatmap color range:", parent_range[1], "to", parent_range[2], "\n")
            }
            
            # Export the brushed matrix (already processed - no transformation needed)
            matrix_filename <- paste0("brushed_heatmap_matrix_", timestamp, ".csv")
            matrix_file <- file.path(export_dir, matrix_filename)
            write.csv(brushed_matrix, matrix_file, row.names = TRUE)
            cat("Exported brushed matrix to:", matrix_filename, "\n")
            
            # Export metadata
            metadata_filename <- paste0("brushed_heatmap_metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            metadata <- as.data.frame(colData(myValues$dds))[colnames(brushed_matrix), , drop = FALSE]
            metadata$sizeFactor <- NULL
            metadata$replaceable <- NULL
            write.csv(metadata, metadata_file, row.names = TRUE)
            cat("Exported metadata to:", metadata_filename, "\n")
            
            # Set parameters for brushed heatmap (simpler - just needs color range from parent)
            params <- list(
                matrix_file = matrix_filename,
                metadata_file = metadata_filename,
                fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8,
                parent_color_range = parent_range  # Use parent's color range for consistency
            )
            
            # Generate simple R code for brushed heatmap
            r_code <- generateBrushedHeatmapCodeSimple(params)
            
            # Write R code
            code_filename <- paste0("brushed_heatmap_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "Brushed Sub-Heatmap Export\n",
                "===========================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "This export contains the brushed/selected portion of the heatmap.\n",
                "All data processing was done by Shiny - the matrix is already log2-transformed.\n",
                "The R script just loads and plots the brushed matrix.\n\n",
                "Selected genes: ", nrow(brushed_matrix), " genes (from interactive brush)\n",
                "Selected samples: ", ncol(brushed_matrix), " samples\n\n",
                "Files included:\n",
                "- ", code_filename, " : R script to create the brushed heatmap\n",
                "- ", matrix_filename, " : Pre-processed brushed matrix (log2-transformed)\n",
                "- ", metadata_filename, " : Sample metadata for selected samples\n",
                "- README.txt : This file\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script in RStudio\n",
                "3. Run the script to generate the brushed heatmap\n",
                "4. The plot will be saved as brushed_heatmap.pdf and brushed_heatmap.png\n\n",
                "Note: The matrix is already processed and matches exactly what you\n",
                "selected in the Shiny app. Colors will match the parent heatmap.\n"
            )
            writeLines(readme_text, readme_file)
            
            # Create ZIP from the directory
            zip_file <- file.path(temp_dir, paste0("brushed_heatmap_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            
            # Copy to output
            file.copy(zip_file, file)
        } else {
            # Code-only mode: just write R code without data files
            parent_range <- myValues$heatmap_data_range
            
            params <- list(
                matrix_file = "brushed_heatmap_matrix.csv",
                metadata_file = "brushed_heatmap_metadata.csv",
                fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8,
                parent_color_range = parent_range
            )
            
            r_code <- generateBrushedHeatmapCodeSimple(params)
            writeLines(r_code, file)
        }
    }
)

generateHeatmapPdf <- function(logNormCounts, Rowv, annLegend, annCol) {
    p <- pheatmap(logNormCounts, filename = myValues$heatmap_path)
    # heatmaptmp = heatmap(logNormCounts,scale = "none",
    #          revC=TRUE,
    #          fontsize = 10,
    #          cexRow = 1.2,
    #          Rowv = Rowv,
    #          annLegend = annLegend,
    #          #color = colorRampPalette( rev(brewer.pal(9, "Blues")) )(255),
    #          annCol = annCol,
    #          filename = myValues$heatmap_path
    # )
}

# output$heatmapHighResAvailable <- reactive({
#
#   if(is.null(heatmapReactive()))
#     return(F)
#
#   return(file.exists(myValues$heatmap_path))
# })
# outputOptions(output, 'heatmapHighResAvailable', suspendWhenHidden=FALSE)

output$downloadHighResHeatmap <- downloadHandler(
    filename = c("heatmap_highres.pdf"),
    content = function(file) {
        file.copy(myValues$heatmap_path, file)
    }
)

output$heatmapData <- renderDataTable(
    {
        if (!is.null(heatmapReactive())) {
            heatmapReactive()$logNormCounts
        }
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

#
output$heatmapComputed <- reactive({
    print("heatmapComputed")
    return(!is.null(heatmapReactive()))
})
outputOptions(output, "heatmapComputed", suspendWhenHidden = FALSE)


output$downloadHeatmapCsv <- downloadHandler(
    filename = function() {
        "heatmap_data.csv"
    },
    content = function(file) {
        csv <- heatmapReactive()$logNormCounts

        write.csv(csv, file)
    }
)

# Export R code for heatmap
output$download_code_heatmap <- downloadHandler(
    filename = function() {
        # Check if data is available - if not, return error filename
        heatmap_check <- tryCatch(heatmapReactive(), error = function(e) NULL)
        if (is.null(heatmap_check)) {
            filename_val <- "heatmap_error.txt"
        } else {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            export_mode <- get_export_mode(input)
            export_format <- get_export_format(input)
            
            filename_val <- if (export_mode == "full") {
                paste0("heatmap_export_", timestamp, ".zip")
            } else {
                ext <- ".R"  # Only .R format supported
                paste0("heatmap_", timestamp, ext)
            }
        }
        
        filename_val
    },
    content = function(file) {
        # Check if heatmap data is available (instead of req() which fails silently)
        heatmap_data <- tryCatch(heatmapReactive(), error = function(e) NULL)
        if (is.null(heatmap_data)) {
            # Write error message instead of failing
            error_msg <- paste0(
                "Error: Heatmap export not available\n\n",
                "Please configure the Expression Heatmap first:\n",
                "1. Navigate to the 'Heatmap' tab\n",
                "2. Wait for the heatmap to generate\n",
                "3. Configure any desired settings (gene count, clustering, etc.)\n",
                "4. Then return to export the plot code\n"
            )
            writeLines(error_msg, file)
            return(invisible(NULL))
        }
        
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        gene_type <- tryCatch(input$heatmap_sel_gene_type, error = function(e) NULL)
        use_gene_names_val <- (!is.null(gene_type) && gene_type == "gene.name")

        # If full mode, prepare data export FIRST to get the actual displayed genes
        if (export_mode == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- tempdir()
            export_dir <- file.path(temp_dir, paste0("heatmap_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Get the ACTUAL genes displayed in the Shiny heatmap
            # heatmapReactive() returns a list with $logNormCounts component
            if (!is.list(heatmap_data) || is.null(heatmap_data$logNormCounts)) {
                stop("No heatmap data available. Please generate a heatmap first.")
            }
            
            displayed_heatmap <- heatmap_data$logNormCounts
            
            # Ensure it's a matrix, not a data frame
            if (is.data.frame(displayed_heatmap)) {
                displayed_heatmap <- as.matrix(displayed_heatmap)
            }
            
            cat("Displayed heatmap dimensions:", nrow(displayed_heatmap), "×", ncol(displayed_heatmap), "\n")
            
            # Extract the gene IDs that were actually displayed
            # If displayed with gene names, rownames are gene names - need to convert to IDs
            if (use_gene_names_val && !is.null(myValues$geneids)) {
                # Rownames are gene names, convert to gene IDs
                displayed_gene_names <- rownames(displayed_heatmap)
                cat("Converting gene names to IDs for", length(displayed_gene_names), "genes\n")
                displayed_gene_ids <- myValues$geneids[displayed_gene_names, 1]
                displayed_gene_ids <- displayed_gene_ids[!is.na(displayed_gene_ids)]
                cat("Heatmap displayed with gene names, mapped", length(displayed_gene_ids), "to gene IDs\n")
            } else {
                # Rownames are gene IDs already
                displayed_gene_ids <- rownames(displayed_heatmap)
                cat("Heatmap displayed with gene IDs:", length(displayed_gene_ids), "genes\n")
            }
            
            # Verify we have gene IDs to export
            if (length(displayed_gene_ids) == 0) {
                stop("No gene IDs found to export. Please check the heatmap configuration.")
            }
            
            # Export the FINAL matrix that Shiny is displaying (already log2-transformed, genes already selected)
            # No need to re-do gene selection or transformation - Shiny already did all the work!
            logNormCounts <- displayed_heatmap
            
            cat("Exporting final heatmap matrix (already processed by Shiny)\n")
            cat("Matrix dimensions:", nrow(logNormCounts), "×", ncol(logNormCounts), "\n")
            cat("Data range: min =", round(min(logNormCounts, na.rm = TRUE), 2), 
                ", max =", round(max(logNormCounts, na.rm = TRUE), 2), "\n")
            
            # NOTE: params will be set AFTER files are saved so we can use actual filenames
            # (see below after file export)
        } else {
            # For code-only mode (not full), use original logic
            selected_genes <- NULL
            if (input$subsetGenes && !is.null(input$listPasteGenes) && input$listPasteGenes != "") {
                genes <- unlist(strsplit(input$listPasteGenes, ","))
                genes <- gsub("^\\s+|\\s+$", "", genes)
                genes <- gsub("\\n+$|^\\n+", "", genes)
                genes <- gsub("^\"|\"$", "", genes)
                selected_genes <- genes[genes != ""]
            }
            
            params <- list(
                num_genes = input$numGenes,
                selected_genes = selected_genes,
                use_gene_names = use_gene_names_val
            )
        }
        
        # For code-only mode, generate R code now
        # For full mode, generate after saving files (to use actual filenames)
        if (export_mode != "full") {
            r_code <- generateHeatmapCode(params, 
                                         mode = export_mode, 
            )
        }
        
        # If full mode, continue with export (data already prepared above)
        if (export_mode == "full") {
            # logNormCounts, export_dir, timestamp already exist from above
            
            # Export the final matrix (already processed - log2-transformed and gene-selected)
            matrix_filename <- paste0("heatmap_matrix_", timestamp, ".csv")
            matrix_file <- file.path(export_dir, matrix_filename)
            write.csv(logNormCounts, matrix_file, row.names = TRUE)
            cat("Exported final matrix to:", matrix_filename, "\n")
            
            # Export metadata
            metadata_filename <- paste0("heatmap_metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            metadata <- as.data.frame(colData(myValues$dds))
            metadata$sizeFactor <- NULL
            metadata$replaceable <- NULL
            write.csv(metadata, metadata_file, row.names = TRUE)
            cat("Exported metadata to:", metadata_filename, "\n")
            
            # Set parameters for simple template
            params <- list(
                matrix_file = matrix_filename,
                metadata_file = metadata_filename,
                fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8
            )
            
            # Generate simple R code
            r_code <- generateHeatmapCodeSimple(params)
            
            # Write R code
            code_filename <- paste0("heatmap_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "Expression Heatmap Export\n",
                "=========================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "This export contains the final processed heatmap data.\n",
                "All data processing (gene selection, log2 transformation) was done by Shiny.\n",
                "The R script just loads the matrix and creates the heatmap plot.\n\n",
                "Genes: ", nrow(logNormCounts), " genes (exact genes displayed in Shiny)\n",
                "Samples: ", ncol(logNormCounts), " samples\n",
                "\nFiles included:\n",
                "- ", code_filename, " : R script to create the heatmap\n",
                "- ", matrix_filename, " : Pre-processed matrix (log2-transformed)\n",
                "- ", metadata_filename, " : Sample metadata\n",
                "- README.txt : This file\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script in RStudio\n",
                "3. Run the script to generate the heatmap\n",
                "4. The plot will be saved as heatmap.pdf and heatmap.png\n\n",
                "Note: The matrix file is already log2-transformed and contains\n",
                "the exact genes shown in the Shiny heatmap. No additional\n",
                "processing is needed.\n"
            )
            writeLines(readme_text, readme_file)
            
            # Create ZIP from the directory
            zip_file <- file.path(temp_dir, paste0("heatmap_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            
            # Copy to output
            file.copy(zip_file, file)
        } else {
            # Just write the R code
            writeLines(r_code, file)
        }
    }
)
