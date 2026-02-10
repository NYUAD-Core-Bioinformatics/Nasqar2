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
            
            # Export brushed data
            # NOTE: myValues$brushed_heatmap_data contains log2-transformed data
            # We need to export RAW normalized counts so the template can transform them
            
            # Use the stored gene IDs (not gene names) for subsetting
            if (is.null(myValues$brushed_gene_ids) || length(myValues$brushed_gene_ids) == 0) {
                stop("Brushed gene IDs not found. Please brush/select genes on the heatmap and try exporting again.")
            }
            
            if (is.null(myValues$brushed_samples) || length(myValues$brushed_samples) == 0) {
                stop("Brushed sample names not found. Please brush/select genes on the heatmap and try exporting again.")
            }
            
            brushed_gene_ids <- myValues$brushed_gene_ids  # These are gene IDs (may contain NAs)
            brushed_gene_order <- myValues$brushed_gene_order  # Original names/IDs for order
            brushed_sample_names <- myValues$brushed_samples
            
            # Remove NAs but track their positions to preserve order
            valid_idx <- !is.na(brushed_gene_ids)
            valid_gene_ids <- brushed_gene_ids[valid_idx]
            valid_gene_order <- brushed_gene_order[valid_idx]
            
            cat("Exporting brushed heatmap with", length(valid_gene_ids), "genes and", 
                length(brushed_sample_names), "samples\n")
            cat("Gene IDs:", paste(head(valid_gene_ids, 3), collapse = ", "), "...\n")
            cat("Samples:", paste(head(brushed_sample_names, 3), collapse = ", "), "...\n")
            
            # Get raw normalized counts for these genes and samples
            raw_norm_counts <- counts(myValues$dds, normalized = TRUE, replaced = FALSE)
            
            # Ensure it's a matrix
            if (is.data.frame(raw_norm_counts)) {
                raw_norm_counts <- as.matrix(raw_norm_counts)
            }
            
            # Subset data - this preserves the order of valid_gene_ids
            brushed_data <- raw_norm_counts[valid_gene_ids, brushed_sample_names, drop = FALSE]
            
            cat("Extracted data dimensions:", nrow(brushed_data), "×", ncol(brushed_data), "\n")
            cat("Data range: min =", round(min(brushed_data), 2), 
                ", max =", round(max(brushed_data), 2), "\n")
            
            # If the heatmap was displayed with gene names, use the original order
            if (use_gene_names_val && !is.null(myValues$genenames)) {
                # Use valid_gene_order (the original names) for rownames to preserve order
                rownames(brushed_data) <- valid_gene_order
                cat("Updated rownames to gene names (order preserved)\n")
            }
            
            # Get parent heatmap color range for consistent color scaling
            # MUST use parent range - do NOT calculate from brushed data
            if (is.null(myValues$heatmap_data_range)) {
                warning("Parent heatmap color range not found. This may cause color scale mismatch.")
                cat("WARNING: Parent heatmap range not available. Using auto-scaling (may not match Shiny).\n")
                parent_range <- NULL  # Let template auto-scale (will warn user)
            } else {
                parent_range <- myValues$heatmap_data_range
                cat("Using parent heatmap color range:", parent_range[1], "to", parent_range[2], "\n")
            }
            
            # NOTE: params will be set AFTER files are saved so we can use actual filenames
            # (see below after file export)
        } else {
            # For code-only mode, use original genes
            # Get parent heatmap color range - MUST use parent range
            if (is.null(myValues$heatmap_data_range)) {
                warning("Parent heatmap color range not found. This may cause color scale mismatch.")
                cat("WARNING: Parent heatmap range not available. Using auto-scaling (may not match Shiny).\n")
                parent_range <- NULL  # Let template auto-scale (will warn user)
            } else {
                parent_range <- myValues$heatmap_data_range
                cat("Using parent heatmap color range:", parent_range[1], "to", parent_range[2], "\n")
            }
            
            params <- list(
                num_genes = length(myValues$brushed_genes),
                selected_genes = myValues$brushed_genes,
                use_gene_names = use_gene_names_val,
                is_brushed_heatmap = TRUE,
                sample_order = colnames(myValues$brushed_heatmap_data),
                color_range = parent_range,
                counts_file = "normalized_counts.csv",
                metadata_file = "metadata.csv",
                fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8,
                is_venn_heatmap = FALSE
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
            # brushed_data, export_dir, timestamp already exist from above
            
            # Add gene names column if requested
            if (use_gene_names_val && !is.null(myValues$genenames)) {
                brushed_rownames <- rownames(brushed_data)
                
                # Determine if rownames are gene names or gene IDs
                are_gene_names <- any(brushed_rownames %in% names(myValues$geneids))
                
                if (are_gene_names) {
                    # Rownames are gene names - use them directly for the gene.names column
                    gene_names_export <- brushed_rownames
                    cat("Brushed data uses gene names as rownames - using directly\n")
                } else {
                    # Rownames are gene IDs - look up the corresponding gene names
                    gene_names_export <- myValues$genenames[brushed_rownames, 1]
                    # Keep gene ID where name is not available
                    gene_names_export[is.na(gene_names_export)] <- brushed_rownames[is.na(gene_names_export)]
                    cat("Looked up gene names from gene IDs\n")
                }
                
                brushed_data_df <- as.data.frame(brushed_data)
                brushed_data_df <- cbind(gene.names = gene_names_export, brushed_data_df)
                brushed_data <- brushed_data_df
                
                cat("Exported", sum(!is.na(gene_names_export)), "genes with gene names\n")
            }
            
            counts_filename <- paste0("brushed_heatmap_counts_", timestamp, ".csv")
            counts_file <- file.path(export_dir, counts_filename)
            write.csv(brushed_data, counts_file, row.names = TRUE)
            
            # Export metadata for brushed samples
            metadata_filename <- paste0("brushed_heatmap_metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            metadata <- as.data.frame(colData(myValues$dds))
            metadata <- metadata[myValues$brushed_samples, , drop = FALSE]
            metadata$sizeFactor <- NULL
            metadata$replaceable <- NULL
            write.csv(metadata, metadata_file, row.names = TRUE)
            
            # NOW set params with actual filenames and generate R code
            params <- list(
                num_genes = nrow(brushed_data),
                selected_genes = rownames(brushed_data),  # Use actual rownames from exported data!
                use_gene_names = use_gene_names_val,
                is_brushed_heatmap = TRUE,
                sample_order = colnames(brushed_data),
                color_range = parent_range,
                counts_file = counts_filename,  # Use actual filename with timestamp!
                metadata_file = metadata_filename,  # Use actual filename with timestamp!
                fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8,
                is_venn_heatmap = FALSE
            )
            
            # Generate R code with correct filenames
            r_code <- generateHeatmapCode(params, 
                                          mode = export_mode, 
            )
            
            # Write R code
            code_filename <- paste0("brushed_heatmap_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "Brushed Sub-Heatmap R Code Export\n",
                "==================================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Selected genes: ", length(myValues$brushed_genes), " genes from interactive brush\n",
                "Selected samples: ", length(myValues$brushed_samples), " samples\n\n",
                "Files included:\n",
                "- ", code_filename, " : R code to generate the heatmap\n",
                "- ", counts_filename, " : Normalized counts for selected genes\n",
                "- ", metadata_filename, " : Sample metadata\n",
                "- README.txt : This file\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script in RStudio\n",
                "3. Run the script to generate the heatmap\n",
                "4. Customize colors, clustering, and other parameters as needed\n"
            )
            writeLines(readme_text, readme_file)
            
            # Create ZIP from the directory
            zip_file <- file.path(temp_dir, paste0("brushed_heatmap_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            
            # Copy to output
            file.copy(zip_file, file)
        } else {
            # Just write the R code
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
            
            # Get raw normalized counts for ONLY the displayed genes
            raw_norm_counts <- counts(myValues$dds, normalized = TRUE)
            
            # Ensure it's a matrix
            if (is.data.frame(raw_norm_counts)) {
                raw_norm_counts <- as.matrix(raw_norm_counts)
            }
            
            cat("Total genes in dataset:", nrow(raw_norm_counts), "\n")
            cat("Total samples in dataset:", ncol(raw_norm_counts), "\n")
            cat("Genes to export:", length(displayed_gene_ids), "\n")
            
            # Verify gene IDs exist in the dataset
            genes_found <- displayed_gene_ids %in% rownames(raw_norm_counts)
            cat("Genes found in dataset:", sum(genes_found), "out of", length(displayed_gene_ids), "\n")
            
            if (!all(genes_found)) {
                missing <- displayed_gene_ids[!genes_found]
                cat("Warning:", sum(!genes_found), "genes not found in dataset\n")
                cat("Missing genes:", paste(head(missing, 5), collapse = ", "), "\n")
                displayed_gene_ids <- displayed_gene_ids[genes_found]
            }
            
            if (length(displayed_gene_ids) == 0) {
                stop("No valid genes to export after filtering.")
            }
            
            # Subset to get only displayed genes
            logNormCounts <- raw_norm_counts[displayed_gene_ids, , drop = FALSE]
            cat("Successfully extracted data:", nrow(logNormCounts), "×", ncol(logNormCounts), "\n")
            
            cat("Extracted raw counts for", nrow(logNormCounts), "genes\n")
            cat("Data range: min =", round(min(logNormCounts), 2), 
                ", max =", round(max(logNormCounts), 2), "\n")
            
            # If displayed with gene names, update rownames for export
            if (use_gene_names_val && !is.null(myValues$genenames)) {
                gene_names_display <- myValues$genenames[displayed_gene_ids, 1]
                gene_names_display[is.na(gene_names_display)] <- displayed_gene_ids[is.na(gene_names_display)]
                rownames(logNormCounts) <- gene_names_display
                cat("Updated rownames to gene names for display\n")
            }
            
            # Add gene names column if requested
            if (use_gene_names_val && !is.null(myValues$genenames)) {
                gene_names_export <- rownames(logNormCounts)
                logNormCounts_df <- as.data.frame(logNormCounts)
                logNormCounts_df <- cbind(gene.names = gene_names_export, logNormCounts_df)
                logNormCounts <- logNormCounts_df
            }
            
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
            
            counts_filename <- paste0("heatmap_counts_", timestamp, ".csv")
            counts_file <- file.path(export_dir, counts_filename)
            write.csv(logNormCounts, counts_file, row.names = TRUE)
            
            # Export metadata
            metadata_filename <- paste0("heatmap_metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            metadata <- as.data.frame(colData(myValues$dds))
            metadata$sizeFactor <- NULL
            metadata$replaceable <- NULL
            write.csv(metadata, metadata_file, row.names = TRUE)
            
            # NOW set parameters with actual filenames and generate R code
            params <- list(
                num_genes = nrow(logNormCounts),
                selected_genes = rownames(logNormCounts),  # Actual displayed genes
                use_gene_names = use_gene_names_val,
                counts_file = counts_filename,  # Use actual filename with timestamp!
                metadata_file = metadata_filename,  # Use actual filename with timestamp!
                fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8,
                is_brushed_heatmap = FALSE,
                sample_order = NULL,
                is_venn_heatmap = FALSE,
                color_range = NULL
            )
            
            # Generate R code with correct filenames
            r_code <- generateHeatmapCode(params, 
                                         mode = export_mode, 
            )
            
            # Write R code
            code_filename <- paste0("heatmap_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "Expression Heatmap R Code Export\n",
                "=================================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Genes: ", nrow(logNormCounts), " genes (exact genes displayed in Shiny heatmap)\n",
                "\nFiles included:\n",
                "- ", code_filename, " : R code to generate the heatmap\n",
                "- ", counts_filename, " : Normalized counts data\n",
                "- ", metadata_filename, " : Sample metadata\n",
                "- README.txt : This file\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script in RStudio\n",
                "3. Run the script to generate the plot\n",
                "4. Customize colors, clustering, and other parameters as needed\n"
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
