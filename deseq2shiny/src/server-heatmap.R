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

# Force the heatmap to be (re)generated when the button is clicked; report
# failures instead of letting an observer error take the session down.
observe({
    req(input$genHeatmap > 0)
    tryCatch(heatmapReactive(), error = function(e) {
        shinyjs::enable("genHeatmap")
        updateActionButton(session, "genHeatmap", label = "Generate Plot")
        msg <- conditionMessage(e)
        if (nzchar(msg)) {
            showNotification(paste("Heatmap:", msg), type = "error")
        }
    })
})

heatmapReactive <- reactive({
    # Read dds outside isolate() so a new DESeq2 run invalidates the heatmap
    dds <- myValues$dds
    if (isTRUE(input$genHeatmap > 0) && !is.null(dds)) {
        isolate({

        updateActionButton(session, "genHeatmap",
      label = "Generating heatmap...")

        shinyjs::disable("genHeatmap")


            logNormCounts <- log2((counts(dds, normalized = TRUE, replaced = FALSE) + .5))

            if (!input$subsetGenes) {
                validate(need(
                    !is.null(input$numGenes) && is.finite(input$numGenes) && input$numGenes >= 2,
                    "Enter the number of genes (at least 2)"
                ))
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

                req(input$gene_alias, input$heatmap_sel_gene_type)
                if (input$gene_alias == "included" && input$heatmap_sel_gene_type == "gene.name") {
                    # get gene.ids from gene.name (geneids is a data frame)
                    genes <- myValues$geneids[genes, 1]
                }
                genesNotFound <- genes[!(genes %in% rownames(logNormCounts))]

                genes <- genes[!(genes %in% genesNotFound)]

                selectGenes <- genes
            }



            logNormCounts <- logNormCounts[selectGenes, , drop = FALSE]
            if (input$gene_alias == "included" && input$heatmap_sel_gene_type == "gene.name") {
                # get gene.ids from gene.name (genenames is a data frame)
                rownames(logNormCounts) <- myValues$genenames[selectGenes, 1]
            }

            ht <- Heatmap(logNormCounts)
            # draw() needs an open graphics device; this runs outside any
            # render, so use a null device rather than letting R open Rplots.pdf
            pdf(NULL)
            ht <- tryCatch(draw(ht), finally = dev.off())
            makeInteractiveComplexHeatmap(input, output, session, ht,
                click_action = heat_map_click_action, brush_action = heat_map_brush_action,"ht2"
            )

            return(list("logNormCounts" = logNormCounts, "genesNotFound" = genesNotFound))
        })
    } else {
        return(NULL)
    }
})

heat_map_click_action <- function(df, output) {
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

## Download handler for brushed heatmap R code
output$download_code_brushed_heatmap <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
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

        # Get current parameters
        gene_type <- tryCatch(input$heatmap_sel_gene_type, error = function(e) NULL)
        use_gene_names_val <- (!is.null(gene_type) && gene_type == "gene.name")
        
        # If full mode, prepare data export FIRST to get correct rownames
        if (get_export_mode(input) == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- session_dir
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
                fontsize_row = 8,
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
                fontsize_row = 8,
                parent_color_range = parent_range
            )
            
            r_code <- generateBrushedHeatmapCodeSimple(params)
            writeLines(r_code, file)
        }
    }
)

# Render the displayed heatmap (same matrix/object as heatmapReactive) to a PDF on demand
output$downloadHighResHeatmap <- nasqar_download_handler(
    filename = "heatmap_highres.pdf",
    content = function(file) {
        heatmap_data <- heatmapReactive()
        req(heatmap_data)
        logNormCounts <- heatmap_data$logNormCounts
        pdf(file, width = 10, height = max(7, min(200, 0.15 * nrow(logNormCounts) + 2)))
        on.exit(dev.off(), add = TRUE)
        draw(Heatmap(logNormCounts))
    }
)

output$heatmapComputed <- reactive({
    return(!is.null(heatmapReactive()))
})
outputOptions(output, "heatmapComputed", suspendWhenHidden = FALSE)

# Export R code for heatmap
output$download_code_heatmap <- nasqar_download_handler(
    filename = function() {
        # Check if data is available - if not, return error filename
        heatmap_check <- tryCatch(heatmapReactive(), error = function(e) NULL)
        if (is.null(heatmap_check)) {
            filename_val <- "heatmap_error.txt"
        } else {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            export_mode <- get_export_mode(input)

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

        gene_type <- tryCatch(input$heatmap_sel_gene_type, error = function(e) NULL)
        use_gene_names_val <- (!is.null(gene_type) && gene_type == "gene.name")

        # If full mode, prepare data export FIRST to get the actual displayed genes
        if (export_mode == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- session_dir
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
                fontsize_row = 8
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
