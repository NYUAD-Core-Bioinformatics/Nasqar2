output$ddsColData <- renderTable(
    {
        colDataTable <- colData(myValues$dds)
        colsToSelect <- labels(terms(design(myValues$dds)))

        if (length(colsToSelect) == 0) {
            return(head(colDataTable, n = 5))
        }
        if (length(colsToSelect) == 1) {
            h <- head(colDataTable[, colsToSelect], n = 5)
            h <- as.data.frame(h)
            colnames(h) <- c(colsToSelect[[1]])
            return(h)
        }

        head(colDataTable[, colsToSelect], n = 5)
    },
    bordered = TRUE,
    spacing = "xs",
    rownames = T
)

output$ddsDesignFormula <- renderText({
    dds <- myValues$dds
    paste(as.character(design(dds)))
})

observe({
    ddsReactive()
})

ddsReactive <- eventReactive(input$run_deseq2, {
    # print('ddsReactive')
    dds <- myValues$dds # ()

    withProgress(message = "Running DESeq2 , please wait", {
        js$addStatusIcon("deseqTab", "loading")
        shiny::setProgress(value = 0.4, detail = " ...")

        removeNotification("errorNotify")
        removeNotification("errorNotify1")
        reset_downstream_tabs("deseq")
        # nasqar use 3 cores
        validate(need(
            tryCatch(
                {
                    dds <- DESeq(
                        dds,
                        parallel = TRUE,
                        BPPARAM = MulticoreParam(3)
                    )
                },
                error = function(e) {
                    myValues$status <- paste("DESeq2 Error: ", e$message)

                    showNotification(id = "errorNotify", myValues$status, type = "error", duration = NULL)
                    showNotification(id = "errorNotify1", "If this is intended, please select 'No Replicates' in Input Data step. OR use ~ 1 as the design formula", type = "error", duration = NULL)


                    js$addStatusIcon("deseqTab", "fail")

                    return(NULL)
                }
            ),
            "Error"
        ))

        # A failing transformation is reported and skipped; dds is kept.
        transform_or_notify <- function(label, expr) {
            tryCatch(expr, error = function(e) {
                showNotification(paste0(label, " error: ", conditionMessage(e)), type = "error", duration = NULL)
                NULL
            })
        }

        if (input$computeRlog) {
            shiny::setProgress(value = 0.5, detail = "Calculating RLog transformation ...")
            rld <- transform_or_notify("rlog", rlog(dds))
            myValues$rld <- rld
            myValues$rlogMat <- if (is.null(rld)) NULL else assay(rld)

            myValues$rldColNames <- if (is.null(rld)) NULL else colnames(rld)
        }


        shiny::setProgress(value = 0.7, detail = "Computing Variance Stabilizing Transformation ...")

        vsd <- transform_or_notify("VST", varianceStabilizingTransformation(dds))
        myValues$vsd <- vsd
        myValues$vstMat <- if (is.null(vsd)) NULL else assay(vsd)
        myValues$vsdColNames <- if (is.null(vsd)) NULL else colnames(vsd)

        shiny::setProgress(value = 0.8, detail = "Formatting data ...")

        shiny::setProgress(value = 1, detail = "...")

        js$addStatusIcon("deseqTab", "done")

        myValues$dds <- dds

        shinyjs::show(selector = "a[data-value=\"boxplotTab\"]")
        shinyjs::show(selector = "a[data-value=\"resultsTab\"]")
        shinyjs::show(selector = "a[data-value=\"heatmapTab\"]")
        if (!is.null(vsd)) {
            shinyjs::show(selector = "a[data-value=\"vstTab\"]")
        }

        if (input$computeRlog && !is.null(myValues$rld)) {
            shinyjs::show(selector = "a[data-value=\"rlogTab\"]")
        }


        factorChoices <- as.vector(colnames(colData(dds)))  # Ensure simple vector

        # print('factorChoices')
        # print(factorChoices)
        factorChoices <- factorChoices[!grepl("^SV[[:digit:]]+$", factorChoices)]

        updateSelectInput(session, "rlogIntGroupsInput", choices = factorChoices, selected = factorChoices[1])
        updateSelectInput(session, "vsdIntGroupsInput", choices = factorChoices, selected = factorChoices[1])

        factorChoices <- factorChoices[!(factorChoices %in% c("sizeFactor", "replaceable"))]

        # Filter factorChoices to only include categorical factors that are in the design formula
        design_terms <- labels(terms(design(dds)))
        
        validFactorChoices <- getValidCategoricalFactors(factorChoices, design_terms, myValues$DF)
        
        # If no valid factors found, keep original list
        if (length(validFactorChoices) == 0) {
            validFactorChoices <- factorChoices
        }

        # Ensure simple vectors for updateSelectizeInput
        result_names <- as.vector(resultsNames(dds))
        valid_factors <- as.vector(validFactorChoices)
        
        updateSelectizeInput(session, "resultNamesInput", choices = result_names, selected = NULL)
        updateSelectizeInput(session, "factorNameInput", choices = valid_factors, selected = if(length(valid_factors) > 0) valid_factors[1] else NULL)

        # Update condition dropdowns now that DESeq2 analysis is complete
        if (!is.null(input$factorNameInput) && input$factorNameInput != "" && 
            input$factorNameInput %in% valid_factors) {
            factor_levels <- as.vector(levels(myValues$DF[, input$factorNameInput]))
            if (length(factor_levels) >= 2) {
                updateSelectInput(session, "condition1", choices = factor_levels)
                updateSelectInput(session, "condition2", choices = factor_levels)
            }
        }

        disable("data_file_type")
        disable("no_replicates")

        js$addStatusIcon("conditionsTab", "done")
    })

    # }
})

observeEvent(input$factorNameInput,
    {
        # Validate that the selected factor still exists in myValues$DF
        if (is.null(myValues$DF) || is.null(input$factorNameInput) || input$factorNameInput == "") {
            return()
        }
        
        # Check if the selected column exists in the dataframe
        if (!(input$factorNameInput %in% colnames(myValues$DF))) {
            print(paste("Warning: Column", input$factorNameInput, "not found in myValues$DF"))
            return()
        }
        
        # Only populate conditions if DESeq2 analysis has been completed and results exist
        if (is.null(myValues$dds)) {
            print("DESeq2 analysis not completed - conditions will be populated after analysis")
            updateSelectInput(session, "condition1", choices = NULL)
            updateSelectInput(session, "condition2", choices = NULL)
            return()
        }
        
        myValues$DF[] <- lapply(myValues$DF, function(x) {
            if (is.character(x) || is.factor(x)) {
                as.factor(x)
            } else {
                x
            }
        })
        
        # Verify that the factor is present in the design formula and results
        design_terms <- labels(terms(design(myValues$dds)))
        if (!(input$factorNameInput %in% design_terms)) {
            print(paste("Factor", input$factorNameInput, "not found in design formula"))
            updateSelectInput(session, "condition1", choices = NULL)
            updateSelectInput(session, "condition2", choices = NULL)
            return()
        }
        
        # Verify that the factor is actually categorical (not continuous)
        factor_data <- myValues$DF[, input$factorNameInput]
        if (!isCategoricalFactor(factor_data, nrow(myValues$DF))) {
            print(paste("Factor", input$factorNameInput, "appears to be continuous, not suitable for categorical comparison"))
            updateSelectInput(session, "condition1", choices = NULL)
            updateSelectInput(session, "condition2", choices = NULL)
            return()
        }
        
        # Get factor levels properly, converting to factor if necessary
        factor_data <- myValues$DF[, input$factorNameInput]
        
        # Convert to factor if it's not already one
        if (!is.factor(factor_data)) {
            factor_data <- as.factor(factor_data)
        }
        
        factor_levels <- as.vector(levels(factor_data))  # Ensure simple vector
        
        # Only populate if there are at least 2 levels for comparison
        if (length(factor_levels) < 2) {
            print(paste("Factor", input$factorNameInput, "must have at least 2 levels for differential analysis"))
            updateSelectInput(session, "condition1", choices = NULL)
            updateSelectInput(session, "condition2", choices = NULL)
            return()
        }
        
        updateSelectInput(session, "condition1", choices = factor_levels)
        updateSelectInput(session, "condition2", choices = factor_levels)
    },
    ignoreInit = T
)


observe({
    if (isTRUE(input$goto_svaTab > 0)) {
        GotoTab("svaseqTab")
    }
})

output$rlogData <- renderDataTable(
    {
        if (!is.null(myValues$rlogMat)) {
            myValues$rlogMat
        }
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

# The on-screen heatmap and the downloaded figure share one palette and one
# clustering (complete linkage on the sample distances), so they show the
# same picture.
sample_distance_colors <- function() {
    grDevices::colorRampPalette(
        rev(RColorBrewer::brewer.pal(9, "Blues"))
    )(255)
}

transformed_distance_plot <- function(matrix) {
    validate(need(!is.null(matrix), "Run DESeq2 before exporting this figure."))
    sample_distances <- stats::dist(t(matrix))
    sample_matrix <- as.matrix(sample_distances)
    pheatmap::pheatmap(
        sample_matrix,
        clustering_distance_rows = sample_distances,
        clustering_distance_cols = sample_distances,
        color = sample_distance_colors(),
        border_color = NA,
        main = "Sample distance heatmap"
    )
}

transformed_distance_plotly <- function(matrix) {
    sample_distances <- stats::dist(t(matrix))
    sample_matrix <- as.matrix(sample_distances)
    dend <- stats::as.dendrogram(stats::hclust(sample_distances))
    # heatmaply draws columns in reverse dendrogram order; reversing the
    # column dendrogram puts the samples in the same top-left to bottom-right
    # order that pheatmap uses in the download.
    heatmaply::heatmaply(
        sample_matrix,
        Rowv = dend,
        Colv = rev(dend),
        symm = FALSE,
        revC = FALSE,
        colors = sample_distance_colors(),
        main = "Sample distance heatmap"
    )
}

transformed_pca_plot <- function(transformed, intgroup) {
    validate(need(!is.null(transformed), "Run DESeq2 before exporting this figure."))
    if (is.null(intgroup) || !nzchar(intgroup)) {
        intgroup <- names(SummarizedExperiment::colData(transformed))[1]
    }
    pca_data <- DESeq2::plotPCA(
        transformed,
        intgroup = intgroup,
        returnData = TRUE
    )
    percent_var <- attr(pca_data, "percentVar")
    ggplot2::ggplot(
        pca_data,
        ggplot2::aes(x = PC1, y = PC2, color = group, label = name)
    ) +
        ggplot2::geom_point(size = 3.5, alpha = 0.9) +
        ggplot2::labs(
            x = sprintf("PC1: %.1f%% variance", percent_var[1] * 100),
            y = sprintf("PC2: %.1f%% variance", percent_var[2] * 100),
            color = intgroup,
            title = "PCA Plot"
        ) +
        ggplot2::theme_classic(base_size = 12)
}

output$rlogPlot <- renderPlotly({
    if (!is.null(myValues$rlogMat)) {
        transformed_distance_plotly(myValues$rlogMat)
    }
})

output$rlogPcaPlot <- renderPlotly({
    if (!is.null(myValues$rld)) {
        intgroups <- input$rlogIntGroupsInput
        if (is.null(intgroups) | intgroups == "") {
            intgroups <- names(colData(myValues$dds))[1]
        }



        # Use DESeq2's plotPCA and returnData=TRUE to get the data
        rld <- myValues$rld
        
        # Get PCA data from DESeq2
        pca_data <- DESeq2::plotPCA(rld, intgroup = intgroups, returnData = TRUE)
        percentVar <- attr(pca_data, "percentVar")
        
        # Add sample names for hover
        pca_data$SampleName <- rownames(pca_data)
        
        # Create group column for coloring and display
        if (length(intgroups) == 1) {
            pca_data$group <- pca_data[[intgroups]]
        } else {
            # If multiple groups, combine them
            pca_data$group <- pca_data$group  # DESeq2 already creates this
        }
        
        # Prepare hover text with all available information
        hover_text <- paste(
            "Sample:", pca_data$SampleName,
            "<br>Group of Interest:", pca_data$group
        )
        
        # Add additional columns to hover if they exist
        sample_data <- as.data.frame(colData(rld))
        additional_cols <- setdiff(names(sample_data), c(intgroups, "sizeFactor", "replaceable"))
        if (length(additional_cols) > 0) {
            for (col in additional_cols) {
                if (!is.null(sample_data[[col]])) {
                    hover_text <- paste(hover_text, 
                                      paste0("<br>", col, ":"), sample_data[[col]])
                }
            }
        }
        
        # Create plotly visualization
        p <- plot_ly(
            data = pca_data,
            x = ~PC1, 
            y = ~PC2,
            color = ~group,
            # ggplot's default hue palette, the same colours as the download
            colors = scales::hue_pal()(nlevels(factor(pca_data$group))),
            text = hover_text,
            hoverinfo = "text",
            type = "scatter",
            mode = "markers",
            marker = list(
                size = 16,
                opacity = 0.9,
                line = list(width = 1.5, color = "white")
            )
        ) %>%
        layout(
            title = list(text = "PCA Plot", font = list(size = 22)),
            font = list(size = 16),
            xaxis = list(
                title = list(
                    text = paste0("PC1: ", round(percentVar[1] * 100, 1), "% variance"),
                    font = list(size = 18)
                ),
                tickfont = list(size = 14),
                zeroline = FALSE
            ),
            yaxis = list(
                title = list(
                    text = paste0("PC2: ", round(percentVar[2] * 100, 1), "% variance"),
                    font = list(size = 18)
                ),
                tickfont = list(size = 14),
                zeroline = FALSE
            ),
            showlegend = TRUE,
            legend = list(
                title = list(text = "Group of Interest"),
                font = list(size = 15)
            ),
            margin = list(l = 90, r = 40, b = 80, t = 70)
        ) %>%
        config(responsive = TRUE, displaylogo = FALSE)
        
        return(p)
    }
})


output$vsdPlot <- renderPlotly({
    if (!is.null(myValues$vstMat)) {
        transformed_distance_plotly(myValues$vstMat)
    }
})

output$vsdPcaPlot <- renderPlotly({
    if (!is.null(myValues$vsd)) {
        intgroups <- input$vsdIntGroupsInput
        if (is.null(intgroups) | intgroups == "") {
            intgroups <- names(colData(myValues$dds))[1]
        }

        print("colData")
        print(colData(myValues$dds))


        vsd <- myValues$vsd
        
        # Get PCA data from DESeq2  (default)
        pca_data <- DESeq2::plotPCA(vsd, intgroup = intgroups, returnData = TRUE)
        percentVar <- attr(pca_data, "percentVar")
        
        # Add sample names for hover
        pca_data$SampleName <- rownames(pca_data)
        
        # Create group column for coloring and display
        if (length(intgroups) == 1) {
            pca_data$group <- pca_data[[intgroups]]
        } else {
            # If multiple groups, combine them
            pca_data$group <- pca_data$group  # DESeq2 already creates this
        }
        
        # Prepare hover text with all available information
        hover_text <- paste(
            "Sample:", pca_data$SampleName,
            "<br>Group of Interest:", pca_data$group
        )
        
        # Add additional columns to hover if they exist
        sample_data <- as.data.frame(colData(vsd))
        additional_cols <- setdiff(names(sample_data), c(intgroups, "sizeFactor", "replaceable"))
        if (length(additional_cols) > 0) {
            for (col in additional_cols) {
                if (!is.null(sample_data[[col]])) {
                    hover_text <- paste(hover_text, 
                                      paste0("<br>", col, ":"), sample_data[[col]])
                }
            }
        }
        
        # Create plotly visualization
        p <- plot_ly(
            data = pca_data,
            x = ~PC1, 
            y = ~PC2,
            color = ~group,
            # ggplot's default hue palette, the same colours as the download
            colors = scales::hue_pal()(nlevels(factor(pca_data$group))),
            text = hover_text,
            hoverinfo = "text",
            type = "scatter",
            mode = "markers",
            marker = list(
                size = 16,
                opacity = 0.9,
                line = list(width = 1.5, color = "white")
            )
        ) %>%
        layout(
            title = list(text = "PCA Plot", font = list(size = 22)),
            font = list(size = 16),
            xaxis = list(
                title = list(
                    text = paste0("PC1: ", round(percentVar[1] * 100, 1), "% variance"),
                    font = list(size = 18)
                ),
                tickfont = list(size = 14),
                zeroline = FALSE
            ),
            yaxis = list(
                title = list(
                    text = paste0("PC2: ", round(percentVar[2] * 100, 1), "% variance"),
                    font = list(size = 18)
                ),
                tickfont = list(size = 14),
                zeroline = FALSE
            ),
            showlegend = TRUE,
            legend = list(
                title = list(text = "Group of Interest"),
                font = list(size = 15)
            ),
            margin = list(l = 90, r = 40, b = 80, t = 70)
        ) %>%
        config(responsive = TRUE, displaylogo = FALSE)
        
        return(p)
    }
})

register_publication_downloads(
    output, "download_vst_distance",
    function() "deseq2-vst-sample-distance",
    function() transformed_distance_plot(myValues$vstMat),
    width = 8, height = 7
)
register_publication_downloads(
    output, "download_vst_pca",
    function() "deseq2-vst-pca",
    function() transformed_pca_plot(myValues$vsd, input$vsdIntGroupsInput),
    width = 8, height = 6
)
register_publication_downloads(
    output, "download_rlog_distance",
    function() "deseq2-rlog-sample-distance",
    function() transformed_distance_plot(myValues$rlogMat),
    width = 8, height = 7
)
register_publication_downloads(
    output, "download_rlog_pca",
    function() "deseq2-rlog-pca",
    function() transformed_pca_plot(myValues$rld, input$rlogIntGroupsInput),
    width = 8, height = 6
)

output$vsdData <- renderDataTable(
    {
        if (!is.null(myValues$vstMat)) {
            myValues$vstMat
        }
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

output$downloadRlogCsv <- nasqar_download_handler(
    filename = function() {
        paste0("rlog", ".csv")
    },
    content = function(file) {
        csv <- myValues$rlogMat

        write.csv(csv, file, row.names = T)
    }
)

output$downloadVsdCsv <- nasqar_download_handler(
    filename = function() {
        paste0("vsd", ".csv")
    },
    content = function(file) {
        csv <- myValues$vstMat

        write.csv(csv, file, row.names = T)
    }
)

output$ddsComputed <- reactive({
    return(!is.null(myValues$dds))
})
outputOptions(output, "ddsComputed", suspendWhenHidden = FALSE)

output$deseqError <- reactive({
    return(!is.null(myValues$status))
})
outputOptions(output, "deseqError", suspendWhenHidden = FALSE)

# Export R code for VST PCA plot
output$download_code_pca_vst <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        if (export_mode == "full") {
            paste0("pca_vst_export_", timestamp, ".zip")
        } else {
            ext <- ".R"  # Only .R format supported
            paste0("pca_vst_", timestamp, ext)
        }
    },
    content = function(file) {
        req(myValues$vsd)
        
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        # For code-only mode, set params now
        # For full mode, set params after saving files (to use actual filenames)
        if (export_mode != "full") {
            params <- list(
                intgroup = if(!is.null(input$vsdIntGroupsInput)) input$vsdIntGroupsInput else names(colData(myValues$dds))[1],
                transform_type = "vst",
                transformed_data_file = "vst_data.csv",
                metadata_file = "metadata.csv"
            )
            
            r_code <- generatePCACode(params, 
                                     mode = export_mode, 
            )
        }
        
        if (export_mode == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- session_dir
            export_dir <- file.path(temp_dir, paste0("pca_vst_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Calculate PCA coordinates (same as Shiny's display)
            intgroup <- if(!is.null(input$vsdIntGroupsInput)) input$vsdIntGroupsInput else names(colData(myValues$dds))[1]
            pca_data <- DESeq2::plotPCA(myValues$vsd, intgroup = intgroup, returnData = TRUE)
            percentVar <- attr(pca_data, "percentVar")
            
            # Export PCA coordinates with metadata
            pca_filename <- paste0("pca_coordinates_", timestamp, ".csv")
            pca_file <- file.path(export_dir, pca_filename)
            write.csv(pca_data, pca_file, row.names = TRUE)
            
            # Export full metadata for reference
            metadata_filename <- paste0("metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            write.csv(as.data.frame(colData(myValues$dds)), metadata_file, row.names = TRUE)
            
            # NOW set params with actual filenames and generate simplified R code
            params <- list(
                intgroup = intgroup,
                transform_type = "vst",
                pca_data_file = pca_filename,  # Pre-calculated PCA coordinates!
                pc1_var = round(percentVar[1] * 100, 1),
                pc2_var = round(percentVar[2] * 100, 1)
            )
            
            # Use simplified template - no gene selection or PCA calculation
            r_code <- generatePCACodeSimple(params)
            
            code_filename <- paste0("pca_vst_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "PCA Plot (VST) R Code Export\n",
                "============================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Files included:\n",
                "- ", code_filename, " : R code to generate the PCA plot\n",
                "- ", pca_filename, " : Pre-calculated PCA coordinates (PC1, PC2) with metadata\n",
                "- ", metadata_filename, " : Full sample metadata (for reference)\n\n",
                "Variance explained:\n",
                "- PC1: ", round(percentVar[1] * 100, 1), "%\n",
                "- PC2: ", round(percentVar[2] * 100, 1), "%\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script (", code_filename, ") in RStudio\n",
                "3. Run the script to generate the plot\n\n",
                "Note: PCA coordinates were pre-calculated by Shiny from VST-transformed data.\n",
                "The script just loads these coordinates and creates the plot.\n"
            )
            writeLines(readme_text, readme_file)
            
            zip_file <- file.path(temp_dir, paste0("pca_vst_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            file.copy(zip_file, file)
        } else {
            writeLines(r_code, file)
        }
    }
)

# Export R code for RLOG PCA plot
output$download_code_pca_rlog <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        if (export_mode == "full") {
            paste0("pca_rlog_export_", timestamp, ".zip")
        } else {
            ext <- ".R"  # Only .R format supported
            paste0("pca_rlog_", timestamp, ext)
        }
    },
    content = function(file) {
        req(myValues$rld)
        
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        # For code-only mode, set params now
        # For full mode, set params after saving files (to use actual filenames)
        if (export_mode != "full") {
            params <- list(
                intgroup = if(!is.null(input$rlogIntGroupsInput)) input$rlogIntGroupsInput else names(colData(myValues$dds))[1],
                transform_type = "rlog",
                transformed_data_file = "rlog_data.csv",
                metadata_file = "metadata.csv"
            )
            
            r_code <- generatePCACode(params, 
                                     mode = export_mode, 
            )
        }
        
        if (export_mode == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- session_dir
            export_dir <- file.path(temp_dir, paste0("pca_rlog_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Calculate PCA coordinates (same as Shiny's display)
            intgroup <- if(!is.null(input$rlogIntGroupsInput)) input$rlogIntGroupsInput else names(colData(myValues$dds))[1]
            pca_data <- DESeq2::plotPCA(myValues$rld, intgroup = intgroup, returnData = TRUE)
            percentVar <- attr(pca_data, "percentVar")
            
            # Export PCA coordinates with metadata
            pca_filename <- paste0("pca_coordinates_", timestamp, ".csv")
            pca_file <- file.path(export_dir, pca_filename)
            write.csv(pca_data, pca_file, row.names = TRUE)
            
            # Export full metadata for reference
            metadata_filename <- paste0("metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            write.csv(as.data.frame(colData(myValues$dds)), metadata_file, row.names = TRUE)
            
            # NOW set params with actual filenames and generate simplified R code
            params <- list(
                intgroup = intgroup,
                transform_type = "rlog",
                pca_data_file = pca_filename,  # Pre-calculated PCA coordinates!
                pc1_var = round(percentVar[1] * 100, 1),
                pc2_var = round(percentVar[2] * 100, 1)
            )
            
            # Use simplified template - no gene selection or PCA calculation
            r_code <- generatePCACodeSimple(params)
            
            code_filename <- paste0("pca_rlog_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "PCA Plot (RLOG) R Code Export\n",
                "=============================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Files included:\n",
                "- ", code_filename, " : R code to generate the PCA plot\n",
                "- ", pca_filename, " : Pre-calculated PCA coordinates (PC1, PC2) with metadata\n",
                "- ", metadata_filename, " : Full sample metadata (for reference)\n\n",
                "Variance explained:\n",
                "- PC1: ", round(percentVar[1] * 100, 1), "%\n",
                "- PC2: ", round(percentVar[2] * 100, 1), "%\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script (", code_filename, ") in RStudio\n",
                "3. Run the script to generate the plot\n\n",
                "Note: PCA coordinates were pre-calculated by Shiny from RLOG-transformed data.\n",
                "The script just loads these coordinates and creates the plot.\n"
            )
            writeLines(readme_text, readme_file)
            
            zip_file <- file.path(temp_dir, paste0("pca_rlog_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            file.copy(zip_file, file)
        } else {
            writeLines(r_code, file)
        }
    }
)

# Export R code for VST Distance Heatmap
output$download_code_distheat_vst <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        if (export_mode == "full") {
            paste0("distance_heatmap_vst_export_", timestamp, ".zip")
        } else {
            ext <- ".R"  # Only .R format supported
            paste0("distance_heatmap_vst_", timestamp, ext)
        }
    },
    content = function(file) {
        req(myValues$vsd)
        
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        # For code-only mode, set params now
        # For full mode, set params after saving files (to use actual filenames)
        if (export_mode != "full") {
            params <- list(
                transform_type = "vst",
                transformed_data_file = "vst_data.csv",
                metadata_file = "metadata.csv"
            )
            
            r_code <- generateDistHeatmapCode(params, 
                                              mode = export_mode, 
             )
        }
        
        if (export_mode == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- session_dir
            export_dir <- file.path(temp_dir, paste0("distance_heatmap_vst_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Calculate sample distance matrix (same as Shiny's display)
            sample_dists <- dist(t(myValues$vstMat))
            sample_dist_matrix <- as.matrix(sample_dists)
            
            # Export distance matrix
            dist_filename <- paste0("distance_matrix_", timestamp, ".csv")
            dist_file <- file.path(export_dir, dist_filename)
            write.csv(sample_dist_matrix, dist_file, row.names = TRUE)
            
            # Export metadata
            metadata_filename <- paste0("metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            write.csv(as.data.frame(colData(myValues$dds)), metadata_file, row.names = TRUE)
            
            # NOW set params with actual filenames and generate simplified R code
            params <- list(
                transform_type = "vst",
                distance_matrix_file = dist_filename,  # Pre-calculated distance matrix!
                metadata_file = metadata_filename
            )
            
            # Use simplified template - no distance calculation
            r_code <- generateDistHeatmapCodeSimple(params)
            
            code_filename <- paste0("distance_heatmap_vst_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "Distance Heatmap (VST) R Code Export\n",
                "====================================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Files included:\n",
                "- ", code_filename, " : R code to generate distance heatmaps\n",
                "- ", dist_filename, " : Pre-calculated sample-to-sample distance matrix\n",
                "- ", metadata_filename, " : Sample metadata\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script (", code_filename, ") in RStudio\n",
                "3. Run the script to generate both interactive (HTML) and static (PDF/PNG) heatmaps\n\n",
                "Note: Distance matrix was pre-calculated by Shiny from VST-transformed data.\n",
                "The script just loads this matrix and creates the heatmaps.\n"
            )
            writeLines(readme_text, readme_file)
            
            zip_file <- file.path(temp_dir, paste0("distance_heatmap_vst_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            file.copy(zip_file, file)
        } else {
            writeLines(r_code, file)
        }
    }
)

# Export R code for RLOG Distance Heatmap
output$download_code_distheat_rlog <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        if (export_mode == "full") {
            paste0("distance_heatmap_rlog_export_", timestamp, ".zip")
        } else {
            ext <- ".R"  # Only .R format supported
            paste0("distance_heatmap_rlog_", timestamp, ext)
        }
    },
    content = function(file) {
        req(myValues$rld)
        
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        # For code-only mode, set params now
        # For full mode, set params after saving files (to use actual filenames)
        if (export_mode != "full") {
            params <- list(
                transform_type = "rlog",
                transformed_data_file = "rlog_data.csv",
                metadata_file = "metadata.csv"
            )
            
            r_code <- generateDistHeatmapCode(params, 
                                              mode = export_mode, 
             )
        }
        
        if (export_mode == "full") {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- session_dir
            export_dir <- file.path(temp_dir, paste0("distance_heatmap_rlog_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Calculate sample distance matrix (same as Shiny's display)
            sample_dists <- dist(t(myValues$rlogMat))
            sample_dist_matrix <- as.matrix(sample_dists)
            
            # Export distance matrix
            dist_filename <- paste0("distance_matrix_", timestamp, ".csv")
            dist_file <- file.path(export_dir, dist_filename)
            write.csv(sample_dist_matrix, dist_file, row.names = TRUE)
            
            # Export metadata
            metadata_filename <- paste0("metadata_", timestamp, ".csv")
            metadata_file <- file.path(export_dir, metadata_filename)
            write.csv(as.data.frame(colData(myValues$dds)), metadata_file, row.names = TRUE)
            
            # NOW set params with actual filenames and generate simplified R code
            params <- list(
                transform_type = "rlog",
                distance_matrix_file = dist_filename,  # Pre-calculated distance matrix!
                metadata_file = metadata_filename
            )
            
            # Use simplified template - no distance calculation
            r_code <- generateDistHeatmapCodeSimple(params)
            
            code_filename <- paste0("distance_heatmap_rlog_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "Distance Heatmap (RLOG) R Code Export\n",
                "=====================================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Files included:\n",
                "- ", code_filename, " : R code to generate distance heatmaps\n",
                "- ", dist_filename, " : Pre-calculated sample-to-sample distance matrix\n",
                "- ", metadata_filename, " : Sample metadata\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script (", code_filename, ") in RStudio\n",
                "3. Run the script to generate both interactive (HTML) and static (PDF/PNG) heatmaps\n\n",
                "Note: Distance matrix was pre-calculated by Shiny from RLOG-transformed data.\n",
                "The script just loads this matrix and creates the heatmaps.\n"
            )
            writeLines(readme_text, readme_file)
            
            zip_file <- file.path(temp_dir, paste0("distance_heatmap_rlog_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            file.copy(zip_file, file)
        } else {
            writeLines(r_code, file)
        }
    }
)
