observe({
    compareReactive()
})

compareReactive <- reactive({
    cat("DEBUG: compareReactive triggered, getDiffResVs value:", input$getDiffResVs, "\n")
    if (isTRUE(input$getDiffResVs > 0)) {
        cat("DEBUG: getDiffResVs > 0, proceeding with DE analysis\n")
        withProgress(message = "Getting DESeq results , please wait ...", {
            isolate({
                factorsStr <- "Intercept: no replicates"
                if (input$no_replicates) {
                    myValues$vsResults <- results(myValues$dds)
                } else {
                    if (input$resultNameOrFactor == "Result Names") {
                        validate(
                            need((length(input$resultNamesInput) > 0 & length(input$resultNamesInput) < 3), message = "Need to choose at least 1 (Max. 2)")
                        )
                        js$addStatusIcon("resultsTab", "loading")

                        myValues$vsResults <- results(myValues$dds, contrast = list(input$resultNamesInput))
                        factorsStr <- paste(list(input$resultNamesInput))
                    } else if (input$resultNameOrFactor == "Factors") {
                        cat("DEBUG: Using Factors method with factorNameInput:", input$factorNameInput, "condition1:", input$condition1, "condition2:", input$condition2, "\n")
                        validate(
                            need((input$condition1 != input$condition2), message = "condition 1 must be different from condition 2")
                        )
                        js$addStatusIcon("resultsTab", "loading")

                        myValues$vsResults <- results(myValues$dds, contrast = c(input$factorNameInput, input$condition1, input$condition2))
                        factorsStr <- paste(input$factorNameInput, " : ", input$condition1, input$condition2)
                        cat("DEBUG: Factors analysis completed, factorsStr:", factorsStr, "\n")
                    }
                }


                js$addStatusIcon("resultsTab", "done")
                
                cat("DEBUG: DE analysis completed successfully\n")
                return(list("results" = myValues$vsResults, "conditions" = factorsStr))
            })
        })
    } else {
        cat("DEBUG: getDiffResVs <= 0, returning NULL\n")
        return(NULL)
    }
})

output$maPlot <- renderPlot({
    cat("DEBUG: maPlot renderPlot called\n")
    comparison_result <- compareReactive()
    if (!is.null(comparison_result)) {
        cat("DEBUG: compareReactive() returned valid results, generating MA plot\n")
        isolate({
            plotMA(comparison_result$results, main = "MA Plot", ylim = c(-input$ylim, input$ylim), alpha = input$alpha, colSig = "red")
        })
    } else {
        cat("DEBUG: compareReactive() returned NULL, MA plot will be empty\n")
        cat("DEBUG: Current state - getDiffResVs:", input$getDiffResVs, "vsResults exists:", !is.null(myValues$vsResults), "\n")
        
        # If we have stored results but compareReactive is returning NULL, try to use those directly
        if (!is.null(myValues$vsResults)) {
            cat("DEBUG: Using stored vsResults for MA plot\n")
            plotMA(myValues$vsResults, main = "MA Plot (from stored results)", ylim = c(-input$ylim, input$ylim), alpha = input$alpha, colSig = "red")
        }
    }
})

# Download MA plot with custom size and format
output$download_ma_plot <- downloadHandler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- input$ma_plot_format
        paste0("ma_plot_", timestamp, ".", ext)
    },
    content = function(file) {
        comparison_result <- compareReactive()
        results_data <- if (!is.null(comparison_result)) {
            comparison_result$results
        } else if (!is.null(myValues$vsResults)) {
            myValues$vsResults
        } else {
            return()
        }
        
        plot_width <- input$ma_plot_width
        plot_height <- input$ma_plot_height
        plot_format <- input$ma_plot_format
        plot_dpi <- input$ma_plot_dpi
        
        if (plot_format == "pdf") {
            pdf(file, width = plot_width, height = plot_height)
            plotMA(results_data, main = "MA Plot", ylim = c(-input$ylim, input$ylim), alpha = input$alpha, colSig = "red")
            dev.off()
        } else if (plot_format == "png") {
            png(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi)
            plotMA(results_data, main = "MA Plot", ylim = c(-input$ylim, input$ylim), alpha = input$alpha, colSig = "red")
            dev.off()
        } else if (plot_format == "jpeg") {
            jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, quality = 95)
            plotMA(results_data, main = "MA Plot", ylim = c(-input$ylim, input$ylim), alpha = input$alpha, colSig = "red")
            dev.off()
        } else if (plot_format == "tiff") {
            tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw")
            plotMA(results_data, main = "MA Plot", ylim = c(-input$ylim, input$ylim), alpha = input$alpha, colSig = "red")
            dev.off()
        }
    }
)



filelist <- reactiveValues()
filelist$file_list <- list()
contrast_specs <- reactiveValues()
contrast_specs$specs <- list()



observeEvent(input$do, {
    req(compareReactive()$results)
    if (input$resultNameOrFactor == "Result Names") {
        filename <- safe_file_name(
            paste(input$resultNamesInput, collapse = "_"),
            ".csv"
        )
        contrast_specs$specs[[filename]] <- list(
            method = "result_names",
            contrast = list(as.vector(input$resultNamesInput))
        )
    } else {
        filename <- safe_file_name(
            paste0(
                input$factorNameInput,
                "_",
                input$condition1,
                "_vs_",
                input$condition2
            ),
            ".csv"
        )
        contrast_specs$specs[[filename]] <- list(
            method = "factor",
            contrast = c(
                input$factorNameInput,
                input$condition1,
                input$condition2
            )
        )
    }

    csv <- myValues$vsResults
    file <- file.path(session_dir, filename)
    filelist$file_list[[filename]] <- file
    write.csv(csv, file, row.names = T)
    # print( names(filelist$file_list))
    Saved.Results <- names(filelist$file_list)
    output$savedFileList <- renderDataTable({
        data.frame(Saved.Results)
    })
    print(Saved.Results)
    shinyjs::show(selector = "a[data-value=\"volcanoplotTab\"]")

    if (length(Saved.Results) > 1) {
        shinyjs::show(selector = "a[data-value=\"venndiagramTab\"]")
    }
    shinyjs::show(selector = "a[data-value=\"resultsTab\"]")
})








output$comparisonData <- renderDataTable(
    {
        if (!is.null(compareReactive())) {
            df <- as.data.frame(compareReactive()$results)
            df
        }
    },
    options = list(scrollX = TRUE, pageLength = 5)
)

output$factorsStr <- renderText({
    if (!is.null(compareReactive())) {
        return(compareReactive()$conditions)
    }

    return(NULL)
})



output$analysisRes_enrichGo <- renderUI({
    fileUrl <- new_exchange_file(".csv")
    csv <- myValues$vsResults

    write.csv(csv, file = fileUrl)
    exchangeToken <- encryptUrlParam(fileUrl)
    basePath <- Sys.getenv("NASQAR_BASE_PATH", unset = "")
    oraPath <- paste0(basePath, "/ClusterProfShinyORA/?countsdata=", exchangeToken)
    gseaPath <- paste0(basePath, "/ClusterProfShinyGSEA/?countsdata=", exchangeToken)
    return(tags$div(
        class = "BoxArea3", 
        style = "text-align: center; padding: 10px;",
        p(strong("Enrichment Analysis")),
        a("ClusterProfShinyORA (ORA)",
          href = oraPath,
          `data-handoff-path` = oraPath,
          onclick = paste(
            "this.href = window.location.origin +",
            "this.getAttribute('data-handoff-path');"
          ),
          class = "btn btn-success btn-block", 
          target = "_blank", 
          style = "margin-bottom: 10px;"),
        a("ClusterProfShinyGSEA (GSEA)",
          href = gseaPath,
          `data-handoff-path` = gseaPath,
          onclick = paste(
            "this.href = window.location.origin +",
            "this.getAttribute('data-handoff-path');"
          ),
          class = "btn btn-success btn-block", 
          target = "_blank")
    ))
})










output$downloadVsCsv <- downloadHandler(
    filename = function() {
        paste0(input$condition1, "_vs_", input$condition2, ".csv")
    },
    content = function(file) {
        csv <- myValues$vsResults

        write.csv(csv, file, row.names = T)
    }
)

# Export R code for MA plot
output$download_code_ma <- downloadHandler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_mode <- get_export_mode(input)
        
        if (export_mode == "full") {
            paste0("ma_plots_export_", timestamp, ".zip")
        } else {
            comparison_name <- paste0(input$condition1, "_vs_", input$condition2)
            paste0("ma_plot_", comparison_name, "_", timestamp, ".R")
        }
    },
    content = function(file) {
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        
        if (export_mode == "full") {
            # FULL MODE: Export MA plots for multiple saved contrasts
            req(exists("filelist"), !is.null(filelist$file_list), length(filelist$file_list) > 0)
            
            temp_dir <- session_dir
            export_dir <- file.path(temp_dir, paste0("ma_plots_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            saved_files <- names(filelist$file_list)
            cat("Exporting MA plots for", length(saved_files), "saved contrasts...\n")
            
            ma_code_sections <- list()
            exported_files_list <- c()
            
            # Export each saved contrast
            for (file_idx in seq_along(saved_files)) {
                filename <- saved_files[file_idx]
                cat("  - Processing:", filename, "\n")
                
                # Remove .csv extension
                comparison_name_clean <- gsub("\\.csv$", "", filename)
                
                # Get results data for this contrast
                results_data <- read.csv(filelist$file_list[[filename]], row.names = 1)
                
                # Add gene names if available (for consistency with volcano plots)
                # Check multiple sources for gene names
                if (!("gene.name" %in% colnames(results_data))) {
                    # Gene names not already in results, try to add them
                    if (!is.null(myValues$genenames) && !is.null(myValues$geneids)) {
                        # Use myValues$genenames if available
                        gene_names <- myValues$genenames[rownames(results_data), 1]
                        # Keep gene ID where name is not available
                        gene_names[is.na(gene_names)] <- rownames(results_data)[is.na(gene_names)]
                        results_data$gene.name <- gene_names
                        cat("    Added gene names from myValues$genenames\n")
                    } else if (!is.null(myValues$fileContent) && "gene.names" %in% colnames(myValues$fileContent)) {
                        # Try to get gene names from original file content
                        gene_ids_in_results <- rownames(results_data)
                        # Match by gene ID (first column of fileContent)
                        gene_id_col <- myValues$fileContent[[1]]
                        gene_name_col <- myValues$fileContent$gene.names
                        
                        # Create a lookup
                        gene_name_lookup <- setNames(gene_name_col, gene_id_col)
                        
                        # Get gene names for results
                        gene_names <- gene_name_lookup[gene_ids_in_results]
                        # Keep gene ID where name is not available
                        gene_names[is.na(gene_names)] <- gene_ids_in_results[is.na(gene_names)]
                        results_data$gene.name <- gene_names
                        cat("    Added gene names from original file content\n")
                    } else {
                        cat("    No gene names available\n")
                    }
                } else {
                    cat("    Gene names already present in results\n")
                }
                
                # Save results data (now includes gene.name column)
                results_filename <- paste0(comparison_name_clean, "_results_", timestamp, ".csv")
                results_file <- file.path(export_dir, results_filename)
                write.csv(results_data, results_file, row.names = TRUE)
                exported_files_list <- c(exported_files_list, results_filename)
                
                # Generate MA plot code (skip helpers as they're already at the top)
                params <- list(
                    comparison_name = comparison_name_clean,
                    alpha = if(!is.null(input$alpha)) input$alpha else 0.1,
                    ylim = if(!is.null(input$ylim)) input$ylim else 5,
                    data_file = results_filename
                )
                
                r_code <- generateMAPlotCode(params, mode = "full", use_existing_objects = FALSE, include_helpers = FALSE)
                
                # Store code section
                ma_code_sections[[comparison_name_clean]] <- list(
                    title = paste0("MA Plot: ", comparison_name_clean),
                    code = r_code,
                    order = file_idx
                )
                
                cat("    ✓ MA plot for", comparison_name_clean, "prepared\n")
            }
            
            # Create combined R script for MA plots
            combined_code <- paste0(
                "################################################################################\n",
                "#                                                                              #\n",
                "#       MA PLOTS FOR SAVED CONTRASTS                                          #\n",
                "#       Generated from DESeq2Shiny                                            #\n",
                "#       ", timestamp, "                                                       #\n",
                "#                                                                              #\n",
                "################################################################################\n\n",
                "# This script generates MA plots for ", length(saved_files), " DESeq2 contrasts.\n",
                "# Each contrast is analyzed in a separate section below.\n\n",
                "# Set working directory to the location of this script and data files\n",
                "# setwd(\"/path/to/extracted/folder\")\n\n",
                "# Install required packages if needed:\n",
                "# install.packages('ggplot2')\n\n\n"
            )
            
            # Add helper functions (MA plot specific only)
            helper_functions_ma <- readTemplate("template_helper_ma")
            combined_code <- paste0(
                combined_code,
                "################################################################################\n",
                "# HELPER FUNCTIONS (MA PLOT)\n",
                "################################################################################\n\n",
                "cat(\"\\n\")\n",
                "cat(\"=\", rep(\"=\", 78), \"\\n\", sep = \"\")\n",
                "cat(\"HELPER FUNCTIONS: MA Plot specific functions\\n\")\n",
                "cat(\"=\", rep(\"=\", 78), \"\\n\", sep = \"\")\n\n",
                helper_functions_ma,
                "\n\n"
            )
            
            # Add each MA plot section
            sections_ordered <- ma_code_sections[order(sapply(ma_code_sections, function(x) x$order))]
            for (section in sections_ordered) {
                combined_code <- paste0(
                    combined_code,
                    "################################################################################\n",
                    "#  ", toupper(section$title), "\n",
                    "################################################################################\n\n",
                    section$code,
                    "\n\n"
                )
            }
            
            # Write combined R script
            code_filename <- paste0("ma_plots_", timestamp, ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(combined_code, code_file)
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "MA Plots for Saved Contrasts\n",
                "=============================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Total contrasts: ", length(saved_files), "\n",
                "Contrasts included:\n",
                paste(sapply(saved_files, function(f) paste0("  - ", gsub("\\.csv$", "", f), "\n")), collapse = ""),
                "\nFiles included:\n",
                "- ", code_filename, " : R code to generate MA plots\n",
                paste(sapply(exported_files_list, function(f) paste0("- ", f, " : DESeq2 results data (includes gene.name if available)\n")), collapse = ""),
                "- README.txt : This file\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open ", code_filename, " in RStudio\n",
                "3. Run the entire script to generate ", length(saved_files), " MA plots\n",
                "4. Each plot is saved as PDF and PNG in the same directory\n\n",
                "Plot Settings:\n",
                "- Alpha threshold: ", if(!is.null(input$alpha)) input$alpha else 0.1, "\n",
                "- Y-axis limit: ±", if(!is.null(input$ylim)) input$ylim else 5, "\n\n",
                "Data Format:\n",
                "Each results file contains DESeq2 statistics with columns:\n",
                "- baseMean, log2FoldChange, lfcSE, stat, pvalue, padj\n",
                "- gene.name (if gene symbols are available in your data)\n"
            )
            writeLines(readme_text, readme_file)
            
            # Create ZIP
            zip_file <- file.path(temp_dir, paste0("ma_plots_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            
            # Copy to output
            file.copy(zip_file, file)
            
            cat("\n✓ Exported", length(saved_files), "MA plots successfully!\n")
        } else {
            # CODE-ONLY MODE: Export only currently selected comparison
            req(myValues$vsResults)
            
            comparison_name <- paste0(input$condition1, "_vs_", input$condition2)
            
            params <- list(
                comparison_name = comparison_name,
                alpha = input$alpha,
                ylim = input$ylim,
                data_file = paste0(comparison_name, "_results.csv")
            )
            
            r_code <- generateMAPlotCode(params, mode = export_mode)
            
            # Just write the R code
            writeLines(r_code, file)
        }
    }
)

output$comparisonComputed <- reactive({
    return(!is.null(myValues$vsResults))
})
outputOptions(output, "comparisonComputed", suspendWhenHidden = FALSE)

output$noReplicates <- reactive({
    return(input$no_replicates)
})
outputOptions(output, "noReplicates", suspendWhenHidden = FALSE)
