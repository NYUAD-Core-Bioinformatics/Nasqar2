observe({
    compareReactive()
})

compareReactive <- reactive({
    cat("DEBUG: compareReactive triggered, getDiffResVs value:", input$getDiffResVs, "\n")
    if (input$getDiffResVs > 0) {
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



observeEvent(input$do, {
    df <- as.data.frame(compareReactive()$results)
    df[is.na(df)] <- 0
    if (input$resultNameOrFactor == "Result Names") {
        filename <- paste(input$resultNamesInput, collapse = "_")
    } else {
        filename <- paste0(input$factorNameInput, input$condition1, "_vs_", input$condition2, ".csv")
    }

    csv <- myValues$vsResults
    file <- paste0(tempdir(), "/", filename)
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
            df[is.na(df)] <- 0
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
    fileUrl <- UUIDgenerate()
    fileUrl <- paste0(tempdir(), "/", fileUrl, ".csv")
    csv <- myValues$vsResults

    write.csv(csv, file = fileUrl)
    return(tags$div(
        class = "BoxArea3", style = "text-align: center;",
        p(strong("ClusterProfShinyORA")),
        a("goEnrich", href = paste0("/ClusterProfShinyORA?countsdata=", encryptUrlParam(fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
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
        comparison_name <- paste0(input$condition1, "_vs_", input$condition2)
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        if (export_mode == "full") {
            paste0("ma_plot_export_", timestamp, ".zip")
        } else {
            ext <- ".R"  # Only .R format supported
            paste0("ma_plot_", comparison_name, "_", timestamp, ext)
        }
    },
    content = function(file) {
        req(myValues$vsResults)
        
        # Safe access to export mode/format with defaults
        export_mode <- get_export_mode(input)
        export_format <- get_export_format(input)
        
        comparison_name <- paste0(input$condition1, "_vs_", input$condition2)
        
        # Prepare data filename for full mode
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        results_filename <- if (export_mode == "full") {
            paste0("ma_plot_data_", comparison_name, "_", timestamp, ".csv")
        } else {
            NULL
        }
        
        params <- list(
            comparison_name = comparison_name,
            alpha = input$alpha,
            ylim = input$ylim,
            data_file = results_filename  # Add data filename for full mode
        )
        
        r_code <- generateMAPlotCode(params, 
                                     mode = export_mode, 
)
        
        if (export_mode == "full") {
            temp_dir <- tempdir()
            export_dir <- file.path(temp_dir, paste0("ma_plot_export_", timestamp))
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Export results data
            results_file <- file.path(export_dir, results_filename)
            write.csv(as.data.frame(myValues$vsResults), results_file, row.names = TRUE)
            
            # Write R code
            code_filename <- paste0("ma_plot_", comparison_name, "_", timestamp, 
                                   ".R")
            code_file <- file.path(export_dir, code_filename)
            writeLines(r_code, code_file)
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            readme_text <- paste0(
                "MA Plot R Code Export\n",
                "====================\n\n",
                "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
                "Comparison: ", comparison_name, "\n",
                "Alpha threshold: ", params$alpha, "\n\n",
                "Files included:\n",
                "- ", code_filename, " : R code to generate the MA plot\n",
                "- ", results_filename, " : DESeq2 results data\n",
                "- README.txt : This file\n\n",
                "Instructions:\n",
                "1. Extract all files to the same directory\n",
                "2. Open the R script in RStudio\n",
                "3. Ensure the data CSV file is in the same directory\n",
                "4. Run the script to generate the plot\n",
                "5. The plot will be saved as a PDF file\n"
            )
            writeLines(readme_text, readme_file)
            
            # Create ZIP from the directory
            zip_file <- file.path(temp_dir, paste0("ma_plot_export_", timestamp, ".zip"))
            zip_export_dir(export_dir, zip_file)
            
            # Copy to output
            file.copy(zip_file, file)
        } else {
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
