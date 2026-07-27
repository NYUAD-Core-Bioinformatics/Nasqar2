# Max upload size
options(shiny.maxRequestSize = 600 * 1024^2)
suppressPackageStartupMessages(library(kableExtra))

# Define server

# Function to generate n distinct random colors
generate_random_colors <- function(n) {
  colors <- grDevices::colors()[sample(1:657, n)]
  return(colors)
}

# Helper function to check if a variable is categorical (suitable for factor analysis)
isCategoricalFactor <- function(factor_data, sample_count) {
    is_categorical_factor(factor_data, sample_count)
}

    # Helper function to safely validate input values for restoration
    safeInputValue <- function(value) {
        if (is.null(value)) {
            return(NULL)
        }
        
        # Debug logging
        cat("DEBUG: safeInputValue - input type:", class(value), "length:", length(value), "\n")
        
        # CRITICAL: Strip all attributes first to avoid object issues
        # This ensures we're working with simple values only
        if (!is.null(value) && !is.function(value) && !is.environment(value)) {
            # For lists, process recursively
            if (is.list(value) && !is.data.frame(value)) {
                value <- lapply(value, function(x) {
                    if (is.atomic(x)) {
                        as.vector(x)  # Strip all attributes
                    } else {
                        x
                    }
                })
            } else if (is.atomic(value)) {
                value <- as.vector(value)  # Strip all attributes including names, dims, etc.
            }
        }
        
        # First, try to serialize and deserialize to catch any serialization issues
        tryCatch({
            # Test if the value can be serialized to JSON (which is what Shiny uses)
            json_value <- jsonlite::toJSON(value, auto_unbox = TRUE)
            parsed_value <- jsonlite::fromJSON(json_value)
            
            # If it's a simple value after JSON round-trip, use it
            if (is.atomic(parsed_value) && (is.character(parsed_value) || is.numeric(parsed_value) || is.logical(parsed_value))) {
                # Ensure it's a simple vector with no attributes
                result <- as.vector(parsed_value)
                cat("DEBUG: safeInputValue - JSON round-trip successful:", paste(result, collapse = ", "), "\n")
                return(result)
            }
        }, error = function(e) {
            cat("DEBUG: safeInputValue - JSON serialization failed:", e$message, "\n")
        })
        
        # Handle complex objects that might be JavaScript objects or other non-serializable types
        if (is.object(value) && !is.data.frame(value) && !is.factor(value)) {
            cat("DEBUG: safeInputValue - detected complex object, attempting to extract values\n")
            # Try to extract meaningful values from complex objects
            if (is.list(value)) {
                if (length(value) > 0) {
                    # Try to extract the first meaningful element
                    for (i in seq_along(value)) {
                        if (!is.null(value[[i]]) && (is.character(value[[i]]) || is.numeric(value[[i]]) || is.logical(value[[i]]))) {
                            # Test JSON serialization of extracted value
                            tryCatch({
                                json_test <- jsonlite::toJSON(value[[i]], auto_unbox = TRUE)
                                cat("DEBUG: safeInputValue - extracted element", i, ":", paste(value[[i]], collapse = ", "), "\n")
                                return(value[[i]])
                            }, error = function(e) {
                                cat("DEBUG: safeInputValue - extracted element", i, "failed JSON test:", e$message, "\n")
                            })
                        }
                    }
                }
            }
            # If we can't extract meaningful values, return NULL to avoid errors
            cat("DEBUG: safeInputValue - complex object could not be processed, returning NULL\n")
            return(NULL)
        }
        
        # If it's already a simple vector (character, numeric, logical), test JSON serialization
        if (is.vector(value) && !is.list(value) && (is.character(value) || is.numeric(value) || is.logical(value))) {
            tryCatch({
                json_test <- jsonlite::toJSON(value, auto_unbox = TRUE)
                # Ensure it's a simple vector with no attributes
                result <- as.vector(value)
                cat("DEBUG: safeInputValue - returning simple vector:", paste(result, collapse = ", "), "\n")
                return(result)
            }, error = function(e) {
                cat("DEBUG: safeInputValue - simple vector failed JSON test:", e$message, "\n")
                # Try to convert to character and test again
                tryCatch({
                    char_value <- as.vector(as.character(value))
                    json_test <- jsonlite::toJSON(char_value, auto_unbox = TRUE)
                    cat("DEBUG: safeInputValue - converted to character and passed JSON test:", paste(char_value, collapse = ", "), "\n")
                    return(char_value)
                }, error = function(e2) {
                    cat("DEBUG: safeInputValue - character conversion also failed:", e2$message, "\n")
                    return(NULL)
                })
            })
        }
        
        # If it's a list, try to extract meaningful values
        if (is.list(value)) {
            cat("DEBUG: safeInputValue - processing list with", length(value), "elements\n")
            if (length(value) > 0) {
                # If first element is a vector, use it
                if (is.vector(value[[1]]) && (is.character(value[[1]]) || is.numeric(value[[1]]) || is.logical(value[[1]]))) {
                    tryCatch({
                        json_test <- jsonlite::toJSON(value[[1]], auto_unbox = TRUE)
                        # Ensure it's a simple vector with no attributes
                        result <- as.vector(value[[1]])
                        cat("DEBUG: safeInputValue - returning first vector element:", paste(result, collapse = ", "), "\n")
                        return(result)
                    }, error = function(e) {
                        cat("DEBUG: safeInputValue - first vector element failed JSON test:", e$message, "\n")
                    })
                } else {
                    # Convert to character only if it's a simple type
                    tryCatch({
                        char_value <- as.vector(as.character(value[[1]]))
                        json_test <- jsonlite::toJSON(char_value, auto_unbox = TRUE)
                        cat("DEBUG: safeInputValue - converted to character:", paste(char_value, collapse = ", "), "\n")
                        return(char_value)
                    }, error = function(e) {
                        cat("DEBUG: safeInputValue - conversion failed:", e$message, "\n")
                        return(NULL)
                    })
                }
            }
        }
        
        # If it's a data frame, try to get column names
        if (is.data.frame(value)) {
            cat("DEBUG: safeInputValue - processing data frame with", ncol(value), "columns\n")
            if (ncol(value) > 0) {
                col_names <- colnames(value)
                tryCatch({
                    json_test <- jsonlite::toJSON(col_names, auto_unbox = TRUE)
                    cat("DEBUG: safeInputValue - returning column names:", paste(col_names, collapse = ", "), "\n")
                    return(col_names)
                }, error = function(e) {
                    cat("DEBUG: safeInputValue - column names failed JSON test:", e$message, "\n")
                    return(NULL)
                })
            }
        }
        
        # Fallback: convert to character with error handling
        tryCatch({
            char_value <- as.vector(as.character(value))
            json_test <- jsonlite::toJSON(char_value, auto_unbox = TRUE)
            cat("DEBUG: safeInputValue - fallback to character:", paste(char_value, collapse = ", "), "\n")
            return(char_value)
        }, error = function(e) {
            cat("DEBUG: safeInputValue - fallback conversion failed:", e$message, "\n")
            return(NULL)
        })
    }
    
    # Helper function to safely update Shiny inputs with error handling
    safeUpdateInput <- function(update_func, session, input_id, ...) {
        tryCatch({
            # Extract the arguments
            args <- list(...)
            
            # Validate all arguments that might be sent to the client
            validated_args <- list()
            for (arg_name in names(args)) {
                if (arg_name %in% c("selected", "value", "choices")) {
                    # These are the arguments that get sent to the client
                    validated_value <- validateForClient(args[[arg_name]])
                    if (!is.null(validated_value)) {
                        validated_args[[arg_name]] <- validated_value
                    } else {
                        cat("DEBUG: safeUpdateInput - skipping", arg_name, "due to validation failure\n")
                        return() # Skip this update entirely
                    }
                } else {
                    # Other arguments (like choices for selectize) can be complex
                    validated_args[[arg_name]] <- args[[arg_name]]
                }
            }
            
            # Call the update function with validated arguments
            do.call(update_func, c(list(session, input_id), validated_args))
        }, error = function(e) {
            cat("DEBUG: Failed to update", input_id, ":", e$message, "\n")
            # Don't propagate the error, just log it
        })
    }
    
    # Helper function to validate values before sending to client (SSL-safe)
    validateForClient <- function(value) {
        if (is.null(value)) {
            return(NULL)
        }
        
        # Test JSON serialization which is what Shiny uses for client communication
        tryCatch({
            # Test the exact serialization that Shiny will use
            json_value <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
            
            # Try to parse it back to ensure it's valid
            parsed_value <- jsonlite::fromJSON(json_value)
            
            # Additional check: ensure it's not a complex object
            if (is.atomic(parsed_value) || (is.list(parsed_value) && all(sapply(parsed_value, function(x) is.atomic(x) || is.null(x))))) {
                return(value)
            } else {
                cat("DEBUG: validateForClient - value contains non-atomic elements, returning NULL\n")
                return(NULL)
            }
        }, error = function(e) {
            cat("DEBUG: validateForClient - JSON serialization failed:", e$message, "\n")
            return(NULL)
        })
    }
    
    # Helper function to clean state object and remove problematic values
    cleanStateObject <- function(state_obj) {
        if (!is.list(state_obj)) {
            return(state_obj)
        }
        
        # Clean the saved_inputs section specifically
        if (!is.null(state_obj$saved_inputs) && is.list(state_obj$saved_inputs)) {
            cat("DEBUG: Cleaning saved_inputs section\n")
            cleaned_inputs <- list()
            
            for (input_name in names(state_obj$saved_inputs)) {
                input_value <- state_obj$saved_inputs[[input_name]]
                cleaned_value <- safeInputValue(input_value)
                
                if (!is.null(cleaned_value)) {
                    # Double-check with client validation
                    if (!is.null(validateForClient(cleaned_value))) {
                        cleaned_inputs[[input_name]] <- cleaned_value
                        cat("DEBUG: Cleaned input", input_name, "successfully\n")
                    } else {
                        cat("DEBUG: Input", input_name, "failed client validation, skipping\n")
                    }
                } else {
                    cat("DEBUG: Input", input_name, "could not be cleaned, skipping\n")
                }
            }
            
            state_obj$saved_inputs <- cleaned_inputs
        }
        
        return(state_obj)
    }
    
    # Helper function to get valid categorical factors from design formula
    getValidCategoricalFactors <- function(factorChoices, design_terms, metadata_df) {
    # First filter by design formula presence
    validFactorChoices <- factorChoices[factorChoices %in% design_terms]
    
    # Then check if they are actually categorical factors
    if (length(validFactorChoices) > 0) {
        actualFactors <- c()
        for (factor_name in validFactorChoices) {
            if (factor_name %in% colnames(metadata_df)) {
                factor_data <- metadata_df[, factor_name]
                factor_data_clean <- factor_data[!is.na(factor_data)]
                unique_count <- length(unique(factor_data_clean))
                data_type <- class(factor_data)[1]
                
                if (isCategoricalFactor(factor_data, nrow(metadata_df))) {
                    print(paste("✓ Factor", factor_name, "is categorical:", data_type, "with", unique_count, "levels"))
                    actualFactors <- c(actualFactors, factor_name)
                } else {
                    print(paste("✗ Skipping", factor_name, "-", data_type, "with", unique_count, "unique values (not suitable for categorical analysis)"))
                }
            }
        }
        validFactorChoices <- actualFactors
    }
    
    return(validFactorChoices)
}


stateExportServer <- function(id, root_input, root_output, root_session) {
  moduleServer(id, function(input, output, session) {
    input <- root_input
    output <- root_output
    session <- root_session
    myValues <- reactiveValues(selected_genes = 0)

    session_dir <- file.path(
        tempdir(),
        paste0("deseq2-session-", safe_file_name(session$token))
    )
    dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)

    # Add error handling for client-side communication issues
    session$onFlushed(function() {
        # This runs after the session is flushed to the client
        # Add any post-flush validation here if needed
    })
    
    # Add error handling for session errors
    session$onSessionEnded(function() {
        unlink(session_dir, recursive = TRUE, force = TRUE)
    })

    GotoTab <- function(name) {
        shinyjs::show(selector = paste0("a[data-value=\"", name, "\"]"))

        shinyjs::runjs("window.scrollTo(0, 0)")
    }
    js_api <- js
    factor_api <- list(
        is_categorical = isCategoricalFactor,
        get_valid = getValidCategoricalFactors
    )

    design_api <- designServer(
        "design", input, output, session, myValues, factor_api, js_api
    )
    input_api <- inputDataServer(
        "input_data", input, output, session, myValues, design_api, js_api
    )
    sva_api <- svaServer(
        "sva", input, output, session, myValues, design_api, js_api, GotoTab
    )
    deseq_api <- deseqServer(
        "deseq", input, output, session, myValues, session_dir, factor_api,
        js_api, GotoTab
    )
    results_api <- resultsServer(
        "results", input, output, session, myValues, session_dir, js_api
    )
    venn_api <- vennServer(
        "venn", input, output, session, myValues, session_dir, results_api
    )
    volcano_api <- volcanoServer(
        "volcano", input, output, session, myValues, session_dir, results_api
    )
    boxplot_api <- boxplotServer(
        "boxplot", input, output, session, myValues, session_dir,
        generate_random_colors
    )
    heatmap_api <- heatmapServer(
        "heatmap", input, output, session, myValues, session_dir
    )

    # Explicit child-module outputs consumed by state/export orchestration.
    filelist <- results_api$files
    contrast_specs <- results_api$contrast_specs
    custom_colors <- boxplot_api$colors
    selected_matrix <- venn_api$selected_matrix
    heatmapReactive <- heatmap_api$heatmap
    updateDesignFormula <- design_api$update_formula
    
    # State saving/loading functionality
    saveAppState <- function() {
        # Create a comprehensive state object with all important myValues components
        state_object <- list(
            # Core data
            dataCounts = myValues$dataCounts,
            fileContent = myValues$fileContent,
            DF = myValues$DF,
            conditions = myValues$conditions,
            
            # Gene information
            geneids = myValues$geneids,
            genenames = myValues$genenames,
            selected_genes = myValues$selected_genes,
            
            # DESeq2 objects
            dds = myValues$dds,
            ddsSva = myValues$ddsSva,
            ddsAddSV = myValues$ddsAddSV,
            
            # Transformation results
            rld = myValues$rld,
            rlogMat = myValues$rlogMat,
            rldColNames = myValues$rldColNames,
            vsd = myValues$vsd,
            vstMat = myValues$vstMat,
            vsdColNames = myValues$vsdColNames,
            vsdSva = myValues$vsdSva,
            
            # Analysis results
            vsResults = myValues$vsResults,
            status = myValues$status,
            
            # Plot-specific data and selections
            heatmap_path = NULL,
            
            # Volcano plot and Venn diagram data (saved analysis results for different datasets)
            filelist_file_list = if(exists("filelist") && !is.null(filelist$file_list)) {
                snapshot_saved_results(filelist$file_list)
            } else {
                NULL
            },
            contrast_specs = if(exists("contrast_specs") && !is.null(contrast_specs$specs)) {
                contrast_specs$specs
            } else {
                NULL
            },
            
            # Boxplot custom colors
            custom_colors_colors = if(exists("custom_colors") && !is.null(custom_colors$colors)) custom_colors$colors else NULL,
            custom_colors_globalcolors = if(exists("custom_colors") && !is.null(custom_colors$globalcolors)) custom_colors$globalcolors else NULL,
            
            # Venn diagram selected matrix (for selected genes from Venn intersections)
            selected_matrix_matrix = if(exists("selected_matrix") && !is.null(selected_matrix$matrix)) selected_matrix$matrix else NULL,
            
            # Current input selections for plots (capture key parameters)
            saved_inputs = list(
                # Volcano plot parameters
                volcano_significance_threshold = if(!is.null(input$significance_threshold)) safeInputValue(input$significance_threshold) else NULL,
                volcano_log_fold_change_threshold = if(!is.null(input$log_fold_change_threshold)) safeInputValue(input$log_fold_change_threshold) else NULL,
                volcano_threshold_type = if(!is.null(input$volcano_threshold_type)) safeInputValue(input$volcano_threshold_type) else NULL,
                volcano_direct_padj = if(!is.null(input$volcano_direct_padj)) safeInputValue(input$volcano_direct_padj) else NULL,
                select_avo_de_file = if(!is.null(input$select_avo_de_file)) safeInputValue(input$select_avo_de_file) else NULL,
                sig_genes_selection = if(!is.null(input$sig_genes_selection)) safeInputValue(input$sig_genes_selection) else NULL,
                
                # Venn diagram parameters
                venn_significance_threshold = if(!is.null(input$venn_significance_threshold)) safeInputValue(input$venn_significance_threshold) else NULL,
                venn_log_fold_change_threshold = if(!is.null(input$venn_log_fold_change_threshold)) safeInputValue(input$venn_log_fold_change_threshold) else NULL,
                venn_threshold_type = if(!is.null(input$venn_threshold_type)) safeInputValue(input$venn_threshold_type) else NULL,
                venn_direct_padj = if(!is.null(input$venn_direct_padj)) safeInputValue(input$venn_direct_padj) else NULL,
                venn_sig_genes_selection = if(!is.null(input$venn_sig_genes_selection)) safeInputValue(input$venn_sig_genes_selection) else NULL,
                select_avo_de_venn_files = if(!is.null(input$select_avo_de_venn_files)) safeInputValue(input$select_avo_de_venn_files) else NULL,
                venn_set_expression_input = if(!is.null(input$venn_set_expression_input)) safeInputValue(input$venn_set_expression_input) else NULL,
                venn_sel_gene_type = if(!is.null(input$venn_sel_gene_type)) safeInputValue(input$venn_sel_gene_type) else NULL,
                select_expression = if(!is.null(input$select_expression)) safeInputValue(input$select_expression) else NULL,
                venn_gene_list = if(!is.null(input$venn_gene_list)) safeInputValue(input$venn_gene_list) else NULL,
                venn_input_genes_sep = if(!is.null(input$venn_input_genes_sep)) safeInputValue(input$venn_input_genes_sep) else NULL,
                evaluateExpression = if(!is.null(input$evaluateExpression)) safeInputValue(input$evaluateExpression) else NULL,
                
                # Boxplot parameters
                boxplotFill = if(!is.null(input$boxplotFill)) safeInputValue(input$boxplotFill) else NULL,
                boxplotX = if(!is.null(input$boxplotX)) safeInputValue(input$boxplotX) else NULL,
                sel_gene = if(!is.null(input$sel_gene)) safeInputValue(input$sel_gene) else NULL,
                sel_groups = if(!is.null(input$sel_groups)) safeInputValue(input$sel_groups) else NULL,
                sel_factors = if(!is.null(input$sel_factors)) safeInputValue(input$sel_factors) else NULL,
                box_plot_sel_gene_type = if(!is.null(input$box_plot_sel_gene_type)) safeInputValue(input$box_plot_sel_gene_type) else NULL,
                levelSelect = if(!is.null(input$levelSelect)) safeInputValue(input$levelSelect) else NULL,
                levelColor = if(!is.null(input$levelColor)) safeInputValue(input$levelColor) else NULL,
                applyColor = if(!is.null(input$applyColor)) safeInputValue(input$applyColor) else NULL,
                
                # Heatmap parameters
                numGenes = if(!is.null(input$numGenes)) safeInputValue(input$numGenes) else NULL,
                subsetGenes = if(!is.null(input$subsetGenes)) safeInputValue(input$subsetGenes) else NULL,
                listPasteGenes = if(!is.null(input$listPasteGenes)) safeInputValue(input$listPasteGenes) else NULL,
                heatmap_sel_gene_type = if(!is.null(input$heatmap_sel_gene_type)) safeInputValue(input$heatmap_sel_gene_type) else NULL,
                heat_group = if(!is.null(input$heat_group)) safeInputValue(input$heat_group) else NULL,
                genHeatmap = if(!is.null(input$genHeatmap)) safeInputValue(input$genHeatmap) else NULL,
                
                # Additional UI state
                gene_alias = if(!is.null(input$gene_alias)) safeInputValue(input$gene_alias) else NULL,
                no_replicates = if(!is.null(input$no_replicates)) safeInputValue(input$no_replicates) else NULL,
                
                # Differential Expression Analysis parameters
                resultNameOrFactor = if(!is.null(input$resultNameOrFactor)) safeInputValue(input$resultNameOrFactor) else NULL,
                resultNamesInput = if(!is.null(input$resultNamesInput)) safeInputValue(input$resultNamesInput) else NULL,
                factorNameInput = if(!is.null(input$factorNameInput)) safeInputValue(input$factorNameInput) else NULL,
                condition1 = if(!is.null(input$condition1)) safeInputValue(input$condition1) else NULL,
                condition2 = if(!is.null(input$condition2)) safeInputValue(input$condition2) else NULL,
                getDiffResVs = if(!is.null(input$getDiffResVs)) safeInputValue(input$getDiffResVs) else NULL,
                
                # MA Plot parameters
                alpha = if(!is.null(input$alpha)) safeInputValue(input$alpha) else NULL,
                ylim = if(!is.null(input$ylim)) safeInputValue(input$ylim) else NULL,
                
                # Plot generation state
                plot_generation_status = list(
                    heatmap_generated = if(exists("input") && !is.null(input$genHeatmap)) input$genHeatmap > 0 else FALSE,
                    venn_generated = if(exists("input") && !is.null(input$plotVenn)) input$plotVenn > 0 else FALSE
                )
            ),
            
            # Metadata for state restoration
            save_timestamp = Sys.time(),
            app_version = "deseq2shiny_v2_portable_state"
        )
        
        return(state_object)
    }
    
    loadAppState <- function(state_object) {
        state_object <- validate_state_object(state_object)
        
        # Clean the state object to remove any problematic values
        state_object <- cleanStateObject(state_object)
        
        # Load core data components
        if (!is.null(state_object$dataCounts)) myValues$dataCounts <- state_object$dataCounts
        if (!is.null(state_object$fileContent)) myValues$fileContent <- state_object$fileContent
        if (!is.null(state_object$DF)) myValues$DF <- state_object$DF
        if (!is.null(state_object$conditions)) myValues$conditions <- state_object$conditions
        
        # Load gene information
        if (!is.null(state_object$geneids)) myValues$geneids <- state_object$geneids
        if (!is.null(state_object$genenames)) myValues$genenames <- state_object$genenames
        if (!is.null(state_object$selected_genes)) myValues$selected_genes <- state_object$selected_genes
        
        if (!is.null(state_object$dds)) myValues$dds <- state_object$dds
        if (!is.null(state_object$ddsSva)) myValues$ddsSva <- state_object$ddsSva
        if (!is.null(state_object$ddsAddSV)) myValues$ddsAddSV <- state_object$ddsAddSV
        
        if (!is.null(state_object$rld)) myValues$rld <- state_object$rld
        if (!is.null(state_object$rlogMat)) myValues$rlogMat <- state_object$rlogMat
        if (!is.null(state_object$rldColNames)) myValues$rldColNames <- state_object$rldColNames
        if (!is.null(state_object$vsd)) myValues$vsd <- state_object$vsd
        if (!is.null(state_object$vstMat)) myValues$vstMat <- state_object$vstMat
        if (!is.null(state_object$vsdColNames)) myValues$vsdColNames <- state_object$vsdColNames
        if (!is.null(state_object$vsdSva)) myValues$vsdSva <- state_object$vsdSva
        
        if (!is.null(state_object$vsResults)) myValues$vsResults <- state_object$vsResults
        if (!is.null(state_object$status)) myValues$status <- state_object$status
        if (!is.null(state_object$heatmap_path)) myValues$heatmap_path <- state_object$heatmap_path
        
        # Restore plot-specific data and components
        
        # Restore volcano plot and Venn diagram data files
        if (!is.null(state_object$filelist_file_list)) {
            if (!exists("filelist")) {
                filelist <<- reactiveValues()
            }
            filelist$file_list <<- materialize_saved_results(
                state_object$filelist_file_list,
                session_dir
            )
            state_object$filelist_file_list <- filelist$file_list
        }

        if (!is.null(state_object$contrast_specs)) {
            if (!exists("contrast_specs")) {
                contrast_specs <<- reactiveValues()
            }
            contrast_specs$specs <<- state_object$contrast_specs
        }
        
        # Restore boxplot custom colors
        if (!is.null(state_object$custom_colors_colors)) {
            if (!exists("custom_colors")) {
                custom_colors <<- reactiveValues()
            }
            custom_colors$colors <<- state_object$custom_colors_colors
        }
        if (!is.null(state_object$custom_colors_globalcolors)) {
            if (!exists("custom_colors")) {
                custom_colors <<- reactiveValues()
            }
            custom_colors$globalcolors <<- state_object$custom_colors_globalcolors
        }
        
        # Restore Venn diagram selected matrix
        if (!is.null(state_object$selected_matrix_matrix)) {
            if (!exists("selected_matrix")) {
                selected_matrix <<- reactiveValues()
            }
            selected_matrix$matrix <<- state_object$selected_matrix_matrix
        }
        
        # Update UI elements based on loaded state
        updateUIAfterStateLoad(state_object)
        
        # Restore all input parameters immediately with error handling
        restoreInputParameters(state_object)
        
        # Try immediate restoration of boxplot parameters (may fail if choices not ready)
        tryCatch({
            cat("DEBUG: Attempting immediate boxplot restoration\n")
            
            # Restore sel_gene with enhanced validation
            if (!is.null(state_object$saved_inputs$sel_gene)) {
                cat("DEBUG: Original sel_gene:", paste(state_object$saved_inputs$sel_gene, collapse = ", "), "\n")
                sel_gene_safe <- safeInputValue(state_object$saved_inputs$sel_gene)
                if (!is.null(sel_gene_safe) && !is.null(validateForClient(sel_gene_safe))) {
                    cat("DEBUG: Updating sel_gene with:", paste(sel_gene_safe, collapse = ", "), "\n")
                    safeUpdateInput(updateSelectizeInput, session, "sel_gene", selected = sel_gene_safe)
                } else {
                    cat("DEBUG: sel_gene_safe failed validation, skipping update\n")
                }
            }
            
            # Restore sel_groups with enhanced validation
            if (!is.null(state_object$saved_inputs$sel_groups)) {
                cat("DEBUG: Original sel_groups:", paste(state_object$saved_inputs$sel_groups, collapse = ", "), "\n")
                sel_groups_safe <- safeInputValue(state_object$saved_inputs$sel_groups)
                if (!is.null(sel_groups_safe) && !is.null(validateForClient(sel_groups_safe))) {
                    cat("DEBUG: Updating sel_groups with:", paste(sel_groups_safe, collapse = ", "), "\n")
                    safeUpdateInput(updateSelectizeInput, session, "sel_groups", selected = sel_groups_safe)
                } else {
                    cat("DEBUG: sel_groups_safe failed validation, skipping update\n")
                }
            }
            
            # Restore sel_factors with enhanced validation
            if (!is.null(state_object$saved_inputs$sel_factors)) {
                cat("DEBUG: Original sel_factors:", paste(state_object$saved_inputs$sel_factors, collapse = ", "), "\n")
                sel_factors_safe <- safeInputValue(state_object$saved_inputs$sel_factors)
                if (!is.null(sel_factors_safe) && !is.null(validateForClient(sel_factors_safe))) {
                    cat("DEBUG: Updating sel_factors with:", paste(sel_factors_safe, collapse = ", "), "\n")
                    safeUpdateInput(updateSelectizeInput, session, "sel_factors", selected = sel_factors_safe)
                } else {
                    cat("DEBUG: sel_factors_safe failed validation, skipping update\n")
                }
            }
        }, error = function(e) {
            cat("DEBUG: Immediate restoration failed (expected):", e$message, "\n")
        })
        
        # Dynamic parameters are now restored within restoreInputParameters function
        
        # Force trigger DE analysis display if vsResults exists
        if (!is.null(myValues$vsResults)) {
            # Navigate to results tab to trigger reactive updates
            updateTabItems(session, "tabs", "resultsTab")
            
            # Also trigger DE analysis regeneration if it was previously run
            if (!is.null(state_object$saved_inputs$getDiffResVs) && state_object$saved_inputs$getDiffResVs > 0) {
                cat("DEBUG: DE analysis was previously run, getDiffResVs count:", state_object$saved_inputs$getDiffResVs, "\n")
                
                # Force trigger the DE analysis reactive
                de_analysis_restore <- reactiveValues(done = FALSE)
                de_analysis_attempts <- reactiveValues(count = 0)
                observe({
                    if (!de_analysis_restore$done && de_analysis_attempts$count < 15) {
                        invalidateLater(1000)  # Check every second
                        isolate({
                            de_analysis_attempts$count <- de_analysis_attempts$count + 1
                            cat("DEBUG: DE analysis trigger attempt", de_analysis_attempts$count, "\n")
                            cat("DEBUG: Current DE inputs - factorNameInput:", input$factorNameInput, "condition1:", input$condition1, "condition2:", input$condition2, "\n")
                            cat("DEBUG: resultNameOrFactor:", input$resultNameOrFactor, "\n")
                            
                            # Check if we should use Result Names or Factors method
                            analysis_ready <- FALSE
                            
                            if (!is.null(input$resultNameOrFactor)) {
                                if (input$resultNameOrFactor == "Result Names") {
                                    # Check if result names are properly restored
                                    if (!is.null(input$resultNamesInput) && length(input$resultNamesInput) > 0) {
                                        cat("DEBUG: Result Names method ready\n")
                                        analysis_ready <- TRUE
                                    }
                                } else if (input$resultNameOrFactor == "Factors") {
                                    # Check if conditions are properly restored
                                    if (!is.null(input$factorNameInput) && 
                                        !is.null(input$condition1) && 
                                        !is.null(input$condition2) && 
                                        input$condition1 != input$condition2 &&
                                        input$condition1 != "" && input$condition2 != "") {
                                        cat("DEBUG: Factors method ready\n")
                                        analysis_ready <- TRUE
                                    }
                                }
                            }
                            
                            if (analysis_ready) {
                                cat("DEBUG: Analysis inputs ready, proceeding with DE analysis\n")
                                
                                # Use shinyjs to trigger a click on the DE analysis button
                                tryCatch({
                                    shinyjs::click("getDiffResVs")
                                    cat("DEBUG: Simulated getDiffResVs button click\n")
                                }, error = function(e) {
                                    cat("DEBUG: DE button click failed:", e$message, "\n")
                                })
                                
                                showNotification(
                                    "DE analysis regeneration triggered.",
                                    type = "default", duration = 3
                                )
                                
                                de_analysis_restore$done <- TRUE
                                
                            } else if (de_analysis_attempts$count >= 15) {
                                cat("DEBUG: Giving up on DE analysis trigger after 15 attempts\n")
                                cat("DEBUG: Final state - factorNameInput:", input$factorNameInput, "condition1:", input$condition1, "condition2:", input$condition2, "\n")
                                showNotification(
                                    "DE analysis inputs not fully restored. Please check the DE Results tab and run analysis manually.",
                                    type = "warning", duration = 8
                                )
                                de_analysis_restore$done <- TRUE
                            } else {
                                cat("DEBUG: Waiting for conditions to be properly restored...\n")
                            }
                        })
                    }
                })
            }
        }
        
        # Ensure heatmap data is ready for regeneration
        if (!is.null(state_object$saved_inputs$genHeatmap) && state_object$saved_inputs$genHeatmap > 0) {
            # Check if required data for heatmap is available
            if (!is.null(myValues$dds) && !is.null(myValues$DF)) {
                showNotification(
                    "Heatmap parameters restored. Visit the Heatmap tab to see your plot.",
                    type = "message", duration = 4
                )
            } else {
                showNotification(
                    "Heatmap parameters restored, but some data is missing. Please check your DESeq2 analysis.",
                    type = "warning", duration = 6
                )
            }
        }
        
        # Trigger plot regeneration after state restoration
        triggerPlotRegeneration(state_object)
        
        # Schedule delayed restoration of dynamic parameters after reactive observers run
        # Use a reactive approach to wait for the data to be loaded
        delayed_restore <- reactiveValues(triggered = FALSE)
        
        # Create a global restoration state that observers can check
        if (!exists("restoration_state")) {
            restoration_state <<- reactiveValues(
                in_progress = FALSE,
                saved_sel_gene = NULL,
                saved_sel_groups = NULL,
                saved_sel_factors = NULL,
                boxplot_restoration_done = FALSE
            )
        }
        
        # Set restoration state
        restoration_state$in_progress <- TRUE
        restoration_state$saved_sel_gene <- state_object$saved_inputs$sel_gene
        restoration_state$saved_sel_groups <- state_object$saved_inputs$sel_groups
        restoration_state$saved_sel_factors <- state_object$saved_inputs$sel_factors
        restoration_state$boxplot_restoration_done <- FALSE
        
        observe({
            # Check if the required data is available and we haven't triggered restoration yet
            if (!is.null(myValues$dataCounts) && !is.null(myValues$DF) && !delayed_restore$triggered) {
                cat("DEBUG: Scheduling delayed restoration in 2 seconds\n")
                # Longer delay to allow observers to populate choices and settle
                invalidateLater(2000)
                isolate({
                    cat("DEBUG: Executing delayed restoration now\n")
                    restoreDelayedInputParameters(state_object)
                    
                    # Add an additional short delay to ensure restoration takes effect
                    # before any observers might run again
                    final_restore <- reactiveValues(done = FALSE)
                    observe({
                        if (!final_restore$done) {
                            invalidateLater(500)
                            isolate({
                                cat("DEBUG: Final restoration check - re-applying selections\n")
                                sel_gene_safe <- safeInputValue(state_object$saved_inputs$sel_gene)
                                if (!is.null(sel_gene_safe)) {
                                    updateSelectizeInput(session, "sel_gene", selected = sel_gene_safe)
                                }
                                sel_groups_safe <- safeInputValue(state_object$saved_inputs$sel_groups)
                                if (!is.null(sel_groups_safe)) {
                                    updateSelectizeInput(session, "sel_groups", selected = sel_groups_safe)
                                }
                                sel_factors_safe <- safeInputValue(state_object$saved_inputs$sel_factors)
                                if (!is.null(sel_factors_safe)) {
                                    updateSelectizeInput(session, "sel_factors", selected = sel_factors_safe)
                                }
                                # Clear restoration state
                                restoration_state$in_progress <- FALSE
                                restoration_state$boxplot_restoration_done <- TRUE
                                final_restore$done <- TRUE
                                cat("DEBUG: Restoration state cleared - observers can now run normally\n")
                            })
                        }
                    })
                    
                    delayed_restore$triggered <- TRUE
                })
            }
        })
        
        showNotification(
            paste("State loaded successfully from", format(state_object$save_timestamp, "%Y-%m-%d %H:%M:%S")),
            type = "default", duration = 5
        )
        
        return(TRUE)
    }
    
    restoreInputParameters <- function(state_object) {
        # Restore input parameters for plots
        if (!is.null(state_object$saved_inputs)) {
            inputs <- state_object$saved_inputs
            
            # Restore volcano plot parameters
            if (!is.null(inputs$volcano_significance_threshold)) {
                safeUpdateInput(updateNumericInput, session, "significance_threshold", value = inputs$volcano_significance_threshold)
            }
            if (!is.null(inputs$volcano_log_fold_change_threshold)) {
                safeUpdateInput(updateNumericInput, session, "log_fold_change_threshold", value = inputs$volcano_log_fold_change_threshold)
            }
            if (!is.null(inputs$volcano_threshold_type)) {
                safeUpdateInput(updateRadioButtons, session, "volcano_threshold_type", selected = inputs$volcano_threshold_type)
            }
            if (!is.null(inputs$volcano_direct_padj)) {
                safeUpdateInput(updateNumericInput, session, "volcano_direct_padj", value = inputs$volcano_direct_padj)
            }
            if (!is.null(inputs$select_avo_de_file) && !is.null(filelist$file_list)) {
                select_avo_de_file_safe <- safeInputValue(inputs$select_avo_de_file)
                if (!is.null(select_avo_de_file_safe)) {
                    safeUpdateInput(updateSelectInput, session, "select_avo_de_file", selected = select_avo_de_file_safe)
                }
            }
            if (!is.null(inputs$sig_genes_selection)) {
                safeUpdateInput(updateRadioButtons, session, "sig_genes_selection", selected = inputs$sig_genes_selection)
            }
            
            # Restore Venn diagram parameters
            if (!is.null(inputs$venn_significance_threshold)) {
                safeUpdateInput(updateNumericInput, session, "venn_significance_threshold", value = inputs$venn_significance_threshold)
            }
            if (!is.null(inputs$venn_log_fold_change_threshold)) {
                safeUpdateInput(updateNumericInput, session, "venn_log_fold_change_threshold", value = inputs$venn_log_fold_change_threshold)
            }
            if (!is.null(inputs$venn_threshold_type)) {
                safeUpdateInput(updateRadioButtons, session, "venn_threshold_type", selected = inputs$venn_threshold_type)
            }
            if (!is.null(inputs$venn_direct_padj)) {
                safeUpdateInput(updateNumericInput, session, "venn_direct_padj", value = inputs$venn_direct_padj)
            }
            if (!is.null(inputs$venn_sig_genes_selection)) {
                venn_sig_genes_selection_safe <- safeInputValue(inputs$venn_sig_genes_selection)
                if (!is.null(venn_sig_genes_selection_safe)) {
                    safeUpdateInput(updateSelectInput, session, "venn_sig_genes_selection", selected = venn_sig_genes_selection_safe)
                }
            }
            # Note: select_avo_de_venn_files is dynamic and restored in restoreDelayedInputParameters
            if (!is.null(inputs$venn_set_expression_input)) {
                venn_set_expression_input_safe <- safeInputValue(inputs$venn_set_expression_input)
                if (!is.null(venn_set_expression_input_safe)) {
                    safeUpdateInput(updateTextInput, session, "venn_set_expression_input", value = venn_set_expression_input_safe)
                }
            }
            if (!is.null(inputs$venn_sel_gene_type)) {
                safeUpdateInput(updateRadioButtons, session, "venn_sel_gene_type", selected = inputs$venn_sel_gene_type)
            }
            if (!is.null(inputs$select_expression)) {
                select_expression_safe <- safeInputValue(inputs$select_expression)
                if (!is.null(select_expression_safe)) {
                    safeUpdateInput(updateSelectInput, session, "select_expression", selected = select_expression_safe)
                }
            }
            if (!is.null(inputs$venn_gene_list)) {
                venn_gene_list_safe <- safeInputValue(inputs$venn_gene_list)
                if (!is.null(venn_gene_list_safe)) {
                    safeUpdateInput(updateTextAreaInput, session, "venn_gene_list", value = venn_gene_list_safe)
                }
            }
            if (!is.null(inputs$venn_input_genes_sep)) {
                safeUpdateInput(updateRadioButtons, session, "venn_input_genes_sep", selected = inputs$venn_input_genes_sep)
            }
            # Note: evaluateExpression is an action button, no update needed
            
            # Restore boxplot parameters (static ones only - dynamic ones handled later)
            if (!is.null(inputs$box_plot_sel_gene_type)) {
                safeUpdateInput(updateRadioButtons, session, "box_plot_sel_gene_type", selected = inputs$box_plot_sel_gene_type)
            }
            if (!is.null(inputs$levelColor)) {
                safeUpdateInput(updateColourInput, session, "levelColor", value = inputs$levelColor)
            }
            # Note: Dynamic boxplot inputs (sel_gene, sel_groups, etc.) are restored in restoreDelayedInputParameters
            
            # Restore heatmap parameters
            if (!is.null(inputs$numGenes)) {
                safeUpdateInput(updateNumericInput, session, "numGenes", value = inputs$numGenes)
            }
            if (!is.null(inputs$subsetGenes)) {
                safeUpdateInput(updateCheckboxInput, session, "subsetGenes", value = inputs$subsetGenes)
            }
            if (!is.null(inputs$listPasteGenes)) {
                listPasteGenes_safe <- safeInputValue(inputs$listPasteGenes)
                if (!is.null(listPasteGenes_safe)) {
                    safeUpdateInput(updateTextAreaInput, session, "listPasteGenes", value = listPasteGenes_safe)
                }
            }
            if (!is.null(inputs$heatmap_sel_gene_type)) {
                safeUpdateInput(updateRadioButtons, session, "heatmap_sel_gene_type", selected = inputs$heatmap_sel_gene_type)
            }
            if (!is.null(inputs$heat_group)) {
                heat_group_safe <- safeInputValue(inputs$heat_group)
                if (!is.null(heat_group_safe)) {
                    safeUpdateInput(updateSelectizeInput, session, "heat_group", selected = heat_group_safe)
                }
            }
            
            # Restore global UI state
            if (!is.null(inputs$gene_alias)) {
                safeUpdateInput(updateRadioButtons, session, "gene_alias", selected = inputs$gene_alias)
            }
            if (!is.null(inputs$no_replicates)) {
                safeUpdateInput(updateCheckboxInput, session, "no_replicates", value = inputs$no_replicates)
            }
            
            # Restore Differential Expression Analysis parameters (static ones only)
            if (!is.null(inputs$resultNameOrFactor)) {
                safeUpdateInput(updateRadioButtons, session, "resultNameOrFactor", selected = inputs$resultNameOrFactor)
            }
            if (!is.null(inputs$resultNamesInput)) {
                resultNamesInput_safe <- safeInputValue(inputs$resultNamesInput)
                if (!is.null(resultNamesInput_safe)) {
                    safeUpdateInput(updateSelectizeInput, session, "resultNamesInput", selected = resultNamesInput_safe)
                }
            }
            # Note: Dynamic DE inputs (factorNameInput, condition1, condition2) are restored in restoreDelayedInputParameters
            
            # Restore MA Plot parameters
            if (!is.null(inputs$alpha)) {
                safeUpdateInput(updateSliderInput, session, "alpha", value = inputs$alpha)
            }
            if (!is.null(inputs$ylim)) {
                safeUpdateInput(updateNumericInput, session, "ylim", value = inputs$ylim)
            }
            
            # Also try to restore dynamic parameters here (with error handling)
            tryCatch({
                # Restore boxplot dynamic selections
                sel_gene_safe <- safeInputValue(inputs$sel_gene)
                if (!is.null(sel_gene_safe)) {
                    safeUpdateInput(updateSelectizeInput, session, "sel_gene", selected = sel_gene_safe)
                }
                sel_groups_safe <- safeInputValue(inputs$sel_groups)
                if (!is.null(sel_groups_safe)) {
                    safeUpdateInput(updateSelectizeInput, session, "sel_groups", selected = sel_groups_safe)
                }
                sel_factors_safe <- safeInputValue(inputs$sel_factors)
                if (!is.null(sel_factors_safe)) {
                    safeUpdateInput(updateSelectizeInput, session, "sel_factors", selected = sel_factors_safe)
                }
                if (!is.null(inputs$boxplotX)) {
                    boxplotX_safe <- safeInputValue(inputs$boxplotX)
                    if (!is.null(boxplotX_safe)) {
                        safeUpdateInput(updateSelectInput, session, "boxplotX", selected = boxplotX_safe)
                    }
                }
                if (!is.null(inputs$boxplotFill)) {
                    boxplotFill_safe <- safeInputValue(inputs$boxplotFill)
                    if (!is.null(boxplotFill_safe)) {
                        safeUpdateInput(updateSelectInput, session, "boxplotFill", selected = boxplotFill_safe)
                    }
                }
                if (!is.null(inputs$levelSelect)) {
                    levelSelect_safe <- safeInputValue(inputs$levelSelect)
                    if (!is.null(levelSelect_safe)) {
                        safeUpdateInput(updateSelectInput, session, "levelSelect", selected = levelSelect_safe)
                    }
                }
                
                # Restore DE analysis dynamic selections
                factorNameInput_safe <- safeInputValue(inputs$factorNameInput)
                if (!is.null(factorNameInput_safe)) {
                    safeUpdateInput(updateSelectizeInput, session, "factorNameInput", selected = factorNameInput_safe)
                }
                if (!is.null(inputs$condition1)) {
                    condition1_safe <- safeInputValue(inputs$condition1)
                    if (!is.null(condition1_safe)) {
                        safeUpdateInput(updateSelectInput, session, "condition1", selected = condition1_safe)
                    }
                }
                if (!is.null(inputs$condition2)) {
                    condition2_safe <- safeInputValue(inputs$condition2)
                    if (!is.null(condition2_safe)) {
                        safeUpdateInput(updateSelectInput, session, "condition2", selected = condition2_safe)
                    }
                }
                resultNamesInput_safe <- safeInputValue(inputs$resultNamesInput)
                if (!is.null(resultNamesInput_safe)) {
                    safeUpdateInput(updateSelectizeInput, session, "resultNamesInput", selected = resultNamesInput_safe)
                }
                
                # Restore Venn diagram dynamic selections
                if (!is.null(inputs$select_avo_de_venn_files)) {
                    select_avo_de_venn_files_safe <- safeInputValue(inputs$select_avo_de_venn_files)
                    if (!is.null(select_avo_de_venn_files_safe)) {
                        safeUpdateInput(updateSelectInput, session, "select_avo_de_venn_files", selected = select_avo_de_venn_files_safe)
                    }
                }
            }, error = function(e) {
                # Dynamic restoration may fail if choices aren't populated yet - that's okay
                cat("DEBUG: Dynamic restoration failed (expected):", e$message, "\n")
            })
        }
    }
    
    # Function to restore inputs that depend on dynamic choices
    restoreDelayedInputParameters <- function(state_object) {
        if (is.null(state_object$saved_inputs)) return()
        
        # Add safety check for session context
        if (!exists("session") || is.null(session)) return()
        
        inputs <- state_object$saved_inputs
        
        # Show notification with error handling
        tryCatch({
            showNotification(
                "Restoring dynamic selections...",
                type = "default", duration = 3
            )
        }, error = function(e) {
            # Silently continue if notification fails
        })
        
        # Restore boxplot selections with dedicated observer to avoid conflicts
        if (!is.null(inputs$sel_gene) || !is.null(inputs$sel_groups) || !is.null(inputs$sel_factors)) {
            boxplot_restore_observer <- reactiveValues(done = FALSE, attempts = 0)
            
            observe({
                if (!boxplot_restore_observer$done && boxplot_restore_observer$attempts < 10) {
                    invalidateLater(1000)  # Check every second
                    isolate({
                        boxplot_restore_observer$attempts <- boxplot_restore_observer$attempts + 1
                        cat("DEBUG: Boxplot restoration attempt", boxplot_restore_observer$attempts, "\n")
                        
                        # Check if data is ready
                        if (!is.null(myValues$dataCounts) && !is.null(myValues$DF)) {
                            cat("DEBUG: Data is ready, attempting boxplot restoration\n")
                            
                            tryCatch({
                                # Restore sel_gene
                                if (!is.null(inputs$sel_gene)) {
                                    sel_gene_safe <- safeInputValue(inputs$sel_gene)
                                    cat("DEBUG: Restoring sel_gene (safe):", paste(sel_gene_safe, collapse = ", "), "\n")
                                    
                                    # Check if gene type is set correctly first
                                    if (!is.null(inputs$box_plot_sel_gene_type)) {
                                        if (inputs$box_plot_sel_gene_type == "gene.name") {
                                            genenames <- as.vector(myValues$genenames[rownames(myValues$dataCounts), ])
                                            if (all(sel_gene_safe %in% genenames)) {
                                                updateSelectizeInput(session, "sel_gene", 
                                                    choices = genenames,
                                                    selected = sel_gene_safe,
                                                    server = TRUE
                                                )
                                                cat("DEBUG: sel_gene restored successfully\n")
                                            } else {
                                                cat("DEBUG: sel_gene values not found in genenames\n")
                                            }
                                        } else {
                                            gene_ids <- as.vector(rownames(myValues$dataCounts))
                                            if (all(sel_gene_safe %in% gene_ids)) {
                                                updateSelectizeInput(session, "sel_gene", 
                                                    choices = gene_ids,
                                                    selected = sel_gene_safe,
                                                    server = TRUE
                                                )
                                                cat("DEBUG: sel_gene restored successfully\n")
                                            } else {
                                                cat("DEBUG: sel_gene values not found in gene_ids\n")
                                            }
                                        }
                                    }
                                }
                                
                                # Restore sel_groups
                                if (!is.null(inputs$sel_groups)) {
                                    sel_groups_safe <- safeInputValue(inputs$sel_groups)
                                    cat("DEBUG: Restoring sel_groups (safe):", paste(sel_groups_safe, collapse = ", "), "\n")
                                    
                                    # Get available groups from DF columns
                                    available_groups <- as.vector(colnames(myValues$DF))
                                    if (all(sel_groups_safe %in% available_groups)) {
                                        updateSelectizeInput(session, "sel_groups", 
                                            choices = available_groups,
                                            selected = sel_groups_safe,
                                            server = TRUE
                                        )
                                        cat("DEBUG: sel_groups restored successfully\n")
                                    } else {
                                        cat("DEBUG: sel_groups values not found in available groups\n")
                                    }
                                }
                                
                                # Restore sel_factors
                                if (!is.null(inputs$sel_factors)) {
                                    sel_factors_safe <- safeInputValue(inputs$sel_factors)
                                    cat("DEBUG: Restoring sel_factors (safe):", paste(sel_factors_safe, collapse = ", "), "\n")
                                    
                                    # Get available factors from DF columns
                                    available_factors <- as.vector(colnames(myValues$DF))
                                    if (all(sel_factors_safe %in% available_factors)) {
                                        updateSelectizeInput(session, "sel_factors", 
                                            choices = available_factors,
                                            selected = sel_factors_safe,
                                            server = TRUE
                                        )
                                        cat("DEBUG: sel_factors restored successfully\n")
                                    } else {
                                        cat("DEBUG: sel_factors values not found in available factors\n")
                                    }
                                }
                                
                                # Mark as done
                                restoration_state$boxplot_restoration_done <- TRUE
                                boxplot_restore_observer$done <- TRUE
                                cat("DEBUG: Boxplot restoration completed successfully\n")
                                
                            }, error = function(e) {
                                cat("DEBUG: Boxplot restoration attempt failed:", e$message, "\n")
                            })
                        }
                        
                        # Give up after 10 attempts
                        if (boxplot_restore_observer$attempts >= 10) {
                            cat("DEBUG: Giving up on boxplot restoration after 10 attempts\n")
                            boxplot_restore_observer$done <- TRUE
                        }
                    })
                }
            })
        }
        if (!is.null(inputs$boxplotX)) {
            boxplotX_safe <- safeInputValue(inputs$boxplotX)
            updateSelectInput(session, "boxplotX", selected = boxplotX_safe)
        }
        if (!is.null(inputs$boxplotFill)) {
            boxplotFill_safe <- safeInputValue(inputs$boxplotFill)
            updateSelectInput(session, "boxplotFill", selected = boxplotFill_safe)
        }
        if (!is.null(inputs$levelSelect)) {
            levelSelect_safe <- safeInputValue(inputs$levelSelect)
            updateSelectInput(session, "levelSelect", selected = levelSelect_safe)
        }
        
        # Restore DE analysis selections (these depend on myValues$dds)
        # First restore factorNameInput, which will trigger condition choices to be populated
        if (!is.null(inputs$factorNameInput)) {
            factorNameInput_safe <- safeInputValue(inputs$factorNameInput)
            cat("DEBUG: Restoring factorNameInput:", factorNameInput_safe, "\n")
            updateSelectizeInput(session, "factorNameInput", selected = factorNameInput_safe)
        }
        
        # Schedule condition restoration with multiple attempts and active monitoring
        if (!is.null(inputs$condition1) || !is.null(inputs$condition2)) {
            de_condition_restore <- reactiveValues(done = FALSE, attempts = 0)
            
            observe({
                if (!de_condition_restore$done && de_condition_restore$attempts < 10) {
                    invalidateLater(500)  # Check every 500ms
                    isolate({
                        de_condition_restore$attempts <- de_condition_restore$attempts + 1
                        cat("DEBUG: Condition restoration attempt", de_condition_restore$attempts, "\n")
                        
                        # Check if condition choices are available
                        current_condition1_choices <- NULL
                        current_condition2_choices <- NULL
                        
                        # Try to get the current choices (this might fail if not ready)
                        tryCatch({
                            # Force the factorNameInput observer to run by manually updating it
                            if (!is.null(inputs$factorNameInput) && !is.null(myValues$DF)) {
                                # Ensure DF columns are factors
                                myValues$DF[] <- lapply(myValues$DF, as.factor)
                                
                                if (inputs$factorNameInput %in% colnames(myValues$DF)) {
                                    factor_levels <- as.vector(levels(myValues$DF[, inputs$factorNameInput]))
                                    cat("DEBUG: Available factor levels for", inputs$factorNameInput, ":", paste(factor_levels, collapse = ", "), "\n")
                                    
                                    # Manually update the condition choices
                                    updateSelectInput(session, "condition1", choices = factor_levels)
                                    updateSelectInput(session, "condition2", choices = factor_levels)
                                    
                                    # Now restore the selected values
                                    if (!is.null(inputs$condition1) && inputs$condition1 %in% factor_levels) {
                                        condition1_safe <- safeInputValue(inputs$condition1)
                                        cat("DEBUG: Restoring condition1:", condition1_safe, "\n")
                                        updateSelectInput(session, "condition1", selected = condition1_safe)
                                    }
                                    if (!is.null(inputs$condition2) && inputs$condition2 %in% factor_levels) {
                                        condition2_safe <- safeInputValue(inputs$condition2)
                                        cat("DEBUG: Restoring condition2:", condition2_safe, "\n")
                                        updateSelectInput(session, "condition2", selected = condition2_safe)
                                    }
                                    
                                    # Mark as done if both conditions are valid
                                    if (!is.null(inputs$condition1) && !is.null(inputs$condition2) && 
                                        inputs$condition1 %in% factor_levels && inputs$condition2 %in% factor_levels &&
                                        inputs$condition1 != inputs$condition2) {
                                        cat("DEBUG: Condition restoration successful\n")
                                        de_condition_restore$done <- TRUE
                                    }
                                }
                            }
                        }, error = function(e) {
                            cat("DEBUG: Condition restoration attempt failed:", e$message, "\n")
                        })
                        
                        # Give up after 10 attempts (5 seconds)
                        if (de_condition_restore$attempts >= 10) {
                            cat("DEBUG: Giving up on condition restoration after 10 attempts\n")
                            de_condition_restore$done <- TRUE
                        }
                    })
                }
            })
        }
        
        if (!is.null(inputs$resultNamesInput)) {
            resultNamesInput_safe <- safeInputValue(inputs$resultNamesInput)
            updateSelectizeInput(session, "resultNamesInput", selected = resultNamesInput_safe)
        }
        
        # Restore Venn diagram dynamic selections
            # Note: select_avo_de_venn_files is dynamic and restored in restoreDelayedInputParameters
        
        # Show final notification with error handling
        tryCatch({
            showNotification(
                "All selections restored! Visit each tab to see your restored plots.",
                type = "default", duration = 5
            )
        }, error = function(e) {
            # Silently continue if notification fails
        })
    }
    
    updateUIAfterStateLoad <- function(state_object = NULL) {
        # Show/hide appropriate tabs based on loaded state
        if (!is.null(myValues$dds)) {
            shinyjs::show(selector = "a[data-value=\"conditionsTab\"]")
            shinyjs::show(selector = "a[data-value=\"deseqTab\"]")
            shinyjs::show(selector = "a[data-value=\"resultsTab\"]")
            shinyjs::show(selector = "a[data-value=\"boxplotTab\"]")
            shinyjs::show(selector = "a[data-value=\"heatmapTab\"]")
            shinyjs::show(selector = "a[data-value=\"vstTab\"]")
            
            if (!is.null(myValues$rld)) {
                shinyjs::show(selector = "a[data-value=\"rlogTab\"]")
            }
            
            # Update design formula if DF exists
            if (!is.null(myValues$DF)) {
                updateDesignFormula()
            }
            
            # Update factor choices for analysis
            if (!is.null(myValues$dds)) {
                factorChoices <- as.vector(colnames(colData(myValues$dds)))
                factorChoices <- factorChoices[!grepl("^SV[::digit::]*", factorChoices)]
                factorChoices <- factorChoices[!(factorChoices %in% c("sizeFactor", "replaceable"))]
                
                updateSelectInput(session, "rlogIntGroupsInput", choices = factorChoices, selected = factorChoices[1])
                updateSelectInput(session, "vsdIntGroupsInput", choices = factorChoices, selected = factorChoices[1])
                updateSelectizeInput(session, "resultNamesInput", choices = as.vector(resultsNames(myValues$dds)), selected = NULL)
                updateSelectizeInput(session, "factorNameInput", choices = factorChoices, selected = factorChoices[1])
            }
        }
        
        # Show plot tabs if they have saved data
        if (!is.null(state_object)) {
            # Show volcano plot tab if there are saved analysis files
            if (!is.null(state_object$filelist_file_list) && length(state_object$filelist_file_list) > 0) {
                shinyjs::show(selector = "a[data-value=\"volcanoplotTab\"]")
                shinyjs::show(selector = "a[data-value=\"venndiagramTab\"]")
            }
            
            # Show other tabs based on available data
            if (!is.null(myValues$vsResults)) {
                shinyjs::show(selector = "a[data-value=\"volcanoplotTab\"]")
                shinyjs::show(selector = "a[data-value=\"venndiagramTab\"]")
            }
        }
    }
    
    # Function to trigger plot regeneration after state load
    triggerPlotRegeneration <- function(state_object) {
        if (is.null(state_object$saved_inputs)) return()
        
        inputs <- state_object$saved_inputs
        
        # Show notification about plot regeneration
        showNotification(
            "Regenerating plots from saved state...",
            type = "default", duration = 3
        )
        
        # Clear any stuck loading states first
        js$addStatusIcon("heatmapTab", "done")
        js$addStatusIcon("venndiagramTab", "done")
        js$addStatusIcon("resultsTab", "done")
        
        # Regenerate saved file list display if filelist exists
        if (!is.null(state_object$filelist_file_list) && length(state_object$filelist_file_list) > 0) {
            cat("DEBUG: Restoring saved file list with", length(state_object$filelist_file_list), "files\n")
            
            # Restore the filelist reactive values
            if (!exists("filelist")) {
                filelist <<- reactiveValues()
            }
            filelist$file_list <<- state_object$filelist_file_list
            
            # Regenerate the display
            Saved.Results <- names(state_object$filelist_file_list)
            output$savedFileList <- renderDataTable({
                data.frame(Saved.Results)
            })
            
            cat("DEBUG: Restored files:", paste(Saved.Results, collapse = ", "), "\n")
            
            # Show volcano and venn diagram tabs if there are saved files
            if (length(Saved.Results) > 0) {
                shinyjs::show(selector = "a[data-value=\"volcanoplotTab\"]")
                if (length(Saved.Results) > 1) {
                    shinyjs::show(selector = "a[data-value=\"venndiagramTab\"]")
                }
            }
        }
        
        # Trigger heatmap regeneration if heatmap was previously generated
        if (!is.null(inputs$genHeatmap) && inputs$genHeatmap > 0) {
            cat("DEBUG: Heatmap was previously generated, genHeatmap count:", inputs$genHeatmap, "\n")
            
            # Force trigger the heatmap reactive by updating the button value
            # This ensures the reactive will run even if the restored value is the same
            heatmap_restore <- reactiveValues(done = FALSE)
            observe({
                if (!heatmap_restore$done) {
                    invalidateLater(3000)  # Wait 3 seconds for data to be fully loaded
                    isolate({
                        cat("DEBUG: Triggering heatmap regeneration\n")
                        # Temporarily increment and then restore the genHeatmap value to force reactivity
                        current_value <- if(is.null(input$genHeatmap)) 0 else input$genHeatmap
                        target_value <- inputs$genHeatmap
                        
                        # Force trigger heatmap reactive by simulating button click
                        cat("DEBUG: Forcing heatmap reactive trigger\n")
                        
                        # Use shinyjs to trigger a click on the heatmap button
                        tryCatch({
                            shinyjs::click("genHeatmap")
                            cat("DEBUG: Simulated genHeatmap button click\n")
                        }, error = function(e) {
                            cat("DEBUG: Button click failed, trying alternative method:", e$message, "\n")
                            
                            # Alternative: force invalidation of heatmap reactive
                            # This is a bit of a hack but should work
                            if (exists("heatmapReactive") && is.function(heatmapReactive)) {
                                try({
                                    # Force re-evaluation by accessing the reactive
                                    temp_result <- heatmapReactive()
                                    cat("DEBUG: Forced heatmap reactive evaluation\n")
                                }, silent = TRUE)
                            }
                        })
                        
                        showNotification(
                            "Heatmap regeneration triggered. Check the Heatmap tab.",
                            type = "default", duration = 3
                        )
                        
                        heatmap_restore$done <- TRUE
                    })
                }
            })
        }
        
        # Volcano plots and boxplots are reactive and regenerate automatically
        # when their inputs are restored - no additional action needed
        
        # Remove loading indicators that might be stuck
        showNotification(
            "Analysis results and plots will be visible when you visit each tab.",
            type = "default", duration = 5
        )
    }
    
    # Save state handler
    output$downloadState <- downloadHandler(
        filename = function() {
            paste0("deseq2shiny_state_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
        },
        content = function(file) {
            withProgress(message = "Saving application state...", value = 0, {
                # Step 1: Collecting core data
                setProgress(value = 0.1, detail = "Collecting core data...")
                
                # Step 2: Collecting DESeq2 objects
                setProgress(value = 0.3, detail = "Collecting DESeq2 objects...")
                
                # Step 3: Collecting analysis results
                setProgress(value = 0.5, detail = "Collecting analysis results...")
                
                # Step 4: Collecting UI inputs
                setProgress(value = 0.7, detail = "Collecting UI inputs...")
                
                # Step 5: Creating state object
                setProgress(value = 0.8, detail = "Creating state object...")
                state_object <- saveAppState()
                
                # Step 6: Writing to file
                setProgress(value = 0.9, detail = "Writing state file...")
                save(state_object, file = file)
                
                # Step 7: Complete
                setProgress(value = 1.0, detail = "State saved successfully!")
            })
            
            showNotification("Application state saved successfully!", type = "default", duration = 3)
        }
    )
    
    # Load state handler
    observeEvent(input$loadStateFile, {
        req(input$loadStateFile)
        
        withProgress(message = "Loading application state...", value = 0, {
            tryCatch({
                # Step 1: Reading state file
                setProgress(value = 0.1, detail = "Reading state file...")
                state_file <- input$loadStateFile$datapath
                state_size <- file.info(state_file)$size
                if (is.na(state_size) || state_size > 100 * 1024^2) {
                    stop("State file exceeds the 100 MB safety limit.")
                }
                state_environment <- new.env(parent = emptyenv())
                loaded_names <- load(state_file, envir = state_environment)
                
                # Step 2: Validating state object
                setProgress(value = 0.2, detail = "Validating state file...")
                if (!identical(loaded_names, "state_object") ||
                    !exists("state_object", envir = state_environment, inherits = FALSE)) {
                    showNotification("Invalid state file: 'state_object' not found", type = "error", duration = 5)
                    return()
                }
                state_object <- get(
                    "state_object",
                    envir = state_environment,
                    inherits = FALSE
                )
                if (!is.list(state_object)) {
                    stop("Invalid state file: state_object must be a list.")
                }
                state_object <- validate_state_object(state_object)
                
                # Step 3: Load all components using the comprehensive loadAppState function
                setProgress(value = 0.4, detail = "Restoring application state...")
                success <- loadAppState(state_object)
                
                if (success) {
                    # Step 4: Restoring UI inputs
                    setProgress(value = 0.7, detail = "Restoring UI inputs...")
                    
                    # Step 5: Determining navigation
                    setProgress(value = 0.85, detail = "Preparing interface...")
                    
                    # Step 6: Navigate to appropriate tab
                    setProgress(value = 0.95, detail = "Finalizing restoration...")
                    if (!is.null(myValues$vsResults)) {
                        updateTabItems(session, "tabs", "resultsTab")
                    } else if (!is.null(myValues$dds)) {
                        updateTabItems(session, "tabs", "deseqTab")
                    } else if (!is.null(myValues$DF)) {
                        updateTabItems(session, "tabs", "conditionsTab")
                    } else {
                        updateTabItems(session, "tabs", "inputdata")
                    }
                    
                    # Step 7: Complete
                    setProgress(value = 1.0, detail = "State loaded successfully!")
                    showNotification("Application state loaded successfully!", type = "message", duration = 3)
                    
                    # Close the modal
                    removeModal()
                } else {
                    showNotification("State loaded with some errors. Please check your data.", type = "warning", duration = 5)
                }
                
            }, error = function(e) {
                showNotification(
                    paste("Error loading state file:", e$message),
                    type = "error", duration = 5
                )
            })
        })
    })
    
    # Export All Plots handler
    output$download_code_all <- downloadHandler(
        filename = function() {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            paste0("all_plots_export_", timestamp, ".zip")
        },
        contentType = "application/zip",
        content = function(file) {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            temp_dir <- session_dir
            export_dir <- file.path(temp_dir, paste0("all_plots_", timestamp))
            dir.create(export_dir, showWarnings = FALSE)
            
            all_files <- c()
            plot_count <- 0
            all_r_code_sections <- list()  # Collect all R code sections for single script
            
            # Export original raw counts and metadata first (for reproducibility)
            # Use fileContent if available (preserves gene.name column), otherwise use dataCounts
            if (!is.null(myValues$fileContent)) {
                # Export the complete original file with all columns (gene.id, gene.name, samples)
                raw_counts_file <- file.path(export_dir, paste0("raw_counts_", timestamp, ".csv"))
                write.csv(myValues$fileContent, raw_counts_file, row.names = FALSE)  # row.names=FALSE since first column is gene.id
                all_files <- c(all_files, raw_counts_file)
                cat("Exported original raw counts (complete):", nrow(myValues$fileContent), "genes x", ncol(myValues$fileContent), "columns (including gene names)\n")
            } else if (!is.null(myValues$dataCounts)) {
                # Fallback to dataCounts if fileContent not available
                raw_counts_file <- file.path(export_dir, paste0("raw_counts_", timestamp, ".csv"))
                write.csv(myValues$dataCounts, raw_counts_file, row.names = TRUE)
                all_files <- c(all_files, raw_counts_file)
                cat("Exported original raw counts:", nrow(myValues$dataCounts), "genes x", ncol(myValues$dataCounts), "samples\n")
            } else {
                cat("WARNING: No raw counts available (myValues$fileContent and myValues$dataCounts are NULL)\n")
            }
            
            # Export metadata - try both coldata and other possible sources
            metadata_exported <- FALSE
            if (!is.null(myValues$coldata) && (is.data.frame(myValues$coldata) || is.matrix(myValues$coldata))) {
                original_metadata_file <- file.path(export_dir, paste0("original_metadata_", timestamp, ".csv"))
                write.csv(myValues$coldata, original_metadata_file, row.names = TRUE)
                all_files <- c(all_files, original_metadata_file)
                cat("Exported original metadata:", nrow(myValues$coldata), "samples x", ncol(myValues$coldata), "factors\n")
                metadata_exported <- TRUE
            } else if (!is.null(myValues$dds)) {
                # Try to extract from DESeq2 object
                original_metadata_file <- file.path(export_dir, paste0("original_metadata_", timestamp, ".csv"))
                col_data <- as.data.frame(colData(myValues$dds))
                # Remove DESeq2-specific columns
                col_data$sizeFactor <- NULL
                col_data$replaceable <- NULL
                write.csv(col_data, original_metadata_file, row.names = TRUE)
                all_files <- c(all_files, original_metadata_file)
                cat("Exported metadata from DESeq2 object:", nrow(col_data), "samples x", ncol(col_data), "factors\n")
                metadata_exported <- TRUE
            }
            
            if (!metadata_exported) {
                cat("WARNING: No metadata available - creating minimal metadata from sample names\n")
                # Create minimal metadata from sample names
                if (!is.null(myValues$dataCounts)) {
                    minimal_metadata <- data.frame(
                        sample = colnames(myValues$dataCounts),
                        row.names = colnames(myValues$dataCounts)
                    )
                    original_metadata_file <- file.path(export_dir, paste0("original_metadata_", timestamp, ".csv"))
                    write.csv(minimal_metadata, original_metadata_file, row.names = TRUE)
                    all_files <- c(all_files, original_metadata_file)
                    cat("Created minimal metadata file\n")
                }
            }
            
            withProgress(message = "Exporting all plots...", value = 0, {
                
                # Debug: Check which plots are available
                cat("\n=== Checking Available Plots ===\n")
                cat("filelist$file_list:", !is.null(filelist$file_list), length(if(!is.null(filelist$file_list)) filelist$file_list else 0), "\n")
                cat("myValues$dds:", !is.null(myValues$dds), "\n")
                cat("myValues$dataCounts:", !is.null(myValues$dataCounts), "\n")
                cat("myValues$vsd:", !is.null(myValues$vsd), "\n")
                cat("myValues$rld:", !is.null(myValues$rld), "\n")
                cat("myValues$vsResults:", !is.null(myValues$vsResults), "\n")
                cat("myValues$brushed_heatmap_data:", !is.null(myValues$brushed_heatmap_data), 
                    if(!is.null(myValues$brushed_heatmap_data)) paste0("(", nrow(myValues$brushed_heatmap_data), " genes)") else "", "\n")
                cat("myValues$brushed_venn_heatmap_data:", !is.null(myValues$brushed_venn_heatmap_data), 
                    if(!is.null(myValues$brushed_venn_heatmap_data)) paste0("(", nrow(myValues$brushed_venn_heatmap_data), " genes)") else "", "\n")
                cat("myValues$brushed_venn_genes:", !is.null(myValues$brushed_venn_genes), 
                    if(!is.null(myValues$brushed_venn_genes)) paste0("(", length(myValues$brushed_venn_genes), " genes)") else "", "\n")
                cat("input$select_avo_de_file:", if(!is.null(input$select_avo_de_file)) input$select_avo_de_file else "NULL", "\n")
                cat("input$sel_gene:", if(!is.null(input$sel_gene)) paste(input$sel_gene, collapse=", ") else "NULL", "\n")
                cat("input$boxplotX:", if(!is.null(input$boxplotX)) input$boxplotX else "NULL", "\n")
                cat("input$condition1:", if(!is.null(input$condition1)) input$condition1 else "NULL", "\n")
                cat("================================\n\n")
                
                # Check and export each plot type
                total_plots <- 0
                num_saved_contrasts <- if (exists("filelist") && !is.null(filelist$file_list)) length(filelist$file_list) else 0
                
                # Volcano plots - one per saved contrast
                if (num_saved_contrasts > 0) total_plots <- total_plots + num_saved_contrasts
                
                # Boxplot
                if (!is.null(myValues$dds) && !is.null(myValues$dataCounts)) total_plots <- total_plots + 1
                
                # Heatmap
                if (!is.null(myValues$dds)) total_plots <- total_plots + 1
                
                # Brushed sub-heatmap (if user selected genes via brush)
                if (!is.null(myValues$brushed_heatmap_data) && nrow(myValues$brushed_heatmap_data) > 0) total_plots <- total_plots + 1
                
                # MA plots - one per saved contrast
                if (num_saved_contrasts > 0) total_plots <- total_plots + num_saved_contrasts
                
                # PCA (VST and rlog)
                if (!is.null(myValues$vsd)) total_plots <- total_plots + 1
                if (!is.null(myValues$rld)) total_plots <- total_plots + 1
                
                # Distance heatmaps (VST and rlog)
                if (!is.null(myValues$vsd)) total_plots <- total_plots + 1
                if (!is.null(myValues$rld)) total_plots <- total_plots + 1
                
                # Venn diagram
                if (num_saved_contrasts > 1) total_plots <- total_plots + 1
                
                # Venn set heatmap (if user has created set operations)
                if (num_saved_contrasts > 1 && !is.null(input$venn_set_expression_input) && 
                    length(input$venn_set_expression_input) > 0 && nchar(input$venn_set_expression_input) > 0) {
                    total_plots <- total_plots + 1
                }
                
                # Brushed Venn set heatmap (if user brushed genes from Venn heatmap)
                if (!is.null(myValues$brushed_venn_heatmap_data) && 
                    nrow(myValues$brushed_venn_heatmap_data) > 0 &&
                    !is.null(myValues$brushed_venn_genes) && 
                    length(myValues$brushed_venn_genes) > 0) {
                    total_plots <- total_plots + 1
                }
                
                cat("Total plots expected:", total_plots, "(including", num_saved_contrasts, "volcano and", num_saved_contrasts, "MA plots)\n\n")
                
                current <- 0
                
                # Export Volcano plots for ALL saved contrasts
                cat("Checking Volcano conditions...\n")
                if (exists("filelist") && !is.null(filelist$file_list) && length(filelist$file_list) > 0) {
                    
                    saved_files <- names(filelist$file_list)
                    cat("Exporting Volcano plots for", length(saved_files), "saved contrasts...\n")
                    
                    for (file_idx in seq_along(saved_files)) {
                        tryCatch({
                            filename <- saved_files[file_idx]
                            cat("  - Processing:", filename, "\n")
                            
                            setProgress(value = current/total_plots, detail = paste("Exporting Volcano plot", file_idx, "of", length(saved_files)))
                            
                            padj_threshold <- if (!is.null(input$volcano_threshold_type) && input$volcano_threshold_type == "slider") {
                                1 / 10^as.numeric(input$significance_threshold)
                            } else if (!is.null(input$volcano_direct_padj)) {
                                as.numeric(input$volcano_direct_padj)
                            } else {
                                0.05  # Default
                            }
                            
                            # Remove .csv extension if present
                            comparison_name_clean <- gsub("\\.csv$", "", filename)
                            
                            params <- list(
                                comparison_name = comparison_name_clean,
                                padj_threshold = padj_threshold,
                                log2fc_threshold = if(!is.null(input$log_fold_change_threshold)) input$log_fold_change_threshold else 1,
                                use_gene_names = (!is.null(input$volcano_sel_gene_type) && input$volcano_sel_gene_type == "gene.name"),
                                gene_type = if(!is.null(input$volcano_sel_gene_type)) input$volcano_sel_gene_type else "gene.id",
                                data_file = paste0(comparison_name_clean, "_results.csv"),  # File exported from Section 0
                                genes_of_interest = NULL  # Add missing parameter for template
                            )
                            
                            # Generate code: use existing objects when in full mode for combined script
                            # Skip helper functions since they're already loaded at the top
                            r_code <- generateVolcanoCode(params, mode = "full", 
                                                          use_existing_objects = TRUE,
                                                          include_helpers = FALSE)
                            
                            # Store R code section for combined script with unique key
                            section_key <- paste0("volcano_", file_idx)
                            all_r_code_sections[[section_key]] <- list(
                                title = paste0("Volcano Plot: ", comparison_name_clean),
                                code = r_code,
                                order = 6 + (file_idx - 1) * 0.1  # 6.0, 6.1, 6.2, etc. (after MA plots)
                            )
                            
                            plot_count <- plot_count + 1
                            cat("    ✓ Volcano plot for", comparison_name_clean, "added\n")
                        }, error = function(e) { cat("    ✗ Volcano export error for", filename, ":", e$message, "\n") })
                    }
                    current <- current + 1
                } else {
                    cat("No saved contrasts found for volcano plots, skipping.\n")
                }
                
                # Export Boxplot if available
                cat("Checking Boxplot conditions...\n")
                if (!is.null(myValues$dds) && !is.null(myValues$dataCounts) &&
                    !is.null(input$sel_gene) && !is.null(input$boxplotX)) {
                    
                    cat("Exporting Boxplot...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Boxplot...")
                        
                        params <- list(
                            selected_genes = input$sel_gene,
                            x_axis = input$boxplotX,
                            fill_by = input$boxplotFill,
                            factors = input$sel_factors,
                            use_gene_names = (!is.null(input$box_plot_sel_gene_type) && length(input$box_plot_sel_gene_type) > 0 && input$box_plot_sel_gene_type == "gene.name"),
                            custom_colors = if(!is.null(custom_colors$colors)) custom_colors$colors else NULL,
                            counts_file = "normalized_counts.csv",  # Not needed when use_existing_objects = TRUE
                            metadata_file = "metadata.csv",  # Not needed when use_existing_objects = TRUE
                            num_cols = 2  # Default number of columns for multi-gene plots
                        )
                        
                        # Generate code: use existing objects when in full mode for combined script
                        r_code <- generateBoxplotCode(params, mode = "full",
                                                      use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["boxplot"]] <- list(
                            title = "Gene Expression Boxplot",
                            code = r_code,
                            order = 8  # After Venn diagrams, following tab order
                        )
                        
                        # Data will be generated from raw counts in Section 0 - no need to export intermediate files
                        
                        plot_count <- plot_count + 1
                    }, error = function(e) { cat("Boxplot export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("Boxplot conditions not met, skipping.\n")
                }
                
                # Export Heatmap if available
                cat("Checking Heatmap conditions...\n")
                if (!is.null(myValues$dds) && !is.null(heatmapReactive())) {
                    cat("Exporting Heatmap...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Heatmap...")
                        
                        selected_genes <- NULL
                        if (!is.null(input$subsetGenes) && length(input$subsetGenes) > 0 && input$subsetGenes && 
                            !is.null(input$listPasteGenes) && length(input$listPasteGenes) > 0 && input$listPasteGenes != "") {
                            genes <- unlist(strsplit(input$listPasteGenes, ","))
                            genes <- gsub("^\\s+|\\s+$", "", genes)
                            selected_genes <- genes[genes != ""]
                        }
                        
                        params <- list(
                            num_genes = input$numGenes,
                            selected_genes = selected_genes,
                            use_gene_names = (!is.null(input$heatmap_sel_gene_type) && length(input$heatmap_sel_gene_type) > 0 && input$heatmap_sel_gene_type == "gene.name"),
                            counts_file = "normalized_counts.csv",
                            metadata_file = "metadata.csv",
                            fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8,
                            is_brushed_heatmap = FALSE,
                            sample_order = NULL,
                            is_venn_heatmap = FALSE,
                            color_range = NULL
                        )
                        
                        # Generate code: use existing objects when in full mode for combined script
                        r_code <- generateHeatmapCode(params, mode = "full",
                                                     use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["heatmap"]] <- list(
                            title = "Expression Heatmap",
                            code = r_code,
                            order = 9  # After boxplot, following tab order
                        )
                        
                        # Data will be generated from raw counts in Section 0 - no need to export intermediate files
                        
                        plot_count <- plot_count + 1
                    }, error = function(e) { cat("Heatmap export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("Heatmap conditions not met, skipping.\n")
                }
                
                # Export Brushed Sub-Heatmap if user has selected genes via brush
                cat("Checking Brushed Heatmap conditions...\n")
                if (!is.null(myValues$brushed_heatmap_data) && nrow(myValues$brushed_heatmap_data) > 0 &&
                    !is.null(myValues$brushed_genes) && length(myValues$brushed_genes) > 0) {
                    cat("Exporting Brushed Sub-Heatmap...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Brushed Heatmap...")
                        
                        # Use gene IDs for data extraction (same pattern as Venn brushed heatmap)
                        brushed_gene_ids <- if(!is.null(myValues$brushed_gene_ids)) {
                            myValues$brushed_gene_ids
                        } else {
                            myValues$brushed_genes  # Fallback if IDs not stored
                        }
                        
                        # Remove NAs from gene IDs (in case mapping failed)
                        valid_idx <- !is.na(brushed_gene_ids)
                        valid_gene_ids <- brushed_gene_ids[valid_idx]
                        
                        # Get parent heatmap color range for consistent scaling
                        # MUST use parent range - do NOT calculate from brushed data
                        if (is.null(myValues$heatmap_data_range)) {
                            warning("Parent heatmap color range not found. Brushed heatmap may have incorrect colors.")
                            cat("WARNING: Parent heatmap range not available. Cannot export brushed heatmap with correct colors.\n")
                            parent_range <- NULL  # Will cause warning in export
                        } else {
                            parent_range <- myValues$heatmap_data_range
                            cat("Using parent heatmap color range:", parent_range[1], "to", parent_range[2], "\n")
                        }
                        
                        params <- list(
                            num_genes = length(valid_gene_ids),
                            selected_genes = valid_gene_ids,  # Use gene IDs for data extraction
                            brushed_gene_order = if(!is.null(myValues$brushed_gene_order)) myValues$brushed_gene_order[valid_idx] else NULL,
                            use_gene_names = (!is.null(input$heatmap_sel_gene_type) && length(input$heatmap_sel_gene_type) > 0 && input$heatmap_sel_gene_type == "gene.name"),
                            is_brushed_heatmap = TRUE,  # This is a brushed sub-heatmap
                            sample_order = colnames(myValues$brushed_heatmap_data),  # Preserve original sample order
                            color_range = parent_range,  # Use parent heatmap's color scale
                            counts_file = "normalized_counts.csv",
                            metadata_file = "metadata.csv",
                            fontsize_row = if(!is.null(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8,
                            is_venn_heatmap = FALSE
                        )
                        
                        # Generate code
                        r_code <- generateHeatmapCode(params, mode = "full",
                                                     use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["heatmap_brushed"]] <- list(
                            title = paste0("Brushed Sub-Heatmap (", length(valid_gene_ids), " genes selected)"),
                            code = r_code,
                            order = 9.5  # After main heatmap, following tab order
                        )
                        
                        plot_count <- plot_count + 1
                        cat("  ✓ Brushed heatmap with", length(valid_gene_ids), "genes added\n")
                    }, error = function(e) { cat("  ✗ Brushed heatmap export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("No brushed heatmap selection found, skipping.\n")
                }
                
                # Export MA Plots for ALL saved contrasts
                cat("Checking MA plot conditions...\n")
                if (exists("filelist") && !is.null(filelist$file_list) && length(filelist$file_list) > 0) {
                    
                    saved_files <- names(filelist$file_list)
                    cat("Exporting MA plots for", length(saved_files), "saved contrasts...\n")
                    
                    for (file_idx in seq_along(saved_files)) {
                        tryCatch({
                            filename <- saved_files[file_idx]
                            cat("  - Processing:", filename, "\n")
                            
                            setProgress(value = current/total_plots, detail = paste("Exporting MA plot", file_idx, "of", length(saved_files)))
                            
                            # Remove .csv extension if present
                            comparison_name_clean <- gsub("\\.csv$", "", filename)
                            
                            params <- list(
                                comparison_name = comparison_name_clean,
                                alpha = if(!is.null(input$alpha)) input$alpha else 0.1,
                                ylim = if(!is.null(input$ylim)) input$ylim else 5,
                                data_file = paste0(comparison_name_clean, "_results.csv")  # File exported from Section 0
                            )
                            
                            # Generate code: use existing objects when in full mode for combined script
                            # Skip helper functions since they're already loaded at the top
                            r_code <- generateMAPlotCode(params, mode = "full",
                                                        use_existing_objects = TRUE,
                                                        include_helpers = FALSE)
                            
                            # Store R code section for combined script with unique key
                            section_key <- paste0("ma_plot_", file_idx)
                            all_r_code_sections[[section_key]] <- list(
                                title = paste0("MA Plot: ", comparison_name_clean),
                                code = r_code,
                                order = 5 + (file_idx - 1) * 0.1  # 5.0, 5.1, 5.2, etc. (after distance heatmaps, in DE Results tab)
                            )
                            
                            plot_count <- plot_count + 1
                            cat("    ✓ MA plot for", comparison_name_clean, "added\n")
                        }, error = function(e) { cat("    ✗ MA plot export error for", filename, ":", e$message, "\n") })
                    }
                    current <- current + 1
                } else {
                    cat("No saved contrasts found for MA plots, skipping.\n")
                }
                
                # Export PCA (VST) if available
                cat("Checking PCA (VST) conditions...\n")
                if (!is.null(myValues$vsd) && !is.null(myValues$vstMat) && !is.null(myValues$dds)) {
                    cat("Exporting PCA (VST)...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting PCA (VST)...")
                        
                        # Get intgroup parameter with proper fallback
                        intgroup_param <- "Conditions"  # Default fallback
                        if (!is.null(input$vsdIntGroupsInput) && length(input$vsdIntGroupsInput) > 0) {
                            intgroup_param <- input$vsdIntGroupsInput
                        } else {
                            # Try to get from dds colData
                            tryCatch({
                                coldata_names <- names(colData(myValues$dds))
                                if (length(coldata_names) > 0) {
                                    intgroup_param <- coldata_names[1]
                                }
                            }, error = function(e) {
                                cat("Warning: Could not extract column names from dds, using default\n")
                            })
                        }
                        
                        params <- list(
                            intgroup = intgroup_param,
                            transform_type = "vst",
                            transformed_data_file = "vst_data.csv",
                            metadata_file = "metadata.csv"
                        )
                        
                        # Generate code: use existing objects when in full mode for combined script
                        r_code <- generatePCACode(params, mode = "full",
                                                 use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["pca_vst"]] <- list(
                            title = "PCA Plot (VST)",
                            code = r_code,
                            order = 1  # First plot after pipeline, VST tab
                        )
                        
                        # Data will be generated from raw counts in Section 0 - no need to export intermediate files
                        
                        plot_count <- plot_count + 1
                    }, error = function(e) { cat("PCA (VST) export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("PCA (VST) conditions not met, skipping.\n")
                }
                
                # Export PCA (RLOG) if available
                cat("Checking PCA (RLOG) conditions...\n")
                if (!is.null(myValues$rld) && !is.null(myValues$rlogMat) && !is.null(myValues$dds)) {
                    cat("Exporting PCA (RLOG)...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting PCA (RLOG)...")
                        
                        # Get intgroup parameter with proper fallback
                        intgroup_param <- "Conditions"  # Default fallback
                        if (!is.null(input$rlogIntGroupsInput) && length(input$rlogIntGroupsInput) > 0) {
                            intgroup_param <- input$rlogIntGroupsInput
                        } else {
                            # Try to get from dds colData
                            tryCatch({
                                coldata_names <- names(colData(myValues$dds))
                                if (length(coldata_names) > 0) {
                                    intgroup_param <- coldata_names[1]
                                }
                            }, error = function(e) {
                                cat("Warning: Could not extract column names from dds, using default\n")
                            })
                        }
                        
                        params <- list(
                            intgroup = intgroup_param,
                            transform_type = "rlog",
                            transformed_data_file = "rlog_data.csv",
                            metadata_file = "metadata.csv"
                        )
                        
                        # Generate code: use existing objects when in full mode for combined script
                        r_code <- generatePCACode(params, mode = "full",
                                                 use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["pca_rlog"]] <- list(
                            title = "PCA Plot (RLOG)",
                            code = r_code,
                            order = 3  # RLOG tab, after VST
                        )
                        
                        # Data will be generated from raw counts in Section 0 - no need to export intermediate files
                        
                        plot_count <- plot_count + 1
                    }, error = function(e) { cat("PCA (RLOG) export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("PCA (RLOG) conditions not met, skipping.\n")
                }
                
                # Export Distance Heatmap (VST) if available
                cat("Checking Distance Heatmap (VST) conditions...\n")
                if (!is.null(myValues$vsd) && !is.null(myValues$vstMat) && !is.null(myValues$dds)) {
                    cat("Exporting Distance Heatmap (VST)...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Distance Heatmap (VST)...")
                        
                        params <- list(
                            transform_type = "vst",
                            transformed_data_file = "vst_data.csv",
                            metadata_file = "metadata.csv"
                        )
                        
                        # Generate code: use existing objects when in full mode for combined script
                        r_code <- generateDistHeatmapCode(params, mode = "full",
                                                         use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["distheat_vst"]] <- list(
                            title = "Distance Heatmap (VST)",
                            code = r_code,
                            order = 2  # VST tab, after PCA
                        )
                        
                        # Data will be generated from raw counts in Section 0 - no need to export intermediate files
                        
                        plot_count <- plot_count + 1
                    }, error = function(e) { cat("Distance Heatmap (VST) export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("Distance Heatmap (VST) conditions not met, skipping.\n")
                }
                
                # Export Distance Heatmap (RLOG) if available
                cat("Checking Distance Heatmap (RLOG) conditions...\n")
                if (!is.null(myValues$rld) && !is.null(myValues$rlogMat) && !is.null(myValues$dds)) {
                    cat("Exporting Distance Heatmap (RLOG)...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Distance Heatmap (RLOG)...")
                        
                        params <- list(
                            transform_type = "rlog",
                            transformed_data_file = "rlog_data.csv",
                            metadata_file = "metadata.csv"
                        )
                        
                        # Generate code: use existing objects when in full mode for combined script
                        r_code <- generateDistHeatmapCode(params, mode = "full",
                                                         use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["distheat_rlog"]] <- list(
                            title = "Distance Heatmap (RLOG)",
                            code = r_code,
                            order = 4  # RLOG tab, after PCA
                        )
                        
                        # Data will be generated from raw counts in Section 0 - no need to export intermediate files
                        
                        plot_count <- plot_count + 1
                    }, error = function(e) { cat("Distance Heatmap (RLOG) export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("Distance Heatmap (RLOG) conditions not met, skipping.\n")
                }
                
                # Export Venn Diagram if available
                cat("Checking Venn Diagram conditions...\n")
                if (!is.null(filelist$file_list) && length(filelist$file_list) > 1 &&
                    !is.null(input$select_avo_de_venn_files) && length(input$select_avo_de_venn_files) > 1) {
                    cat("Exporting Venn Diagram...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Venn Diagram...")
                        
                        comparisons <- input$select_avo_de_venn_files
                        
                        # Extract thresholds from Shiny inputs (to match Venn diagram filtering)
                        if (!is.null(input$venn_threshold_type) && input$venn_threshold_type == "slider") {
                            padj_thresh <- 1 / 10^as.numeric(input$venn_significance_threshold)
                        } else {
                            padj_thresh <- as.numeric(input$venn_direct_padj)
                        }
                        fc_thresh <- as.numeric(input$venn_log_fold_change_threshold)
                        
                        params <- list(
                            comparisons = comparisons,
                            num_sets = length(comparisons),
                            padj_threshold = padj_thresh,
                            fc_threshold = fc_thresh,
                            venn_colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8"),
                            gene_files = NULL  # Not needed when use_existing_objects = TRUE
                        )
                        
                        # Generate code: use existing objects when in full mode for combined script
                        r_code <- generateVennCode(params, mode = "full",
                                                   use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["venn"]] <- list(
                            title = "Venn Diagram",
                            code = r_code,
                            order = 7  # Venn tab, after volcano plots
                        )
                        
                        # Data will be generated from raw counts in Section 0 - no need to export intermediate files
                        
                        plot_count <- plot_count + 1
                    }, error = function(e) { cat("Venn Diagram export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("Venn Diagram conditions not met, skipping.\n")
                }
                
                # Export Venn Set Heatmap if user has created one
                cat("Checking Venn Set Heatmap conditions...\n")
                if (!is.null(filelist$file_list) && length(filelist$file_list) > 1 &&
                    !is.null(input$venn_set_expression_input) && length(input$venn_set_expression_input) > 0 && 
                    nchar(input$venn_set_expression_input) > 0) {
                    cat("Exporting Venn Set Heatmap...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Venn Set Heatmap...")
                        
                        # Extract thresholds (same as Venn diagram)
                        if (!is.null(input$venn_threshold_type) && input$venn_threshold_type == "slider") {
                            padj_thresh <- 1 / 10^as.numeric(input$venn_significance_threshold)
                        } else {
                            padj_thresh <- as.numeric(input$venn_direct_padj)
                        }
                        fc_thresh <- as.numeric(input$venn_log_fold_change_threshold)
                        
                        # Get the actual matrix from Shiny's evaluation (with real log2FC values!)
                        # This avoids NA/NaN/Inf issues from rebuilding the matrix
                        set_operation_data <- tryCatch({
                            # Get the complete matrix from heatmap_matrix reactive
                            hm <- heatmap_matrix()
                            if (!is.null(hm) && nrow(hm) > 0) {
                                # Convert gene names back to gene IDs for rownames if needed
                                gene_ids <- rownames(hm)
                                if (!is.null(input$gene_alias) && input$gene_alias == "included" && 
                                    !is.null(input$venn_sel_gene_type) && input$venn_sel_gene_type == "gene.name" &&
                                    !is.null(myValues$geneids)) {
                                    # Reverse lookup: gene names -> gene IDs
                                    gene_ids_mapped <- rownames(myValues$geneids)[match(gene_ids, myValues$geneids[,1])]
                                    gene_ids <- ifelse(is.na(gene_ids_mapped), gene_ids, gene_ids_mapped)
                                }
                                
                                # Create matrix with gene IDs as rownames and letter labels as colnames
                                matrix_with_ids <- hm
                                rownames(matrix_with_ids) <- gene_ids
                                
                                cat("Captured", nrow(hm), "genes ×", ncol(hm), "comparisons from Shiny's heatmap matrix\n")
                                list(
                                    genes = gene_ids,
                                    matrix = matrix_with_ids,
                                    has_data = TRUE
                                )
                            } else {
                                list(genes = NULL, matrix = NULL, has_data = FALSE)
                            }
                        }, error = function(e) {
                            cat("Warning: Could not capture data from heatmap_matrix:", e$message, "\n")
                            list(genes = NULL, matrix = NULL, has_data = FALSE)
                        })
                        
                        # Export the actual matrix as CSV for complete analysis
                        if (set_operation_data$has_data && !is.null(set_operation_data$matrix)) {
                            # Map letter labels (A, B, C) back to comparison names for CSV columns
                            export_matrix <- set_operation_data$matrix
                            comparison_mapping <- setNames(input$select_avo_de_venn_files, LETTERS[1:length(input$select_avo_de_venn_files)])
                            colnames(export_matrix) <- comparison_mapping[colnames(export_matrix)]
                            
                            # Export matrix with comparison names as columns
                            matrix_file <- file.path(export_dir, paste0("venn_set_matrix_", timestamp, ".csv"))
                            write.csv(export_matrix, matrix_file, row.names = TRUE)
                            all_files <- c(all_files, matrix_file)
                            cat("  - Exported Venn set matrix:", nrow(export_matrix), "genes ×", ncol(export_matrix), "comparisons\n")
                        }
                        
                        params <- list(
                            comparisons = input$select_avo_de_venn_files,
                            set_expression = input$venn_set_expression_input,
                            padj_threshold = padj_thresh,
                            fc_threshold = fc_thresh,
                            use_gene_names = (!is.null(input$venn_sel_gene_type) && 
                                            input$venn_sel_gene_type == "gene.name"),
                            num_genes = if(set_operation_data$has_data) length(set_operation_data$genes) else 50,
                            expression_matrix_file = paste0("venn_set_matrix_", timestamp, ".csv"),  # Exported matrix!
                            fontsize_row = 8,  # Default font size for gene labels
                            is_brushed = FALSE,
                            brushed_genes = NULL,
                            brushed_gene_order = NULL,
                            set_operation_genes = set_operation_data$genes,  # Still pass gene list for metadata
                            sample_order = NULL,
                            color_range = NULL
                        )
                        
                        # Generate code
                        r_code <- generateVennSetHeatmapCode(params, mode = "full",
                                                            use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["venn_set_heatmap"]] <- list(
                            title = paste0("Venn Set Heatmap: ", input$venn_set_expression_input),
                            code = r_code,
                            order = 7.5  # Immediately after Venn diagram (connected analysis)
                        )
                        
                        plot_count <- plot_count + 1
                        cat("  ✓ Venn set heatmap for", input$venn_set_expression_input, "added\n")
                    }, error = function(e) { cat("  ✗ Venn Set Heatmap export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("Venn Set Heatmap conditions not met, skipping.\n")
                }
                
                # Export Brushed Venn Set Heatmap if user has brushed one
                cat("Checking Brushed Venn Set Heatmap conditions...\n")
                if (!is.null(myValues$brushed_venn_heatmap_data) && 
                    !is.null(myValues$brushed_venn_genes) && 
                    length(myValues$brushed_venn_genes) > 0) {
                    cat("Exporting Brushed Venn Set Heatmap...\n")
                    tryCatch({
                        setProgress(value = current/total_plots, detail = "Exporting Brushed Venn Set Heatmap...")
                        
                        # Extract thresholds (same as Venn diagram)
                        if (!is.null(input$venn_threshold_type) && input$venn_threshold_type == "slider") {
                            padj_thresh <- 1 / 10^as.numeric(input$venn_significance_threshold)
                        } else {
                            padj_thresh <- as.numeric(input$venn_direct_padj)
                        }
                        fc_thresh <- as.numeric(input$venn_log_fold_change_threshold)
                        
                        # Get parent heatmap data range for consistent color scaling
                        # MUST use parent range - do NOT calculate from brushed data
                        if (is.null(myValues$venn_heatmap_data_range)) {
                            warning("Parent Venn heatmap color range not found. Brushed heatmap may have incorrect colors.")
                            cat("WARNING: Parent Venn heatmap range not available for brushed export.\n")
                            parent_range <- NULL  # Will cause warning in export
                        } else {
                            parent_range <- myValues$venn_heatmap_data_range
                            cat("Using parent Venn heatmap color range:", parent_range[1], "to", parent_range[2], "\n")
                        }
                        
                        # Export the brushed matrix directly (already clean from Shiny)
                        brushed_matrix <- myValues$brushed_venn_heatmap_data
                        
                        # Convert gene IDs back to rownames if needed
                        brushed_gene_ids <- if(!is.null(myValues$brushed_venn_gene_ids)) {
                            myValues$brushed_venn_gene_ids
                        } else {
                            myValues$brushed_venn_genes
                        }
                        
                        # Remove NAs
                        valid_idx <- !is.na(brushed_gene_ids)
                        valid_gene_ids <- brushed_gene_ids[valid_idx]
                        
                        # Create export matrix with gene IDs as rownames
                        export_brushed_matrix <- brushed_matrix[valid_idx, , drop = FALSE]
                        rownames(export_brushed_matrix) <- valid_gene_ids
                        
                        # Map letter labels to comparison names for columns
                        comparison_mapping <- setNames(input$select_avo_de_venn_files, LETTERS[1:length(input$select_avo_de_venn_files)])
                        colnames(export_brushed_matrix) <- comparison_mapping[colnames(export_brushed_matrix)]
                        
                        # Export brushed matrix
                        brushed_matrix_file <- file.path(export_dir, paste0("venn_brushed_matrix_", timestamp, ".csv"))
                        write.csv(export_brushed_matrix, brushed_matrix_file, row.names = TRUE)
                        all_files <- c(all_files, brushed_matrix_file)
                        cat("  - Exported brushed Venn matrix:", nrow(export_brushed_matrix), "genes ×", ncol(export_brushed_matrix), "comparisons\n")
                        
                        params <- list(
                            comparisons = input$select_avo_de_venn_files,
                            set_expression = paste0("Brushed subset (", length(valid_gene_ids), " genes)"),
                            padj_threshold = padj_thresh,
                            fc_threshold = fc_thresh,
                            use_gene_names = (!is.null(input$venn_sel_gene_type) && 
                                            input$venn_sel_gene_type == "gene.name"),
                            num_genes = length(valid_gene_ids),
                            expression_matrix_file = paste0("venn_brushed_matrix_", timestamp, ".csv"),  # Exported brushed matrix!
                            fontsize_row = if(length(valid_gene_ids) > 50) 6 else 8,
                            is_brushed = TRUE,
                            brushed_genes = valid_gene_ids,
                            brushed_gene_order = if(!is.null(myValues$brushed_venn_gene_order)) myValues$brushed_venn_gene_order[valid_idx] else NULL,
                            set_operation_genes = valid_gene_ids,  # Add for template compatibility
                            sample_order = colnames(myValues$brushed_venn_heatmap_data),
                            color_range = parent_range
                        )
                        
                        # Generate code
                        r_code <- generateVennSetHeatmapCode(params, mode = "full",
                                                            use_existing_objects = TRUE)
                        
                        # Store R code section for combined script
                        all_r_code_sections[["venn_set_heatmap_brushed"]] <- list(
                            title = paste0("Brushed Venn Sub-Heatmap (", length(myValues$brushed_venn_genes), " genes selected)"),
                            code = r_code,
                            order = 7.6  # Immediately after Venn set heatmap
                        )
                        
                        plot_count <- plot_count + 1
                        cat("  ✓ Brushed Venn heatmap with", length(myValues$brushed_venn_genes), "genes added\n")
                    }, error = function(e) { cat("  ✗ Brushed Venn Set Heatmap export error:", e$message, "\n") })
                    current <- current + 1
                } else {
                    cat("Brushed Venn Set Heatmap conditions not met, skipping.\n")
                    cat("  - brushed_venn_heatmap_data:", if(!is.null(myValues$brushed_venn_heatmap_data)) 
                        paste0("EXISTS (", nrow(myValues$brushed_venn_heatmap_data), " genes)") else "NULL", "\n")
                    cat("  - brushed_venn_genes:", if(!is.null(myValues$brushed_venn_genes)) 
                        paste0("EXISTS (", length(myValues$brushed_venn_genes), " genes)") else "NULL", "\n")
                }
                
                cat("\n=== Export Summary ===\n")
                cat("Total plots exported:", plot_count, "\n")
                cat("======================\n\n")
                
                setProgress(value = 1, detail = paste("Exported", plot_count, "plots"))
            })
            
            # Create combined R script with all analyses
            if (length(all_r_code_sections) > 0) {
                # Create DESeq2 pipeline section using the comprehensive generator
                cat("Generating Section 0: DESeq2 Pipeline...\n")
                
                # Prepare parameters for pipeline generator
                pipeline_params <- list(
                    design_formula = NULL,  # Will be determined from coldata
                    contrasts = NULL,  # Will be determined from current analysis
                    # Match Shiny prefiltering if it was applied
                    prefilter_applied = if(!is.null(myValues$prefilter_applied)) myValues$prefilter_applied else FALSE,
                    prefilter_threshold = if(!is.null(myValues$prefilter_threshold)) myValues$prefilter_threshold else 0,
                    alpha = 0.1,
                    counts_filename = paste0("raw_counts_", timestamp, ".csv"),
                    metadata_filename = paste0("original_metadata_", timestamp, ".csv")
                )
                
                # Get design formula from the actual dds object
                main_factor <- NULL
                if (!is.null(myValues$dds)) {
                    # Extract design formula from dds object
                    design_obj <- design(myValues$dds)
                    pipeline_params$design_formula <- paste(deparse(design_obj), collapse = " ")
                    
                    cat("Extracted design formula:", pipeline_params$design_formula, "\n")
                    
                    # Extract main factor from design
                    design_terms <- attr(terms(design_obj), "term.labels")
                    if (length(design_terms) > 0) {
                        # Last term is typically the main comparison factor
                        main_factor <- design_terms[length(design_terms)]
                        cat("Main factor:", main_factor, "\n")
                    }
                } else if (!is.null(myValues$coldata) && ncol(myValues$coldata) > 0) {
                    # Fallback: determine from coldata
                    factor_cols <- names(myValues$coldata)[sapply(myValues$coldata, function(x) is.factor(x) || is.character(x))]
                    if (length(factor_cols) > 0) {
                        main_factor <- factor_cols[length(factor_cols)]
                        pipeline_params$design_formula <- paste0("~ ", paste(factor_cols, collapse = " + "))
                    } else {
                        pipeline_params$design_formula <- "~ 1"
                    }
                } else {
                    # Final fallback
                    pipeline_params$design_formula <- "~ 1"
                }
                
                # Add all saved contrasts using their structured specifications.
                pipeline_params$contrasts <- list()
                
                if (exists("contrast_specs") &&
                    !is.null(contrast_specs$specs) &&
                    length(contrast_specs$specs) > 0) {
                    for (filename in names(contrast_specs$specs)) {
                        specification <- contrast_specs$specs[[filename]]
                        contrast_name <- sub("\\.csv$", "", filename)
                        pipeline_params$contrasts[[contrast_name]] <- specification$contrast
                        cat("Added structured contrast:", contrast_name, "\n")
                    }
                } else if (!is.null(myValues$vsResults) && !is.null(input$condition1) && !is.null(input$condition2) && !is.null(main_factor)) {
                    # Fallback: if no saved contrasts, use current contrast from UI
                    contrast_name <- paste0(input$condition1, "_vs_", input$condition2)
                    pipeline_params$contrasts[[contrast_name]] <- c(main_factor, input$condition1, input$condition2)
                    cat("No saved contrasts found, using current contrast:", contrast_name, "\n")
                }
                
                # Print final contrast list
                if (!is.null(pipeline_params$contrasts)) {
                    cat("Final contrasts for export:\n")
                    print(pipeline_params$contrasts)
                } else {
                    cat("No contrasts available for export\n")
                }
                
                # Generate the comprehensive DESeq2 pipeline code
                # Only .R format is supported
                deseq2_pipeline <- generateDESeq2PipelineCode(
                    params = pipeline_params
                )
                
                cat("Section 0 pipeline generated successfully!\n")
                
                deseq2_pipeline <- paste0(deseq2_pipeline,
                    "\ncat(\"=\", rep(\"=\", 70), \"\\n\", sep = \"\")\n",
                    "cat(\"DESeq2 Pipeline Complete!\\n\")\n",
                    "cat(\"You can now proceed to generate individual plots below.\\n\")\n",
                    "cat(\"=\", rep(\"=\", 70), \"\\n\\n\\n\")\n\n"
                )
                
                # Load helper functions template for optimized code generation
                cat("Loading helper functions template...\n")
                helper_functions_code <- tryCatch({
                    readLines("templates/template_helper_functions.R", warn = FALSE)
                }, error = function(e) {
                    cat("Warning: Could not load helper functions template:", e$message, "\n")
                    NULL
                })
                
                # Convert to string with proper line breaks
                if (!is.null(helper_functions_code)) {
                    helper_functions_section <- paste0(
                        paste(helper_functions_code, collapse = "\n"),
                        "\n\n"
                    )
                    cat("Helper functions loaded successfully!\n")
                } else {
                    helper_functions_section <- ""
                    cat("Skipping helper functions (not found)\n")
                }
                
                # Sort sections by order for plot generation
                sections_ordered <- all_r_code_sections[order(sapply(all_r_code_sections, function(x) x$order))]
                
                # Generate complete script using template wrapper (cleaner than paste0)
                cat("Assembling complete script from template...\n")
                combined_script <- generateScriptWrapper(
                    helper_functions = helper_functions_section,
                    deseq2_pipeline = deseq2_pipeline,
                    plot_sections = sections_ordered,
                    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    num_plots = plot_count
                )
                
                # Write combined script
                script_filename <- paste0("complete_analysis_", timestamp, ".R")
                script_file <- file.path(export_dir, script_filename)
                writeLines(combined_script, script_file)
                
                cat("Created comprehensive script:", script_filename, "\n")
            }
            
            # Create README
            readme_file <- file.path(export_dir, "README.txt")
            
            # List data files - only raw data is included since everything else is generated!
            data_files_list <- ""
            csv_files <- list.files(export_dir, pattern = "\\.csv$", full.names = FALSE)
            if (length(csv_files) > 0) {
                data_files_list <- paste0("\n📊 INPUT DATA FILES (Everything else is generated!):\n")
                for (csv_file in csv_files) {
                    if (grepl("raw_counts", csv_file)) {
                        data_files_list <- paste0(data_files_list, "✅ ", csv_file, " → Original raw count matrix (REQUIRED)\n")
                    } else if (grepl("original_metadata", csv_file)) {
                        data_files_list <- paste0(data_files_list, "✅ ", csv_file, " → Original sample metadata (REQUIRED)\n")
                    }
                }
                data_files_list <- paste0(data_files_list, "\n",
                    "💡 NOTE: No intermediate data files are included because the script\n",
                    "   generates EVERYTHING from these two raw files:\n",
                    "   • Normalized counts → DESeq2 results → Transformations → Plots\n",
                    "   This ensures complete reproducibility from the ground up!\n\n")
            }
            
            # Build plot list for README
            plot_list <- ""
            if (length(all_r_code_sections) > 0) {
                sections_ordered <- all_r_code_sections[order(sapply(all_r_code_sections, function(x) x$order))]
                for (i in seq_along(sections_ordered)) {
                    section <- sections_ordered[[i]]
                    plot_list <- paste0(plot_list, sprintf("%2d. %s\n", i, section$title))
                }
            }
            
            # Generate README from template (much cleaner than 122 lines of paste0!)
            cat("Generating README from template...\n")
            readme_text <- generateREADME(
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                script_filename = script_filename,
                data_files_list = data_files_list,
                plot_list = plot_list,
                num_plots = plot_count
            )
            writeLines(readme_text, readme_file)
            all_files <- c(all_files, readme_file)
            
            # Create final ZIP
            if (length(all_files) > 0) {
                # Debug: List files in export directory
                cat("\n=== Export ALL Debug ===\n")
                cat("Export directory:", export_dir, "\n")
                cat("Files in export directory:\n")
                files_to_zip <- list.files(export_dir, full.names = FALSE)
                cat(paste(files_to_zip, collapse = "\n"), "\n")
                cat("Total files:", length(files_to_zip), "\n")
                cat("========================\n\n")
                
                if (length(files_to_zip) > 0) {
                    zip_file <- file.path(temp_dir, paste0("all_plots_export_", timestamp, ".zip"))
                    zip_export_dir(export_dir, zip_file)
                    file.copy(zip_file, file)
                    showNotification(paste("Exported", plot_count, "plots (", length(files_to_zip), "files)!"), type = "message", duration = 3)
                } else {
                    showNotification("No files were created to export", type = "error", duration = 5)
                }
            } else {
                showNotification("No plots available to export", type = "warning", duration = 3)
            }
        }
    )
    
    # Observer for showing state modal using Shiny's native modal (better nginx support)
    observeEvent(input$showStateModal, {
        canSave <- !is.null(myValues$dataCounts) || !is.null(myValues$dds)
        
        # Check which plots are available (only show buttons when plots are actually configured/visible)
        hasVolcano <- !is.null(input$select_avo_de_file) && 
                      input$select_avo_de_file != "Select data"
        hasBoxplot <- !is.null(myValues$dds) && !is.null(myValues$dataCounts) && 
                      !is.null(input$sel_gene) && !is.null(input$sel_groups)
        hasHeatmap <- tryCatch({
            !is.null(myValues$dds) && !is.null(heatmapReactive())
        }, error = function(e) FALSE)
        hasBrushedHeatmap <- !is.null(myValues$brushed_heatmap_data) && 
                             nrow(myValues$brushed_heatmap_data) > 0
        hasPCA <- !is.null(myValues$vsd) || !is.null(myValues$rld)
        hasMA <- !is.null(myValues$vsResults) && 
                 !is.null(input$condition1) && !is.null(input$condition2)
        hasVenn <- !is.null(input$select_avo_de_venn_files) && 
                   length(input$select_avo_de_venn_files) > 1 &&
                   !is.null(input$venn_set_expression_input)
        hasVennSetHeatmap <- !is.null(input$select_avo_de_venn_files) && 
                             length(input$select_avo_de_venn_files) > 1 &&
                             !is.null(input$venn_set_expression_input) &&
                             length(input$venn_set_expression_input) > 0 &&
                             nchar(input$venn_set_expression_input) > 0
        hasBrushedVennHeatmap <- !is.null(selected_matrix$matrix) && 
                                 nrow(selected_matrix$matrix) > 0
        
        showModal(modalDialog(
            title = "Save/Load State & Export R Code",
            size = "l",
            easyClose = TRUE,
            footer = modalButton("Close"),
            
            fluidRow(
                column(6,
                    # Save State Section
                    h4(icon("save"), " Save Current State"),
                    p("Save all your analysis data, results, and plot configurations.", style = "color: #666; margin-bottom: 15px;"),
                    
                    if (canSave) {
                        div(style = "background: linear-gradient(to bottom, #ffffff, #f8f9fa); border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                            div(style = "text-align: center; margin-bottom: 15px;",
                                icon("database", class = "fa-3x", style = "color: #28a745; margin-bottom: 10px;"),
                                div(style = "font-size: 13px; color: #6c757d; margin-top: 10px;",
                                    "Preserves all data, results, and settings"
                                )
                            ),
                            downloadButton("downloadState", 
                                         "💾 Download State File (.RData)", 
                                         class = "btn btn-success", 
                                         style = "width: 100%; font-weight: bold; font-size: 14px; padding: 10px; border-radius: 6px; box-shadow: 0 2px 6px rgba(40,167,69,0.3);")
                        )
                    } else {
                        div(style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                            div(style = "text-align: center; color: #856404;",
                                icon("exclamation-triangle", class = "fa-2x", style = "margin-bottom: 10px;"),
                                div(style = "font-weight: 600; margin-top: 10px;", "No Data Available"),
                                div(style = "font-size: 12px; margin-top: 5px;", "Please load and analyze data first")
                            )
                        )
                    },
                    
                    hr(style = "margin: 25px 0; border-top: 2px solid #dee2e6;"),
                    
                    # Load State Section
                    h4(icon("folder-open"), " Load Previous State"),
                    p("Restore a previously saved analysis session.", style = "color: #666; margin-bottom: 15px;"),
                    
                    div(style = "background: linear-gradient(to bottom, #ffffff, #f8f9fa); border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                        fileInput("loadStateFile", 
                                label = div(style = "font-weight: 600; color: #495057; margin-bottom: 10px;",
                                          icon("upload"), " Choose State File (.RData)"),
                                accept = c(".RData", ".rdata"),
                                multiple = FALSE,
                                width = "100%",
                                buttonLabel = "Browse...",
                                placeholder = "No file selected"
                        ),
                        div(style = "margin-top: 10px; padding: 10px; background-color: #e7f3ff; border-left: 3px solid #0066cc; border-radius: 4px; font-size: 11px; color: #004085;",
                            icon("info-circle"), " Loads all data, results, and plot settings from saved session"
                        )
                    )
                ),
                column(6,
                    h4(icon("code"), " Export R Code for Publication"),
                    p("Generate publication-ready R scripts for your plots.", style = "color: #666; margin-bottom: 15px;"),
                    
                    if (hasVolcano || hasBoxplot || hasHeatmap || hasPCA || hasMA || hasVenn) {
                        tagList(
                            # Export configuration box
                            div(style = "background: linear-gradient(to bottom, #ffffff, #f8f9fa); border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
                                div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                                    div(style = "flex: 1;",
                                        div(style = "font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 3px;", "Export Type"),
                                        div(style = "font-weight: 600; color: #212529;", "Full Reproducible Script")
                                    ),
                                    div(style = "flex: 1; text-align: right;",
                                        div(style = "font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 3px;", "File Format"),
                                        div(style = "font-weight: 600; color: #212529;", "R Script (.R)")
                                    )
                                ),
                                div(style = "font-size: 11px; color: #6c757d; padding: 8px; background-color: #e7f3ff; border-left: 3px solid #0066cc; border-radius: 4px;",
                                    icon("info-circle"), " Includes data, parameters, and code"
                                )
                            ),
                            
                            # Export ALL button - prominent
                            downloadButton("download_code_all", 
                                         "📦 Export ALL Available Plots", 
                                         class = "btn btn-primary",
                                         style = "width: 100%; margin-bottom: 20px; font-weight: bold; font-size: 15px; padding: 12px; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,123,255,0.3);"),
                            
                            # Individual plots section
                            div(style = "border-top: 2px solid #dee2e6; padding-top: 15px;",
                                h5(style = "color: #495057; font-size: 13px; font-weight: 600; margin-bottom: 12px; text-transform: uppercase; letter-spacing: 0.5px;",
                                   icon("list"), " Individual Plots"
                                ),
                                
                                # Scrollable plot list
                                div(style = "max-height: 350px; overflow-y: auto; padding-right: 5px;",
                                
                                    # PCA and Distance Heatmaps (VST/RLOG) - Following tab order
                                    if (hasPCA) {
                                        tagList(
                                            if (!is.null(myValues$vsd)) {
                                                downloadButton("download_code_pca_vst", 
                                                             "PCA Plot (VST)", 
                                                             class = "btn btn-outline-success btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            } else NULL,
                                            if (!is.null(myValues$vsd)) {
                                                downloadButton("download_code_distheat_vst", 
                                                             "Distance Heatmap (VST)", 
                                                             class = "btn btn-outline-success btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            } else NULL,
                                            if (!is.null(myValues$rld)) {
                                                downloadButton("download_code_pca_rlog", 
                                                             "PCA Plot (RLOG)", 
                                                             class = "btn btn-outline-success btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            } else NULL,
                                            if (!is.null(myValues$rld)) {
                                                downloadButton("download_code_distheat_rlog", 
                                                             "Distance Heatmap (RLOG)", 
                                                             class = "btn btn-outline-success btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            } else NULL
                                        )
                                    } else NULL,
                                    
                                    # MA Plot (DE Results tab)
                                    if (hasMA) {
                                        downloadButton("download_code_ma", 
                                                     "MA Plot", 
                                                     class = "btn btn-outline-success btn-sm",
                                                     style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                    } else NULL,
                                    
                                    # Volcano Plot
                                    if (hasVolcano) {
                                        downloadButton("download_code_volcano", 
                                                     "Volcano Plot", 
                                                     class = "btn btn-outline-success btn-sm",
                                                     style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                    } else NULL,
                                    
                                    # Venn Diagram (Venn tab)
                                    if (hasVenn || hasVennSetHeatmap || hasBrushedVennHeatmap) {
                                        tagList(
                                            if (hasVenn) {
                                                downloadButton("download_code_venn", 
                                                             "Venn Diagram", 
                                                             class = "btn btn-outline-success btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            },
                                            if (hasVennSetHeatmap) {
                                                downloadButton("download_code_venn_set_heatmap", 
                                                             paste0("Venn Set Heatmap: ", input$venn_set_expression_input),
                                                             class = "btn btn-outline-info btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            },
                                            if (hasBrushedVennHeatmap) {
                                                downloadButton("download_code_brushed_venn_heatmap", 
                                                             "Brushed Venn Sub-Heatmap", 
                                                             class = "btn btn-outline-warning btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            }
                                        )
                                    } else NULL,
                                    
                                    # Boxplot (Boxplot tab)
                                    if (hasBoxplot) {
                                        downloadButton("download_code_boxplot", 
                                                     "Gene Boxplot", 
                                                     class = "btn btn-outline-success btn-sm",
                                                     style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                    } else NULL,
                                    
                                    # Heatmap (Heatmap tab)
                                    if (hasHeatmap || hasBrushedHeatmap) {
                                        tagList(
                                            if (hasHeatmap) {
                                                downloadButton("download_code_heatmap", 
                                                             "Expression Heatmap", 
                                                             class = "btn btn-outline-success btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            },
                                            if (hasBrushedHeatmap) {
                                                downloadButton("download_code_brushed_heatmap", 
                                                             paste0("Brushed Sub-Heatmap (", nrow(myValues$brushed_heatmap_data), " genes)"),
                                                             class = "btn btn-outline-info btn-sm",
                                                             style = "width: 100%; margin-bottom: 6px; text-align: left; border-radius: 4px; transition: all 0.2s;")
                                            }
                                        )
                                    } else NULL
                                ) # Close scrollable div
                            ) # Close individual plots section div
                            
                            # Note at bottom
                            # div(style = "margin-top: 15px; padding: 10px; background-color: #e7f3ff; border-left: 3px solid #0066cc; border-radius: 4px; font-size: 11px; color: #004085;",
                            #     icon("lightbulb"), " Tip: Exported code uses current plot parameters and generates publication-quality plots."
                            # )
                        )
                    } else {
                        div(class = "alert alert-warning",
                            icon("exclamation-triangle"), " No plots available to export. Generate plots first."
                        )
                    }
                )
            )
        ))
    })

    list(
        state = myValues,
        session_dir = session_dir,
        modules = list(
            input = input_api,
            design = design_api,
            sva = sva_api,
            deseq = deseq_api,
            results = results_api,
            venn = venn_api,
            volcano = volcano_api,
            boxplot = boxplot_api,
            heatmap = heatmap_api
        )
    )
  })
}

server <- function(input, output, session) {
  stateExportServer("application", input, output, session)
}
