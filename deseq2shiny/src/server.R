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
    # Remove NA values for analysis
    factor_data_clean <- factor_data[!is.na(factor_data)]
    
    if (length(factor_data_clean) == 0) {
        return(FALSE)  # All NA values
    }
    
    # Explicit factor or character variables are categorical
    if (is.factor(factor_data) || is.character(factor_data)) {
        unique_count <- length(unique(factor_data_clean))
        # Must have at least 2 levels and no more than half the sample size
        return(unique_count >= 2 && unique_count <= max(2, sample_count * 0.5))
    }
    
    # For numeric variables, be very strict
    if (is.numeric(factor_data)) {
        unique_count <- length(unique(factor_data_clean))
        # Only consider numeric as categorical if:
        # 1. Very few unique values (≤ 5)
        # 2. At least 2 levels for comparison
        # 3. No more than 20% of sample size
        return(unique_count >= 2 && unique_count <= min(5, sample_count * 0.2))
    }
    
    return(FALSE)
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


server <- function(input, output, session) {
    # Add error handling for client-side communication issues
    session$onFlushed(function() {
        # This runs after the session is flushed to the client
        # Add any post-flush validation here if needed
    })
    
    # Add error handling for session errors
    session$onSessionEnded(function() {
        # Clean up any resources
    })
    
    source("server-inputdata.R", local = TRUE)

    source("server-conditions.R", local = TRUE)

    source("server-svaseq.R", local = TRUE)

    source("server-runDeseq.R", local = TRUE)

    source("server-analysisRes.R", local = TRUE)
    source("server-venndiagram.R", local = TRUE)
    source("server-volcanoplot.R", local = TRUE)

    source("server-boxplot.R", local = TRUE)

    source("server-heatmap.R", local = TRUE)

    GotoTab <- function(name) {
        shinyjs::show(selector = paste0("a[data-value=\"", name, "\"]"))

        shinyjs::runjs("window.scrollTo(0, 0)")
    }
    
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
            heatmap_path = myValues$heatmap_path,
            
            # Volcano plot and Venn diagram data (saved analysis results for different datasets)
            filelist_file_list = if(exists("filelist") && !is.null(filelist$file_list)) filelist$file_list else NULL,
            
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
            app_version = "deseq2shiny_v1.1_enhanced_plots"
        )
        
        return(state_object)
    }
    
    loadAppState <- function(state_object) {
        # Validate state object
        if (!is.list(state_object)) {
            showNotification("Invalid state file format", type = "error", duration = 5)
            return(FALSE)
        }
        
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
            filelist$file_list <<- state_object$filelist_file_list
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
                load(input$loadStateFile$datapath)
                
                # Step 2: Validating state object
                setProgress(value = 0.2, detail = "Validating state file...")
                if (!exists("state_object")) {
                    showNotification("Invalid state file: 'state_object' not found", type = "error", duration = 5)
                    return()
                }
                
                # Step 3: Loading core data
                setProgress(value = 0.3, detail = "Loading core data...")
                
                # Load DESeq2 objects
                setProgress(value = 0.4, detail = "Loading DESeq2 objects...")
                if (!is.null(state_object$dds)) myValues$dds <- state_object$dds
                if (!is.null(state_object$ddsSva)) myValues$ddsSva <- state_object$ddsSva
                if (!is.null(state_object$ddsAddSV)) myValues$ddsAddSV <- state_object$ddsAddSV
                
                # Load analysis results
                setProgress(value = 0.5, detail = "Loading analysis results...")
                if (!is.null(state_object$vsResults)) myValues$vsResults <- state_object$vsResults
                if (!is.null(state_object$vsResultsSva)) myValues$vsResultsSva <- state_object$vsResultsSva
                if (!is.null(state_object$vsResultsAddSV)) myValues$vsResultsAddSV <- state_object$vsResultsAddSV
                
                # Load transformations
                setProgress(value = 0.55, detail = "Loading transformations...")
                if (!is.null(state_object$rlogTransformation)) myValues$rlogTransformation <- state_object$rlogTransformation
                if (!is.null(state_object$vstTransformation)) myValues$vstTransformation <- state_object$vstTransformation
                
                # Load other components
                setProgress(value = 0.6, detail = "Loading additional components...")
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
                }
                
            }, error = function(e) {
                showNotification(
                    paste("Error loading state file:", e$message),
                    type = "error", duration = 5
                )
            })
        })
    })
    
    # Check if state can be saved (has meaningful data)
    output$canSaveState <- reactive({
        return(!is.null(myValues$dataCounts) || !is.null(myValues$dds))
    })
    outputOptions(output, "canSaveState", suspendWhenHidden = FALSE)
    
    # Observer for showing state modal (no action needed as bsModal handles it automatically)
}
