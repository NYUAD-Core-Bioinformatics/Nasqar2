# Max upload size
options(shiny.maxRequestSize = 600 * 1024^2)
suppressPackageStartupMessages(library(kableExtra))

# Define server

# Function to generate n distinct random colors
generate_random_colors <- function(n) {
  colors <- grDevices::colors()[sample(1:657, n)]
  return(colors)
}


server <- function(input, output, session) {
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
                volcano_significance_threshold = if(!is.null(input$significance_threshold)) input$significance_threshold else NULL,
                volcano_log_fold_change_threshold = if(!is.null(input$log_fold_change_threshold)) input$log_fold_change_threshold else NULL,
                volcano_threshold_type = if(!is.null(input$volcano_threshold_type)) input$volcano_threshold_type else NULL,
                volcano_direct_padj = if(!is.null(input$volcano_direct_padj)) input$volcano_direct_padj else NULL,
                select_avo_de_file = if(!is.null(input$select_avo_de_file)) input$select_avo_de_file else NULL,
                sig_genes_selection = if(!is.null(input$sig_genes_selection)) input$sig_genes_selection else NULL,
                
                # Venn diagram parameters
                venn_significance_threshold = if(!is.null(input$venn_significance_threshold)) input$venn_significance_threshold else NULL,
                venn_log_fold_change_threshold = if(!is.null(input$venn_log_fold_change_threshold)) input$venn_log_fold_change_threshold else NULL,
                venn_threshold_type = if(!is.null(input$venn_threshold_type)) input$venn_threshold_type else NULL,
                venn_direct_padj = if(!is.null(input$venn_direct_padj)) input$venn_direct_padj else NULL,
                venn_sig_genes_selection = if(!is.null(input$venn_sig_genes_selection)) input$venn_sig_genes_selection else NULL,
                select_avo_de_venn_files = if(!is.null(input$select_avo_de_venn_files)) input$select_avo_de_venn_files else NULL,
                venn_set_expression_input = if(!is.null(input$venn_set_expression_input)) input$venn_set_expression_input else NULL,
                venn_sel_gene_type = if(!is.null(input$venn_sel_gene_type)) input$venn_sel_gene_type else NULL,
                select_expression = if(!is.null(input$select_expression)) input$select_expression else NULL,
                venn_gene_list = if(!is.null(input$venn_gene_list)) input$venn_gene_list else NULL,
                venn_input_genes_sep = if(!is.null(input$venn_input_genes_sep)) input$venn_input_genes_sep else NULL,
                evaluateExpression = if(!is.null(input$evaluateExpression)) input$evaluateExpression else NULL,
                
                # Boxplot parameters
                boxplotFill = if(!is.null(input$boxplotFill)) input$boxplotFill else NULL,
                boxplotX = if(!is.null(input$boxplotX)) input$boxplotX else NULL,
                sel_gene = if(!is.null(input$sel_gene)) input$sel_gene else NULL,
                sel_groups = if(!is.null(input$sel_groups)) input$sel_groups else NULL,
                sel_factors = if(!is.null(input$sel_factors)) input$sel_factors else NULL,
                box_plot_sel_gene_type = if(!is.null(input$box_plot_sel_gene_type)) input$box_plot_sel_gene_type else NULL,
                levelSelect = if(!is.null(input$levelSelect)) input$levelSelect else NULL,
                levelColor = if(!is.null(input$levelColor)) input$levelColor else NULL,
                applyColor = if(!is.null(input$applyColor)) input$applyColor else NULL,
                
                # Heatmap parameters
                numGenes = if(!is.null(input$numGenes)) input$numGenes else NULL,
                subsetGenes = if(!is.null(input$subsetGenes)) input$subsetGenes else NULL,
                listPasteGenes = if(!is.null(input$listPasteGenes)) input$listPasteGenes else NULL,
                heatmap_sel_gene_type = if(!is.null(input$heatmap_sel_gene_type)) input$heatmap_sel_gene_type else NULL,
                heat_group = if(!is.null(input$heat_group)) input$heat_group else NULL,
                genHeatmap = if(!is.null(input$genHeatmap)) input$genHeatmap else NULL,
                
                # Additional UI state
                gene_alias = if(!is.null(input$gene_alias)) input$gene_alias else NULL,
                no_replicates = if(!is.null(input$no_replicates)) input$no_replicates else NULL,
                
                # Differential Expression Analysis parameters
                resultNameOrFactor = if(!is.null(input$resultNameOrFactor)) input$resultNameOrFactor else NULL,
                resultNamesInput = if(!is.null(input$resultNamesInput)) input$resultNamesInput else NULL,
                factorNameInput = if(!is.null(input$factorNameInput)) input$factorNameInput else NULL,
                condition1 = if(!is.null(input$condition1)) input$condition1 else NULL,
                condition2 = if(!is.null(input$condition2)) input$condition2 else NULL,
                getDiffResVs = if(!is.null(input$getDiffResVs)) input$getDiffResVs else NULL,
                
                # MA Plot parameters
                alpha = if(!is.null(input$alpha)) input$alpha else NULL,
                ylim = if(!is.null(input$ylim)) input$ylim else NULL,
                
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
        
        # Load all state components back into myValues
        if (!is.null(state_object$dataCounts)) myValues$dataCounts <- state_object$dataCounts
        if (!is.null(state_object$fileContent)) myValues$fileContent <- state_object$fileContent
        if (!is.null(state_object$DF)) myValues$DF <- state_object$DF
        if (!is.null(state_object$conditions)) myValues$conditions <- state_object$conditions
        
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
            if (!is.null(state_object$saved_inputs$sel_gene)) {
                updateSelectizeInput(session, "sel_gene", selected = state_object$saved_inputs$sel_gene)
            }
            if (!is.null(state_object$saved_inputs$sel_groups)) {
                updateSelectizeInput(session, "sel_groups", selected = state_object$saved_inputs$sel_groups)
            }
            if (!is.null(state_object$saved_inputs$sel_factors)) {
                updateSelectizeInput(session, "sel_factors", selected = state_object$saved_inputs$sel_factors)
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
                saved_sel_factors = NULL
            )
        }
        
        # Set restoration state
        restoration_state$in_progress <- TRUE
        restoration_state$saved_sel_gene <- state_object$saved_inputs$sel_gene
        restoration_state$saved_sel_groups <- state_object$saved_inputs$sel_groups
        restoration_state$saved_sel_factors <- state_object$saved_inputs$sel_factors
        
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
                                if (!is.null(state_object$saved_inputs$sel_gene)) {
                                    updateSelectizeInput(session, "sel_gene", selected = state_object$saved_inputs$sel_gene)
                                }
                                if (!is.null(state_object$saved_inputs$sel_groups)) {
                                    updateSelectizeInput(session, "sel_groups", selected = state_object$saved_inputs$sel_groups)
                                }
                                if (!is.null(state_object$saved_inputs$sel_factors)) {
                                    updateSelectizeInput(session, "sel_factors", selected = state_object$saved_inputs$sel_factors)
                                }
                                # Clear restoration state
                                restoration_state$in_progress <- FALSE
                                final_restore$done <- TRUE
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
                updateNumericInput(session, "significance_threshold", value = inputs$volcano_significance_threshold)
            }
            if (!is.null(inputs$volcano_log_fold_change_threshold)) {
                updateNumericInput(session, "log_fold_change_threshold", value = inputs$volcano_log_fold_change_threshold)
            }
            if (!is.null(inputs$volcano_threshold_type)) {
                updateRadioButtons(session, "volcano_threshold_type", selected = inputs$volcano_threshold_type)
            }
            if (!is.null(inputs$volcano_direct_padj)) {
                updateNumericInput(session, "volcano_direct_padj", value = inputs$volcano_direct_padj)
            }
            if (!is.null(inputs$select_avo_de_file) && !is.null(filelist$file_list)) {
                updateSelectInput(session, "select_avo_de_file", selected = inputs$select_avo_de_file)
            }
            if (!is.null(inputs$sig_genes_selection)) {
                updateRadioButtons(session, "sig_genes_selection", selected = inputs$sig_genes_selection)
            }
            
            # Restore Venn diagram parameters
            if (!is.null(inputs$venn_significance_threshold)) {
                updateNumericInput(session, "venn_significance_threshold", value = inputs$venn_significance_threshold)
            }
            if (!is.null(inputs$venn_log_fold_change_threshold)) {
                updateNumericInput(session, "venn_log_fold_change_threshold", value = inputs$venn_log_fold_change_threshold)
            }
            if (!is.null(inputs$venn_threshold_type)) {
                updateRadioButtons(session, "venn_threshold_type", selected = inputs$venn_threshold_type)
            }
            if (!is.null(inputs$venn_direct_padj)) {
                updateNumericInput(session, "venn_direct_padj", value = inputs$venn_direct_padj)
            }
            if (!is.null(inputs$venn_sig_genes_selection)) {
                updateSelectInput(session, "venn_sig_genes_selection", selected = inputs$venn_sig_genes_selection)
            }
            # Note: select_avo_de_venn_files is dynamic and restored in restoreDelayedInputParameters
            if (!is.null(inputs$venn_set_expression_input)) {
                updateTextInput(session, "venn_set_expression_input", value = inputs$venn_set_expression_input)
            }
            if (!is.null(inputs$venn_sel_gene_type)) {
                updateRadioButtons(session, "venn_sel_gene_type", selected = inputs$venn_sel_gene_type)
            }
            if (!is.null(inputs$select_expression)) {
                updateSelectInput(session, "select_expression", selected = inputs$select_expression)
            }
            if (!is.null(inputs$venn_gene_list)) {
                updateTextAreaInput(session, "venn_gene_list", value = inputs$venn_gene_list)
            }
            if (!is.null(inputs$venn_input_genes_sep)) {
                updateRadioButtons(session, "venn_input_genes_sep", selected = inputs$venn_input_genes_sep)
            }
            # Note: evaluateExpression is an action button, no update needed
            
            # Restore boxplot parameters (static ones only - dynamic ones handled later)
            if (!is.null(inputs$box_plot_sel_gene_type)) {
                updateRadioButtons(session, "box_plot_sel_gene_type", selected = inputs$box_plot_sel_gene_type)
            }
            if (!is.null(inputs$levelColor)) {
                updateColourInput(session, "levelColor", value = inputs$levelColor)
            }
            # Note: Dynamic boxplot inputs (sel_gene, sel_groups, etc.) are restored in restoreDelayedInputParameters
            
            # Restore heatmap parameters
            if (!is.null(inputs$numGenes)) {
                updateNumericInput(session, "numGenes", value = inputs$numGenes)
            }
            if (!is.null(inputs$subsetGenes)) {
                updateCheckboxInput(session, "subsetGenes", value = inputs$subsetGenes)
            }
            if (!is.null(inputs$listPasteGenes)) {
                updateTextAreaInput(session, "listPasteGenes", value = inputs$listPasteGenes)
            }
            if (!is.null(inputs$heatmap_sel_gene_type)) {
                updateRadioButtons(session, "heatmap_sel_gene_type", selected = inputs$heatmap_sel_gene_type)
            }
            if (!is.null(inputs$heat_group)) {
                updateSelectizeInput(session, "heat_group", selected = inputs$heat_group)
            }
            
            # Restore global UI state
            if (!is.null(inputs$gene_alias)) {
                updateRadioButtons(session, "gene_alias", selected = inputs$gene_alias)
            }
            if (!is.null(inputs$no_replicates)) {
                updateCheckboxInput(session, "no_replicates", value = inputs$no_replicates)
            }
            
            # Restore Differential Expression Analysis parameters (static ones only)
            if (!is.null(inputs$resultNameOrFactor)) {
                updateRadioButtons(session, "resultNameOrFactor", selected = inputs$resultNameOrFactor)
            }
            if (!is.null(inputs$resultNamesInput)) {
                updateSelectizeInput(session, "resultNamesInput", selected = inputs$resultNamesInput)
            }
            # Note: Dynamic DE inputs (factorNameInput, condition1, condition2) are restored in restoreDelayedInputParameters
            
            # Restore MA Plot parameters
            if (!is.null(inputs$alpha)) {
                updateSliderInput(session, "alpha", value = inputs$alpha)
            }
            if (!is.null(inputs$ylim)) {
                updateNumericInput(session, "ylim", value = inputs$ylim)
            }
            
            # Also try to restore dynamic parameters here (with error handling)
            tryCatch({
                # Restore boxplot dynamic selections
                if (!is.null(inputs$sel_gene)) {
                    updateSelectizeInput(session, "sel_gene", selected = inputs$sel_gene)
                }
                if (!is.null(inputs$sel_groups)) {
                    updateSelectizeInput(session, "sel_groups", selected = inputs$sel_groups)
                }
                if (!is.null(inputs$sel_factors)) {
                    updateSelectizeInput(session, "sel_factors", selected = inputs$sel_factors)
                }
                if (!is.null(inputs$boxplotX)) {
                    updateSelectInput(session, "boxplotX", selected = inputs$boxplotX)
                }
                if (!is.null(inputs$boxplotFill)) {
                    updateSelectInput(session, "boxplotFill", selected = inputs$boxplotFill)
                }
                if (!is.null(inputs$levelSelect)) {
                    updateSelectInput(session, "levelSelect", selected = inputs$levelSelect)
                }
                
                # Restore DE analysis dynamic selections
                if (!is.null(inputs$factorNameInput)) {
                    updateSelectizeInput(session, "factorNameInput", selected = inputs$factorNameInput)
                }
                if (!is.null(inputs$condition1)) {
                    updateSelectInput(session, "condition1", selected = inputs$condition1)
                }
                if (!is.null(inputs$condition2)) {
                    updateSelectInput(session, "condition2", selected = inputs$condition2)
                }
                if (!is.null(inputs$resultNamesInput)) {
                    updateSelectizeInput(session, "resultNamesInput", selected = inputs$resultNamesInput)
                }
                
                # Restore Venn diagram dynamic selections
                if (!is.null(inputs$select_avo_de_venn_files)) {
                    updateSelectInput(session, "select_avo_de_venn_files", selected = inputs$select_avo_de_venn_files)
                }
            }, error = function(e) {
                # Dynamic restoration may fail if choices aren't populated yet - that's okay
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
        
        # Restore boxplot selections (these depend on myValues$DF and dynamic choices)
        tryCatch({
            if (!is.null(inputs$sel_gene)) {
                cat("DEBUG: Restoring sel_gene:", paste(inputs$sel_gene, collapse = ", "), "\n")
                updateSelectizeInput(session, "sel_gene", selected = inputs$sel_gene)
            }
            if (!is.null(inputs$sel_groups)) {
                cat("DEBUG: Restoring sel_groups:", paste(inputs$sel_groups, collapse = ", "), "\n")
                updateSelectizeInput(session, "sel_groups", selected = inputs$sel_groups)
            }
            if (!is.null(inputs$sel_factors)) {
                cat("DEBUG: Restoring sel_factors:", paste(inputs$sel_factors, collapse = ", "), "\n")
                updateSelectizeInput(session, "sel_factors", selected = inputs$sel_factors)
            }
        }, error = function(e) {
            cat("DEBUG: Error in boxplot restoration:", e$message, "\n")
        })
        if (!is.null(inputs$boxplotX)) {
            updateSelectInput(session, "boxplotX", selected = inputs$boxplotX)
        }
        if (!is.null(inputs$boxplotFill)) {
            updateSelectInput(session, "boxplotFill", selected = inputs$boxplotFill)
        }
        if (!is.null(inputs$levelSelect)) {
            updateSelectInput(session, "levelSelect", selected = inputs$levelSelect)
        }
        
        # Restore DE analysis selections (these depend on myValues$dds)
        # First restore factorNameInput, which will trigger condition choices to be populated
        if (!is.null(inputs$factorNameInput)) {
            cat("DEBUG: Restoring factorNameInput:", inputs$factorNameInput, "\n")
            updateSelectizeInput(session, "factorNameInput", selected = inputs$factorNameInput)
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
                                    factor_levels <- levels(myValues$DF[, inputs$factorNameInput])
                                    cat("DEBUG: Available factor levels for", inputs$factorNameInput, ":", paste(factor_levels, collapse = ", "), "\n")
                                    
                                    # Manually update the condition choices
                                    updateSelectInput(session, "condition1", choices = factor_levels)
                                    updateSelectInput(session, "condition2", choices = factor_levels)
                                    
                                    # Now restore the selected values
                                    if (!is.null(inputs$condition1) && inputs$condition1 %in% factor_levels) {
                                        cat("DEBUG: Restoring condition1:", inputs$condition1, "\n")
                                        updateSelectInput(session, "condition1", selected = inputs$condition1)
                                    }
                                    if (!is.null(inputs$condition2) && inputs$condition2 %in% factor_levels) {
                                        cat("DEBUG: Restoring condition2:", inputs$condition2, "\n")
                                        updateSelectInput(session, "condition2", selected = inputs$condition2)
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
            updateSelectizeInput(session, "resultNamesInput", selected = inputs$resultNamesInput)
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
                factorChoices <- colnames(colData(myValues$dds))
                factorChoices <- factorChoices[!grepl("^SV[::digit::]*", factorChoices)]
                factorChoices <- factorChoices[!(factorChoices %in% c("sizeFactor", "replaceable"))]
                
                updateSelectInput(session, "rlogIntGroupsInput", choices = factorChoices, selected = factorChoices[1])
                updateSelectInput(session, "vsdIntGroupsInput", choices = factorChoices, selected = factorChoices[1])
                updateSelectizeInput(session, "resultNamesInput", choices = resultsNames(myValues$dds), selected = NULL)
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
            state_object <- saveAppState()
            save(state_object, file = file)
            showNotification("Application state saved successfully!", type = "default", duration = 3)
        }
    )
    
    # Load state handler
    observeEvent(input$loadStateFile, {
        req(input$loadStateFile)
        
        tryCatch({
            # Load the state file
            load(input$loadStateFile$datapath)
            
            # Check if state_object exists in the loaded file
            if (!exists("state_object")) {
                showNotification("Invalid state file: 'state_object' not found", type = "error", duration = 5)
                return()
            }
            
            # Load the state
            success <- loadAppState(state_object)
            
            if (success) {
                # Navigate to appropriate tab
                if (!is.null(myValues$vsResults)) {
                    updateTabItems(session, "tabs", "resultsTab")
                } else if (!is.null(myValues$dds)) {
                    updateTabItems(session, "tabs", "deseqTab")
                } else if (!is.null(myValues$DF)) {
                    updateTabItems(session, "tabs", "conditionsTab")
                } else {
                    updateTabItems(session, "tabs", "inputdata")
                }
            }
            
        }, error = function(e) {
            showNotification(
                paste("Error loading state file:", e$message),
                type = "error", duration = 5
            )
        })
    })
    
    # Check if state can be saved (has meaningful data)
    output$canSaveState <- reactive({
        return(!is.null(myValues$dataCounts) || !is.null(myValues$dds))
    })
    outputOptions(output, "canSaveState", suspendWhenHidden = FALSE)
    
    # Observer for showing state modal (no action needed as bsModal handles it automatically)
}
