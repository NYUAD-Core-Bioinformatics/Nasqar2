expression_set_data <- reactive({
    input$evaluateExpression

    infix_expression <- isolate(input$venn_set_expression_input)
    postfix_expression <- infix_to_postfix(infix_expression)
    genes <- postfix_eval(postfix_expression)

    return(genes)
})
output$select_venn_ui <- renderUI({
    selectizeInput("select_avo_de_venn_files",
        label = h5("Select DE data"), selected = NULL, multiple = TRUE,
        choices = names(filelist$file_list),
        options = list(maxItems = 5)
    )
})


heatmap_matrix <- reactive({
    input$evaluateExpression

    req(isolate(input$venn_set_expression_input))

    req(length(isolate(input$select_avo_de_venn_files)) > 1)


    genes <- expression_set_data()


    common_genes <- genes[[1]]


    df_list <- list()

    for (f in isolate(input$select_avo_de_venn_files)) {
        df <- read.csv(filelist$file_list[[f]])
        # Index by gene id so every column follows the set-operation order
        df_list[[f]] <- df$log2FoldChange[match(common_genes, df$X)]
    }


    d <- data.frame(df_list)
    colnames(d) <- LETTERS[1:length(df_list)]


     if (input$gene_alias == "included" && input$venn_sel_gene_type == "gene.name") {
        common_genes <- myValues$genenames[common_genes, 1]
    }

    rownames(d) <- common_genes


    return(data.matrix(d))
})



click_action <- function(df, output) {
    output[["info"]] <- renderUI({
        if (!is.null(df)) {
            HTML(qq("<p style='background-color:#FF8080;color:white;padding:5px;'>You have clicked on heatmap @{df$heatmap}, row @{df$row_index}, column @{df$column_index}</p>"))
        }
    })
}



selected_matrix <- reactiveValues()

reset_venn_selection <- function() {
    selected_matrix$matrix <- NULL
    myValues$brushed_venn_heatmap_data <- NULL
    myValues$brushed_venn_genes <- NULL
    myValues$brushed_venn_samples <- NULL
    myValues$brushed_venn_gene_ids <- NULL
    myValues$brushed_venn_gene_order <- NULL
    myValues$venn_heatmap_data_range <- NULL
}

observeEvent(input$evaluateExpression, reset_venn_selection(), priority = 10)
observeEvent(input$plotVenn, reset_venn_selection())
observeEvent(myValues$dds, {
    # a restored state sets dds and the brushed matrix together: keep it
    if (!isTRUE(myValues$restoration_in_progress)) reset_venn_selection()
}, ignoreInit = TRUE)

brush_action <- function(df, output) {
    input$evaluateExpression

    row_index <- unique(unlist(df$row_index))
    column_index <- unique(unlist(df$column_index))
    matrix <- isolate(heatmap_matrix())



    m <- matrix[row_index, column_index, drop = FALSE]
    selected_matrix$matrix <- m

    # Store brushed data for export (similar to server-heatmap.R)
    myValues$brushed_venn_heatmap_data <- m
    myValues$brushed_venn_genes <- rownames(m)  # These might be gene names or gene IDs depending on display mode
    myValues$brushed_venn_samples <- colnames(m)

    # IMPORTANT: Store the original gene IDs for data extraction AND preserve order
    # If rownames are gene names, convert back to gene IDs
    gene_type <- tryCatch(input$venn_sel_gene_type, error = function(e) NULL)

    if (!is.null(gene_type) && gene_type == "gene.name" && !is.null(myValues$geneids)) {
        # Rownames are gene names, convert to gene IDs while preserving order
        gene_ids_mapped <- myValues$geneids[rownames(m), 1]
        # Keep all genes, even if mapping returns NA (preserve order!)
        myValues$brushed_venn_gene_ids <- gene_ids_mapped
        myValues$brushed_venn_gene_order <- rownames(m)  # Original gene names for display
    } else {
        # Rownames are already gene IDs
        myValues$brushed_venn_gene_ids <- rownames(m)
        myValues$brushed_venn_gene_order <- rownames(m)
    }

    # Store the parent heatmap's data range for consistent color scaling in export
    myValues$venn_heatmap_data_range <- c(min(matrix, na.rm = TRUE), max(matrix, na.rm = TRUE))
}











infix_to_postfix <- function(infix) {
    # indix <- "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
    operators <- c("+", "-", "*")
    precedence <- c(1, 1, 1)
    precedence1 <- c(1, 1, 1)
    # infix_tokens <- unlist(strsplit(infix, " "))
    infix <- gsub("\\s", "", infix)
    infix_tokens <- unlist(strsplit(infix, ""))
    stack <- c()
    result <- c()

    # print(infix_tokens)
    for (token in infix_tokens) {
        # print(token)
        # print(operators)
        if (token %in% operators) {
            if (length(stack) > 0 && (stack[length(stack)] %in% operators) && is.logical(precedence[precedence == stack[length(stack)]] >= precedence[precedence == token])) {
                # print(length(stack))
                result <- c(result, stack[length(stack)])
                stack <- stack[-length(stack)]
            }
            stack <- c(stack, token)
        } else if (token == "(") {
            stack <- c(stack, token)
        } else if (token == ")") {
            while (length(stack) > 0 && stack[length(stack)] != "(") {
                result <- c(result, stack[length(stack)])
                stack <- stack[-length(stack)]
            }
            if (length(stack) > 0 && stack[length(stack)] == "(") {
                stack <- stack[-length(stack)]
            } else {
                stop("Unmatched parentheses.")
            }
        } else {
            result <- c(result, token)
        }
    }

    while (length(stack) > 0) {
        if (stack[length(stack)] %in% operators) {
            result <- c(result, stack[length(stack)])
        } else {
            stop("Unmatched parentheses.")
        }
        stack <- stack[-length(stack)]
    }

    paste(result, collapse = " ")
}
postfix_eval <- function(postfix) {
    stack <- list()
    operators <- c("+", "-", "*", "/", "^")

    n_postfix <- list()

    d <- avo_venn_frames_data()
    df_list <- d[[1]]


    i <- 1

    for (token in unlist(strsplit(postfix, " "))) {
        if (length(token) == 1 && token %in% operators) {
            n_postfix[[i]] <- token
        } else {
            n_postfix[[i]] <- df_list[[token]]
        }


        i <- i + 1
    }

    j <- 1
    # print(n_postfix)
    for (token in n_postfix) {
        # print(token)
        if (length(token) == 1 && token %in% operators) {
            if (length(stack) < 2) {
                # print(stack)
                stop("Invalid postfix expression1.")
            }
            operand2 <- stack[[length(stack)]]
            stack <- stack[-length(stack)]
            operand1 <- stack[[length(stack)]]
            stack <- stack[-length(stack)]
            j <- j - 2
            result <- switch(token,
                "+" = union(operand1, operand2),
                "-" = setdiff(operand1, operand2),
                "*" = intersect(operand1, operand2)
            )
            stack[[j]] <- result
        } else {
            stack[[j]] <- token
        }
        j <- j + 1
    }
    # print(stack)
    if (length(stack) != 1) {
        stop("Invalid postfix expression.3")
    }

    stack[1]
}



output$panelStatus <- reactive({
    input$plotVenn > 0
})

outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)


# Set-operation heatmap: built once per "Evaluate Expression" click
venn_set_heatmap <- eventReactive(input$evaluateExpression, {
    matrix <- heatmap_matrix()
    req(nrow(matrix) > 0)
    ht <- Heatmap(matrix)

    pdf(NULL)
    on.exit(dev.off())
    draw(ht)
})

observeEvent(input$evaluateExpression, {
    ht <- tryCatch(venn_set_heatmap(), error = function(e) NULL)
    req(ht)
    makeInteractiveComplexHeatmap(input, output, session, ht,
        click_action = click_action, brush_action = brush_action, "ht1"
    )
})

# Expression result table: built once per "Evaluate Expression" click
venn_expression_table <- eventReactive(input$evaluateExpression, {
    matrix <- heatmap_matrix()
    req(nrow(matrix) > 0)

    req(length(input$select_avo_de_venn_files) > 1)

    req(input$venn_set_expression_input)

    genes <- expression_set_data()
    req(length(genes[1]) > 0)

    values <- list()
    common_genes <- genes[[1]]
    values[["gene.id"]] <- common_genes


    if (input$gene_alias == "included") {
        values[["gene.name"]] <- myValues$genenames[common_genes, ]
    }

    tagnames <- LETTERS[1:length(input$select_avo_de_venn_files)]
    j <- 1
    for (f in input$select_avo_de_venn_files) {
        df <- read.csv(filelist$file_list[[f]])
        df[is.na(df)] <- 0

        # Index by gene id so every column follows the set-operation order
        values[[paste0(tagnames[j], ".logFC")]] <- df$log2FoldChange[match(common_genes, df$X)]
        j <- j + 1
    }

    data.frame(values)
})

output$venn_expression_result <- renderDataTable(
    {
        DT::datatable(venn_expression_table())
    },
    options = list(scrollX = TRUE, pageLength = 5)
)




avo_venn_frames_data <- reactive({
    input$plotVenn

    # req(filelist$file_list)
    select_avo_de_venn_files <- isolate(input$select_avo_de_venn_files)
    venn_significance_threshold <- isolate(input$venn_significance_threshold)
    venn_log_fold_change_threshold <- isolate(input$venn_log_fold_change_threshold)
    req(length(select_avo_de_venn_files) > 1)
    validate(need(
        length(select_avo_de_venn_files) <= 5,
        "Venn diagrams support at most five comparisons."
    ))




    # print(input$avo_de_file)
    df_list <- c()
    namelist <- c()
    j <- 1

    for (f in select_avo_de_venn_files) {
        # print(f)
        # print(filelist$file_list[[f]])
        i <- 1
        df <- read.csv(filelist$file_list[[f]])
        df <- na.omit(df)
        # df <- df[df$padj > 0, ]

        # Use either the slider threshold or direct padj threshold based on user selection
        if (isolate(input$venn_threshold_type) == "slider") {
            df <- df[(df$padj < 1 / 10^as.numeric(venn_significance_threshold)) & abs(df$log2FoldChange) > as.numeric(venn_log_fold_change_threshold), ]
        } else {
            df <- df[(df$padj < as.numeric(isolate(input$venn_direct_padj))) & abs(df$log2FoldChange) > as.numeric(venn_log_fold_change_threshold), ]
        }

        if (input$venn_sig_genes_selection == "1") {

        }



        if (input$venn_sig_genes_selection == "2") {
            df <- df[df$log2FoldChange > as.numeric(venn_log_fold_change_threshold), ]
        }


        if (input$venn_sig_genes_selection == "3") {
            df <- df[
                df$log2FoldChange < -as.numeric(venn_log_fold_change_threshold),
            ]
        }

        colnames(df)[1] <- "gene.id"
        # print(df)

        df_list[[j]] <- df$gene.id
        namelist[j] <- f
        j <- j + 1
    }



    names(df_list) <- LETTERS[1:length(df_list)]

    return(list(df_list, namelist))
})


generateBinaryMatrix <- function(set_names) {
    bits <- length(set_names)
    values <- c(0, 1)
    combinations <- expand.grid(replicate(bits, values, simplify = FALSE))
    colnames(combinations) <- set_names
    sortedCombinations <- combinations[do.call(order, combinations), ]
    rownames(sortedCombinations) <- apply(sortedCombinations, 1, function(row) {
        selectedColumns <- colnames(sortedCombinations)[row == 1]
        unselectedColumns <- colnames(sortedCombinations)[row == 0]
        if (length(selectedColumns) != 0) {
            if (length(unselectedColumns) == 0) {
                v <- paste0(selectedColumns, collapse = "*")
            } else {
                v <- paste0(c(paste0("(", paste0(selectedColumns, collapse = "*"), ")"), paste0("(", paste0(unselectedColumns, collapse = "+"), ")")), collapse = "-")
            }
        } else {
            v <- ""
        }

        return(v)
    })
    return(sortedCombinations)
}

observeEvent(input$select_expression, {
    updateTextInput(session, "venn_set_expression_input", value = input$select_expression)
})

output$gene_data_sets <- DT::renderDataTable(
    {
        d <- avo_venn_frames_data()

        df <- data.frame("File Name" = d[[2]], "Label" = names(d[[1]]))
        set_names <- as.vector(names(d[[1]]))  # Ensure simple vector

        combinations <- generateBinaryMatrix(set_names)
        set_expressions <- as.vector(rownames(combinations[-1, ]))  # Ensure simple vector
        updateSelectInput(session, "select_expression",
            choices = set_expressions,
            selected = set_expressions[length(set_expressions)]
        )



        DT::datatable(df)
    },
    options = list(scrollX = TRUE)
)



output$vennDiagram <- renderPlot({
    req(input$plotVenn)
    a <- avo_venn_frames_data()
    df_list <- a[[1]]

    v1 <- venn.diagram(df_list, filename = NULL, fill = rainbow(length(df_list)))

    grid.newpage()
    grid.draw(v1)
})

# Download Venn diagram with custom size and format
output$download_venn_plot <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- input$venn_plot_format
        paste0("venn_diagram_", timestamp, ".", ext)
    },
    content = function(file) {
        req(input$plotVenn)
        req(avo_venn_frames_data())

        a <- avo_venn_frames_data()
        df_list <- a[[1]]

        plot_width <- input$venn_plot_width
        plot_height <- input$venn_plot_height
        plot_format <- input$venn_plot_format
        plot_dpi <- input$venn_plot_dpi

        # Generate the Venn diagram
        v1 <- venn.diagram(df_list, filename = NULL, fill = rainbow(length(df_list)))

        # Save with specified format and dimensions
        if (plot_format == "pdf") {
            pdf(file, width = plot_width, height = plot_height)
            grid.newpage()
            grid.draw(v1)
            dev.off()
        } else if (plot_format == "png") {
            png(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi)
            grid.newpage()
            grid.draw(v1)
            dev.off()
        } else if (plot_format == "jpeg") {
            jpeg(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, quality = 95)
            grid.newpage()
            grid.draw(v1)
            dev.off()
        } else if (plot_format == "tiff") {
            tiff(file, width = plot_width, height = plot_height, units = "in", res = plot_dpi, compression = "lzw")
            grid.newpage()
            grid.draw(v1)
            dev.off()
        }
    }
)

# Export R code for Venn diagram
output$download_code_venn <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("venn_diagram_export_", timestamp, ".zip")
    },
    content = function(file) {
        req(input$plotVenn)
        req(input$select_avo_de_venn_files, length(input$select_avo_de_venn_files) > 1)

        # Get comparison names
        comparisons <- input$select_avo_de_venn_files

        # Check if set operation heatmap exists
        has_set_heatmap <- FALSE
        set_heatmap_code <- ""
        set_heatmap_matrix <- NULL
        set_expression <- NULL

        tryCatch({
            if (!is.null(input$venn_set_expression_input) &&
                nchar(input$venn_set_expression_input) > 0 &&
                input$evaluateExpression > 0) {
                # Try to get the heatmap matrix (already filtered by Shiny)
                set_heatmap_matrix <- isolate(heatmap_matrix())
                if (!is.null(set_heatmap_matrix) && nrow(set_heatmap_matrix) > 0) {
                    has_set_heatmap <- TRUE
                    set_expression <- isolate(input$venn_set_expression_input)
                }
            }
        }, error = function(e) {
            # Silently ignore if heatmap doesn't exist
            has_set_heatmap <<- FALSE
        })

        # Get timestamp for consistent filenames across all exports
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        # Check if brushed heatmap exists
        has_brushed_heatmap <- FALSE
        brushed_heatmap_code <- ""
        brushed_matrix <- NULL

        tryCatch({
            if (!is.null(selected_matrix$matrix) && nrow(selected_matrix$matrix) > 0) {
                has_brushed_heatmap <- TRUE
                brushed_matrix <- selected_matrix$matrix
            }
        }, error = function(e) {
            # Silently ignore if brushed heatmap doesn't exist
            has_brushed_heatmap <<- FALSE
        })

        temp_dir <- session_dir
        export_dir <- file.path(temp_dir, paste0("venn_diagram_export_", timestamp))
        dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)

        # Get Venn data
        venn_data <- avo_venn_frames_data()
        df_list <- venn_data[[1]]

        # Export gene lists for each comparison (one CSV per set)
        gene_filenames <- c()
        for (i in 1:length(comparisons)) {
            comp_name <- gsub("\\.csv$", "", comparisons[i])
            genes <- df_list[[LETTERS[i]]]

            gene_filename <- paste0("genes_set_", LETTERS[i], "_", comp_name, "_", timestamp, ".csv")
            gene_file <- file.path(export_dir, gene_filename)
            write.csv(data.frame(gene_id = genes), gene_file, row.names = FALSE)
            gene_filenames <- c(gene_filenames, gene_filename)
        }

        # Create simplified params for Venn diagram
        params <- list(
            num_sets = length(comparisons),
            comparison_names = gsub("\\.csv$", "", comparisons),
            gene_list_files = gene_filenames,
            venn_colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8")
        )

        # Use simplified template - just loads gene lists and draws Venn
        r_code <- generateVennCodeSimple(params)

        # Export set operation heatmap data if it exists
        if (has_set_heatmap) {
            # Export set heatmap matrix (log2FC values for genes in the set)
            set_data_filename <- paste0("venn_set_heatmap_matrix_", timestamp, ".csv")
            set_data_file <- file.path(export_dir, set_data_filename)
            write.csv(set_heatmap_matrix, set_data_file, row.names = TRUE)

            # Use simplified params - just load matrix and plot
            set_params <- list(
                set_expression = set_expression,
                heatmap_matrix_file = set_data_filename,
                fontsize_row = if(nrow(set_heatmap_matrix) > 50) 6 else 8,
                num_genes = nrow(set_heatmap_matrix)
            )

            # Use simplified template
            set_heatmap_code <- generateVennSetHeatmapCodeSimple(set_params)
        }

        # Export brushed heatmap data if it exists
        if (has_brushed_heatmap) {
            # Export brushed matrix (exact matrix from brush - log2FC values)
            brushed_data_filename <- paste0("venn_brushed_heatmap_matrix_", timestamp, ".csv")
            brushed_data_file <- file.path(export_dir, brushed_data_filename)
            write.csv(brushed_matrix, brushed_data_file, row.names = TRUE)

            # Use simplified params - just load matrix and plot (no clustering)
            brushed_params <- list(
                set_description = paste0("Brushed subset (", nrow(brushed_matrix), " genes)"),
                heatmap_matrix_file = brushed_data_filename,
                fontsize_row = if(nrow(brushed_matrix) > 50) 6 else 8,
                num_genes = nrow(brushed_matrix)
            )

            # Use dedicated brushed template - no clustering, preserves brush order
            brushed_heatmap_code <- generateVennBrushedHeatmapCodeSimple(brushed_params)
        }

        # Combine all R code sections
        combined_code <- r_code

        if (has_set_heatmap) {
            combined_code <- paste0(
                combined_code,
                "\n\n",
                "################################################################################\n",
                "#  VENN SET OPERATION HEATMAP\n",
                "################################################################################\n\n",
                set_heatmap_code
            )
        }

        if (has_brushed_heatmap) {
            combined_code <- paste0(
                combined_code,
                "\n\n",
                "################################################################################\n",
                "#  BRUSHED SUB-HEATMAP FROM VENN SET OPERATION\n",
                "################################################################################\n\n",
                brushed_heatmap_code
            )
        }

        # Write R code
        code_filename <- paste0("venn_diagram_", timestamp, ".R")
        code_file <- file.path(export_dir, code_filename)
        writeLines(combined_code, code_file)

        # Create README
        readme_file <- file.path(export_dir, "README.txt")
        readme_text <- paste0(
            "Venn Diagram R Code Export\n",
            "==========================\n\n",
            "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
            "Comparisons: ", paste(comparisons, collapse = ", "), "\n"
        )

        if (has_set_heatmap) {
            readme_text <- paste0(readme_text,
                "Set Expression: ", set_expression, "\n",
                "Set Operation Genes: ", nrow(set_heatmap_matrix), "\n"
            )
        }

        if (has_brushed_heatmap) {
            readme_text <- paste0(readme_text,
                "Brushed Heatmap Genes: ", nrow(brushed_matrix), "\n"
            )
        }

        readme_text <- paste0(readme_text,
            "\nFiles included:\n",
            "- ", code_filename, " : R code to generate the Venn diagram",
            if(has_set_heatmap) " and set operation heatmap" else "",
            if(has_brushed_heatmap) " and brushed sub-heatmap" else "",
            "\n",
            paste(sapply(1:length(gene_filenames), function(i) {
                paste0("- ", gene_filenames[i], " : Gene list for set ", LETTERS[i], " (", length(df_list[[LETTERS[i]]]), " genes)\n")
            }), collapse = "")
        )

        if (has_set_heatmap) {
            readme_text <- paste0(readme_text,
                "- venn_set_heatmap_matrix_", timestamp, ".csv : Pre-processed heatmap matrix (log2FC values)\n"
            )
        }

        if (has_brushed_heatmap) {
            readme_text <- paste0(readme_text,
                "- venn_brushed_heatmap_matrix_", timestamp, ".csv : Brushed sub-heatmap matrix (log2FC values)\n"
            )
        }

        readme_text <- paste0(readme_text,
            "- README.txt : This file\n\n",
            "Instructions:\n",
            "1. Extract all files to the same directory\n",
            "2. Open the R script in RStudio\n",
            "3. Install VennDiagram package if needed: install.packages('VennDiagram')\n",
            "4. Run the script to generate the Venn diagram",
            if(has_set_heatmap || has_brushed_heatmap) " and heatmaps" else "",
            "\n\n",
            "Note: All gene filtering (padj, log2FC thresholds) was done by Shiny.\n",
            "The gene list files contain only the genes that passed your filters.\n"
        )

        writeLines(readme_text, readme_file)

        # Create ZIP from the directory
        zip_file <- file.path(temp_dir, paste0("venn_diagram_export_", timestamp, ".zip"))
        zip_export_dir(export_dir, zip_file)

        # Copy to output
        file.copy(zip_file, file)
    }
)

# Export R code for Venn Set Operation Heatmap
output$download_code_venn_set_heatmap <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        # Check if data is available
        heatmap_check <- tryCatch(heatmap_matrix(), error = function(e) NULL)
        if (is.null(heatmap_check) || is.null(input$venn_set_expression_input) ||
            nchar(input$venn_set_expression_input) == 0) {
            return("venn_set_heatmap_error.txt")
        }

        paste0("venn_set_heatmap_export_", timestamp, ".zip")
    },
    content = function(file) {
        # Check if data is available
        heatmap_data <- tryCatch(heatmap_matrix(), error = function(e) NULL)
        if (is.null(heatmap_data) || is.null(input$venn_set_expression_input) ||
            nchar(input$venn_set_expression_input) == 0) {
            error_msg <- paste0(
                "Error: Venn Set Heatmap export not available\n\n",
                "Please configure the Venn Set Operation first:\n",
                "1. Navigate to the 'Venn Diagram' tab\n",
                "2. Select 2+ comparison files\n",
                "3. Enter a set expression (e.g., A&B, A|B, A-B)\n",
                "4. Click 'Evaluate Expression' and wait for the heatmap to render\n",
                "5. Then return to export the plot code\n"
            )
            writeLines(error_msg, file)
            return(invisible(NULL))
        }

        set_expression <- isolate(input$venn_set_expression_input)

        # Full reproducible export
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        export_dir <- file.path(session_dir, paste0("venn_set_heatmap_export_", timestamp))
        dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)

        # Save heatmap matrix data (already processed by Shiny - just log2FC values)
        matrix_filename <- paste0("venn_set_heatmap_matrix_", timestamp, ".csv")
        matrix_file <- file.path(export_dir, matrix_filename)
        write.csv(heatmap_data, matrix_file, row.names = TRUE)

        # NOW set params with actual filenames and generate simplified R code
        params <- list(
            set_expression = set_expression,
            heatmap_matrix_file = matrix_filename,  # Pre-processed matrix!
            fontsize_row = if(nrow(heatmap_data) > 50) 6 else 8,
            num_genes = nrow(heatmap_data)
        )

        # Use simplified template - no filtering, no processing, just load and plot
        r_code <- generateVennSetHeatmapCodeSimple(params)

        # Save R code
        code_file <- file.path(export_dir, paste0("venn_set_heatmap_", timestamp,
                                                   ".R"))
        writeLines(r_code, code_file)

        # Create README
        readme_file <- file.path(export_dir, "README.txt")
        readme_text <- paste0(
            "Venn Set Operation Heatmap R Code Export\n",
            "=========================================\n\n",
            "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
            "Set Expression: ", set_expression, "\n",
            "Genes in heatmap: ", nrow(heatmap_data), "\n",
            "Contrasts: ", paste(colnames(heatmap_data), collapse = ", "), "\n\n",
            "Files included:\n",
            "- ", basename(code_file), " : R code to generate the heatmap\n",
            "- ", matrix_filename, " : Pre-processed heatmap matrix (log2 fold changes)\n",
            "- README.txt : This file\n\n",
            "Instructions:\n",
            "1. Extract all files to the same directory\n",
            "2. Open the R script in RStudio\n",
            "3. Run the script to generate the heatmap\n",
            "4. Plots will be saved as PDF and PNG\n\n",
            "Note: The matrix contains log2 fold change values for genes in the Venn set.\n",
            "All filtering and set operations were done by Shiny - the script just loads and plots.\n"
        )
        writeLines(readme_text, readme_file)

        # Create zip file
        zip_file <- paste0(export_dir, ".zip")
        zip_export_dir(export_dir, zip_file)

        # Copy to output
        file.copy(zip_file, file, overwrite = TRUE)
    }
)

# Export R code for Brushed Venn Set Heatmap
output$download_code_brushed_venn_heatmap <- nasqar_download_handler(
    filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        # Check if brushed data is available
        if (is.null(selected_matrix$matrix) || nrow(selected_matrix$matrix) == 0) {
            return("brushed_venn_heatmap_error.txt")
        }

        paste0("brushed_venn_heatmap_export_", timestamp, ".zip")
    },
    content = function(file) {
        # Check if brushed data is available
        if (is.null(selected_matrix$matrix) || nrow(selected_matrix$matrix) == 0) {
            error_msg <- paste0(
                "Error: Brushed Venn Heatmap export not available\n\n",
                "Please brush/select an area on the Venn Set Heatmap first:\n",
                "1. Navigate to the 'Venn Diagram' tab\n",
                "2. Generate a Venn Set Operation heatmap\n",
                "3. Click and drag on the heatmap to select genes\n",
                "4. Then export the brushed sub-heatmap\n"
            )
            writeLines(error_msg, file)
            return(invisible(NULL))
        }

        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        brushed_matrix <- selected_matrix$matrix

        # Use simplified params - just export matrix and plot
        brushed_params <- list(
            set_description = paste0("Brushed subset (", nrow(brushed_matrix), " genes)"),
            heatmap_matrix_file = paste0("venn_brushed_heatmap_matrix_", timestamp, ".csv"),
            fontsize_row = if(nrow(brushed_matrix) > 50) 6 else 8,
            num_genes = nrow(brushed_matrix)
        )

        # Use dedicated brushed template - no clustering, preserves brush order
        r_code <- generateVennBrushedHeatmapCodeSimple(brushed_params)

        # Full reproducible export
        export_dir <- file.path(session_dir, paste0("brushed_venn_heatmap_export_", timestamp))
        dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)

        # Save brushed heatmap matrix (log2FC values)
        matrix_filename <- paste0("venn_brushed_heatmap_matrix_", timestamp, ".csv")
        matrix_file <- file.path(export_dir, matrix_filename)
        write.csv(brushed_matrix, matrix_file, row.names = TRUE)

        # Save R code
        code_filename <- paste0("brushed_venn_heatmap_", timestamp, ".R")
        code_file <- file.path(export_dir, code_filename)
        writeLines(r_code, code_file)

        # Create README
        readme_file <- file.path(export_dir, "README.txt")
        readme_text <- paste0(
            "Brushed Venn Set Heatmap R Code Export\n",
            "======================================\n\n",
            "Generated from DESeq2Shiny on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
            "Brushed genes: ", nrow(brushed_matrix), "\n",
            "Contrasts: ", ncol(brushed_matrix), " (", paste(colnames(brushed_matrix), collapse = ", "), ")\n\n",
            "Files included:\n",
            "- ", code_filename, " : R code to generate the brushed heatmap\n",
            "- ", matrix_filename, " : Brushed matrix (log2 fold changes)\n",
            "- README.txt : This file\n\n",
            "Instructions:\n",
            "1. Extract all files to the same directory\n",
            "2. Open the R script (", code_filename, ") in RStudio\n",
            "3. Run the script to generate the heatmap\n\n",
            "Note: The matrix contains the exact brushed genes with log2 fold changes.\n",
            "Gene and contrast order are preserved from your brush selection.\n",
            "No clustering applied to maintain your selected view.\n"
        )
        writeLines(readme_text, readme_file)

        # Create zip file
        zip_file <- paste0(export_dir, ".zip")
        zip_export_dir(export_dir, zip_file)

        # Copy to output
        file.copy(zip_file, file, overwrite = TRUE)
    }
)
