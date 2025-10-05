# Reactive values to store the custom colors
custom_colors <- reactiveValues(colors = list())

# Update the level selection dropdown based on the selected fill variable
observe({
    req(input$sel_groups)
    # req(input$boxplotX)
    filtered <- geneExrReactive()
    # levels <- unique(filtered[[input$boxplotFill]])
    print('input$sel_groups')

    
    
    # mylevels <- factor(filtered[c(input$boxplotFill)], input$sel_factors)
    isolate({
        tmpgroups <- input$sel_groups
        mylevels <- unlist(lapply(tmpgroups, function(x) {
            levels(myValues$DF[, x])
        }))
        # mylevels <- levels(myValues$DF[, input$boxplotFill])
    # Generate random colors for each level if not already set
    # if (length(custom_colors$colors) == 0) {
        random_colors <- generate_random_colors(length(mylevels))
        custom_colors$globalcolors <- setNames(as.list(random_colors), mylevels)
        mylevels <- levels(myValues$DF[, input$boxplotFill])
        custom_colors$colors <- custom_colors$globalcolors[mylevels]
        
        print(custom_colors$colors)
    # }
    })
    
})

observe({
    req(input$boxplotFill)
    mylevels <- levels(myValues$DF[, input$boxplotFill])
    custom_colors$colors <- custom_colors$globalcolors[mylevels]
    updateSelectInput(session, "levelSelect", choices = mylevels)
})

# Update the custom colors when the user clicks the apply button
  observeEvent(input$applyColor, {

    print('applyColor1')
    print(custom_colors$colors)
    print('applyColor2')
    print(input$levelSelect)
    print('applyColor3')
    print(input$levelColor)
    print('applyColor4')
    req(input$levelSelect, input$levelColor)
    custom_colors$colors[[input$levelSelect]] <- input$levelColor
    print(custom_colors$colors[[input$levelSelect]])
  })

observe({
    # Only trigger when data or gene type changes, not when selections change
    myValues$dataCounts
    myValues$DF
    input$box_plot_sel_gene_type
    
    cat("DEBUG: First boxplot observer triggered\n")
    
    # Isolate current selections to prevent reactive loops
    current_sel_gene <- isolate(input$sel_gene)
    current_sel_groups <- isolate(input$sel_groups)
    
    # Check if restoration is in progress and use saved values
    if (exists("restoration_state") && restoration_state$in_progress) {
        if (!is.null(restoration_state$saved_sel_gene)) {
            current_sel_gene <- restoration_state$saved_sel_gene
        }
        if (!is.null(restoration_state$saved_sel_groups)) {
            current_sel_groups <- restoration_state$saved_sel_groups
        }
        cat("DEBUG: Using restoration state values\n")
    }
    
    cat("DEBUG: Current sel_gene:", if(is.null(current_sel_gene)) "NULL" else paste(current_sel_gene, collapse = ", "), "\n")
    cat("DEBUG: Current sel_groups:", if(is.null(current_sel_groups)) "NULL" else paste(current_sel_groups, collapse = ", "), "\n")
    
    if (input$box_plot_sel_gene_type == "gene.name") {
        genenames <- myValues$genenames[rownames(myValues$dataCounts), ]
        
        # Preserve existing valid selections
        gene_selection <- if(!is.null(current_sel_gene) && all(current_sel_gene %in% genenames)) {
            current_sel_gene
        } else {
            NULL
        }
        
        cat("DEBUG: Updating sel_gene choices, preserving selection:", if(is.null(gene_selection)) "NULL" else paste(gene_selection, collapse = ", "), "\n")
        
        updateSelectizeInput(session, "sel_gene",
            choices = genenames,
            selected = gene_selection,
            server = TRUE
        )
    } else {
        gene_ids <- rownames(myValues$dataCounts)
        
        # Preserve existing valid selections
        gene_selection <- if(!is.null(current_sel_gene) && all(current_sel_gene %in% gene_ids)) {
            current_sel_gene
        } else {
            NULL
        }
        
        cat("DEBUG: Updating sel_gene choices (gene.id), preserving selection:", if(is.null(gene_selection)) "NULL" else paste(gene_selection, collapse = ", "), "\n")
        
        updateSelectizeInput(session, "sel_gene",
            choices = gene_ids,
            selected = gene_selection,
            server = TRUE
        )
    }

    # Preserve existing valid group selections
    group_selection <- if(!is.null(current_sel_groups) && all(current_sel_groups %in% colnames(myValues$DF))) {
        current_sel_groups
    } else {
        NULL
    }
    
    cat("DEBUG: Updating sel_groups choices, preserving selection:", if(is.null(group_selection)) "NULL" else paste(group_selection, collapse = ", "), "\n")
    
    updateSelectizeInput(session, "sel_groups",
        choices = colnames(myValues$DF),
        selected = group_selection,
        server = TRUE
    )
})
observe({
    # Only trigger when data or sel_groups changes, not when other selections change
    myValues$DF
    sel_groups <- input$sel_groups
    
    cat("DEBUG: Second boxplot observer triggered, sel_groups:", if(is.null(sel_groups)) "NULL" else paste(sel_groups, collapse = ", "), "\n")
    
    # Isolate current selections to prevent reactive loops
    current_boxplotX <- isolate(input$boxplotX)
    current_boxplotFill <- isolate(input$boxplotFill)
    current_sel_factors <- isolate(input$sel_factors)
    
    # Check if restoration is in progress and use saved values
    if (exists("restoration_state") && restoration_state$in_progress) {
        if (!is.null(restoration_state$saved_sel_factors)) {
            current_sel_factors <- restoration_state$saved_sel_factors
        }
        cat("DEBUG: Using restoration state for factors\n")
    }
    
    cat("DEBUG: Current sel_factors:", if(is.null(current_sel_factors)) "NULL" else paste(current_sel_factors, collapse = ", "), "\n")
    
    updateSelectInput(session, "boxplotX",
        choices = colnames(myValues$DF),
        selected = if(!is.null(current_boxplotX) && current_boxplotX %in% colnames(myValues$DF)) current_boxplotX else colnames(myValues$DF)[1]
    )

    updateSelectInput(session, "boxplotFill",
        choices = colnames(myValues$DF),
        selected = if(!is.null(current_boxplotFill) && current_boxplotFill %in% colnames(myValues$DF)) current_boxplotFill else colnames(myValues$DF)[1]
    )
    
    # Get available factors based on selected groups
    if (!is.null(sel_groups) && length(sel_groups) > 0) {
        tmpgroups <- unlist(lapply(sel_groups, function(x) {
            levels(myValues$DF[, x])
        }))

        # Preserve existing factor selection if valid
        # If no current selection or invalid selection, select all available factors
        factors_to_select <- if(!is.null(current_sel_factors) && length(current_sel_factors) > 0 && all(current_sel_factors %in% tmpgroups)) {
            current_sel_factors
        } else if (!is.null(tmpgroups) && length(tmpgroups) > 0) {
            tmpgroups
        } else {
            NULL
        }

        updateSelectizeInput(session, "sel_factors",
            choices = tmpgroups, selected = factors_to_select, server = TRUE
        )
    }
})

observe({
    geneExrReactive()
})

geneExrReactive <- reactive({
    validate(need(length(input$sel_gene) > 0, "Please select a gene."))
    validate(need(length(input$sel_groups) > 0, "Please select a group(s)."))
    validate(need(length(input$sel_factors) > 0, "Please select factors."))

    box_plot_sel_gene_type <- isolate(input$box_plot_sel_gene_type)
    sel_gene <- input$sel_gene

    if (box_plot_sel_gene_type == "gene.name") {
        sel_gene <- myValues$geneids[sel_gene, ]
    }


    filtered <- t(log2((counts(myValues$dds[sel_gene, ], normalized = TRUE, replaced = FALSE) + .5))) %>%
        merge(colData(myValues$dds), ., by = "row.names") %>%
        gather(gene, expression, (ncol(.) - length(sel_gene) + 1):ncol(.))


    factors <- input$sel_groups
    


    filtered_new <- filtered
    for (i in 1:length(factors))
    {
        f <- factors[i]
        filtered_new <- inner_join(filtered_new, filtered[filtered[, f] %in% input$sel_factors, ])
    }



    if (box_plot_sel_gene_type == "gene.name") {
        gene.name <- myValues$genenames[filtered_new$gene, ]
        filtered_new <- cbind(filtered_new, gene.name)
    }

    print(filtered_new)



   

    return(filtered_new)
})

output$boxPlot <- renderPlotly({

    if (!is.null(geneExrReactive())) {
        filtered <- geneExrReactive()
        print(input$sel_factors)

        validate(need(length(input$boxplotX) > 0, "Please select a group."))
        validate(need(length(input$boxplotFill) > 0, "Please select a fill by group."))

        
        # levels <- unique(filtered[[input$boxplotFill]])
    
        # # Retrieve or set default colors
        # colors <- sapply(levels, function(level) {
        #     custom_colors$colors[[level]]
        # })
        # names(colors) <- levels

        ## Adapted from STARTapp dotplot

         #Order bar plots based on order user select
        filtered[[input$boxplotX]] <- factor(filtered[[input$boxplotX]], input$sel_factors)


        if (isolate(input$box_plot_sel_gene_type) == "gene.name") {
            p <- ggplot(filtered, aes_string(input$boxplotX, "expression", fill = input$boxplotFill)) +
                geom_boxplot() +
                facet_wrap(~gene.name, scales = "free_y") +
                scale_fill_manual(values = custom_colors$colors)
        } else {
            p <- ggplot(filtered, aes_string(input$boxplotX, "expression", fill = input$boxplotFill)) +
                geom_boxplot() +
                facet_wrap(~gene, scales = "free_y") +
                scale_fill_manual(values = custom_colors$colors)

        }


        p <- p + xlab(" ") + theme(
            plot.margin = unit(c(1, 1, 1, 1), "cm"),
            axis.text.x = element_text(angle = 45),
            legend.position = "bottom"
        )

        p
    }
})

output$boxplotData <- renderDataTable(
    {
        if (!is.null(geneExrReactive())) {
            geneExrReactive()
        }
    },
    options = list(scrollX = TRUE, pageLength = 5)
)


output$boxplotComputed <- reactive({
    return(!is.null(geneExrReactive()))
})
outputOptions(output, "boxplotComputed", suspendWhenHidden = FALSE)


output$downloadBoxCsv <- downloadHandler(
    filename = function() {
        paste0(input$boxplotX, "_", input$boxplotFill, "_boxplotdata.csv")
    },
    content = function(file) {
        csv <- geneExrReactive()

        write.csv(csv, file, row.names = F)
    }
)
