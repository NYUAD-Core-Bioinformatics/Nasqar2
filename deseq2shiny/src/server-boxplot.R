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
        if (!is.null(myValues$DF) && !is.null(input$sel_groups)) {
            tmpgroups <- input$sel_groups
            mylevels <- unlist(lapply(tmpgroups, function(x) {
                if (x %in% colnames(myValues$DF)) {
                    levels(myValues$DF[, x])
                } else {
                    NULL
                }
            }))
            # Remove NULL values
            mylevels <- mylevels[!is.null(mylevels)]
            
            # mylevels <- levels(myValues$DF[, input$boxplotFill])
        # Generate random colors for each level if not already set
        # if (length(custom_colors$colors) == 0) {
            if (length(mylevels) > 0) {
                random_colors <- generate_random_colors(length(mylevels))
                custom_colors$globalcolors <- setNames(as.list(random_colors), mylevels)
                
                if (!is.null(input$boxplotFill) && input$boxplotFill %in% colnames(myValues$DF)) {
                    mylevels <- levels(myValues$DF[, input$boxplotFill])
                    custom_colors$colors <- custom_colors$globalcolors[mylevels]
                }
                
                print(custom_colors$colors)
            }
        }
    # }
    })
    
})

observe({
    req(input$boxplotFill)
    if (!is.null(myValues$DF) && input$boxplotFill %in% colnames(myValues$DF)) {
        mylevels <- levels(myValues$DF[, input$boxplotFill])
        custom_colors$colors <- custom_colors$globalcolors[mylevels]
        updateSelectInput(session, "levelSelect", choices = mylevels)
    }
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
    # print('sel_gene')
    # print(myValues)
    if (input$box_plot_sel_gene_type == "gene.name") {
        genenames <- myValues$genenames[rownames(myValues$dataCounts), ]
        updateSelectizeInput(session, "sel_gene",
            choices = genenames,
            server = TRUE
        )
    } else {
        updateSelectizeInput(session, "sel_gene",
            choices = rownames(myValues$dataCounts),
            server = TRUE
        )
    }

    updateSelectizeInput(session, "sel_groups",
        choices = colnames(myValues$DF),
        server = TRUE
    )
})
observe({
    if (!is.null(myValues$DF) && ncol(myValues$DF) > 0) {
        updateSelectInput(session, "boxplotX",
            choices = colnames(myValues$DF),
            selected = colnames(myValues$DF)[1]
        )

        updateSelectInput(session, "boxplotFill",
            choices = colnames(myValues$DF),
            selected = colnames(myValues$DF)[1]
        )
        # tmpgroups = unique(myValues$DF$Conditions)

        if (!is.null(input$sel_groups)) {
            tmpgroups <- input$sel_groups
            tmpgroups <- unlist(lapply(tmpgroups, function(x) {
                if (x %in% colnames(myValues$DF)) {
                    levels(myValues$DF[, x])
                } else {
                    NULL
                }
            }))
            # Remove NULL values
            tmpgroups <- tmpgroups[!is.null(tmpgroups)]

            updateSelectizeInput(session, "sel_factors",
                choices = tmpgroups, selected = tmpgroups, server = T
            )
        }
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
