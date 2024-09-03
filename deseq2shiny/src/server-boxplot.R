# Reactive values to store the custom colors
custom_colors <- reactiveValues(colors = list())

# Update the level selection dropdown based on the selected fill variable
observe({
    req(input$boxplotFill)
    filtered <- geneExrReactive()
    # levels <- unique(filtered[[input$boxplotFill]])
    levels <- factor(filtered[[input$sel_groups]], input$sel_factors)
    isolate({
    # Generate random colors for each level if not already set
    # if (length(custom_colors$colors) == 0) {
        random_colors <- generate_random_colors(length(levels))
        custom_colors$colors <- setNames(as.list(random_colors), levels)
        print(custom_colors$colors)
    # }
    })

updateSelectInput(session, "levelSelect", choices = levels)
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
    updateSelectInput(session, "boxplotX",
        choices = colnames(myValues$DF),
        selected = colnames(myValues$DF)[1]
    )

    updateSelectInput(session, "boxplotFill",
        choices = colnames(myValues$DF),
        selected = colnames(myValues$DF)[1]
    )
    # tmpgroups = unique(myValues$DF$Conditions)

    tmpgroups <- input$sel_groups
    tmpgroups <- unlist(lapply(tmpgroups, function(x) {
        levels(myValues$DF[, x])
    }))

    updateSelectizeInput(session, "sel_factors",
        choices = tmpgroups, selected = tmpgroups, server = T
    )
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



    #Order bar plots based on order user select
    filtered_new[[input$sel_groups]] <- factor(filtered_new[[input$sel_groups]], input$sel_factors)

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
