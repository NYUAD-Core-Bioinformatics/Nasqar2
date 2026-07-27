observe({
    svaReactive()
})

svaReactive <- eventReactive(input$runSVA, {
    # print(input$designFormulaSva)
    sva_design <- parse_design_formula(
        input$designFormulaSva,
        colnames(colData(myValues$dds))
    )
    validate(need(
        length(attr(terms(sva_design), "term.labels")) > 0,
        "Need biological factors to estimate SVs!"
    ))
    isolate({
        dds <- ddsInitReactive()
    })


    withProgress(message = "Running SVA , please wait", {
        js$addStatusIcon("svaseqTab", "loading")

        removeNotification("errorNotify")
        removeNotification("errorNotify1")

        validate(need(
            tryCatch(
                {
                    dds <- estimateSizeFactors(dds)
                    norm.cts <- counts(dds, normalized = TRUE)

                    ### SVA
                    isolate({
                        mm <- model.matrix(
                            parse_design_formula(
                                input$designFormulaSva,
                                colnames(colData(dds))
                            ),
                            colData(dds)
                        )
                        mm0 <- model.matrix(~1, colData(dds))
                        norm.cts <- norm.cts[rowSums(norm.cts) > 0, ]
                        svafit <- svaseq(norm.cts, mod = mm, mod0 = mm0, n.sv = input$numSVA)

                        svNames <- paste0("SV", 1:ncol(svafit$sv))

                        for (i in 1:length(svNames)) {
                            colData(dds)[, svNames[i]] <- svafit$sv[, i]
                        }

                        varNames <- colnames(colData(dds))
                        varNames <- varNames[varNames != "sizeFactor"]

                        svaFormula <- paste("~", paste(rev(varNames), collapse = "+"))

                        myValues$ddsSva <- dds

                        # Ensure simple vectors for all update functions
                        col_data_names <- as.vector(colnames(colData(dds)))
                        
                        updateTextInput(session, "newFormulaSva", value = svaFormula)
                        updateSelectInput(session, "xaxisSva", choices = col_data_names, selected = "SV1")
                        updateSelectInput(session, "yaxisSva", choices = col_data_names, selected = "SV2")
                        updateSelectInput(session, "colorBy", choices = col_data_names, selected = col_data_names[1])

                        updateSelectizeInput(session, "varsToRegress", choices = col_data_names, selected = c("SV1", "SV2"))

                        updateSelectizeInput(session, "factorNameInputSva", choices = col_data_names, selected = col_data_names[1])

                        js$addStatusIcon("svaseqTab", "done")
                        return(list("svafit" = svafit, "ddsSva" = dds))
                    })
                },
                error = function(e) {
                    myValues$status <- paste("SVA Error: ", e$message)

                    showNotification(id = "errorNotify", myValues$status, type = "error", duration = NULL)
                    # showNotification(id="errorNotify1", "If this is intended, please select 'No Replicates' in Input Data step. OR use ~ 1 as the design formula", type = "error", duration = NULL)

                    js$addStatusIcon("svaseqTab", "fail")

                    return(NULL)
                }
            ),
            "Error"
        ))
    })
})

output$svaText <- renderText({
    # print(input$designFormulaSva)
    sva_design <- parse_design_formula(
        input$designFormulaSva,
        colnames(colData(myValues$dds))
    )
    validate(need(
        length(attr(terms(sva_design), "term.labels")) > 0,
        "Cannot use ~ 1 to estimate SVs. Biological factors are required!"
    ))

    return(paste("Using biological factors:", input$designFormulaSva, "to estimate Surrogate Variables (SVs)"))
})

output$svaPlot <- renderPlotly({
    dds <- svaReactive()$ddsSva

    if (!is.null(dds)) {
        df <- as.data.frame(colData(dds))

        xaxis <- input$xaxisSva
        yaxis <- input$yaxisSva
        colorBy <- input$colorBy

        if (xaxis == "" && yaxis == "" && colorBy == "") {
            xaxis <- colnames(df)[1]
            yaxis <- colnames(df)[1]
            colorBy <- colnames(df)[1]
        }

        ggplot(df, aes_string(xaxis, yaxis, col = colorBy)) +
            geom_point(size = 4, alpha = 0.9) +
            geom_text(aes(label = rownames(df)), hjust = 0, vjust = 0,
                      size = 4) +
            theme_minimal(base_size = 15)
    }
})


observeEvent(input$regressVarsBatch, ignoreInit = TRUE, {
    withProgress(message = "Removing batch effect, this may take a long time.", {
        dds <- myValues$ddsSva

        svaFormula <- parse_design_formula(
            input$newFormulaSva,
            colnames(colData(dds))
        )

        design(dds) <- svaFormula

        shiny::setProgress(value = 0.3, detail = "Running DESeq ...")
        dds <- DESeq(
            dds,
            parallel = TRUE,
            BPPARAM = MulticoreParam(3)
        )

        shiny::setProgress(value = 0.6, detail = "Computing VST matrix ...")
        vsd <- varianceStabilizingTransformation(dds)

        shiny::setProgress(value = 0.8, detail = "limma::removeBatchEffect ...")
        assay(vsd) <- limma::removeBatchEffect(assay(vsd), covariates = colData(dds)[, input$varsToRegress])


        myValues$vsdSva <- vsd
        myValues$ddsAddSV <- dds
    })
})


output$pcaSvaPlot <- renderPlotly({
    validate(need(length(input$factorNameInputSva) > 0, "Need at least one condition!"))

    vsd <- myValues$vsdSva

    if (!is.null(vsd)) {
        p <- DESeq2::plotPCA(vsd, intgroup = input$factorNameInputSva)
        if (length(p$layers) > 0) {
            p$layers[[1]]$aes_params$size <- 4
            p$layers[[1]]$aes_params$alpha <- 0.9
        }
        ggplotly(p + theme_minimal(base_size = 15)) %>%
            layout(
                font = list(size = 15),
                margin = list(l = 90, r = 40, b = 80, t = 60)
            ) %>%
            config(responsive = TRUE, displaylogo = FALSE)
    }
})

output$pcaSvaAvailable <- reactive({
    return(!is.null(myValues$vsdSva))
})
outputOptions(output, "pcaSvaAvailable", suspendWhenHidden = FALSE)

output$ddsSvaAvailable <- reactive({
    return(!is.null(svaReactive()$ddsSva))
})
outputOptions(output, "ddsSvaAvailable", suspendWhenHidden = FALSE)

output$varsToIncludeInDeseq <- renderText({
    return("")
})

observe({
    if (isTRUE(input$runDeseqWithSVs > 0)) {
        #
        # myValues$DF = colData(myValues$ddsAddSV)
        myValues$dds <- myValues$ddsAddSV

        GotoTab("deseqTab")
    }
})

observe({
    if (isTRUE(input$runDeseqWithoutSVs > 0)) {
        myValues$dds <- ddsInitReactive()
        GotoTab("deseqTab")
    }
})
