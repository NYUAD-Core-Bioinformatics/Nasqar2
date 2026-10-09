# Dropdown values of the conditions added in the table, keyed by column name.
isolate(if (!is.list(myValues$conditions)) myValues$conditions <- list())

observe({
    colnamesChoices <- as.vector(colnames(myValues$DF[!(names(myValues$DF) %in% c("Samples", "Groups"))]))
    updateSelectInput(session, "colToRemove", choices = colnamesChoices, selected = NULL)
    
    # Update sample exclusion choices when DF changes
    if (!is.null(myValues$DF)) {
        sampleChoices <- as.vector(rownames(myValues$DF))  # Ensure simple vector
        # Ensure selected value is also a simple vector
        selectedSamples <- if(!is.null(input$samplesToExclude)) as.vector(input$samplesToExclude) else NULL
        updateSelectizeInput(session, "samplesToExclude", choices = sampleChoices, selected = selectedSamples)
    }
})

observeEvent(input$removeCol, {
    validate(
        need(input$colToRemove != "", message = "need to select column to remove")
    )

    if (length(colnames(myValues$DF)) == 1) {
        showNotification(id = "errorNotify1", "At least two column required in the table, to remove one column", type = "error", duration = NULL)
    }
    validate(
        need(length(colnames(myValues$DF)) > 1, message = "At least two column required in the table, to remove one column")
    )
    removeNotification("errorNotify1")
    myValues$DF <- hot_to_r(input$table)
    myValues$DF[, input$colToRemove] <- NULL
    myValues$conditions[[input$colToRemove]] <- NULL
    updateDesignFormula()
    
    # Update factorNameInput choices only if DESeq2 analysis is completed
    if (exists("input") && !is.null(input) && "factorNameInput" %in% names(input) && !is.null(myValues$dds)) {
        factorChoices <- colnames(myValues$DF)
        factorChoices <- factorChoices[!(factorChoices %in% c("sizeFactor", "replaceable"))]
        
        # Filter to only include categorical factors that are in the design formula
        design_terms <- labels(terms(design(myValues$dds)))
        validFactorChoices <- getValidCategoricalFactors(factorChoices, design_terms, myValues$DF)
        
        if (length(validFactorChoices) > 0) {
            # Ensure simple vector
            validFactorChoices <- as.vector(validFactorChoices)
            updateSelectizeInput(session, "factorNameInput", choices = validFactorChoices, selected = validFactorChoices[1])
        }
    }
})

observe({
    tableEditReactive()
})

tableEditReactive <- reactive({
    if (isTRUE(input$addConditions > 0)) {
        isolate({
            myValues$DF <- hot_to_r(input$table)
            DF <- myValues$DF

            validate(
                need(!(input$conditionName %in% colnames(DF)), message = "Condition name already exists"),
                need(trimws(input$conditionName) != "", message = "Condition name empty"),
                need(trimws(input$conditions) != "", message = "Conditions empty")
            )

            newDF <- data.frame(newCol = character(dim(DF)[1]))
            names(newDF) <- c(input$conditionName)

            DF <- cbind(DF, newDF)

            myValues$DF <- DF

            myValues$conditions[[input$conditionName]] <- input$conditions

            updateTextInput(session, "conditionName", value = "")
            updateTextInput(session, "conditions", value = "")
            updateDesignFormula()
        })
    }
})

# output$tableCreated <-
#   reactive({
#     return(!is.null(myValues$DF))
#   })
# outputOptions(output, 'tableCreated', suspendWhenHidden=FALSE)

output$table <- renderRHandsontable({
    DF1 <- myValues$DF
    if (is.null(DF1)) {
        return()
    }


    table <- rhandsontable(DF1, rowHeaderWidth = 100) %>%
        hot_cols(colWidths = 150)

    table <- table %>% hot_table(highlightCol = TRUE, highlightRow = TRUE, colHeaders = NULL)

    for (i in 2:dim(DF1)[2])
    {
        if (dim(DF1)[2] < 2) {
            break()
        }
        conditionsStr <- myValues$conditions[[colnames(DF1)[i]]]
        if (!is.null(conditionsStr)) {
            table <- table %>% hot_col(col = colnames(DF1)[i], type = "dropdown", source = getConditionsListFromStr(conditionsStr))
        }
    }

    table
})

output$downloadCSV <- nasqar_download_handler(
    filename = paste0("metadatatable_", format(Sys.time(), "%y-%m-%d_%H-%M-%S"), ".csv"),
    content = function(file) {
        write.csv(hot_to_r(input$table), file, row.names = T)
    }
)

observe({
    metadataFileReactive()
})
metadataFileReactive <- reactive({
    # Check if example selected, or if not then ask to upload a file.
    shiny::validate(
        need((!is.null(input$metadatafile)),
            message = "Please select a file"
        )
    )

    inFile <- input$metadatafile
    if (is.null(inFile)) {
        return(NULL)
    }

    inFile <- inFile$datapath

    # print('metadataFileReactive')
    # print(inFile)
    if (length(inFile) == 0) {
        return(NULL)
    }

    metadata_expected <- c(
        "A plain-text table (CSV or tab-delimited) with a header row",
        "Column 1: sample names, exactly matching the column names of the counts table",
        "Every following column: one experimental factor (e.g. condition, batch) with its value per sample"
    )
    fileContent <- nasqar_read_table(inFile[1], metadata_expected, label = "metadata file",
        check = function(df) {
            if (ncol(df) < 2) return("The table has fewer than 2 columns (is the separator a comma or a tab?)")
            if (!nrow(df)) return("The table has a header but no rows.")
            if (anyDuplicated(df[[1]])) return("Column 1 must hold unique sample names.")
            counts <- myValues$dataCounts
            if (!is.null(counts)) {
                missing <- setdiff(colnames(counts), as.character(df[[1]]))
                if (length(missing)) {
                    return(sprintf("Sample(s) %s%s of the counts table are not in column 1.",
                                   paste(shQuote(head(missing, 5)), collapse = ", "), if (length(missing) > 5) ", ..." else ""))
                }
            }
            NULL
        })

    sampleN <- colnames(fileContent)[-1]

    # metaData <- fileContent[,sampleN]
    # if meta dat has only one entry returns as vector.
    metaData <- data.frame(fileContent[, colnames(fileContent)[-1]])
    colnames(metaData) <- colnames(fileContent)[-1]

    metaData[] <- lapply(metaData, function(x) {
        if (is.character(x) || is.factor(x)) {
            as.factor(x)
        } else {
            x
        }
    })
    # metaData <- data.frame(sapply( metaData, as.factor ))
    # print(levels(metaData$Stage))
    row.names(metaData) <- fileContent[, 1]

    myValues$DF <- metaData
    myValues$conditions <- list()

    updateDesignFormula()

    return(metaData)
})


observe({
    ddsInitReactive()
})

# Handle clearing excluded samples
observeEvent(input$clearExcludedSamples, {
    updateSelectizeInput(session, "samplesToExclude", selected = character(0))
})

# Initialize excluded samples reactive value
if (!exists("myValues$excludedSamples")) {
    myValues$excludedSamples <- character(0)
}

# Update excluded samples when selection changes
observeEvent(input$samplesToExclude, {
    myValues$excludedSamples <- input$samplesToExclude
}, ignoreNULL = FALSE)


ddsInitReactive <- eventReactive(input$init_deseq2, {
    withProgress(message = "Initializing DESeq2 ...", {
        # print("Initializing DESeq2 ...")
        removeNotification("errorNotify")
        removeNotification("errorNotify1")
        reset_downstream_tabs("conditions")

        myValues$status <- NULL
        myValues$dds <- NULL

        js$addStatusIcon("conditionsTab", "loading")
        shiny::setProgress(value = 0.2, detail = "...")


        myValues$DF <- hot_to_r(input$table)

        samples <- myValues$DF
        dataCounts <- myValues$dataCounts

        samples <- tryCatch(
            align_metadata_to_counts(samples, colnames(dataCounts)),
            error = function(e) {
                myValues$status <- paste("Metadata Error:", conditionMessage(e))
                showNotification(
                    id = "errorNotify",
                    myValues$status,
                    type = "error",
                    duration = NULL
                )
                js$addStatusIcon("conditionsTab", "fail")
                validate(need(FALSE, myValues$status))
            }
        )

        # Exclude selected samples after sample names are properly set
        excludedSamples <- input$samplesToExclude
        if (!is.null(excludedSamples) && length(excludedSamples) > 0) {
            # Filter out excluded samples from both samples and dataCounts
            samplesToKeep <- !rownames(samples) %in% excludedSamples
            samples <- samples[samplesToKeep, , drop = FALSE]
            
            # Filter dataCounts by column names (sample names)
            columnsToKeep <- !colnames(dataCounts) %in% excludedSamples
            dataCounts <- dataCounts[, columnsToKeep, drop = FALSE]
            
            # Store excluded samples info for display (cumulative until the
            # counts are submitted again)
            myValues$excludedSamplesInfo <- union(myValues$excludedSamplesInfo, excludedSamples)
        } else if (is.null(myValues$excludedSamplesInfo)) {
            myValues$excludedSamplesInfo <- character(0)
        }

        # Levels of excluded samples must not survive into the contrast
        # dropdowns, and the counts must match the metadata kept.
        samples <- droplevels(samples)
        myValues$DF <- samples
        myValues$dataCounts <- dataCounts

        # convert factors to unordered
        # factor(samples, ordered = F)

        for (i in 1:ncol(samples)) {
            if (all(class(samples[, i]) %in% c("ordered", "factor"))) {
                samples[, i] <- factor(samples[, i], ordered = F)
            }
        }

        isolate({
            validate(need(
                tryCatch(
                    {
                        design_formula <- parse_design_formula(
                            input$designFormula,
                            colnames(samples)
                        )
                        dds <- DESeqDataSetFromMatrix(
                            dataCounts,
                            colData = samples,
                            design = design_formula
                        )
                    },
                    error = function(e) {
                        myValues$status <- paste("DESeq2 Error: ", e$message)

                        showNotification(id = "errorNotify", myValues$status, type = "error", duration = NULL)
                        showNotification(id = "errorNotify1", "Fix design formula OR Factors/Conditions", type = "error", duration = NULL)


                        js$addStatusIcon("conditionsTab", "fail")
                    }
                ),
                "Error"
            ))
        })

        updateTextInput(session, "designFormulaSva", value = paste(as.character(design(dds)), collapse = " "))

        js$addStatusIcon("conditionsTab", "done")
        shinyjs::show(selector = "a[data-value=\"deseqTab\"]")
        updateTabItems(session, "tabs", "deseqTab")

        shiny::setProgress(value = 0.8, detail = "...")
        myValues$dds <- dds
        return(dds)
    })
})

output$ddsInitAvailable <- reactive({
    return(!is.null(ddsInitReactive()))
})
outputOptions(output, "ddsInitAvailable", suspendWhenHidden = FALSE)

# Display excluded samples
output$excludedSamplesList <- renderText({
    if (!is.null(myValues$excludedSamplesInfo) && length(myValues$excludedSamplesInfo) > 0) {
        paste(myValues$excludedSamplesInfo, collapse = ", ")
    } else {
        "None"
    }
})

# For the DESeq tab display
output$excludedSamplesList2 <- renderText({
    if (!is.null(myValues$excludedSamplesInfo) && length(myValues$excludedSamplesInfo) > 0) {
        paste(myValues$excludedSamplesInfo, collapse = ", ")
    } else {
        ""
    }
})

output$hasExcludedSamples <- reactive({
    return(!is.null(myValues$excludedSamplesInfo) && length(myValues$excludedSamplesInfo) > 0)
})
outputOptions(output, "hasExcludedSamples", suspendWhenHidden = FALSE)





updateDesignFormula <- function() {
    isolate({
        # Check if myValues$DF exists and has columns
        if (is.null(myValues$DF) || ncol(myValues$DF) == 0) {
            return()
        }
        
        groupvars <- colnames(myValues$DF)

        if (length(groupvars) == 1) {
            # Validate that the column still exists
            if (groupvars[1] %in% colnames(myValues$DF) && 
                length(levels(myValues$DF[, groupvars])) == nrow(myValues$DF)) {
                designFormula <- "~ 1"
            } else {
                designFormula <- paste("~ ", groupvars)
            }
        } else {
            designFormula <- paste(" ~ ", paste(groupvars, collapse = " + "))
        }

        updateTextInput(session, "designFormula", value = designFormula)
        updateTextInput(session, "designFormulaSva", value = designFormula)
    })
}


getConditionsListFromStr <- function(conditonsStr) {
    conditions <- isolate(unlist(strsplit(conditonsStr, ",")))
    conditions <- trimws(conditions)
    conditions <- conditions[conditions != ""]
    conditions <- unique(conditions)
    return(conditions)
}
