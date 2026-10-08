# Browse dialog for the private data root (private instance only).
if (!is.null(data_root)) {
    browse_roots <- nasqar_browse_roots()
    shinyFiles::shinyFileChoose(input, "browse_hpc_counts", roots = browse_roots, session = session,
                                filetypes = c("csv", "txt", "tsv", "gz"))
    observeEvent(input$browse_hpc_counts, {
        selected <- tryCatch(shinyFiles::parseFilePaths(browse_roots, input$browse_hpc_counts), error = function(e) NULL)
        if (!is.null(selected) && nrow(selected)) {
            updateTextInput(session, "hpc_counts_path", value = as.character(selected$datapath[1]))
        }
    }, ignoreInit = TRUE)
} else {
    removeUI(selector = "#browse_hpc_counts")
    shinyjs::runjs("$('input[name=data_file_type][value=hpcFile]').closest('.radio').remove()")
}

observe({
    reset_downstream_tabs("input")

    shinyjs::disable("computeVST")

    inputFileReactive()
})


decryptUrlParam <- function(cipher) {
    keyHex <- readr::read_file("private.txt")

    key <- hex2bin(keyHex)
    cipher <- hex2bin(cipher)

    orig <- simple_decrypt(cipher, key)

    unserialize(orig)
}



inputFileReactive <- reactive({
    # print('inputFileReactive')
    query <- parseQueryString(session$clientData$url_search)
    # print(query)
    # Check if example selected, or if not then ask to upload a file.
    shiny::validate(
        need(identical(input$data_file_type, "examplecounts") | identical(input$data_file_type, "examplecountsfactors") | (!is.null(input$datafile)) | (!is.null(query[["countsdata"]])) |
             (identical(input$data_file_type, "hpcFile") && !is.null(input$hpc_counts_path) && nzchar(trimws(input$hpc_counts_path))),
            message = "Please select a file"
        )
    )

    # print('valida')

    if (!is.null(query[["countsdata"]])) {
        handoff_expected <- c(
            "A counts table handed off by another NASQAR module through a link that is still valid",
            "If the link has expired, upload the counts file directly instead"
        )
        inFile <- tryCatch(validate_exchange_path(decryptUrlParam(query[["countsdata"]])),
                           error = function(e) nasqar_input_problem(
                               paste("The handed-off counts file is no longer available:", conditionMessage(e)),
                               handoff_expected, NULL, "handoff"))
        # print('ask inputdata')
        shinyjs::show(selector = "a[data-value=\"inputdata\"]")
        shinyjs::disable("datafile")
        # js$collapse("uploadbox")
    } else if (input$data_file_type == "countsFile") {
        inFile <- input$datafile
        if (is.null(inFile)) {
            return(NULL)
        }

        inFile <- inFile$datapath
    } else if (input$data_file_type == "hpcFile") {
        inFile <- tryCatch(resolve_hpc_file(input$hpc_counts_path),
                           error = function(e) shiny::validate(need(FALSE, conditionMessage(e))))
    } else if (input$data_file_type == "examplecounts") {
        inFile <- "www/exampleData/wang_count_table.csv"
        updateCheckboxInput(session, "no_replicates", value = TRUE)
    } else if (input$data_file_type == "examplecountsfactors") {
        inFile <- "www/exampleData/chenCounts.csv"
        if (input$no_replicates) {
            updateCheckboxInput(session, "no_replicates", value = FALSE)
        }
    }

    # print('inFile')
    # print(inFile)


    if (length(inFile) == 0) {
        return(NULL)
    }

    # A wrong file explains itself (expected layout + first lines received)
    # instead of crashing the session.
    # The gene-alias radio is only offered for uploaded (or handed-off) files;
    # the example data sets have no symbol column.
    geneAliasMode <- if (!is.null(query[["countsdata"]]) || isTRUE(input$data_file_type %in% c("countsFile", "hpcFile"))) {
        input$gene_alias
    } else {
        "notincluded"
    }
    counts_expected_for <- function(mode) c(
        "A plain-text table (CSV or tab-delimited) with a header row",
        "Column 1: gene identifiers, unique, one gene per row",
        if (identical(mode, "included")) "Column 2: gene symbols (choose 'My matrix contains GeneIDs only' if there is no such column)",
        "Every following column: one sample of raw (integer) read counts, with the sample name in the header",
        "No normalised values (TPM/FPKM), no blank cells"
    )
    counts_expected <- counts_expected_for(geneAliasMode)
    fileContent <- nasqar_read_table(inFile[1], counts_expected, label = "counts file",
                                     check = function(df) if (ncol(df) < 2) "The table has fewer than 2 columns (is the separator a comma or a tab?)")

    # A new file starts without the gene-name bookkeeping of the previous one.
    myValues$geneids <- NULL
    myValues$genenames <- NULL
    output$conditional_output <- renderUI(NULL)

    if (!is.null(query[["countsdata"]]) &&
        has_gene_alias_column(fileContent)) {
        geneAliasMode <- "included"
        counts_expected <- counts_expected_for(geneAliasMode)
        updateRadioButtons(session, "gene_alias", selected = geneAliasMode)
    }
    counts_problem <- nasqar_check_count_table(fileContent, id_columns = if (identical(geneAliasMode, "included")) 2L else 1L)
    if (is.null(counts_problem) && identical(geneAliasMode, "included") && is.numeric(fileContent[[2]])) {
        counts_problem <- sprintf("Column 2 ('%s') holds numbers, not gene symbols. If the matrix has no symbol column choose 'My matrix contains GeneIDs only'.",
                                  colnames(fileContent)[2])
    }
    if (!is.null(counts_problem)) {
        nasqar_input_problem(counts_problem, counts_expected, inFile[1], "counts file")
    }

    # Anything the checks above did not foresee is reported the same way
    # rather than ending the session from an observer.
    tryCatch({
    if(geneAliasMode == 'notincluded'){
        sampleN <- colnames(fileContent)[-1]
    } else {
        sampleN <- colnames(fileContent)[c(-1,-2)]

        chosen_column <- "gene.names"
        
        df <-  fileContent
       
        duplicated_names <- df[[2]][duplicated(df[[2]])]
        duplicate_rows <- df[df[[2]] %in% duplicated_names, ]
        output$duplicate_contents <- renderDataTable({duplicate_rows}, options = list(scrollX = TRUE))

        genenames <-fileContent[, c(2)]
        genenames<- make.unique(genenames, sep = '--')
        fileContent[, c(2)] <- genenames

        genenames <- as.data.frame(fileContent[, c(2)])
        geneids <- as.data.frame(fileContent[, c(1)])
        
        row.names(geneids) <- fileContent[, 2]
        row.names(genenames) <- fileContent[, 1]
        genenames <- as.data.frame(genenames)
        geneids <- as.data.frame(geneids)
        #print(genenames)
        # output$d <-TRUE
        

        myValues$geneids <- geneids
        myValues$genenames <- genenames
        if(length(duplicated_names)>0) {
        output$conditional_output <- renderUI({
        div(
             p(style = "color:red;", "Gene name duplicates have been identified. We have renamed all duplicate gene names by appending \"--{replicate number}\" to the gene name. Below is a table listing the duplicate genes. You have the option to either remove the duplicate entries or proceed with the renamed gene names"),
            h2("Duplicate Gene Counts Table"), hr(),
            withSpinner(dataTableOutput("duplicate_contents"))

        )   
        })
        }
       

    }

    
    dataCounts <- fileContent[, sampleN, drop = FALSE]
    row.names(dataCounts) <- fileContent[, 1]
    dataCounts <- validate_count_matrix(dataCounts)
    }, error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        nasqar_input_problem(conditionMessage(e), counts_expected, inFile[1], "counts file")
    })
    myValues$fileContent <- fileContent
    myValues$dataCounts <- dataCounts

    if (is.null(query[["countsdata"]])) {
        js$collapse("uploadbox")
    }
 
    # print('dataCounts.....')
    # print(dataCounts)

   

    return(dataCounts)
})

output$contents <- renderDataTable(
    {
        # A restored project already holds the counts; otherwise the input
        # reactive supplies them (and asks for a file when there is none).
        if (is.null(myValues$fileContent) || is.null(myValues$dataCounts)) {
            tmp <- inputFileReactive()
            req(!is.null(tmp))
        }
        df <- myValues$fileContent
        req(!is.null(df), !is.null(myValues$dataCounts))
        row.names(df) <- df[, 1]
        df[row.names(myValues$dataCounts), , drop = FALSE][, -1]
    },
    options = list(scrollX = TRUE)
)


observeEvent(input$prefilterCounts, ignoreInit = TRUE, {
    validate(need(is.finite(input$minRowCount), "Enter a minimum row count"))
    dataCounts <- inputFileReactive()
    dataCounts <- dataCounts[rowSums(dataCounts) >= input$minRowCount, , drop = FALSE]
    myValues$dataCounts <- dataCounts
    
    # Store filtering parameters for export reproducibility
    myValues$prefilter_applied <- TRUE
    myValues$prefilter_threshold <- input$minRowCount
})


observe({
    csvDataReactive()
})

csvDataReactive <- eventReactive(input$submit, {
    fileContent <- inputFileReactive()

    # inputFileReactive() reparses the uploaded file when the user advances.
    # Reapply the requested prefilter so that "Next" does not silently restore
    # rows removed with the Filter button.
    if (isTRUE(input$prefilterCounts > 0)) {
        validate(need(is.finite(input$minRowCount), "Enter a minimum row count"))
        fileContent <- fileContent[
            rowSums(fileContent) >= input$minRowCount,
            ,
            drop = FALSE
        ]
        myValues$dataCounts <- fileContent
        myValues$prefilter_applied <- TRUE
        myValues$prefilter_threshold <- input$minRowCount
    }

    shinyjs::show(selector = "a[data-value=\"conditionsTab\"]")
    updateTabItems(session, "tabs", "conditionsTab")
    shinyjs::runjs("window.scrollTo(0, 0)")

    sampleN <- colnames(fileContent)

    if (input$data_file_type %in% c("countsFile", "hpcFile")) {
        if (input$no_replicates) {
            sampleConditions <- sampleN

            samples <- data.frame(row.names = sampleN, Conditions = sampleConditions)
        } else {
            sampleConditions <- strsplit(sampleN, "_")

            sampleConditions <- unlist(lapply(sampleConditions, function(x) {
                x[1]
            }))

            samples <- data.frame(row.names = sampleN, Conditions = sampleConditions)
        }
    } else if (identical(input$data_file_type, "examplecounts")) {
        updateCheckboxInput(session, "no_replicates", value = T)


        sampleConditions <- sampleN
        # samples <- data.frame(row.names = sampleN, condition = sampleConditions)
        samples <- data.frame(row.names = sampleN, Conditions = sampleConditions)
    } else {
        samples <- read.csv("www/exampleData/chenMeta.csv", header = TRUE, sep = ",", row.names = 1)
    }


    myValues$DF <- samples
    myValues$excludedSamplesInfo <- character(0)
    myValues$DF[] <- lapply(myValues$DF, function(x) {
        if (is.character(x) || is.factor(x)) {
            as.factor(x)
        } else {
            x
        }
    })

    updateDesignFormula()

    return(list("countsData" = fileContent))
})

output$fileUploaded <- reactive({
    return(!is.null(inputFileReactive()))
})
outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

output$noreplicates <- reactive({
    return(input$no_replicates)
})
outputOptions(output, "noreplicates", suspendWhenHidden = FALSE)


observe({
    if (isTRUE(input$data_file_type %in% c("examplecountsfactors", "countsFile", "hpcFile"))) {
        updateCheckboxInput(session, "no_replicates", value = F)
        inputFileReactive()
    }
})
