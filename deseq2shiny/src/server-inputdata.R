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
    # Hide the loading message when the rest of the server function has executed
    # hide(id = "loading-content", anim = TRUE, animType = "fade")
    #
    # removeClass("app-content", "hidden")


    # shinyjs::hide(selector = "a[data-value=\"conditionsTab1\"]")
    # print("server-inputdata")

    shinyjs::hide(selector = "a[data-value=\"deseqTab\"]")
    shinyjs::hide(selector = "a[data-value=\"svaseqTab\"]")
    shinyjs::hide(selector = "a[data-value=\"conditionsTab\"]")

    shinyjs::hide(selector = "a[data-value=\"rlogTab\"]")
    shinyjs::hide(selector = "a[data-value=\"vstTab\"]")

    shinyjs::hide(selector = "a[data-value=\"resultsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"venndiagramTab\"]")
    shinyjs::hide(selector = "a[data-value=\"volcanoplotTab\"]")
    shinyjs::hide(selector = "a[data-value=\"boxplotTab\"]")
    shinyjs::hide(selector = "a[data-value=\"heatmapTab\"]")

    shinyjs::disable("computeVST")
    # Check if example selected, or if not then ask to upload a file.

    # shiny:: validate(
    #   need((input$data_file_type=="examplecounts")|((!is.null(input$rdatafile))|(!is.null(input$datafile))),
    #        message = "Please select a file")
    # )
    # inFile <- input$datafile

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
        inFile <- validate_exchange_path(decryptUrlParam(query[["countsdata"]]))
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
    geneAliasMode <- input$gene_alias
    counts_expected <- c(
        "A plain-text table (CSV or tab-delimited) with a header row",
        "Column 1: gene identifiers, unique, one gene per row",
        if (identical(geneAliasMode, "included")) "Column 2: gene symbols (choose 'My matrix contains GeneIDs only' if there is no such column)",
        "Every following column: one sample of raw (integer) read counts, with the sample name in the header",
        "No normalised values (TPM/FPKM), no blank cells"
    )
    fileContent <- nasqar_read_table(inFile[1], counts_expected, label = "counts file",
                                     check = function(df) if (ncol(df) < 2) "The table has fewer than 2 columns (is the separator a comma or a tab?)")

    if (!is.null(query[["countsdata"]]) &&
        has_gene_alias_column(fileContent)) {
        geneAliasMode <- "included"
        updateRadioButtons(session, "gene_alias", selected = geneAliasMode)
    }
    counts_problem <- nasqar_check_count_table(fileContent, id_columns = if (identical(geneAliasMode, "included")) 2L else 1L)
    if (!is.null(counts_problem)) {
        nasqar_input_problem(counts_problem, counts_expected, inFile[1], "counts file")
    }

    if(geneAliasMode == 'notincluded'){
        sampleN <- colnames(fileContent)[-1]
    } else {
        sampleN <- colnames(fileContent)[c(-1,-2)]

        chosen_column <- "gene.names"
        print(chosen_column)
        
        df <-  fileContent
       
        duplicated_names <- df[[2]][duplicated(df[[2]])]
        print(duplicated_names)
        duplicate_rows <- df[df[[2]] %in% duplicated_names, ]
        print(duplicate_rows)
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
        print('length(duplicated_names)')
        print(duplicated_names)
        print(length(duplicated_names))
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
    dataCounts <- tryCatch(validate_count_matrix(dataCounts),
                           error = function(e) nasqar_input_problem(conditionMessage(e), counts_expected, inFile[1], "counts file"))
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
    dataCounts <- inputFileReactive()
    dataCounts <- dataCounts[rowSums(dataCounts) >= input$minRowCount, ]
    myValues$dataCounts <- dataCounts
    
    # Store filtering parameters for export reproducibility
    myValues$prefilter_applied <- TRUE
    myValues$prefilter_threshold <- input$minRowCount
    
    print("prefilterCounts")
    print(paste("Applied prefilter with threshold:", input$minRowCount))
})

output$deseqMenu <- renderMenu({
    if (!is.null(csvDataReactive())) {
        menuItem("2. Run DESeq2",
            tabName = "preprocTab", icon = icon("th"), startExpanded = T,
            menuSubItem("Design Conditions", tabName = "conditionsTab"),
            menuSubItem("Hidden Batch Effect", tabName = "svaseqTab"),
            menuSubItem("Run DESeq2", tabName = "deseqTab")
        )
    }
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
        test <- inputFileReactive()
    }
})
