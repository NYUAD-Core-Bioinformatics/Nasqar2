decryptUrlParam <- function(cipher) {
    keyHex <- readr::read_file("private.txt")

    key <- hex2bin(keyHex)
    cipher <- hex2bin(cipher)

    orig <- simple_decrypt(cipher, key)

    unserialize(orig)
}

organismsDbChoices <- installed_organism_choices()

observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query[["gene_names"]])) {
        shinyjs::hide(selector = "a[data-value=\"introTab\"]")

        shinyjs::show(selector = "a[data-value=\"datainput\"]")
        shinyjs::show(selector = "a[data-value=\"enrichGoTab\"]")
    }
    shinyjs::hide(selector = "a[data-value=\"enrichGoTab\"]")
    shinyjs::hide(selector = "a[data-value=\"enrichKeggTab\"]")
    shinyjs::hide(selector = "a[data-value=\"goplotsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"keggPlotsTab\"]")
    shinyjs::hide(selector = "a[data-value=\"pathviewTab\"]")
    shinyjs::hide(selector = "a[data-value=\"wordcloudTab\"]")

    inputDataReactive()
})

inputDataReactive <- reactive({
    print("inputting data")

    query <- parseQueryString(session$clientData$url_search)

    # Check if example selected, or if not then ask to upload a file.
    shiny::validate(
        need(!is.null(query[["countsdata"]]) | !is.null(query[["gene_names"]]) | identical(input$data_file_type, "examplecounts") | (!is.null(input$datafile)),
            message = "Please select a file"
        )
    )
    updateSelectInput(session, "organismDb", choices = organismsDbChoices)
    if (!is.null(query[["gene_names"]])) {
        print("null")
        inFile <- validate_exchange_path(
            decryptUrlParam(query[["gene_names"]])
        )
        data <- normalize_enrichment_columns(
            read.csv(inFile, check.names = FALSE)
        )
        shinyjs::show(selector = "a[data-value=\"datainput\"]")
        # shinyjs::show(selector = "a[data-value=\"iParamsbox\"]")
        shinyjs::disable("datafile")
        shinyjs::disable("log2fcColumn")
        shinyjs::disable("padjColumn")
        shinyjs::disable("padjCutoff")
        shinyjs::disable("logfcCuttoff")

        js$collapse("uploadbox")


        # data <- read.csv('/tmp/t.csv')
        # shinyjs::hide(id="uploadbox")
        # shinyjs::show(id="iParamsbox")
        # shinyjs::show(id="createGoBox")
        # shinyjs::show(selector = "a[data-value=\"datainput\"]")
        # shinyjs::disable("datainput")
        # shinyjs::disable("data_file_type")
        shinyjs::disable("data_file_type")
        shinyjs::disable("datafile")

        # shinyjs::disable("iParamsbox")
        return(list("data" = data))
    } else if (!is.null(query[["countsdata"]])) {
        inFile <- validate_exchange_path(
            decryptUrlParam(query[["countsdata"]])
        )
        data <- normalize_enrichment_columns(
            read.csv(inFile, check.names = FALSE)
        )
        shinyjs::show(selector = "a[data-value=\"datainput\"]")

        shinyjs::show(selector = "a[data-value=\"datainput\"]")
        shinyjs::disable("data_file_type")
        shinyjs::disable("datafile")
        js$collapse("uploadbox")
        return(list("data" = data))
    } else {
        inFile <- input$datafile
        # if (is.null(inFile))
        #   return(NULL)
        #
        # inFile = inFile$datapath
    }

    # inFile <- input$datafile
    js$addStatusIcon("datainput", "loading")

    if (!is.null(inFile)) {
        if (!file.exists(inFile$datapath)) {
            showNotification(
                "The uploaded file is no longer available (session may have restarted). Please re-upload your file.",
                type = "warning", duration = NULL
            )
            return(NULL)
        }
        seqdata <- normalize_enrichment_columns(
            read.csv(inFile$datapath, header = TRUE, sep = ",")
        )
        print("uploaded seqdata")
        if (ncol(seqdata) == 1) { # if file appears not to work as csv try tsv
            seqdata <- normalize_enrichment_columns(
                read.tsv(inFile$datapath, header = TRUE)
            )
            print("changed to tsv, uploaded seqdata")
        }
        shiny::validate(need(ncol(seqdata) > 1,
            message = "File appears to be one column. Check that it is a comma or tab delimited (.csv) file."
        ))

        js$addStatusIcon("datainput", "done")
        js$collapse("uploadbox")

        return(list("data" = seqdata))
    } else {
        if (input$data_file_type == "examplecounts") {
            js$addStatusIcon("datainput", "loading")
            # pbmc.data <- Read10X(data.dir = "www/hg19/")

            # data = read.csv("www/exampleData/SRX003935_vs_SRX003924.csv")

            data <- normalize_enrichment_columns(
                read.csv("www/exampleData/drosphila_example_de.csv")
            )



            js$addStatusIcon("datainput", "done")
            js$collapse("uploadbox")
            return(list("data" = data))
        }
        return(NULL)
    }
})


output$countdataDT <- renderDataTable(
    {
        tmp <- inputDataReactive()

        if (!is.null(tmp)) {
            tmp$data
        }
    },
    options = list(scrollX = TRUE)
)

# check if a file has been uploaded and create output variable to report this
output$fileUploaded <- reactive({
    if (!is.null(inputDataReactive())) {
        updateSelectInput(session, "geneColumn", choices = names(inputDataReactive()$data))
        updateSelectInput(session, "log2fcColumn", choices = names(inputDataReactive()$data))
        updateSelectInput(session, "padjColumn", choices = names(inputDataReactive()$data))

        if (input$data_file_type == "examplecounts") {
            updateSelectInput(session, "geneColumn", selected = "X")
            updateSelectInput(session, "log2fcColumn", selected = "log2FoldChange")
            updateSelectInput(session, "padjColumn", selected = "padj")
        }

        return(T)
    }

    return(F)
})
outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)


observeEvent(input$nextInitParams, {
    js$collapse("iParamsbox")

    updateSelectInput(session, "organismDb", choices = organismsDbChoices)

    if (input$data_file_type == "examplecounts") {
        updateSelectInput(session, "organismDb", selected = "org.Dm.eg.db")
    }


    js$collapse("createGoBox")
})


observeEvent(input$organismDb, {
    req(input$organismDb)
    if (!requireNamespace(input$organismDb, quietly = TRUE)) {
        showNotification(
            paste("The selected annotation database is not installed:",
                  input$organismDb),
            type = "error",
            duration = NULL
        )
        return()
    }
    annDb <- get(input$organismDb, envir = asNamespace(input$organismDb))
    keytypes <- AnnotationDbi::keytypes(annDb)
    updateSelectInput(session, "keytype", choices = keytypes, selected = "ENSEMBL")
})
