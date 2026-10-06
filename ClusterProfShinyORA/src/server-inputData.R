decryptUrlParam <- function(cipher) {
    keyHex <- readr::read_file("private.txt")

    key <- hex2bin(keyHex)
    cipher <- hex2bin(cipher)

    orig <- simple_decrypt(cipher, key)

    unserialize(orig)
}

organismsDbChoices <- installed_organism_choices()

# What a differential-expression table must look like for the column pickers
# and prepare_gene_lists() to work. A wrong file explains itself (this list +
# the first lines received) instead of crashing the session.
de_table_expected <- c(
    "A plain-text table (CSV or tab-delimited) with a header row, one gene per row",
    "A gene identifier column (ENSEMBL, SYMBOL, ENTREZID ... matching the key type chosen later); an unnamed first column, as written by DESeq2, is accepted",
    "A numeric log2 fold-change column",
    "A numeric adjusted p-value (padj) column",
    "Other columns (baseMean, pvalue, ...) are allowed and ignored"
)

de_table_check <- function(df) {
    if (!nrow(df)) return("The table has a header but no rows.")
    if (ncol(df) < 3) {
        return(sprintf("Found %d column(s); need at least a gene identifier column, a log2 fold-change column and an adjusted p-value column (is the separator a comma or a tab?).", ncol(df)))
    }
    numeric_columns <- sum(vapply(df, is.numeric, logical(1)))
    if (numeric_columns < 2) {
        return("Fewer than 2 numeric columns were found; the log2 fold-change and adjusted p-value columns must hold numbers.")
    }
    NULL
}

# The gene_names handoff (e.g. from the variant annotation app) is a one-column
# gene list; prepare_gene_lists() reads it as data$x.
gene_list_expected <- c(
    "A plain-text file with a header row named 'x' and one gene identifier per line",
    "No other columns"
)

gene_list_check <- function(df) {
    if (!nrow(df)) return("The gene list has a header but no genes.")
    if (!"x" %in% names(df)) {
        return(sprintf("No column named 'x' was found (columns: %s).", paste(shQuote(head(names(df), 5)), collapse = ", ")))
    }
    NULL
}

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

# A new upload or another source replaces the restored table.
observeEvent(input$datafile, restored_input(NULL), ignoreInit = TRUE)
observeEvent(input$hpc_file_path, restored_input(NULL), ignoreInit = TRUE)
observeEvent(input$data_file_type, {
    if (!identical(input$data_file_type, restored_file_type())) restored_input(NULL)
}, ignoreInit = TRUE)

inputDataReactive <- reactive({
    print("inputting data")

    query <- parseQueryString(session$clientData$url_search)

    # A restored project supplies its saved input table.
    restored <- restored_input()
    if (!is.null(restored) && file.exists(restored)) {
        updateSelectInput(session, "organismDb", choices = organismsDbChoices)
        data <- normalize_enrichment_columns(
            nasqar_read_table(restored, de_table_expected, check = de_table_check,
                              label = "restored input table", check.names = FALSE)
        )
        js$addStatusIcon("datainput", "done")
        js$collapse("uploadbox")
        return(list("data" = data))
    }

    # A file in the private space is read in place.
    if (identical(input$data_file_type, "hpcfile")) {
        path <- tryCatch(resolve_hpc_path(input$hpc_file_path),
                         error = function(e) shiny::validate(need(FALSE, conditionMessage(e))))
        data <- normalize_enrichment_columns(
            nasqar_read_table(path, de_table_expected, check = de_table_check,
                              label = "table", check.names = FALSE)
        )
        js$addStatusIcon("datainput", "done")
        js$collapse("uploadbox")
        return(list("data" = data))
    }

    # Check if example selected, or if not then ask to upload a file.
    shiny::validate(
        need(!is.null(query[["countsdata"]]) | !is.null(query[["gene_names"]]) | identical(input$data_file_type, "examplecounts") | (!is.null(input$datafile)) |
             (identical(input$data_file_type, "hpcfile") && !is.null(input$hpc_file_path) && nzchar(trimws(input$hpc_file_path))),
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
            nasqar_read_table(inFile, gene_list_expected, check = gene_list_check,
                              label = "gene list", check.names = FALSE)
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
            nasqar_read_table(inFile, de_table_expected, check = de_table_check,
                              label = "results table", check.names = FALSE)
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
        # The separator (comma or tab) is detected from the header line.
        seqdata <- normalize_enrichment_columns(
            nasqar_read_table(inFile$datapath[1], de_table_expected, check = de_table_check,
                              label = "uploaded file", check.names = FALSE)
        )
        print("uploaded seqdata")

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
