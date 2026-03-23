# Installl missing packages
list.of.packages <- c("parallel", "shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
    install.packages(new.packages, repos = "https://cloud.r-project.org/", dependencies = T)
}

library(shiny)
library(shinyBS)
library(parallel)
library(shinyjs)
library(sodium)
library(uuid)
library(readr)
library(tximport)



ui <- tagList(
    tags$head(
        tags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
        tags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}"),
        tags$style(".BoxArea2 { padding:19px; margin: 5px; border: 3px solid; border-color:#c7dbe6; border-radius:10px;}"),
        tags$style(".BoxArea3 { padding:19px; margin: 5px; border: 3px solid; border-color:#ced2d6; border-radius:10px;}"),
        includeCSS("www/custom.css"),
        includeScript("www/shinybs_fix.js")
    ),
    navbarPage(
        theme = shinythemes::shinytheme("cerulean"),
        inverse = T,
        title = "Gene Count Merger (Pre-processing)",
        tabPanel("Home",
            icon = icon("home"),
            useShinyjs(), # Set up shinyjs
            fluidRow(
                column(
                    4,
                    bsCollapse(
                        id = "input_collapse_panel", open = "data_panel", multiple = FALSE,
                        bsCollapsePanel(
                            title = "Upload Files", value = "data_panel", style = "primary",
                            radioButtons("inputType", "Input type:",
                                c(
                                    "Raw gene counts" = "raw",
                                    "Kallisto estimated counts (.tsv)"              = "kallisto"
                                ),
                                selected = "raw"
                            ),
                            hr(),

                            # ---- RAW COUNTS ----
                            conditionalPanel(
                                "input.inputType == 'raw'",
                                p("1. Select multiple files containing counts to upload"),
                                p(strong("Note: "), "File names will be used as sample (column) names in output table"),
                                radioButtons("mergeType", "",
                                    c(
                                        "Merge individual sample counts" = "single",
                                        "Merge 2 or more matrices"       = "multiple"
                                    ),
                                    selected = "single"
                                ),
                                checkboxInput("hasHeader", "Files have header row", value = FALSE),
                                fileInput("datafile", "",
                                    accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        "text/tab-separated-values",
                                        ".csv", ".txt", ".tsv"
                                    ), multiple = TRUE
                                )
                            ),

                            # ---- KALLISTO ----
                            conditionalPanel(
                                "input.inputType == 'kallisto'",
                                p("Upload one Kallisto output file per sample (output of ", code("kallisto quant"), ")."),
                                p(strong("Note: "), "File names will be used as sample (column) names in output table"),
                                fileInput("kallistoFiles", "",
                                    accept  = c(".tsv", "text/tab-separated-values"),
                                    multiple = TRUE
                                )
                            )
                        ),

                        # ------------------------------------------------------------------ #
                        # CONFIGURE panel
                        #   Raw mode    → column selectors (gene ID / count column)
                        #   Kallisto    → transcript-to-gene mapping (prebuilt or custom CSV)
                        # ------------------------------------------------------------------ #
                        bsCollapsePanel(
                            title = "Configure", value = "column_panel", style = "info",

                            # ---- RAW: column selectors ----
                            conditionalPanel(
                                "input.inputType == 'raw'",
                                uiOutput("columnSelectionUI")
                            ),

                            # ---- KALLISTO: tx2gene mapping ----
                            conditionalPanel(
                                "input.inputType == 'kallisto'",
                                p(strong("Transcript-to-gene mapping:")),
                                radioButtons("tx2geneSource", "",
                                    c(
                                        "Use pre-built genome mapping" = "prebuilt",
                                        "Upload custom mapping (.csv)" = "custom"
                                    ),
                                    selected = "prebuilt"
                                ),
                                conditionalPanel(
                                    "input.tx2geneSource == 'prebuilt'",
                                    uiOutput("kallistoRefGenomeUI")
                                ),
                                conditionalPanel(
                                    "input.tx2geneSource == 'custom'",
                                    p("Upload a .csv with two columns:"),
                                    tags$ul(
                                        tags$li(strong("TX_NAME"), " – transcript IDs (must match those in the Kallisto output files)"),
                                        tags$li(strong("GENE_ID"), " – corresponding gene IDs")
                                    ),
                                    p("If not available in the pre-built options and you have a .gtf file for your genome,",
                                      a(href = "scripts/generate_tx2gene.R",
                                        "download this R script to generate a mapping.", download = NA, target = "_blank")),
                                    fileInput("tx2geneFile", "",
                                        accept  = c("text/csv", ".csv"),
                                        multiple = FALSE
                                    )
                                ),
                                uiOutput("tx2genePreviewUI")
                            ),
                            hr(),
                            checkboxInput("addOne", "Add +1 to counts (Pseudocounts)", FALSE),
                            checkboxInput("addGeneNames", "Retrieve gene names from ensembl ids", FALSE),
                            conditionalPanel(
                                "input.addGeneNames",
                                wellPanel(

                                    radioButtons("geneNamesSource", "",
                                        c(
                                            "Use pre-built gene name mapping" = "prebuilt",
                                            "Upload custom mapping (.csv)"    = "custom"
                                        ),
                                        selected = "prebuilt"
                                    ),
                                    conditionalPanel(
                                        "input.geneNamesSource == 'prebuilt'",
                                        uiOutput("refGenomeUI")
                                    ),
                                    conditionalPanel(
                                        "input.geneNamesSource == 'custom'",
                                        p("If not available in the options above and you have a .gtf file for your genome,",
                                          a(href = "scripts/generate_gene_names.R",
                                            "download this R script to generate a mapping.", download = NA, target = "_blank")),
                                        fileInput("gtfMappingFile", "Upload gene/id lookup table (.csv)",
                                            accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                            ), multiple = F
                                        )
                                    ),
                                    uiOutput("geneNamesPreviewUI"),

                                    radioButtons("geneNameColumn", "",
                                        c(
                                            "Add gene.names column after gene ids" = "add",
                                            "Replace gene ids column by gene names" = "replace"
                                        ),
                                        selected = "replace"
                                    )
                                )
                            ),
                            conditionalPanel(
                                "output.filesUploaded",
                                actionButton("upload_data", "Merge Files", class = "btn-danger")
                            ),
                            conditionalPanel(
                                "output.filesMerged",
                                hr(),
                                wellPanel(
                                    style = "background-color: #ffffff;",
                                    uiOutput("tab")
                                )
                            )
                        )
                    ) # bscollapse
                ), # column
                column(
                    8,
                    mainPanel(
                        width = 12,
                        tabsetPanel(
                            id = "tabs",
                            tabPanel(
                                "User Guide",
                                hr(),
                                h4(strong("1) Introduction:")),
                                wellPanel(
                                    p("A preprocessing tool to merge individual gene count files into a single count matrix, supporting both raw count files (e.g. HTSeq, featureCounts) and Kallisto abundance files."),
                                    hr(),
                                    h5(strong("Features")),
                                    tags$ul(
                                        tags$li(
                                            strong("Two input types:"),
                                            tags$ul(
                                                tags$li(strong("Raw counts"), " – merge individual per-sample count files (e.g. HTSeq, featureCounts). First column must contain gene IDs and must match across all files."),
                                                tags$li(strong("Kallisto"), " – merge Kallisto output files. Requires a transcript-to-gene (tx2gene) mapping.")
                                            )
                                        ),
                                        tags$li("Or merge", strong(" multiple pre-merged matrices")),
                                        tags$li(
                                            strong("Transcript-to-gene mapping (Kallisto only)"),
                                            tags$ul(
                                                tags$li("Choose from pre-built mappings for supported genomes"),
                                                tags$li("Or upload a custom two-column CSV with columns ", strong("TX_NAME"), " (transcript ID) and ", strong("GENE_ID"), " (gene ID). If you have a .gtf file, ", a(href = "scripts/generate_tx2gene.R", "download this R script to generate the mapping.", download = NA, target = "_blank"))
                                            )
                                        ),
                                        tags$li(
                                            strong("Convert Ensembl gene IDs to gene names"),
                                            tags$ul(
                                                tags$li("Choose from pre-built mappings for supported genomes"),
                                                tags$li("Or upload a custom two-column CSV with columns ", strong("GENE_ID"), " (Ensembl gene ID) and ", strong("GENE_NAME"), " (gene symbol). If you have a .gtf file, ", a(href = "scripts/generate_gene_names.R", "download this R script to generate the mapping.", download = NA, target = "_blank"))
                                            )
                                        ),
                                        tags$li("Option to add ", strong("pseudocounts (+1)"), " to all counts"),
                                        tags$li(strong("Download"), " merged counts file in .csv format"),
                                        tags$li(
                                            strong("Transcriptome Analysis (Optional)"), " – launch downstream analysis directly from the merged output:",
                                            tags$ul(
                                                tags$li("Use our ", strong("Seurat Wizard"), " for single-cell RNA analysis"),
                                                tags$li("Use ", strong("DESeq2Shiny"), " or ", strong("START"), " for bulk RNA analysis"),
                                                tags$li("If there are ", strong("no replicates"), ", use DESeq2Shiny for exploratory analysis")
                                            )
                                        )
                                    )
                                ),
                                hr(),
                                # wellPanel(
                                h4(strong("2) Sample Input Files:")),
                                tags$div(
                                    class = "BoxArea2",
                                    h5(strong("A) Raw counts")),
                                    p("Upload one count file per sample. Each file must have:"),
                                    tags$ul(
                                        tags$li("Column 1 – gene IDs (must match across all files)"),
                                        tags$li("Column 2 – raw read counts for that sample"),
                                        tags$li(strong("No header row"), " by default (tick ", em("Files have header row"), " if your files include column names)")
                                    ),
                                    fluidRow(
                                        column(
                                            12,
                                            p(strong("Example input files (no header):")),
                                            column(
                                                3,
                                                p(strong(tags$em("File 1 of 8: ")), "CT6_1.txt"),
                                                tags$table(
                                                    class = "table table-bordered table-condensed",
                                                    style = "width:100%; font-size:12px; background-color:#fff;",
                                                    tags$tbody(
                                                        tags$tr(tags$td("ENSMUSG00000000001"), tags$td("27")),
                                                        tags$tr(tags$td("ENSMUSG00000000003"), tags$td("0")),
                                                        tags$tr(tags$td("ENSMUSG00000000028"), tags$td("155")),
                                                        tags$tr(tags$td("ENSMUSG00000000037"), tags$td("5")),
                                                        tags$tr(tags$td("..."),               tags$td("..."))
                                                    )
                                                )
                                            ),
                                            column(
                                                3,
                                                p(strong(tags$em("File 2 of 8: ")), "CT6_2.txt"),
                                                tags$table(
                                                    class = "table table-bordered table-condensed",
                                                    style = "width:100%; font-size:12px; background-color:#fff;",
                                                    tags$tbody(
                                                        tags$tr(tags$td("ENSMUSG00000000001"), tags$td("31")),
                                                        tags$tr(tags$td("ENSMUSG00000000003"), tags$td("0")),
                                                        tags$tr(tags$td("ENSMUSG00000000028"), tags$td("203")),
                                                        tags$tr(tags$td("ENSMUSG00000000037"), tags$td("8")),
                                                        tags$tr(tags$td("..."),               tags$td("..."))
                                                    )
                                                )
                                            ),
                                            column(
                                                6,
                                                p("etc ...")
                                            )
                                        ),
                                        div(style = "clear:both;")
                                    ),
                                    p(strong("Note: "), "File names are used as sample (column) names in the output table. Column names can be edited after merging."),
                                    hr(),
                                    h5(strong("B) Kallisto")),
                                    p("Upload all Kallisto output files (one file per sample)."),
                                    fluidRow(
                                        column(
                                            12,
                                            p(strong("Example input files:")),
                                            column(
                                                5,
                                                p(strong(tags$em("File 1 of N: ")), "sample1.tsv"),
                                                tags$table(
                                                    class = "table table-bordered table-condensed",
                                                    style = "width:100%; font-size:12px; background-color:#fff;",
                                                    tags$thead(
                                                        tags$tr(style = "background-color:#f5f5f5;",
                                                            tags$th("target_id"),
                                                            tags$th("length"),
                                                            tags$th("eff_length"),
                                                            tags$th("est_counts"),
                                                            tags$th("tpm")
                                                        )
                                                    ),
                                                    tags$tbody(
                                                        tags$tr(tags$td("ENSMUST00000193812"), tags$td("1070"), tags$td("880.305"), tags$td("0"),       tags$td("0")),
                                                        tags$tr(tags$td("ENSMUST00000082908"), tags$td("110"),  tags$td("34.085"),  tags$td("0"),       tags$td("0")),
                                                        tags$tr(tags$td("ENSMUST00000162897"), tags$td("4153"), tags$td("3963.3"),  tags$td("0"),       tags$td("0")),
                                                        tags$tr(tags$td("ENSMUST00000195166"), tags$td("3012"), tags$td("3264.45"), tags$td("1.00032"), tags$td("0.1035")),
                                                        tags$tr(tags$td("ENSMUST00000194454"), tags$td("2351"), tags$td("2908.04"), tags$td("1"),       tags$td("0.1161")),
                                                        tags$tr(tags$td("..."),                tags$td("..."),  tags$td("..."),     tags$td("..."),     tags$td("..."))
                                                    )
                                                )
                                            ),
                                            column(
                                                3,
                                                p("etc ...")
                                            )
                                        ),
                                        div(style = "clear:both;")
                                    ),
                                    tags$ul(
                                        tags$li("A transcript-to-gene (tx2gene) mapping is required to summarise transcript-level estimates to gene level."),
                                        tags$li("Choose a pre-built mapping for a supported genome, or upload a custom two-column CSV with columns ", strong("TX_NAME"), " and ", strong("GENE_ID"), "."),
                                        tags$li("If you have a .gtf file for your genome, ", a(href = "scripts/generate_tx2gene.R", "download this R script to generate a mapping.", download = NA, target = "_blank")),
                                        tags$li("After upload, a ", strong("preview of the first uploaded file (first 6 rows)"), " is shown automatically.")
                                    ),
                                    p(strong("Note: "), "File names are used as sample (column) names. Column names can be edited after merging.")
                                ),
                                column(12, hr()),
                                h4(strong("3) Sample Output File:")),
                                div(style = "clear:both;"),
                                tags$div(
                                    class = "BoxArea2",
                                    p(strong("Output depending on options selected:")),
                                    p("The merged output is a ", strong("tab-separated CSV with a header row"), ". Column 1 contains gene IDs (or names if replaced); remaining columns are sample counts named after the uploaded file names."),
                                    column(
                                        12,
                                        p(strong(em("A) Without renaming/converting genes (Default)"))),
                                        tags$div(style = "overflow-x:auto;",
                                            tags$table(
                                                class = "table table-bordered table-condensed",
                                                style = "width:auto; font-size:12px; background-color:#fff;",
                                                tags$thead(tags$tr(style = "background-color:#f5f5f5;",
                                                    tags$th("gene.ids"),            tags$th("CT6_1"), tags$th("CT6_2"), tags$th("CT6_3"), tags$th("...")
                                                )),
                                                tags$tbody(
                                                    tags$tr(tags$td("ENSMUSG00000000001"), tags$td("27"),  tags$td("31"),  tags$td("18"),  tags$td("...")),
                                                    tags$tr(tags$td("ENSMUSG00000000003"), tags$td("0"),   tags$td("0"),   tags$td("0"),   tags$td("...")),
                                                    tags$tr(tags$td("ENSMUSG00000000028"), tags$td("155"), tags$td("203"), tags$td("98"),  tags$td("...")),
                                                    tags$tr(tags$td("ENSMUSG00000000037"), tags$td("5"),   tags$td("8"),   tags$td("3"),   tags$td("...")),
                                                    tags$tr(tags$td("..."),               tags$td("..."), tags$td("..."), tags$td("..."), tags$td("..."))
                                                )
                                            )
                                        )
                                    ),
                                    column(
                                        12,
                                        hr()
                                    ),
                                    column(
                                        12,
                                        p(strong(em("B) Retrieve gene names (replace), E.g. output file"))),
                                        tags$div(style = "overflow-x:auto;",
                                            tags$table(
                                                class = "table table-bordered table-condensed",
                                                style = "width:auto; font-size:12px; background-color:#fff;",
                                                tags$thead(tags$tr(style = "background-color:#f5f5f5;",
                                                    tags$th("gene.names"), tags$th("CT6_1"), tags$th("CT6_2"), tags$th("CT6_3"), tags$th("...")
                                                )),
                                                tags$tbody(
                                                    tags$tr(tags$td("Gnai3"), tags$td("27"),  tags$td("31"),  tags$td("18"),  tags$td("...")),
                                                    tags$tr(tags$td("Pbsn"),  tags$td("0"),   tags$td("0"),   tags$td("0"),   tags$td("...")),
                                                    tags$tr(tags$td("Cdc45"), tags$td("155"), tags$td("203"), tags$td("98"),  tags$td("...")),
                                                    tags$tr(tags$td("Hira"),  tags$td("5"),   tags$td("8"),   tags$td("3"),   tags$td("...")),
                                                    tags$tr(tags$td("..."),   tags$td("..."), tags$td("..."), tags$td("..."), tags$td("..."))
                                                )
                                            )
                                        )
                                    ),
                                    column(
                                        12,
                                        hr()
                                    ),
                                    column(
                                        12,
                                        p(strong(em("C) Retrieve gene names (add), E.g. output file"))),
                                        tags$div(style = "overflow-x:auto;",
                                            tags$table(
                                                class = "table table-bordered table-condensed",
                                                style = "width:auto; font-size:12px; background-color:#fff;",
                                                tags$thead(tags$tr(style = "background-color:#f5f5f5;",
                                                    tags$th("gene.ids"),            tags$th("gene.names"), tags$th("CT6_1"), tags$th("CT6_2"), tags$th("CT6_3"), tags$th("...")
                                                )),
                                                tags$tbody(
                                                    tags$tr(tags$td("ENSMUSG00000000001"), tags$td("Gnai3"), tags$td("27"),  tags$td("31"),  tags$td("18"),  tags$td("...")),
                                                    tags$tr(tags$td("ENSMUSG00000000003"), tags$td("Pbsn"),  tags$td("0"),   tags$td("0"),   tags$td("0"),   tags$td("...")),
                                                    tags$tr(tags$td("ENSMUSG00000000028"), tags$td("Cdc45"), tags$td("155"), tags$td("203"), tags$td("98"),  tags$td("...")),
                                                    tags$tr(tags$td("ENSMUSG00000000037"), tags$td("Hira"),  tags$td("5"),   tags$td("8"),   tags$td("3"),   tags$td("...")),
                                                    tags$tr(tags$td("..."),               tags$td("..."),   tags$td("..."), tags$td("..."), tags$td("..."), tags$td("..."))
                                                )
                                            )
                                        )
                                    ),
                                    div(style = "clear:both;")
                                ),
                                column(12, hr()),
                                h4(strong("4) Transcriptome Analysis (Optional):")),
                                div(style = "clear:both;"),
                                tags$div(
                                    class = "BoxArea2",
                                    p(strong("Start your analysis by launching the appropriate application for your data")),
                                    p(strong("Your merged counts data will be automatically loaded")),
                                    fluidRow(
                                        column(
                                            4,
                                            tags$div(
                                                class = "BoxArea3", style = "text-align:center;",
                                                p(strong("Single-Cell RNA")),
                                                tags$span(class = "btn btn-success btn-sm", style = "width:100%; pointer-events:none;", "Seurat Wizard")
                                            )
                                        ),
                                        column(
                                            4,
                                            tags$div(
                                                class = "BoxArea3", style = "text-align:center;",
                                                p(strong("Bulk RNA")),
                                                tags$span(class = "btn btn-success btn-sm", style = "width:100%; pointer-events:none; margin-bottom:6px;", "DESeq2Shiny"),
                                                tags$br(),
                                                tags$span(class = "btn btn-success btn-sm", style = "width:100%; pointer-events:none; margin-top:6px;", "START")
                                            )
                                        ),
                                        column(
                                            4,
                                            p(em("Buttons appear automatically after merging is complete."))
                                        )
                                    ),
                                    column(
                                        12,
                                        hr()
                                    ),
                                    div(style = "clear:both;")
                                )
                            ),
                            tabPanel(
                                "Output",
                                h4(p(strong("Merged counts"))),
                                hr(),
                                conditionalPanel(
                                    "output.filesMerged",
                                    conditionalPanel(
                                        "!output.columnNamesValid",
                                        div(
                                            class = "alert alert-warning",
                                            p(strong("Sample names are NOT valid for use in our analysis apps")),
                                            p("Edit the column names so they are of the following format:"),
                                            tags$ul(
                                                tags$li(strong("sampleName_replicateNumber. Eg: kidneyA_1, kidneyA_2, liver_1, liver_2, etc ...")),
                                                tags$li(strong("do NOT use any special characters in sample names except underscore'_' to denote replicate numbers"))
                                            )
                                        )
                                    ),
                                    bsCollapse(
                                        id = "editCols_collapse_panel", multiple = FALSE,
                                        bsCollapsePanel(
                                            title = "Edit Column Names", value = "editCols_panel", style = "primary",
                                            uiOutput("editColumnNamesView")
                                        )
                                    ),
                                    p(
                                        downloadLink("downloadData", "Download Merged File", class = "btn btn-warning", style = "color: #fff; background-color: #9E0000; border-color: #9E0000")
                                    ),
                                    hr(),
                                    DT::DTOutput("contents")
                                ),
                                conditionalPanel(
                                    "!output.filesMerged",
                                    p(
                                        # verbatimTextOutput("mergeStatus")
                                        uiOutput("mergeStatus")
                                    ),
                                    h4(p(em(
                                        # verbatimTextOutput("mergeStatus")
                                    ), style = "color:#f56a6a;"))
                                )
                            )
                        )
                    )
                )
            ) # fluidrow
        ), # tabpanel
        tabPanel(
            "Terms of Use",
            fluidRow(
                column(10,
                    offset = 1,
                    includeMarkdown("termsConditions.Rmd")
                )
            )
        ),


        ## ==================================================================================== ##
        ## FOOTER
        ## ==================================================================================== ##
        footer = p(
            hr(), p("Gene Count Merger created by ", "Core Bioinformatics Team", " of ", align = "center", width = 4),
            p(("Center for Genomics and Systems Biology, NYU Abu Dhabi"), align = "center", width = 4),
            p(("Copyright (C) 2018, code licensed under GPLv3"), align = "center", width = 4)
        )
    ) # end navbarpage
) # end taglist




options(shiny.maxRequestSize = 60 * 1024^2)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    observe({
        myValues$status <- "Upload file(s) first"
    })

    myValues <- reactiveValues()

    # ---------------------------------------------------------------------- #
    # tx2gene preview – loads the first 6 rows from whichever source is active
    # ---------------------------------------------------------------------- #
    tx2genePreviewReactive <- reactive({
        if (!isTRUE(input$inputType == "kallisto")) return(NULL)

        if (isTRUE(input$tx2geneSource == "prebuilt")) {
            rda_path <- paste0("www/tx2gene/", input$kallistoRefGenome, ".Rda")
            if (!file.exists(rda_path)) return(NULL)
            e <- new.env(parent = emptyenv())
            load(rda_path, envir = e)
            return(head(e$tx2gene, 6))
        } else {
            if (is.null(input$tx2geneFile)) return(NULL)
            tbl <- tryCatch(
                read.csv(input$tx2geneFile$datapath, header = TRUE,
                         colClasses = c("character", "character"),
                         strip.white = TRUE),
                error = function(e) NULL
            )
            if (is.null(tbl) || ncol(tbl) < 2) return(NULL)
            colnames(tbl)[1:2] <- c("TX_NAME", "GENE_ID")
            return(head(tbl, 6))
        }
    })

    # Dynamically list only the tx2gene .Rda files that actually exist on disk
    output$kallistoRefGenomeUI <- renderUI({
        choices <- tools::file_path_sans_ext(
            list.files("www/tx2gene", pattern = "\\.Rda$")
        )
        selectInput("kallistoRefGenome", "Select pre-built tx2gene mapping:",
                    choices  = choices,
                    selected = choices[1])
    })

    # Dynamically list only the gene name .Rda files that actually exist on disk
    output$refGenomeUI <- renderUI({
        choices <- tools::file_path_sans_ext(
            list.files("www/gene_names", pattern = "\\.Rda$")
        )
        selectInput("refGenome", "Select pre-built gene name mapping:",
                    choices  = choices,
                    selected = choices[1])
    })

    output$tx2genePreviewUI <- renderUI({
        preview <- tx2genePreviewReactive()
        if (is.null(preview)) return(NULL)
        tagList(
            hr(),
            p(strong("Mapping preview (first 6 rows):")),
            tableOutput("tx2genePreviewTable")
        )
    })

    output$tx2genePreviewTable <- renderTable({
        tx2genePreviewReactive()
    }, bordered = TRUE, striped = TRUE, hover = TRUE, width = "100%")

    # ---------------------------------------------------------------------- #
    # Gene names preview – first 6 rows from whichever source is active
    # ---------------------------------------------------------------------- #
    geneNamesPreviewReactive <- reactive({
        if (!isTRUE(input$addGeneNames)) return(NULL)

        if (isTRUE(input$geneNamesSource == "prebuilt")) {
            rda_path <- paste0("www/gene_names/", input$refGenome, ".Rda")
            if (!file.exists(rda_path)) return(NULL)
            e <- new.env(parent = emptyenv())
            load(rda_path, envir = e)
            return(head(e$geneid2name, 6))
        } else {
            if (is.null(input$gtfMappingFile)) return(NULL)
            tbl <- tryCatch(
                read.csv(input$gtfMappingFile$datapath, header = TRUE,
                         colClasses = c("character", "character"),
                         strip.white = TRUE),
                error = function(e) NULL
            )
            if (is.null(tbl) || ncol(tbl) < 2) return(NULL)
            colnames(tbl)[1:2] <- c("GENE_ID", "GENE_NAME")
            return(head(tbl, 6))
        }
    })

    output$geneNamesPreviewUI <- renderUI({
        preview <- geneNamesPreviewReactive()
        if (is.null(preview)) return(NULL)
        tagList(
            hr(),
            p(strong("Mapping preview (first 6 rows):")),
            tableOutput("geneNamesPreviewTable")
        )
    })

    output$geneNamesPreviewTable <- renderTable({
        geneNamesPreviewReactive()
    }, bordered = TRUE, striped = TRUE, hover = TRUE, width = "100%")

    output$mergeStatus <- renderUI({
        div(
            class = "alert alert-danger",
            strong(myValues$status)
        )
    })

    observeEvent(input$viewRenameColumns, {
        toggle("editColumnNamesView", TRUE)
    })

    # Auto-open the Configure panel when Kallisto abundance.tsv files are uploaded,
    # mirroring the raw-mode behaviour (inputDataReactive opens it for raw files).
    observeEvent(input$kallistoFiles, {
        req(input$kallistoFiles)
        updateCollapse(session,
            id = "input_collapse_panel", open = "column_panel",
            style = list("column_panel" = "info", "data_panel" = "success")
        )
    })

    # Trigger that increments each time the Edit Column Names panel is opened.
    # renderUI observes this trigger (not myValues$mergedData) so that the
    # textboxes are only rebuilt when the panel opens – never while the user
    # is actively typing inside them.
    # Increments each time the Edit Column Names panel is opened so renderUI
    # gets fresh input IDs — Shiny never restores a stale cached value.
    editColsTrigger <- reactiveVal(0)
    observeEvent(input$editCols_collapse_panel, {
        if ("editCols_panel" %in% input$editCols_collapse_panel) {
            editColsTrigger(editColsTrigger() + 1)
        }
    })

    output$editColumnNamesView <- renderUI({
        trigger     <- editColsTrigger()
        columnNames <- isolate(colnames(myValues$mergedData))
        req(length(columnNames) > 0)

        outputUI <- lapply(seq_along(columnNames), function(i) {
            id <- paste0("textboxColumns", i, "_", trigger)
            div(style = "margin-bottom: 12px;",
                tags$label(
                    style = "display:block; font-weight:600;",
                    paste0("Column ", i, ":  "),
                    tags$small(style = "font-weight:normal; color:#555;",
                               columnNames[i])
                ),
                tags$input(
                    id       = id,
                    type     = "text",
                    class    = "form-control",
                    value    = columnNames[i],
                    style    = "width:100%;",
                    disabled = if (i == 1) NA else NULL
                )
            )
        })

        outputUI[[length(outputUI) + 1]] <- tags$br()
        outputUI[[length(outputUI) + 2]] <- actionButton("saveColumnNames", "Save")

        wellPanel(style = "overflow-x: auto;", outputUI)
    })

    observeEvent(input$saveColumnNames, {
        req(myValues$mergedData)
        trigger     <- isolate(editColsTrigger())
        newColNames <- sapply(seq_along(colnames(myValues$mergedData)), function(i) {
            val <- input[[paste0("textboxColumns", i, "_", trigger)]]
            if (is.null(val) || nchar(trimws(val)) == 0) colnames(myValues$mergedData)[i] else val
        })
        colnames(myValues$mergedData) <- newColNames
        updateCollapse(session, id = "editCols_collapse_panel", close = "editCols_panel")
    })


    output$tab <- renderUI({
        if (is.null(myValues$fileUrl)) return(NULL)
        tagList(
            h4(strong("Transcriptome Analysis (Optional):")),
            p("Start your analysis by launching the appropriate application for your data"),
            p("* If there are ", strong("NO replicates"), ", use DESeq2Shiny app for exploratory analysis"),
            p(strong("Your merged counts data will be automatically loaded")),
            fluidRow(
                column(8,
                    style = "margin-left: 20%;",
                    div(class = "BoxArea3 para", strong("Select Analysis Type:"))
                )
            ),
            fluidRow(
                column(8,
                    offset = 2,
                    div(class = "brace top")
                )
            ),
            fluidRow(
                column(
                    6,
                    tags$div(
                        class = "BoxArea3", style = "text-align: center;",
                        p(strong("Single-Cell RNA")),
                        a("Seurat Wizard", href = paste0("/SeuratWizard?countsdata=", encryptUrlParam(myValues$fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
                    )
                ),
                column(
                    6,
                    tags$div(
                        class = "BoxArea3", style = "text-align: center;",
                        p(strong("Bulk RNA")),
                        a("DESeq2Shiny", href = paste0("/deseq2shiny?countsdata=", encryptUrlParam(myValues$fileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;"),
                        hr(),
                        a("START", href = paste0("/tsar_nasqar?countsdata=", encryptUrlParam(myValues$startAppFileUrl)), class = "btn btn-success", target = "_blank", style = "width: 100%;")
                    )
                ),
                div(style = "clear:both;")
            )
        )
    })

    encryptUrlParam <- function(paramStr) {
        pubkeyHex <- read_file("public.txt")
        pubkey <- hex2bin(pubkeyHex)

        msg <- serialize(paramStr, NULL)
        ciphertext <- simple_encrypt(msg, pubkey)

        bin2hex(ciphertext)
    }

    output$contents <- DT::renderDT(
        {
            if (!is.null(myValues$mergedData)) myValues$mergedData
        },
        filter  = "none",
        options = list(scrollX = TRUE, pageLength = 10)
    )

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
            write.csv(myValues$mergedData, con, row.names = FALSE)
        }
    )

    inputDataReactive <- reactive({
        inFile <- input$datafile
        if (is.null(inFile)) {
            return(NULL)
        }

        updateCollapse(session,
            id = "input_collapse_panel", open = "column_panel",
            style = list(
                "column_panel" = "info",
                "data_panel" = "success"
            )
        )

        return(inFile)
    })


    # Detect column names/indices from the first uploaded file
    fileColumnsReactive <- reactive({
        inFile <- input$datafile
        if (is.null(inFile)) return(NULL)

        useHeader <- isTRUE(input$hasHeader)

        sep <- "\t"
        firstFile <- tryCatch(
            read.csv(inFile$datapath[1], header = useHeader, sep = "\t", nrows = 5),
            error = function(e) NULL
        )
        if (is.null(firstFile) || ncol(firstFile) < 2) {
            sep <- ","
            firstFile <- tryCatch(
                read.csv(inFile$datapath[1], header = useHeader, sep = ",", nrows = 5),
                error = function(e) NULL
            )
        }

        if (is.null(firstFile)) return(NULL)

        if (useHeader) {
            cols <- colnames(firstFile)
        } else {
            cols <- paste0("Column ", seq_len(ncol(firstFile)))
        }

        list(cols = cols, ncols = length(cols))
    })

    output$columnSelectionUI <- renderUI({
        colInfo <- fileColumnsReactive()

        if (is.null(colInfo)) {
            return(p(em("Upload files first to see column options."), style = "color:#888;"))
        }

        cols <- colInfo$cols
        choices <- setNames(as.list(seq_along(cols)), cols)
        defaultCount <- if (length(cols) >= 2) 2 else 1

        tagList(
            p("Specify which columns in your files contain gene IDs and counts:"),
            selectInput("idColumn", "Gene ID Column:", choices = choices, selected = 1),
            if (input$mergeType == "single") {
                selectInput("countColumn", "Count Column:", choices = choices, selected = defaultCount)
            }
        )
    })

    analyzeDataReactive <-
        eventReactive(input$upload_data,
            ignoreNULL = FALSE,
            {
                removeNotification("errorNotify")

                isKallisto <- isTRUE(input$inputType == "kallisto")

                if (isKallisto) {
                    # ------------------------------------------------------------------ #
                    # KALLISTO PATH – import estimated counts from abundance.h5 via tximport
                    # ------------------------------------------------------------------ #
                    tx2geneReady <- isTRUE(input$tx2geneSource == "prebuilt") || !is.null(input$tx2geneFile)
                    if (is.null(input$kallistoFiles) || !tx2geneReady) {
                        return(NULL)
                    }

                    progress <- Progress$new(session, min = 0, max = 1)
                    on.exit(progress$close())
                    progress$set(message = "Importing Kallisto counts via tximport ...")

                    suppressWarnings(
                        validate(need(
                            tryCatch(
                                {
                                    total <- multmerge_kallisto(input$kallistoFiles)
                                },
                                error = function(e) {
                                    myValues$status <- paste("Error: ", e$message)
                                    updateTabsetPanel(session, "tabs", selected = "Output")
                                    showNotification(id = "errorNotify", myValues$status, type = "error", duration = 20)
                                    return(NULL)
                                }
                            ),
                            "Error importing Kallisto files. Check!"
                        ))
                    )

                } else {
                    # ------------------------------------------------------------------ #
                    # RAW COUNTS PATH – merge individual count files / matrices
                    # ------------------------------------------------------------------ #
                    inFile <- inputDataReactive()
                    if (is.null(inFile)) {
                        return(NULL)
                    }

                    progress <- Progress$new(session, min = 0, max = 1)
                    on.exit(progress$close())
                    progress$set(message = "Merging files ...")

                    sep <- "\t"
                    if (length(inFile$datapath) > 0) {
                        testSep <- read.csv(inFile$datapath[1], header = FALSE, sep = "\t")
                        if (ncol(testSep) < 2) {
                            sep <- ","
                        }
                    } else {
                        return(NULL)
                    }

                    # remove zero size files
                    inFile <- inFile[inFile$size != 0, ]

                    suppressWarnings(
                        validate(need(
                            tryCatch(
                                {
                                    total <- multmerge(inFile, sep, input$mergeType == "multiple")
                                },
                                error = function(e) {
                                    myValues$status <- paste("Error: ", e$message, "\nMake sure your file(s) have the same dimensions")
                                    updateTabsetPanel(session, "tabs", selected = "Output")
                                    showNotification(id = "errorNotify", myValues$status, type = "error", duration = 20)
                                    return(NULL)
                                }
                            ),
                            "Error merging files. Check!"
                        ))
                    )
                }

                # ------------------------------------------------------------------ #
                # SHARED DOWNSTREAM – gene name conversion, pseudocounts, save & launch
                # ------------------------------------------------------------------ #

                if (input$addGeneNames) {
                    geneNames <- getNamesFromEnsembl(total[, 1], progress)

                    if (length(geneNames) != nrow(total)) {
                        myValues$status <- paste("Error converting gene names", "", "\nMake sure to select the correct genome/version")
                        showNotification(id = "errorNotify", myValues$status, type = "error", duration = 20)
                        updateTabsetPanel(session, "tabs", selected = "Output")

                        return(NULL)
                    }


                    if (input$geneNameColumn == "add") {
                        total <- as.data.frame(append(total, list(gene.names = geneNames), after = 1))
                    } else {
                        # total[,1] = list(gene.names= geneNames)

                        total[, 1] <- make.names(geneNames, unique = TRUE)
                        colnames(total)[1] <- "gene.names"
                    }
                }

                if (input$addOne) {
                    total[, !(names(total) %in% c("gene.ids", "gene.names"))] <- total[, !(names(total) %in% c("gene.ids", "gene.names"))] + 1
                }

                myValues$fileUrl <- UUIDgenerate()
                myValues$fileUrl <- paste0(tempdir(), "/", myValues$fileUrl, ".csv")

                updateTabsetPanel(session, "tabs", selected = "Output")

                total[, 1] <- as.character(total[, 1])
                myValues$mergedData <- total
                return(list("data" = total))
            }
        )

    observeEvent(
        {
            myValues$mergedData
        },
        {
            write.csv(myValues$mergedData, myValues$fileUrl, row.names = F)


            # this is specific to STARTapp. need to add _1 when no replicates are present
            myValues$startAppFileUrl <- gsub("\\.csv", "_startapp.csv", myValues$fileUrl)

            mergedCountsOnly <- myValues$mergedData[, -1]
            countsColStartIndex <- 2

            if (class(mergedCountsOnly[, 1]) != "integer") {
                mergedCountsOnly <- mergedCountsOnly[, -1]
                countsColStartIndex <- 3
            }

            # mergedColNames = colnames(mergedCountsOnly)
            #
            # containsUnderscoreReplNum= grepl('_[0-9]+$',mergedColNames)
            # if(!all(containsUnderscoreReplNum))
            # {
            #   #remove all underscores if present
            #   mergedColNames = gsub("_","",mergedColNames)
            #
            #   #add underscore 1
            #   mergedColNames = paste0(mergedColNames, "_1")
            #
            #   startappMergedDf = myValues$mergedData
            #   colnames(startappMergedDf)[seq(countsColStartIndex,ncol(startappMergedDf))] = mergedColNames
            #
            #   write.csv(startappMergedDf, myValues$startAppFileUrl, row.names = F)
            # }
            # else
            write.csv(myValues$mergedData, myValues$startAppFileUrl, row.names = F)
        }
    )

    multmerge <- function(inFiles, sep, isMultiple) {
        filenames <- inFiles$datapath

        idColIdx    <- if (!is.null(input$idColumn))    as.integer(input$idColumn)    else 1
        countColIdx <- if (!is.null(input$countColumn)) as.integer(input$countColumn) else 2

        useHeader <- isTRUE(input$hasHeader)

        datalist <- lapply(filenames, function(x) {
            fileContent <- read.csv(file = x, header = useHeader, sep = sep)

            n <- ncol(fileContent)

            if (!isMultiple) {
                # Single-sample mode: keep only the chosen ID and count columns
                idIdx    <- min(idColIdx, n)
                countIdx <- min(countColIdx, n)
                fileContent <- fileContent[, c(idIdx, countIdx), drop = FALSE]
            } else {
                # Matrix mode: reorder so the chosen ID column is first
                idIdx <- min(idColIdx, n)
                if (idIdx != 1) {
                    otherCols <- setdiff(seq_len(n), idIdx)
                    fileContent <- fileContent[, c(idIdx, otherCols), drop = FALSE]
                }
            }

            colnames(fileContent)[1] <- "gene.ids"
            fileContent <- fileContent[!grepl("__", fileContent[, 1]), ] # remove rows containing underscores

            # Sort by gene_id incase they are not sorted
            fileContent <- fileContent[order(fileContent[, 1]), ]

            fileContent
        })

        reduced <- Reduce(function(x, y) {
            merge(x, y, by = "gene.ids")
        }, datalist)



        if (!isMultiple) {
            samplenames <- unlist(lapply(inFiles$name, function(x) {
                tools::file_path_sans_ext(x)
            }))

            colnames(reduced) <- c("gene.ids", samplenames)
        }


        return(reduced)
    }



    # ---------------------------------------------------------------------- #
    # multmerge_kallisto – import per-sample abundance.h5 files via tximport
    #   inFiles : the data.frame returned by input$kallistoFiles
    #   Reads input$tx2geneFile from the reactive environment (same as multmerge
    #   reads input$idColumn / input$countColumn).
    # ---------------------------------------------------------------------- #
    multmerge_kallisto <- function(inFiles) {
        # Named vector: path on server -> sample name (filename without extension)
        sampleNames <- tools::file_path_sans_ext(inFiles$name)
        files       <- setNames(inFiles$datapath, sampleNames)

        # Load transcript-to-gene mapping (TX_NAME, GENE_ID)
        # – prebuilt: load the pre-generated .Rda for the selected genome/version
        # – custom  : read the user-supplied two-column CSV
        if (isTRUE(input$tx2geneSource == "prebuilt")) {
            rda_path <- paste0("www/tx2gene/", input$kallistoRefGenome, ".Rda")
            if (!file.exists(rda_path)) {
                stop(
                    "Pre-built transcript-to-gene mapping for '", input$kallistoRefGenome, "' is not yet available. ",
                    "Please select 'Upload custom mapping (.csv)' and supply your own tx2gene file, ",
                    "or run www/scripts/generate_tx2gene.R to build the pre-built mappings."
                )
            }
            load(rda_path)   # expects an object named 'tx2gene' in the .Rda
        } else {
            tx2gene <- read.csv(
                input$tx2geneFile$datapath,
                header      = TRUE,
                colClasses  = c("character", "character"),
                strip.white = TRUE
            )
            colnames(tx2gene)[1:2] <- c("TX_NAME", "GENE_ID")
        }

        # Import via tximport (type = "kallisto", TSV format).
        # abundance.tsv columns: target_id, length, eff_length, est_counts, tpm
        txi <- tximport::tximport(
            files,
            type            = "kallisto",
            tx2gene         = tx2gene,
            ignoreTxVersion = TRUE
        )

        # Round estimated counts to integers and convert to data.frame
        counts_mat <- round(txi$counts)
        df         <- as.data.frame(counts_mat, stringsAsFactors = FALSE)
        df         <- cbind(gene.ids = rownames(df), df)
        rownames(df) <- NULL

        # Sort by gene id (consistent with multmerge behaviour)
        df <- df[order(df[, 1]), ]

        return(df)
    }

    getNamesFromEnsembl <- function(ensNames, progress) {
        # <- Progress$new(session, min=0, max=1)
        progress$set(value = 0.3)
        progress$set(message = "Adding gene names ...")


        # load("geneid2name.Rda")

        # Determine which gene name lookup to use:
        #   1. Custom upload → read user-supplied CSV
        #   2. Pre-built     → load from refGenome .Rda
        if (isTRUE(input$geneNamesSource == "custom")) {
            geneid2name <- read.csv2(input$gtfMappingFile$datapath, sep = ",",
                                     colClasses = c("character", "character"))
        } else {
            load(paste0("www/gene_names/", input$refGenome, ".Rda"))
        }

        #   ids<-bitr(ensNames, fromType = "ENSEMBL", toType = "SYMBOL", OrgDb="org.Ce.eg.db")
        #   ids$ENSEMBL <- ids$ENSEMBL
        # # df<-merge(df, ids[, c("WormBaseId", "SYMBOL")], by="WormBaseId",all.x = TRUE)



        # geneStartStr = as.character(ensNames[1])
        #
        # annoDb <- NULL
        # if(gdata::startsWith(geneStartStr, "ENSDAR",ignore.case=TRUE))
        #   annoDb= org.Dr.eg.db
        # else if(gdata::startsWith(geneStartStr, "ENSMUS",ignore.case=TRUE))
        #   annoDb <- org.Mm.eg.db
        # else if(gdata::startsWith(geneStartStr, "FB",ignore.case=TRUE))
        #   annoDb <- org.Dm.eg.db
        # else
        #   annoDb <- org.Hs.eg.db


        # Calculate the number of cores
        no_cores <- detectCores() - 1

        # Initiate cluster
        # cl <- makeCluster(no_cores)
        cl <- makeForkCluster(no_cores)

        print(paste(format(Sys.time(), "%H:%M:%OS3"), ": Started Renaming ", length(ensNames), " genes"))
        # levelsList = character(length(ensNames))
        levelsList <- parallel::parLapply(cl, ensNames, function(x) {
            # return(ids[geneid2name$GENE_ID == as.character(x),]$GENE_NAME)
            return(geneid2name[geneid2name$GENE_ID == as.character(x), ]$GENE_NAME)
        })

        print(paste(format(Sys.time(), "%H:%M:%OS3"), ": Finished renaming"))
        stopCluster(cl)

        progress$set(value = 0.8)

        flatList <- unlist(levelsList)

        progress$set(value = 1)
        return(flatList)

        # return(annoDb$SYMBOL)
    }


    output$filesUploaded <- reactive({
        if (isTRUE(input$inputType == "kallisto")) {
            h5Ready      <- !is.null(input$kallistoFiles)
            tx2geneReady <- isTRUE(input$tx2geneSource == "prebuilt") || !is.null(input$tx2geneFile)
            return(h5Ready && tx2geneReady)
        }
        return(!is.null(inputDataReactive()))
    })
    outputOptions(output, "filesUploaded", suspendWhenHidden = FALSE)

    output$filesMerged <- reactive({
        return(!is.null(analyzeDataReactive()))
    })
    outputOptions(output, "filesMerged", suspendWhenHidden = FALSE)

    output$columnNamesValid <- reactive({
        if (!is.null(myValues$mergedData)) {
            columnNames <- colnames(myValues$mergedData)[-1]

            valid <- all(grepl("^[[:alnum:]]+[_]*[[:digit:]]*$", columnNames, ignore.case = T))
            return(valid)
        }
        return(T)
    })
    outputOptions(output, "columnNamesValid", suspendWhenHidden = FALSE)


    observe({
        # Check if example selected, or if not then ask to upload a file.
        shiny::validate(
            need((input$data_file_type == "examplecounts") | ((!is.null(input$rdatafile)) | (!is.null(input$datafile))),
                message = "Please select a file"
            )
        )
        inFile <- input$datafile
    })
}

shinyApp(ui = ui, server = server)
