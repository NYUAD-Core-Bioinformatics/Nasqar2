tabItem(
    tabName = "conditionsTab",
    fluidRow(
        conditionalPanel(
            "(output.ddsInitAvailable && input.init_deseq2 > 0) || (!output.ddsInitAvailable && input.init_deseq2 < 1)",
            column(
                12,
                box(
                    title = "1. Sample table", solidHeader = T, status = "primary", width = 12,
                    p("Every sample needs a value for each experimental factor. Fill the table in either way:"),
                    fluidRow(
                        column(
                            7,
                            h4(strong(icon("table"), " Edit the table")),
                            helpText("Click a cell to change the value of a sample."),
                            rHandsontableOutput("table"),
                            div(style = "margin-top:12px;",
                                downloadButton("downloadCSV", "Download table (.csv)", class = "btn btn-default btn-sm"))
                        ),
                        column(
                            5,
                            div(
                                style = "border:1px solid #cfe2f3; border-left:4px solid #3c8dbc; border-radius:4px; background:#f4f8fb; padding:14px 18px;",
                                h4(style = "margin-top:0;", strong(icon("upload"), " Or upload a design table")),
                                helpText(
                                    "A .csv or .txt file (comma or tab delimited) with a header row:",
                                    "sample names in the first column, exactly as in the counts table,",
                                    "then one column per factor. Uploading replaces the table."
                                ),
                                fileInput("metadatafile", NULL,
                                    accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"
                                    ), multiple = FALSE
                                ),
                                helpText(style = "margin-bottom:0;",
                                         "Tip: download the table on the left, edit it in a spreadsheet and upload it again.")
                            )
                        )
                    )
                ),
                box(
                    title = "2. Factors and samples (optional)", solidHeader = T, status = "primary", width = 12,
                    fluidRow(
                        column(
                            4,
                            h4(strong("Add a factor")),
                            textInput("conditionName", "Factor name", placeholder = "Time"),
                            textInput(
                                "conditions",
                                "Its values (comma separated)",
                                placeholder = "1hr, 5hr, 6hr"
                            ),
                            actionButton(
                                "addConditions",
                                "Add factor",
                                icon = icon("plus"),
                                class = "btn btn-primary", style = "color:#fff;"
                            )
                        ),
                        column(
                            4,
                            h4(strong("Remove a factor")),
                            selectInput("colToRemove", "Factor", choices = NULL),
                            actionButton(
                                "removeCol",
                                "Remove",
                                class = "btn btn-danger", style = "color:#fff;",
                                icon = icon("times")
                            )
                        ),
                        column(
                            4,
                            h4(strong("Exclude samples")),
                            selectizeInput(
                                "samplesToExclude",
                                "Outliers to leave out of the analysis",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                    placeholder = "Select samples to exclude...",
                                    maxItems = NULL
                                )
                            ),
                            actionButton(
                                "clearExcludedSamples",
                                "Clear all",
                                class = "btn btn-default btn-sm",
                                icon = icon("undo")
                            )
                        )
                    )
                ),
                conditionalPanel(
                    "output.hasExcludedSamples",
                    box(
                        title = "Excluded samples", solidHeader = T, status = "warning", width = 12,
                        p(strong("Samples left out of the DESeq2 analysis:"), style = "color: #d9534f;"),
                        verbatimTextOutput("excludedSamplesList", placeholder = FALSE)
                    )
                ),
                fluidRow(
                    column(
                        8,
                        box(
                            title = "3. Design formula", solidHeader = T, status = "primary", width = 12,
                            textInput("designFormula", NULL, placeholder = "~ Conditions", width = "100%"),
                            helpText("The factors DESeq2 models, joined by +, for example ~ Genotype + Treatment.")
                        )
                    ),
                    column(
                        4,
                        box(
                            title = "4. Initialize DESeq2 dataset", solidHeader = T, status = "success", width = 12,
                            p("Builds the DESeq2 dataset from the counts, the sample table and the design formula."),
                            actionButton("init_deseq2", "Initialize DESeq2", icon = icon("play"),
                                class = "btn btn-success",
                                style = "width:100%; height:46px; color:#fff;"
                            )
                        )
                    )
                ),
                tags$div(class = "clearBoth")
            ),
            tags$div(class = "clearBoth")
        )
    ) # fluidrow
)
