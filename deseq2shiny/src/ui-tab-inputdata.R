tabItem(
    tabName = "inputdata",
    fluidRow(
        column(
            4,
            box(
                title = "Input data", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # One question decides what the rest of the box shows. The two
                # private-space choices are removed by the server on a public
                # instance.
                radioButtons("data_file_type", "Where is your data?",
                    c(
                        "Upload a counts file" = "countsFile",
                        "Counts file in the private space" = "privateFile",
                        "Example data (single-factor)" = "examplecounts",
                        "Example data (multi-factor)" = "examplecountsfactors",
                        "Reopen a saved project" = "savedProject"
                    ),
                    selected = "countsFile"
                ),

                conditionalPanel(condition = "input.data_file_type=='savedProject'",
                    div(class = "nasqar-project-private",
                        nasqar_project_restore_ui(button_label = "Open project", stacked = TRUE),
                        helpText(style = "margin-top:10px;",
                                 "Input, settings and results come back without recomputing.")
                    )
                ),

                conditionalPanel(condition = "input.data_file_type=='privateFile'",
                    div(
                        style = "display:flex; gap:8px; align-items:flex-end;",
                        div(style = "flex:1;",
                            textInput("private_counts_path", "Counts file (.csv/.txt) in the private space", value = "")
                        ),
                        div(style = "margin-bottom:15px;", shinyFiles::shinyFilesButton("browse_private_counts", "Browse...", "Choose a counts file in the private space", multiple = FALSE))
                    ),
                    helpText("The file is read in place; nothing is uploaded.")
                ),

                conditionalPanel(condition = "input.data_file_type=='countsFile' || input.data_file_type=='privateFile'",
                    radioButtons("gene_alias", "Raw counts matrix format",
                        c(
                            "My matrix contains GeneIDs and GeneSymbols" = "included",
                            "My matrix contains only GeneIds OR GeneSymbols" = "notincluded"
                
                        ),
                        selected = "notincluded"
                    )
                ),

                conditionalPanel(condition = "input.data_file_type=='countsFile'",
                    p(".csv/.txt counts file (tab or comma delimited)"),
                    fileInput("datafile", "",
                        accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"
                        ), multiple = FALSE
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='examplecounts'",
                    p(
                        "For details on this data, see ",
                        a(href = "https://www.ncbi.nlm.nih.gov/pubmed?term=18978772", target = "_blank", "this publication")
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='examplecountsfactors'",
                    p(
                        "For details on this data, see ",
                        a(href = "https://doi.org/10.1007/978-3-319-07212-8_3", target = "_blank", "this publication")
                    )
                ),
                conditionalPanel(condition = "input.data_file_type!='savedProject'",
                    div(class = "nasqar-project-private",
                        nasqar_project_name_ui(
                            "deseq2-project",
                            label = "Project name",
                            help = paste(
                                "Saved automatically in the private space under this name: after DESeq2 runs,",
                                "and again whenever you change settings."
                            )
                        )
                    )
                )
            ),
            conditionalPanel(
                "output.fileUploaded",
                box(
                    title = "Config & Prefilter",
                    solidHeader = T,
                    status = "primary",
                    width = 12,
                    checkboxInput("no_replicates", "No Replicates", FALSE),
                    conditionalPanel(
                        "!output.noreplicates",
                        p("* For convenience, if this is a single-factor experiment and column names are denoted by underscore replicate number (eg. sampleX_1,sampleX_2, etc ...)"),
                        p("the sample names will be parsed automatically and the conditions table will be set for the next step.")
                    ),
                    conditionalPanel("output.noreplicates",
                        p("Experiments without replicates do not allow for estimation of the dispersion of counts
                                                        around the expected value for each group, which is critical for differential expression analysis."),
                        span("For more details, click ", a("here", href = "https://www.rdocumentation.org/packages/DESeq2/versions/1.12.3/topics/DESeq", target = "_blank")),
                        style = "color:#f56a6a;"
                    ),
                    tags$div(
                        class = "BoxArea2",
                        numericInput("minRowCount",
                            label = "(Optional) Minimum number of counts to include for each gene (Default 0, to include all)",
                            min = 0, value = 0
                        ),
                        actionButton("prefilterCounts", "Filter"),
                        p("This step is not necessary, but can speed up the processing time")
                    ),
                    hr(),
                    actionButton("submit", "Next: Conditions", class = "btn btn-primary")
                )
            )
        ),
        # column
        column(
            8, h2("Gene Counts Table"), hr(),
            withSpinner(dataTableOutput("contents")),
            hr(),
            uiOutput("conditional_output")
           
        ) # column
    ) # fluidrow
)
