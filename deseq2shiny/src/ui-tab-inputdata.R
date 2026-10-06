tabItem(
    tabName = "inputdata",
    fluidRow(
        column(
            4,
            box(
                title = "Upload Gene Counts", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # h4("Upload Gene Counts"),
                h4("(select .CSV)"),
                radioButtons("data_file_type", "Use example file or upload your own data",
                    c(
                        "Upload Counts File" = "countsFile",
                        "Example Data (single-factor)" = "examplecounts",
                        "Example Data (multi-factor)" = "examplecountsfactors",
                        "Use a counts file in the private space" = "hpcFile"
                    ),
                    selected = "countsFile"
                ),

                conditionalPanel(condition = "input.data_file_type=='hpcFile'",
                    div(
                        style = "display:flex; gap:8px; align-items:flex-end;",
                        div(style = "flex:1;",
                            textInput("hpc_counts_path", "Counts file (.csv/.txt) in the private space", value = "")
                        ),
                        div(style = "margin-bottom:15px;", shinyFiles::shinyFilesButton("browse_hpc_counts", "Browse...", "Choose a counts file in the private space", multiple = FALSE))
                    ),
                    helpText("The file is read in place; nothing is uploaded.")
                ),

                conditionalPanel(condition = "input.data_file_type=='countsFile' || input.data_file_type=='hpcFile'",
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
                    # fileInput('datafile', '',
                    #           accept=c('text/csv',
                    #                    'text/comma-separated-values,text/plain',
                    #                    '.csv'),multiple = FALSE
                    # )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='examplecountsfactors'",
                    p(
                        "For details on this data, see ",
                        a(href = "https://doi.org/10.1007/978-3-319-07212-8_3", target = "_blank", "this publication")
                    )
                ),
                nasqar_project_ui(
                    "deseq2-project",
                    help = paste(
                        "The loaded counts, design, DESeq2 results and plot settings are saved under this",
                        "name in the private space (state.RData), automatically after DESeq2 runs and again",
                        "whenever you change settings, so the project can be restored later."
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
