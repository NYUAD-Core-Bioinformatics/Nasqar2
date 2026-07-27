tabItem(
    tabName = "datainput",
    hr(),
    fluidRow(
        column(
            6,
            box(
                title = "Upload Data", solidHeader = T, status = "success", width = 12, collapsible = T, id = "uploadbox",

                # downloadLink("instructionspdf",label="Download Instructions (pdf)"),
                radioButtons("data_file_type", "Use example file or upload your own data",
                    c(
                        "Upload .CSV" = "csvfile",
                        "Example Data" = "examplecounts"
                    ),
                    selected = "csvfile"
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='csvfile'",
                    p("CSV counts file")
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='csvfile'",
                    fileInput("datafile", "Choose File(s) Containing Data", multiple = TRUE)
                )
            ),
            conditionalPanel(
                "output.fileUploaded",
                box(
                    title = "Initialize Parameters", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "iParamsbox",
                    wellPanel(
                        column(
                            4,
                            selectInput("geneColumn", "Select Genes column:", choices = c("sdfs", "dfs"))
                        ),
                        column(
                            4,
                            selectInput("log2fcColumn", "Select Log2FC column:", choices = c("sdfs", "dfs"))
                        ),
                        column(
                            4,
                            selectInput("padjColumn", "Select padj column:", choices = c("sdfs", "dfs"))
                        ),
                        column(
                            6,
                            numericInput("padjCutoff", "padj cutoff:", value = 0.05)
                        ),
                        column(
                            6,
                            numericInput("logfcCuttoff", "min log2 fold change :", value = 2)
                        ),
                        tags$div(class = "clearBoth"),
                        column(
                            12,
                            tags$hr(),
                            checkboxInput("usePreferredGenes", "Use a preferred gene list (subset of threshold-filtered genes)", value = FALSE)
                        ),
                        conditionalPanel(
                            condition = "input.usePreferredGenes == true",
                            column(
                                12,
                                tags$p(
                                    tags$b("Preferred Gene List:"),
                                    " Paste gene names (one per line) below. Only genes present in both this list ",
                                    "and the threshold-filtered results will be used for enrichment."
                                ),
                                textAreaInput(
                                    "preferredGeneList",
                                    label = NULL,
                                    placeholder = "Paste gene names here, one per line...",
                                    rows = 6,
                                    width = "100%"
                                )
                            )
                        ),
                        tags$div(class = "clearBoth")
                    ),
                    actionButton("nextInitParams", "Next", class = "btn-info", style = "width: 100%")
                ),
                box(
                    title = "EnrichGO object Parameters", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "createGoBox", collapsed = T,
                    wellPanel(
                        column(
                            6,
                            selectInput("organismDb", "Organism:", choices = NULL, selected = NULL)
                        ),
                        column(
                            6,
                            selectInput("keytype", "Keytype:", choices = c(""), selected = NULL)
                        ),
                        column(
                            6,
                            selectInput("ontology", "Ontology:", choices = c("MF", "BP", "CC", "ALL"), selected = "BP")
                        ),
                        conditionalPanel(
                            condition = "input.ontology == 'ALL'",
                            column(
                                6,
                                checkboxInput("poolGo", "Pool terms across ontologies", value = FALSE)
                            )
                        ),
                        column(
                            4,
                            numericInput("minGSSize", "minGSSize:", value = 5)
                        ),
                        column(
                            4,
                            numericInput("maxGSSize", "maxGSSize:", value = 500)
                        ),
                        column(
                            6,
                            numericInput("pvalCuttoff", tags$span(
                                "P-Value Cutoff: ",
                                tags$span(class = "help-tip",
                                    icon("question-circle"),
                                    tags$span(class = "help-tip-content",
                                        "Filters which pathways enter the results table. Pathways with p.adjust above this threshold are excluded. Since the table is already p-value filtered, all visible rows have passed this cutoff — including manually selected ones."
                                    )
                                )
                            ), value = 0.05)
                        ),
                        column(
                            6,
                            numericInput("qvalCuttoff", tags$span(
                                "Q-Value Cutoff: ",
                                tags$span(class = "help-tip",
                                    icon("question-circle"),
                                    tags$span(class = "help-tip-content",
                                        "Secondary FDR filter (Storey's q-value). NOT applied to the results table — only used by plot functions internally. In auto mode (top-N), plots respect this cutoff. When you manually select rows in the enrichKEGG tab, this cutoff is bypassed so your selection is always fully plotted."
                                    )
                                )
                            ), value = 0.1)
                        ),
                        column(
                            6,
                            selectInput("pAdjustMethod", tags$span(
                                "pAdjustMethod: ",
                                tags$span(class = "help-tip",
                                    icon("question-circle"),
                                    tags$span(class = "help-tip-content",
                                        "Multiple testing correction for p-values. 'none' = p.adjust equals raw p-value (use only when relying on q-value for FDR). BH (Benjamini-Hochberg) is recommended for most analyses."
                                    )
                                )
                            ), choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), selected = "none")
                        ),
                        tags$div(class = "clearBoth"),
                        column(
                            12,
                            tags$hr(),
                            checkboxInput("simplifyGo", "Simplify GO results (remove redundant terms)", value = FALSE)
                        ),
                        conditionalPanel(
                            condition = "input.simplifyGo == true",
                            column(
                                6,
                                sliderInput("simplifyCutoff", "Similarity cutoff:", min = 0.1, max = 1.0, value = 0.7, step = 0.05)
                            )
                        ),
                        tags$div(class = "clearBoth")
                    ),
                    actionButton("initGo", "Create EnrichGO Object", class = "btn-info", style = "width: 100%")
                )
            )
        ), # column
        column(
            6,
            bsCollapse(
                id = "input_collapse_panel", open = "data_panel", multiple = FALSE,
                bsCollapsePanel(
                    title = "Data Contents Table:", value = "data_panel",
                    p("Note: if there are more than 20 columns, only the first 20 will show here"),
                    textOutput("inputInfo"),
                    withSpinner(dataTableOutput("countdataDT"))
                )
            ) # bscollapse
        ) # column
    ) # fluidrow
) # tabpanel
