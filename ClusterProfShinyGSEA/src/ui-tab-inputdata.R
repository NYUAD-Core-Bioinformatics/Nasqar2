tabItem(
    tabName = "datainput",
    hr(),
    fluidRow(
        column(
            6,
            box(
                title = "1. Input data", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # One question decides what the rest of the box shows. The two
                # private-space choices are removed by the server on a public
                # instance.
                nasqar_project_step_ui("gsea-project"),
                radioButtons("data_file_type", "Where is your data?",
                    c(
                        "Upload a results table" = "csvfile",
                        "Table in the private space" = "privatefile",
                        "Example data" = "examplecounts"
                    ),
                    selected = "csvfile"
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='privatefile'",
                    div(
                        style = "display:flex; gap:8px; align-items:flex-end;",
                        div(style = "flex:1;",
                            textInput("private_file_path", "Table (.csv/.txt) in the private space", value = "")
                        ),
                        div(style = "margin-bottom:15px;", shinyFiles::shinyFilesButton("browse_private_file", "Browse...", "Choose a table in the private space", multiple = FALSE))
                    ),
                    helpText("The file is read in place; nothing is uploaded.")
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='csvfile'",
                    fileInput("datafile", "Results table (.csv)", multiple = F)
                ),
            ),
            conditionalPanel(
                "output.fileUploaded",
                box(
                    title = "2. Select genes", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "iParamsbox",
                    wellPanel(
                        column(
                            6,
                            selectInput("geneColumn", "Select Genes column:", choices = c("sdfs", "dfs"))
                        ),
                        column(
                            6,
                            selectInput("log2fcColumn", "Select Log2FC column:", choices = c("sdfs", "dfs"))
                        ),
                        # column(4,
                        #        selectInput("padjColumn","Select padj column:", choices = c("sdfs","dfs"))
                        # ),
                        tags$div(class = "clearBoth")
                    ),
                    actionButton("nextInitParams", "Next", class = "btn-info", style = "width: 100%")
                ),
                box(
                    title = "3. Enrichment parameters", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "createGoBox", collapsed = T,
                    wellPanel(
                        column(
                            4,
                            selectInput("organismDb", "Organism:", choices = NULL, selected = NULL)
                        ),
                        column(
                            4,
                            selectInput("keytype", "Keytype:", choices = c(""), selected = NULL)
                        ),
                        column(
                            4,
                            selectInput("ontology", "Ontology:", choices = c("MF", "BP", "CC", "ALL"), selected = "ALL")
                        ),
                        column(
                            4,
                            numericInput("nPerm", "Permutation #s:", value = 1000, min = 1, max = 100000)
                        ),
                        column(
                            4,
                            numericInput("minGSSize", "minGSSize:", value = 10)
                        ),
                        column(
                            4,
                            numericInput("maxGSSize", "maxGSSize:", value = 500)
                        ),
                        column(
                            4,
                            numericInput("pvalCuttoff", "P-Value Cutoff:", value = 0.05)
                        ),
                        column(
                            4,
                            selectInput("pAdjustMethod", "pAdjustMethod:", choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), selected = "none")
                        ),
                        column(
                            12,
                            tags$p(
                                class = "text-muted",
                                "KEGG analysis uses NCBI Gene IDs after converting the selected input key type."
                            )
                        ),
                        column(
                            12,
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
                    actionButton("initGo", "Create gseGO Object", class = "btn-info", style = "width: 100%")
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
                    dataTableOutput("countdataDT")
                )
            ) # bscollapse
        ) # column
    ) # fluidrow
) # tabpanel
