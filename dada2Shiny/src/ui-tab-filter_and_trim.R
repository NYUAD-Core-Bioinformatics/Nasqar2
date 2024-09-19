tabItem(
    tabName = "filter_and_trim_tab",
    fluidRow(
        column(
            8,
            box(
                title = "Sequencing Processing and Analysis: Run DADA2", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,
                column(12,
                    p("The DADA2 pipeline involves several key steps, including filtering, trimming, denoising, merging paired reads, and removing chimeras to ensure the quality of the sequences.")
                ),
                hr(),  # Adds a visual separation
                column(12,
                    h4("Input Parameters for Filtering and Trimming"),  # Header for the input parameters
                    hr()
                
                ),
                column(
                    6,
                    numericInput("truncLen_fwd",
                        label = tagList("Forward Read Truncation Length (truncLen):", 
                                        icon("question-circle", title = "The length at which forward reads are truncated. Longer reads will be trimmed.")),
                        value = 240,
                        min = 50,
                        max = 300
                    ),
                    numericInput("maxEE_fwd",
                        label = tagList("Forward Read Max Expected Errors (maxEE):", 
                                        icon("question-circle", title = "The maximum number of errors allowed in forward reads.")),
                        value = 2,
                        min = 0,
                        max = 10
                    )
                ),
                column(
                    6,
                    conditionalPanel(
                        condition = "input.seq_type == 'paired'",
                        numericInput("truncLen_rev",
                            label = tagList("Reverse Read Truncation Length (truncLen):", 
                                            icon("question-circle", title = "The length at which reverse reads are truncated. Longer reads will be trimmed.")),
                            value = 160,
                            min = 50,
                            max = 300
                        ),
                        numericInput("maxEE_rev",
                            label = tagList("Reverse Read Max Expected Errors (maxEE):", 
                                            icon("question-circle", title = "The maximum number of errors allowed in reverse reads.")),
                            value = 2,
                            min = 0,
                            max = 10
                        )
                    )
                ),
    
                actionButton("runDADA2", "Run DADA2", class = "btn-info btn-success", style = "width: 100%")
            )
        )
    ),

    fluidRow(
        column(
            12,
            conditionalPanel(condition = "output.dada2object_ready === true",
            box(
                title = "Fragments Summary Table", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                withSpinner(dataTableOutput("filterAndTrim_output_table"))
            ))
        )
    )
)
