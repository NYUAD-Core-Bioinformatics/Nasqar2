tabItem(
    tabName = "filter_and_trim_tab",
    fluidRow(
        column(
            8,
            box(
                title = "DADA2 Quality Filtering Parameters", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,
                column(12,
                p('Filter and trim reads to remove low-quality bases and noisy reads, ensuring that only high-quality data is processed.')),
                column(
                    6,


                    # Input for truncLen for single-end
                    numericInput("truncLen_fwd",
                        label = "Forward Read Truncation Length (truncLen):",
                        value = 240,
                        min = 50,
                        max = 300
                    ),

                    # Input for maxEE for single-end
                    numericInput("maxEE_fwd",
                        label = "Forward Read Max Expected Errors (maxEE):",
                        value = 2,
                        min = 0,
                        max = 10
                    )
                ),
                column(
                    6,
                    # Conditional panel for paired-end sequencing
                    conditionalPanel(
                        condition = "input.seq_type == 'paired'",
                        numericInput("truncLen_rev",
                            label = "Reverse Read Truncation Length (truncLen):",
                            value = 160,
                            min = 50,
                            max = 300
                        ),

                        # Input for maxEE for paired-end

                        numericInput("maxEE_rev",
                            label = "Reverse Read Max Expected Errors (maxEE):",
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
