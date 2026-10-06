tabItem(
    tabName = "summary_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Header: INFO fields", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE,
                tags$div(class = "BoxArea2", DT::dataTableOutput("info_table"))
            ),
            box(
                title = "Header: FORMAT fields", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE, collapsed = TRUE,
                tags$div(class = "BoxArea2", DT::dataTableOutput("format_table"))
            ),
            box(
                title = "Samples", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE, collapsed = TRUE,
                tags$div(class = "BoxArea2", DT::dataTableOutput("samples_table"))
            )
        ),
        column(
            6,
            box(
                title = "Contigs (records per contig from the tabix index)", solidHeader = TRUE,
                status = "primary", width = 12, collapsible = TRUE,
                tags$div(class = "BoxArea2", DT::dataTableOutput("contig_table"))
            ),
            box(
                title = "Record statistics", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE,
                p(
                    "Statistics below stream through every record of the VCF once, reading only",
                    "the fixed columns and INFO/DP. Small files are summarised automatically;",
                    "large files start on request because a whole-genome pass can take several minutes."
                ),
                actionButton("compute_summary", "Compute record statistics", class = "btn btn-primary"),
                textOutput("summary_status"),
                conditionalPanel(
                    condition = "output.summary_ready === true",
                    hr(),
                    h4("Variant types"),
                    DT::dataTableOutput("type_counts_table"),
                    h4("FILTER values"),
                    DT::dataTableOutput("filter_counts_table"),
                    h4("QUAL distribution"),
                    DT::dataTableOutput("qual_bins_table"),
                    h4("INFO/DP distribution"),
                    DT::dataTableOutput("dp_bins_table")
                )
            )
        )
    )
)
